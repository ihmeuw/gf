############################################################################################
# GTM TB mortality comparison between raw VR, redistributed VR, and cohort data            #
# Author - J Ross (with pieces from Irena's code)                                          #
# Sept 10, 2018                                                                            #
#                                                                                          #
# Run on the cluster to access shared drive for VR files, or change dirs to a local copy   #
#
#############################################################################################

#Still to work on cleaning up the final variable names for ease of interpretation

rm(list=ls())
library(data.table)
library(ggplot2)
library(readxl)

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j') # have to declare the J Drive differently on the cluster 
vr_dir <-  ('/ihme/geospatial/vr_prep/cod/outputs/gtm_collaborators/')
cohort_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
out_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
who_dir <- paste0(j, 'Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/WHO/')

vrData <- data.table(fread(paste0(vr_dir, "formatted_20180716.csv")))
redistVR <- data.table(fread(paste0(vr_dir, "redistribution_20180716.csv")))
cohort <- data.table(fread(paste0(cohort_dir, "GTM - Tx cohort data 2012-2016.csv")))
who_estimates <- data.table(read_excel(paste0(who_dir, 'WHO_gtm_burden_2018.xlsx')))

## subset TB deaths from the formatted VR data that has not been redistributed
#The following codes all map to cause_id 934 (drug-susceptible TB) except for 104471, which maps to 946 (MDR-TB), but this 
# code_id==104471 is not present within the Guatemala VR data. The most frequently used code in these data is 117, which
# corresponds to TB of the lung without bacterial confirmation = ICD10 code A16.2
# ----------------------------------------------
tb_death_ids <- c(97,
                  98,
                  99,
                  100,
                  101,
                  #102, looks like this is a garbage code mapping to 743
                  103,
                  104,
                  105,
                  106,
                  107,
                  108,
                  109,
                  110,
                  111,
                  112,
                  113,
                  114,
                  115,
                  116,
                  117,
                  118,
                  119,
                  120,
                  121,
                  122,
                  123,
                  124,
                  125,
                  126,
                  127,
                  128,
                  129,
                  130,
                  131,
                  132,
                  133,
                  134,
                  135,
                  136,
                  137,
                  138,
                  139,
                  140,
                  141,
                  142,
                  143,
                  144,
                  145,
                  146,
                  147,
                  148,
                  149,
                  150,
                  151,
                  152,
                  153,
                  154,
                  155,
                  156,
                  157,
                  158,
                  159,
                  160,
                  161,
                  162,
                  163,
                  164,
                  165,
                  166,
                  167,
                  168,
                  169,
                  170,
                  171,
                  172,
                  173,
                  174,
                  175,
                  176,
                  177,
                  178,
                  1339,
                  1340,
                  1341,
                  1342,
                  1343,
                  1344,
                  13990,
                  14223,
                  18433,
                  24801,
                  104471)

#This single code corresponds to ICD10 code B20.0, HIV death due to mycobacterial infection
tb_hiv_death_id <- 895

# get the codes from the "cause_ids" csv 
tb_death_causes <- c(297, #TB without HIV (includes latent, drug-susceptible, MDR, and XDR)
                     954, #latent TB - no deaths attributed here
                     934, #drug-susceptible TB
                     946, #MDR-TB
                     947 #XDR - TB
)                    
tb_hiv_death_causes <- c(
  948,# HIV/TB
  949, #HIV/TB - MDR w/out extensive drug resistance
  950 #HIV/TB - extensively drug-resistant TB
)

#vrTb and vrTb_HIV refer to the raw VR data pre-redistribution. VrTb_causes and vrTb_HIV_causes refer to post-redistribution
vrTb <- vrData[code_id %in% tb_death_ids]
vrTb_HIV <-vrData[code_id %in% tb_hiv_death_id]
vrTb_causes <-redistVR[cause_id%in%tb_death_causes]
vrTb_HIV_causes <-redistVR[cause_id %in% tb_hiv_death_causes]

#Make annual totals and assign cause and datasource variables
tb_pre= vrTb[, list(deaths=sum(na.omit(deaths))), by='year_id']
tb_pre[,cause:='TB']
tb_pre[,source:='Raw VR']

tb_post= vrTb_causes[, list(deaths=sum(na.omit(deaths))), by='year_id']
tb_post[,cause:='TB']
tb_post[,source:="Redistributed VR"]

tb_hiv_pre= vrTb_HIV[, list(deaths=sum(na.omit(deaths))), by='year_id']
tb_hiv_pre[,cause:='TB/HIV']
tb_hiv_pre[,source:="Raw VR"]

tb_hiv_post= vrTb_HIV_causes[, list(deaths=sum(na.omit(deaths))), by='year_id']
tb_hiv_post[,cause:='TB/HIV']
tb_hiv_post[,source:='Redistributed VR']

deaths <- rbind(tb_pre, tb_post, tb_hiv_pre, tb_hiv_post)

#Calculate all forms (TB+ TB/HIV) deaths by merging TB and TB/HIV
full_pre<-merge(tb_pre, tb_hiv_pre, by=c("year_id"))
full_pre[,deaths:=deaths.x+deaths.y]
full_pre[,cause:='All Forms']
full_pre <- full_pre[, .(year_id = year_id, cause=cause, deaths=deaths, source=source.x)]


full_post <-merge(tb_post, tb_hiv_post, by=c("year_id"))
full_post[,deaths:=deaths.x+deaths.y]
full_post[,cause:='All Forms']
full_post <- full_post[, .(year_id = year_id, cause=cause, deaths=deaths, source=source.x)]

#Append all forms estimates to cause-specific
deaths <- rbind(deaths, full_pre, full_post)
#deaths dataset is now long-form for annual death counts by cause (TB, TB/HIV, or All Forms) and data source

#NOW SWITCH TO COHORT DATA-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Cohort data get read-in at the top with the rest of the data

#Subset to avoid overlapping types 
tb_denom <- cohort[table %in% c('Nuevos Pulmonares BK+', 
                                'Nuevos Pulmonares BK-', 'Nuevos Pediatricos', 
                                'Nuevos Extrapulmonares', 'Nuevos TB/VIH', 'Retratamiento')]

#Include only the annual totals and drop the trimester subtotals
#Note that deptocode==0 is the whole country data. Can exclude 0 to look by department.
tb_denom <- tb_denom[col_name=='TOTAL' & deptocode==0]
tb_solo <- tb_denom[table!='Nuevos TB/VIH']
tb_hiv <- tb_denom[table=='Nuevos TB/VIH']

#Check that totals are in the ballpark
tb_denom[year==2016, sum(value,na.rm=T)]

# Initial result - Total of new and retreatment cases in the cohorts per year----------------------------------------------------------------------------
# This isn't strictly necessary for the mortality analysis, but it seems that it will come in handy 
# at some point for notifications
annual_all_forms <- tb_denom[, list(value=sum(value,na.rm=TRUE)), by='year']
annual_tb_solo <- tb_solo[, list(value=sum(value,na.rm=TRUE)), by='year']
annual_tb_hiv <- tb_hiv[, list(value=sum(value,na.rm=TRUE)), by='year']

#4 categories of row_name_B "COMPLETED TREATMENT", "DEATHS", LOST TO FOLLOW-UP", "REFERRED"
#Is it correct the FRACASO_TERAPEUTICO (treatment failure) is under completed treatment?

#subset deaths
deaths_all_forms <- tb_denom[row_name_B=="DEATHS"]
deaths_tb_solo <- tb_solo[row_name_B=='DEATHS']
deaths_tb_hiv <- tb_hiv[row_name_B=='DEATHS']

##Sum annual deaths and add cause and data source
deaths_a_tb_solo = deaths_tb_solo[, list(deaths=sum(value,na.rm=TRUE)), by='year']
deaths_a_tb_solo[,cause:='TB']
deaths_a_tb_solo[,source:='Cohort']

deaths_a_tb_hiv = deaths_tb_hiv[, list(deaths=sum(value,na.rm=TRUE)), by='year']
deaths_a_tb_hiv[,cause:='TB/HIV']
deaths_a_tb_hiv[,source:='Cohort']

deaths_a_all_forms = deaths_all_forms[, list(deaths=sum(value,na.rm=TRUE)), by='year']
deaths_a_all_forms[,cause:='All Forms']
deaths_a_all_forms[,source:='Cohort']

#Need to add NAs for 2009 - 2011 since the cohort dataset does not cover these years?
deaths_cohort <- rbind(deaths_a_tb_solo, deaths_a_tb_hiv, deaths_a_all_forms)
deaths_cohort <- deaths_cohort[, .(year_id=year, cause=cause, deaths=deaths, source=source)]

#Append the cohort deaths with VR
deaths <- rbind(deaths, deaths_cohort)

write.csv(deaths, file=paste0(out_dir,'deaths_vr_cohort.csv'))

#Merge with population data to calculate mortality rates
pops <- data.table(fread(paste0(j, "/Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/pops_bothsex.csv")))
years<-c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
pop_years <- pops[year_id%in%years]

full <- merge(deaths, pops, by = 'year_id')
full[,mort_rate:=deaths/population*100000]

mort <- full[, .(year_id, cause, mort_rate, source)]
write.csv(mort, file=paste0(out_dir,'mort_vr_cohort.csv'))

mort <- read.csv(paste0(out_dir, "mort_vr_cohort.csv"))
mort <- as.data.table(mort)
##Plotting------------------------------------------------------------------------------------------------------

#Remove all forms to simplify the plot
mort_simple <- mort[cause!='All Forms',]
deaths_simple <-deaths[cause!='All Forms',]

#------------  One version of the graph:  ------------#
#------------    TB and TB/HIV faceted    ------------#

# set up who_estimates for graphing
who_estimates <- who_estimates[, .(year, e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi, e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi)]
setnames(who_estimates, 'e_mort_exc_tbhiv_100k', 'e_mort_exc_tbhiv_100k_est')
setnames(who_estimates, 'e_mort_tbhiv_100k', 'e_mort_tbhiv_100k_est')

who_estimates <- melt(who_estimates, id.vars= "year")
who_estimates$variable <- as.character(who_estimates$variable)
who_estimates$variable <- gsub("exc_tbhiv", "tb", who_estimates$variable)
who_estimates[, c("cause", "measure") := tstrsplit(variable, "_", keep=c(3, 5))]

who_estimates[cause== 'tb', cause:= "TB"]
who_estimates[cause== 'tbhiv', cause:= "TB/HIV"]

who_estimates <- dcast.data.table(who_estimates, year + cause ~ measure, value.var = "value")
who_estimates <- who_estimates[year >= 2009, ]
colnames(who_estimates) <- c('year', 'cause', 'who_est', 'who_est_high', 'who_est_low')

# merge who_estimates with mort_simple
dt <- merge(mort_simple, who_estimates, by.x=c('year_id', 'cause'), by.y=c('year', 'cause'))

p1 <- ggplot ()+
  geom_line(data=dt, aes(x=year_id, y=mort_rate, colour=source), size=1) +  
  
  geom_line(data=dt, aes(x=year_id, y=who_est, color="WHO estimates"), size=1) +
  geom_ribbon(data=dt, aes(x=year_id, ymin=who_est_low, ymax=who_est_high), alpha=0.1) +
  
  labs(x="Year", y="Mortality rate", title="TB and TB/HIV mortality rates per 100,000 population in Guatemala", color="Source") +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=15), legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20)) +
  scale_x_continuous(breaks= scales::pretty_breaks()) + 
  facet_wrap( ~ cause) +
  scale_color_manual( values=c("tomato", "steelblue1", "purple", "grey"))
p1

#------------ Another version of the graph: ------------#
#-- TB and TB/HIV on the same graph, different linetypes --#

mort_simple$variable <- paste(mort_simple$cause, mort_simple$source, sep=", ")
who_estimates[, variable:= "WHO estimates"]

p1 <- ggplot ()+
  geom_line(data=mort_simple, aes(x=year_id, y=mort_rate, colour=variable, linetype=variable), size=1.2)+ 

  geom_line(data=who_estimates[year>=2009,], aes(x=year, y=e_mort_exc_tbhiv_100k, color=variable, linetype=variable), size=1, alpha=0.5) +
  geom_ribbon(data=who_estimates[year>=2009,],aes(x=year, ymin=e_mort_exc_tbhiv_100k_lo,ymax=e_mort_exc_tbhiv_100k_hi),alpha=0.1) +

  geom_line(data=who_estimates[year>=2009,], aes(x=year, y=e_mort_tbhiv_100k, color=variable, linetype=variable), size=1, alpha=0.5) +
  geom_ribbon(data=who_estimates[year>=2009,],aes(x=year, ymin=e_mort_tbhiv_100k_lo,ymax=e_mort_tbhiv_100k_hi),alpha=0.1) +

  labs(x="Year", y="Mortality rate", title="TB and TB/HIV mortality rates per 100,000 population in Guatemala") +
       # color="Cause", linetype="Source") +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=15), legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20)) +
  # scale_linetype_manual(values=c("solid", "longdash", "dotdash", "dotted")) +
  scale_x_continuous(breaks= scales::pretty_breaks()) +
  
  scale_color_manual( values=c("tomato", "tomato", "tomato", "steelblue1", "steelblue1", "steelblue1", "black"), guide=guide_legend( nrow=3, byrow=F, title =  "Cause and Source" )) +
  scale_linetype_manual( values=c("longdash", "dotdash",  "dotted", "longdash", "dotdash",  "dotted", "solid"), guide=guide_legend( nrow=3, byrow= F, title =  "Cause and Source" )) +
  theme(plot.margin= unit(c(1,5,0.5,1), "cm"), legend.position="bottom", legend.direction= "vertical")
p1
#-----------------------------------------------------#

p2 <- ggplot (data=deaths_simple, aes(x=year_id, y=deaths, colour=cause, linetype=source))+
  geom_line()+
  labs(x="Year", y="Deaths", title="TB and TB/HIV deaths by data source in Guatemala")
p2
