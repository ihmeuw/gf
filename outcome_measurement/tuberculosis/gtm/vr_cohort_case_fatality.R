############################################################################################
# GTM TB mortality comparison between raw VR, redistributed VR, and cohort data            #
# Author - J Ross (with pieces from Irena's code)                                          #
# Sept 10, 2018                                                                            #
#                                                                                          #
# Run on the cluster to access shared drive for VR files, or change dirs to a local copy   #
#
#############################################################################################

#Still to work on cleaning up the final variable names for ease of interpretation
#Oct 18 - look at splitting the case notifications by HIV status

rm(list=ls())
library(data.table)
library(ggplot2)


j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j') # have to declare the J Drive differently on the cluster 
vr_dir <-  ('/ihme/geospatial/vr_prep/cod/outputs/gtm_collaborators/')
cohort_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
out_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')


cohort <- fread(paste0(cohort_dir, "GTM - Tx cohort data 2012-2016.csv"))
deaths <- fread(paste0(out_dir, "deaths_vr_cohort.csv"))
notif<-fread("J:/Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/GTM - TB notifications 2012-2017 deidentified.csv")
gbd_tb<-fread("J:/Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/tb_no_hiv_bothsex_inc_count.csv")
gbd_tb_hiv<-fread("J:/Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/hiv_tb_incidence_counts.csv")

##Case fatality plausibility analysis


deaths$V1<-NULL

#Reshape long to wide to facilitate subtraction
setkey(deaths, year_id, cause, source)
deaths[CJ(unique(year_id), unique(source), unique(cause))]
wide<- deaths[CJ(unique(year_id), unique(cause), unique(source))][, as.list(deaths), by = (year_id)]
setnames(wide, c("year_id", "cohort_all", "raw_vr_all", "redist_vr_all", "cohort_tb", "raw_vr_tb", "redist_vr_tb", "cohort_tbhiv", "raw_vr_tbhiv", "redist_vr_tbhiv"))

#Do subtraction for absolute counts
wide[,tb_raw_vr:=raw_vr_tb - cohort_tb]
wide[,tb_redist_vr:=redist_vr_tb - cohort_tb]
wide[,tb_hiv_raw_vr:=raw_vr_tbhiv - cohort_tbhiv]
wide[,tb_hiv_redist_vr:=redist_vr_tbhiv - cohort_tbhiv]

diff_counts<-wide[,c("year_id","tb_raw_vr", "tb_redist_vr", "tb_hiv_raw_vr", "tb_hiv_redist_vr")]
diff_counts<-na.omit(diff_counts)

wide[,tb_perc_raw_vr:=tb_raw_vr/cohort_tb*100]
wide[,tb_perc_redist_vr:=tb_redist_vr/cohort_tb*100]
wide[,tb_hiv_perc_raw_vr:=tb_hiv_raw_vr/cohort_tbhiv*100]
wide[,tb_hiv_perc_redist_vr:=tb_hiv_redist_vr/cohort_tbhiv*100]

perc_counts<-wide[,c("year_id","tb_perc_raw_vr","tb_perc_redist_vr","tb_hiv_perc_raw_vr","tb_hiv_perc_redist_vr")]
perc_counts<-na.omit(perc_counts)

#reshape long for plotting
long_counts <- melt(diff_counts, measure.vars=c("tb_raw_vr", "tb_redist_vr", "tb_hiv_raw_vr", "tb_hiv_redist_vr"),
                    variable.name = "measure", value.name = "difference")
#write.csv(long_counts, file=paste0(out_dir, 'long_deaths_vr_cohort.csv'))
long_perc <- melt(perc_counts, measure.vars = c("tb_perc_raw_vr","tb_perc_redist_vr","tb_hiv_perc_raw_vr","tb_hiv_perc_redist_vr"),
                  variable.name = "measure", value.name = "percentage")

#Make intermediate plot of differences in deaths
#long_counts <- fread("J:/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/long_deaths_vr_cohort.csv")
long_counts[, measure:=reorder(measure, -difference)]
p1 <- ggplot(long_counts, aes(x = year_id, y= difference, color=measure)) + 
  geom_line(size = 0.75) +
  ggtitle("Difference between VR and cohort deaths") +
  labs(x = "Year", y = "Deaths") +
  theme_minimal()
p1

long_perc[,measure:=reorder(measure, -percentage)]
p2 <- ggplot(long_perc, aes(x = year_id, y= percentage, color=measure)) + 
  geom_line(size = 0.75) +
  ggtitle("Percent difference between VR and cohort deaths") +
  labs(x = "Year", y = "Percent") +
  theme_minimal()
p2

#Move from deaths to incident cases----------------------------------------------------------------------------------------------------
#Next comparisons are between estimated incident cases, notified cases, and cohort incident cases

#Now working with cohort data
#Subset to avoid overlapping types 
tb_denom <- cohort[table %in% c('Nuevos Pulmonares BK+', 
                                'Nuevos Pulmonares BK-', 'Nuevos Pediatricos', 
                                'Nuevos Extrapulmonares', 'Nuevos TB/VIH', 'Retratamiento')]

#Include only the annual totals and drop the trimester subtotals
#Note that deptocode==0 is the whole country data. Can exclude 0 to look by department.
tb_denom <- tb_denom[col_name=='TOTAL' & deptocode==0]
tb_solo <- tb_denom[table!='Nuevos TB/VIH']
tb_hiv <- tb_denom[table=='Nuevos TB/VIH']

#Sum by TB and TB/HIV per year
annual_all_forms <- tb_denom[, list(cohort_all_notif=sum(value,na.rm=TRUE)), by='year']
annual_tb_solo <- tb_solo[, list(cohort_tb_notif=sum(value,na.rm=TRUE)), by='year']
annual_tb_hiv <- tb_hiv[, list(cohort_tb_hiv_notif=sum(value,na.rm=TRUE)), by='year']

#Compare to notifications
notif<-notif[,c("MUNICIPIO","DEPARTAMENTO", "SEXO", "EDAD", "CONDICIONINGRESO", "CLASIFICACION", "YEAR", "VIH")]
notif<-notif[notif$CONDICIONINGRESO=="nuevo"|notif$CONDICIONINGRESO=="recaida",]

#Challenge in splitting case notifications by HIV status is that HIV status is not known for many persons.
#This comparison may not be feasible for notifications dataset because of the HIV issue
table(notif$VIH, notif$YEAR, exclude=NULL)
#Recode the deaths as unknown HIV status - ask for help in the analagous DT code
notif[VIH=="Death", VIH := NA] 
annual<-setDT(notif)[, .(count = .N), by = c("VIH", "YEAR")]
#Simplest assumption is that the persons with unknown HIV status will have the same likelihood of testing positive as any TB patient, which is ~8% by WHO estimates.
#Reshape wide to do this re-assigment
setkey(annual, YEAR, VIH)
annual[CJ(unique(YEAR), unique(VIH))]
wide_n<- annual[CJ(unique(YEAR), unique(VIH))][, as.list(count), by = (YEAR)]
setnames(wide_n, c("year", "missing", "not reactive", "reactive"))
wide_n[,rev_nr:=`not reactive`+0.92*missing]
wide_n[,rev_r:=reactive+0.08*missing]
#Checks
#wide_n[,orig_total:=missing+`not reactive`+reactive]
#wide_n<-wide_n[,new_total:=rev_nr+rev_r]
#wide_n<-wide_n[,diff:=orig_total-new_total]
#table(wide_n$diff)

#Merge cohort and notifications for incident cases
inc<-merge(annual_tb_solo, wide_n, by=c("year"))
inc<-merge(annual_tb_hiv, inc, by=c("year"))

#Prep the GBD estimates for merging
colnames(gbd_tb)[colnames(gbd_tb)=="Year"] <- "year"
colnames(gbd_tb)[colnames(gbd_tb)=="Value"]<- "gbd_tb_case"
cols = c("year", "gbd_tb_case")
gbd_tb[, .SD, .SDcols = cols] #Not working
gbd_tb<-gbd_tb[,c(year, gbd_tb_case)]
colnames(gbd_tb_hiv)[colnames(gbd_tb_hiv)=="year_id"] <- "year"


#Merge 

#Calculate difference in deaths over difference in cohort - notifications to estimate case fatality for persons not started on treatment.

