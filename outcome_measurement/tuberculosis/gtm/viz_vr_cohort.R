###################################################################
# GTM TB mortality comparison between raw VR, redistributed VR, and cohort data
# Run on the cluster to access shared drive for VR files, or change dirs to a local copy
#


rm(list=ls())
library(data.table)
library(ggplot2)


j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j') # have to declare the J Drive differently on the cluster 
# raster_dir = paste0(j, '/Project/Evaluation/GF/covariates/gtm/')
#muni_dir <- paste0(j, '/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/')
#dept_dir <- paste0(j, '/Project/Evaluation/GF/mapping/gtm/outputs/gtm_collaborators/')
vr_dir <-  ('/ihme/geospatial/vr_prep/cod/outputs/gtm_collaborators/')
cohort_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
#export_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/')
#merge_dir <- paste0(j, "/WORK/11_geospatial/11_vr/vr_data_inputs/muni_merges/GTM/")

vrData <- data.table(fread(paste0(vr_dir, "formatted_20180716.csv")))
redistVR <- data.table(fread(paste0(vr_dir, "redistribution_20180716.csv")))
cohort <- data.table(fread(paste0(cohort_dir, "GTM - Tx cohort data 2012-2016.csv")))

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

vrTb <- vrData[code_id %in% tb_death_ids]
vrTb_HIV <-vrData[code_id %in% tb_hiv_death_id]
vrTb_causes <-redistVR[cause_id%in%tb_death_causes]
vrTb_HIV_causes <-redistVR[cause_id %in% tb_hiv_death_causes]


byVars = names(vrTb)[names(vrTb)%in%c('year_id')]
byVars_HIV = names(vrTb)[names(vrTb)%in%c('year_id')]
byVars_causes = names(redistVR)[names(redistVR)%in%c('year_id')]
byVars_HIV_causes = names(redistVR)[names(redistVR)%in%c('year_id')]

tb_pre= vrTb[, list(deaths=sum(na.omit(deaths))), by=byVars]
tb_post= vrTb_causes[, list(deaths_post=sum(na.omit(deaths))), by=byVars]

tb_hiv_pre= vrTb_HIV, list(deaths=sum(na.omit(deaths))), by=byVars]
tb_hiv_post= vrTb_causes[, list(deaths_post=sum(na.omit(deaths))), by=byVars]

full_tb<-merge(tb_pre, tb_post, by=c("year_id"))

pops <- data.table(fread(paste0(j, "/Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/pops_bothsex.csv")))

years<-c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
pop_years <- pops[year_id%in%years]

#Read the prepped cohort data. This is broken up since I had it as part of a script to run on the cluster.
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j') 
cohort_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
cohort <- data.table(fread(paste0(cohort_dir, "GTM - Tx cohort data 2012-2016.csv")))

#subset to annual totals (drop trimester reports) and deaths (drop other outcomes)
cohort1 <- cohort[col_name=="TOTAL" & row_name_B=="DEATHS"]
cohort1 <- cohort1[order(year, deptocode, table)]

#Number of reporting categories varies by year
cohort1[, .N, by = year]

#See that Departments 1, 14 and 17 consistently have more reporting categories than other departments
#I haven't figured out how to code the two-way frequency table in data table
table(cohort1$year, cohort1$deptocode)


cohort2012 <-cohort1[year==2012] #676 obs
cohort2013 <-cohort1[year==2013] #676 obs
cohort2014 <-cohort1[year==2014] #789 obs
cohort2015 <-cohort1[year==2015] #586 obs
cohort2016 <-cohort1[year==2016] #708 obs

table(cohort2012$deptocode)
#Most depts with 23 values per year, but dept 1 with 86 and dept 14 and 17 with 65

table(cohort2013$deptocode)
#Identical pattern to 2012

table(cohort2014$deptocode)
#Most depts with 27 values per year, dept 1 with 99 and dept 14 and 17 with 75

table(cohort2015$deptocode)
#Most depts with 20 values per year, but dept 1 with 74 and depts 14 and 17 with 56

table(cohort2016$deptocode)
#Most depts with 25 values per year, but dept 1 with 82 and depts 14 and 17 with 63
