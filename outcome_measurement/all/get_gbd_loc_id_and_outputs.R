# Getting GBD locations and outputs for PCE                                                            #
# J. Ross                                     
# July 13, 2018    
#
# Run on the cluster to use central functions.
#
# This code pulls GBD 2016 HIV-TB incidence rates from a process version of COMO because the final model        #
# had zero values in some places from a late-stage HIV correction. Process version 563 is the model    #
# used by GBD TB team to populate 2016 TB-HIV incidence values (confirmed with Emilie Maddison).       #
# We should be able to use final results from GBD 2017 when they are available.                         #
#                                                                                                      #
########################################################################################################

library(data.table)
library(ggplot2)

# Define J header
if (Sys.info()[1]=="Windows"){
  j_head <- "J:/"
} else {
  j_head <- "/home/j/"
}


##This quick little section gives the GBD location ID's for our three countries of interest--------------------------------------------
source(paste0(j_head,"temp/central_comp/libraries/current/r/get_location_metadata.R"))

df <- get_location_metadata(location_set_id = 1) #set id = 1 is reporting hierarchy
head(df)
pce_locs <- df[df$location_name=="Guatemala"|
                 df$location_name=="Democratic Republic of the Congo"|
                 df$location_name=="Uganda"|
                 df$location_name=="El Salvador"|
                 df$location_name=="Honduras"|
                 df$location_name=="Belize",]
#El Salvador = 127
#GTM = 128
#Honduras = 129
#Belize = 108
#COD = 171
#UGA = 190

#----------------------------------------------------------------------------------------------------------------------------------------

#This section uses the central function 'get_outputs' to pull GBD results from the central db

source("/home/j/temp/central_comp/libraries/current/r/get_outputs.R")

#GBD 2016 results
#Cause IDs are for HIV with drug-susceptible, MDR, and XDR TB respectively
inc2016 <- get_outputs("cause",cause_id=c(948,949,950), location_id=128,age_group_id=27, sex_id="all", 
                       year_id=c(2000:2016), measure_id=6, metric_id=3, gbd_round_id=4, process_version_id=563)


#write.csv(inc2016, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/hiv_tb_incidence_rates.csv"))

#Note that trying to pull incident counts (metric_id=1) returns all NA's. Perhaps these are populated at a later stage.
#Can get back to counts by multiplying incidence rate by GBD population.

#This specifies GBD round = 5, which is GBD 2017 populations
#Use GBD round = 4 for GBD 2016
source("/home/j/temp/central_comp/libraries/current/r/get_population.R")
pops <- get_population(age_group_id = 22, year_id=c(2000:2017), location_id=128, sex_id = "all", gbd_round_id = 5)
pop_bothsex <- pops[pops$sex_id==3,]
#write.csv(pop_bothsex, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/pops_bothsex.csv"))

#Merge incidence rates and population to get back to counts
inc_for_merge <- inc2016[,c("sex_id", "year_id", "cause_id", "cause_name", "sex", "val", "upper", "lower")]
pop_for_merge <- pops[, c("year_id", "sex_id", "population")]

counts<- merge(inc_for_merge, pop_for_merge, by=c("sex_id", "year_id"))

#Multiply incidence by population to get count. Not sure if this would be correct for the CI's.
counts[,inc_count:=val*population]
counts <- counts[order(sex_id,cause_id, year_id)]

#write.csv(counts, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/hiv_tb_incidence_counts.csv"))

#---------------------------------------------------------------------------------------------------------------------------------------

#GBD 2017 results are now updated 
#Cause IDs 948, 949, 950 are for HIV with drug-susceptible, MDR, and XDR TB respectively
Inc_hivtb2017 = get_outputs("cause",cause_id=c(948,949,950), location_id=128,age_group_id=27, sex_id=3, year_id=c(2000:2017), measure_id=6, metric_id=3, gbd_round_id=5, version = 'latest')
#write.csv(Inc_hivtb2017, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/hiv_tb_incidence_rates.csv"))
Inc_counts_hivtb2017 = get_outputs("cause",cause_id=c(948,949,950), location_id=128,age_group_id=27, sex_id=3, year_id=c(2000:2017), measure_id=6, metric_id="all", gbd_round_id=5, version = 'latest')
#The above will run, but returns NA's for counts

#Cause ID 297 also includes latent TB, but no incidence is estimated for LTBI (only prevalence), so below works.
#If wanting to double-check, then use cause_ids 934(ds TB), 946 (mdrtb), and 947 (xdr tb)
Inc_tb2017 = get_outputs("cause",cause_id=c(297), location_id=128,age_group_id=27, sex_id=3, year_id=c(2000:2017), measure_id=6, metric_id=3, gbd_round_id=5, version = 'latest')
#write.csv(Inc_tb2017, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/tb_incidence_rates.csv"))
Inc_dstb2017 = get_outputs("cause",cause_id=c(934), location_id=128,age_group_id=27, sex_id=3, year_id=c(2000:2017), measure_id=6, metric_id=3, gbd_round_id=5, version = 'latest')
#write.csv(Inc_dstb2017, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/tb_ds_incidence_rates.csv"))
Inc_mdrtb2017 = get_outputs("cause",cause_id=c(946), location_id=128,age_group_id=27, sex_id=3, year_id=c(2000:2017), measure_id=6, metric_id=3, gbd_round_id=5, version = 'latest')
#write.csv(Inc_mdrtb2017, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/tb_mdr_incidence_rates.csv"))

tbinc_count <- merge(Inc_tb2017, pop_for_merge, by=c("year_id", "sex_id"))
tbinc_count[,inc_count:=val*population]
tbinc_count[,inc_upper:=upper*population]
tbinc_count[,inc_lower:=lower*population]
#write.csv(tbinc_count, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/tb_incidence_counts.csv"))

tbdsinc_count <- merge(Inc_dstb2017, pop_for_merge, by=c("year_id", "sex_id"))
tbdsinc_count[,inc_count:=val*population]
tbdsinc_count[,inc_upper:=upper*population]
tbdsinc_count[,inc_lower:=lower*population]
#write.csv(tbdsinc_count, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/tb_ds_incidence_counts.csv"))

tbmdrinc_count <- merge(Inc_mdrtb2017, pop_for_merge, by=c("year_id", "sex_id"))
tbmdrinc_count[,inc_count:=val*population]
tbmdrinc_count[,inc_upper:=upper*population]
tbmdrinc_count[,inc_lower:=lower*population]
#write.csv(tbmdrinc_count, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/tb_mdr_incidence_counts.csv"))

tbhivinc_count <- merge(Inc_hivtb2017, pop_for_merge, by=c("year_id", "sex_id"))
tbhivinc_count[,inc_count:=val*population]
tbhivinc_count[,inc_upper:=upper*population]
tbhivinc_count[,inc_lower:=lower*population]
#write.csv(tbhivinc_count, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/tbhiv_incidence_counts.csv"))

#GBD 2017 Mortality
HIVTBdeaths2017 = get_outputs("cause",cause_id=c(948,949,950), location_id=128,age_group_id=27, sex_id="all", year_id=c(2000:2017), measure_id=1, metric_id=3, gbd_round_id=5, version = 'latest')
#write.csv(HIVTBdeaths2017, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/hiv_tb_mort_rates.csv"))

#Cause_id 297 is TB (without HIV) including all drug resistance categories
TBdeaths2017 = get_outputs("cause",cause_id=c(297), location_id=128,age_group_id=27, sex_id="all", year_id=c(2000:2017), measure_id=1, metric_id=3, gbd_round_id=5, version = 'latest')
#write.csv(TBdeaths2017, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/tb_mort_rates.csv"))
#Should be allowed to change metric_id to 1 to get counts, but this isn't working as of Oct 7, 2018.

tbdeath_count <- merge(TBdeaths2017, pop_for_merge, by=c("year_id", "sex_id"))
tbdeath_count[,death_count:=val*population]
tbdeath_count[,deaths_upper:=upper*population]
tbdeath_count[,deaths_lower:=lower*population]
#write.csv(tbdeath_count, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2017/tb_mort_counts.csv"))

#---------------------------------------------------------------------------------------------------------------------------------------

#Room for plotting
