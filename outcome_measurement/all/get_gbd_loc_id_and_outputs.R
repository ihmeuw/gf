########################################################################################################
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
                 df$location_name=="Uganda",]

#GTM = 128
#COD = 171
#UGA = 190

#----------------------------------------------------------------------------------------------------------------------------------------

#This section uses the central function 'get_outputs' to pull GBD results from the central db

source("/home/j/temp/central_comp/libraries/current/r/get_outputs.R")

#GBD 2016 results
#Cause IDs are for HIV with drug-susceptible, MDR, and XDR TB respectively
inc2016 <- get_outputs("cause",cause_id=c(948,949,950), location_id=128,age_group_id=27, sex_id="all", 
                       year_id=c(2000:2016), measure_id=6, metric_id=3, gbd_round_id=4, process_version_id=563)


write.csv(inc2016, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/hiv_tb_incidence_rates.csv"))

#Note that trying to pull incident counts (metric_id=1) returns all NA's. Perhaps these are populated at a later stage.
#Can get back to counts by multiplying incidence rate by GBD population.

#This specifies GBD round = 4, which is GBD 2016 populations
source("/home/j/temp/central_comp/libraries/current/r/get_population.R")
pops <- get_population(age_group_id = 22, year_id=c(2000:2016), location_id=128, sex_id = "all", gbd_round_id = 4)
pop_bothsex <- pops[pops$sex_id==3,]
write.csv(pop_bothsex, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/pops_bothsex.csv"))


#Merge incidence rates and population to get back to counts
inc_for_merge <- inc2016[,c("sex_id", "year_id", "cause_id", "cause_name", "sex", "val", "upper", "lower")]
pop_for_merge <- pops[, c("year_id", "sex_id", "population")]

counts<- merge(inc_for_merge, pop_for_merge, by=c("sex_id", "year_id"))

#Multiply incidence by population to get count. Not sure if this would be correct for the CI's.
counts[,inc_count:=val*population]
counts <- counts[order(sex_id,cause_id, year_id)]

write.csv(counts, paste0(j_head, "Project/Evaluation/GF/outcome_measurement/gtm/TUBERCULOSIS/gbd_2016/hiv_tb_incidence_counts.csv"))

#---------------------------------------------------------------------------------------------------------------------------------------

#GBD 2017 results - save this for when GBD 2017 results are final.
Inc2017 = get_outputs("cause",cause_id=c(948,949,950), location_id=128,age_group_id=27, sex_id=3, year_id=c(2000:2017), measure_id=6, metric_id=3, gbd_round_id=5, version = 'latest')
#Won't work at the moment because mdr splits haven't been populated


#---------------------------------------------------------------------------------------------------------------------------------------

#Room for plotting
