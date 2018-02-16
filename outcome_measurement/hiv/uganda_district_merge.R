# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/16/2018
# To merge district names and id#s from Uganda Viral Load Dashboard: https://vldash.cphluganda.org/
# This code merges district names and id#s from jan 2017 - jan 2018, which includes all 122 districts
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(gdata)

# --------------------

# ----------------------------------------------
# Files and directories

# data directory

# output file
dir = '/home/j/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# ----------------------------------------------

#store the url for january 2018 viral load data
url = 'https://vldash.cphluganda.org/live/?age_ids=%5B%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=201701&genders=%5B%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%5D&to_date=201801'

#load the data from the url for january 2017 to 2018
jan1718 = fromJSON(url)

#view the entire data set
jan1718

#view the dist_numbers data fram, which includes district numbers, samples received, patients received
jan1718$dist_numbers

#check that the totals in the data frame for samples_received and total_results match CSV totals
#total_results should match 'Samples Tested' on the CSV
colSums(jan1718$dist_numbers)

#create a data frame specific to the district id data 
dist_frame <- data.frame(jan1718$dist_numbers)


#rename the variables 
colnames(dist_frame) <- c("district_id", "samples_received", "patients_received", "suppressed", "valid_results", "samples_rejected", "dbs", "total_results")
dist_frame
dist_frame$district_id
# ----------------------------------------------

#upload the CSV of district names to be merged with the data frame

#to download CSV of all district names, visit Uganda VL Dashboard and set to/from filters to jan 2017-2018
#download the CSV for january 2017 - 2018 to be merged with the data frame for the same month
#rename the file district_names or change file pathway

dist_names <- read.csv("J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/district_merge/district_names.csv")

#rename the variables to correspond with dist_frame
colnames(dist_names) <- c("district", "samples_received", "patients_for_samples", "total_results", "samples_pending", "samples_rejected", "dbs", "plasma")
dist_names


# ----------------------------------------------
#check for duplicates - the sum of duplicates_samples should be zero
duplicate_samples <- duplicated(dist_frame$samples_received)
sum(duplicate_samples)


# ----------------------------------------------
# merge the files on samples_received if there are no duplicates

#merge the two frames on samples_received 
dist_merge <- merge(dist_names, dist_frame, by="samples_received")
dist_merge$district_id 
dist_merge$district

#create a data frame of the district ids and the 
district_list <- data.frame(dist_merge$district, dist_merge$district_id)
colnames(district_list) <- c("district_name","district_id")

#view the final list of district names and ids
district_list

# ----------------------------------------------
# save raw output
saveRDS(district_list, file="J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/districts.rds")

