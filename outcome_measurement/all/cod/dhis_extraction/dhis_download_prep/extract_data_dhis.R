# DHIS2 Extraction for DRC - SNIS
# Extracts Data from: https://www.snisrdc.com/dhis-web-commons/security/login.action
# Sources dhis_extracting_functions.R for package and extractin functions

#-------------------------------------------
# Caitlin O'Brien-Carelli
# 7/23/2018
# Extract single data set by set number
# Data will be merged with the meta data to create complete data sets
# You must have the source functions and meta data accessible to run
#-------------------------------------------

#----------------------------------
# To run on the cluster:

# Copy shell script into a qlogin session to open an IDE
# request at least 10 slots for extractions 

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 10 -P snis_download

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(stringr) 
library(RCurl)
library(XML)
library(plyr)

#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#------------------------------------
# define main directory

dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#----------------------------------
# source the necessary functions to download the data 
# change the location of the sourced file to your home directory 

source(paste0(dir, 'dhis_extracting_functions.R')) 

# check to make sure the package loaded by viewing a function help file
?extract_all_data

#----------------------------------
# Cthe start year, end year, and output directory

# select the start year and end year for the download
start_year <- '2018'
end_year <- '2018'
start_month <- '01'
end_month <- '07'

# change the update year to before the data begins
update_year <- start_year

#identify the data set(s) you want to download by number (list below)
set <- 17

# change set_name to the name of the data set you are downloading 
# set_name will change the file names for saving the data
set_name <- 'pnls'

# available data sets by number: 

# 1: A- Services de Base
# 2: B- Services Secondaires
# 3: C1- SIGL1
# 4: C2- SIGL2
# 5: DQI Bureau central de la Zone - Trimestriel
# 6: DQI Centre de Santé - Trimestriel
# 7: DQI Hôpital - Trimestriel
# 8: D- Service Hopital
# 9: E- Banque de Sang et Transfusion
# 10: F- Activites BCZ
# 11: G- Hygiene aux frontieres
# 12: H- Relevée Epidémiologique Hebdomadaire
# 13: I-Surveillance EBOLA
# 14: I-Surveillance Journalière EBOLA
# 15: PNLP CS Site Sentinelle
# 16: PNLP HGR Site Sentinelle
# 17: PNLS- Canevas Unique FOSA
# 18: PNLT- Rapport Trimestriel Tuberculose
# 19: Population

#------------------------------------------------

#----------------------------------
# set the country, base_url, username, and password 
country <- 'drc'
base_url <- 'https://www.snisrdc.com'
userID <- 'Bethany_Huntley'
password <- 'Snisrdcongo1'

#------------------------------------------------
# import the necessary meta data for the download
# only organizational units and data sets are necessary for the download
# other meta data is used for the merge 

data_sets <- readRDS(paste0(dir, "meta_data/data_sets.rds")) 
data_sets <- data.table(data_sets)
org_units <- readRDS(paste0(dir, "meta_data/org_units_list.rds")) 
org_units <- data.table(org_units)

#-----------------------------------------------

#------------------------
# to export a list of elements within regularly reported data sets
# this exports the data sets we use regularly along with a catalogue of elements

# select the data sets that are used regularly 
# data_sets_ids <- c("ktBWTI6yjTB", "iriO2vCt72x", "OeWrFwkFMvf", "s6yd0w2KXWa", 
#               "cKVdn82G240", "Fo5ux0Ja21i", "maDtHIFrSHx", "pePpSnKtAh3",
#               "pMbC0FJPkcm", "EbG2JnCIPKD", "ycHbewznGao", "mV0r6yDCZy3")
# 
# # subset to the elements within those data sets
# data_sets[datasets_ID %in% data_sets_ids]
# reported_elements <- elements[datasets_ID %in% data_sets_ids]
# 
# # export a csv of the relevant elements
# write.csv(reported_elements, paste0(dir, 'elements_catalogue.csv'))

#-----------------------------------------------
# extract the data sets that correspond to the meta_data

#------------------------
# extract the data set and export as a RDS/CSV
# click 'plots' tab to watch download progress

extracted_data <- extract_all_data(base_url = base_url, 
                                     data_sets = data_sets[set, ],
                                     org_units = org_units, 
                                     deb_period = paste0(start_year, '-', start_month, '-01'),
                                     end_period = paste0(end_year, '-', end_month, '-01'),
                                     userID = userID, 
                                     password = password,
                                     pace = 20,
                                     update_date = paste0(update_year, '-01-01'))

# save the data table in its individual folder in 'pre_prep' for merge and prep:
saveRDS(extracted_data, paste0(dir, 'pre_prep/', set_name, '/', set_name, '_', country, 
                                 '_', start_month, '_', start_year, '_', end_month, '_', end_year, '.rds'))

# save as a csv - the data sets are very large so use onyl as needed
# write.csv(extracted_data, paste0(dir, 'pre_merge/', set_name, '_', country, '_', start_month, '_', start_year, '_', end_month, '_', end_year, '.csv'))

#-------------------------
