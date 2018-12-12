# DHIS2 Extraction for DRC - SNIS
# Extracts Data from: https://www.snisrdc.com/dhis-web-commons/security/login.action
# Sources dhis_extracting_functions.R on J Drive for package and extraction functions

#---------------------------
# Caitlin O'Brien-Carelli
# 12/10/2018
# Extract single data set by set number
# Data will be merged with the meta data to create complete data sets
# You must have the source functions and meta data accessible to run
#---------------------------

#---------------------------
# To run on the cluster:

# Copy shell script into a qlogin session to open an IDE
# request at least 10 slots for extractions 

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 20 -P snis_download

#---------------------------
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

#---------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#---------------------------
# define main directory

dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#---------------------------
# source the necessary functions to download the data 
# change the location of the sourced file to your home directory 

source(paste0(dir, 'dhis_extracting_functions.R')) 

# check to make sure the package loaded by viewing a help file
?extract_all_data

#---------------------------
# Input the start year, end year, and output directory

# select the start year and end year for the download
start_year = '2017'
end_year = '2018'
start_month = '01'
end_month = '04'

# change the update year to before the data begins
update_year = '2016'

#identify the data set(s) you want to download by number (list below)
set = 1

# change set_name to the name of the data set you are downloading 
# set_name will change the file names for saving the data
set_name = 'base'

#---------------------------
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

#---------------------------

#----------------------------------
# set the country, base_url, username, and password 
country = 'drc'
base_url = 'https://www.snisrdc.com'
userID = 'Bethany_Huntley'
password = 'Snisrdcongo1'

#------------------------------------------------
# import the meta data for the download
# only organizational units and data sets are necessary 
# other meta data is used for the merge 

data_sets = data.table(readRDS(paste0(dir, "meta_data/data_sets.rds")))
org_units = data.table(readRDS(paste0(dir, "meta_data/org_units_list.rds")))

# convert factors to strings
org_units[ ,org_unit_ID:=as.character(org_unit_ID)]
org_units[ ,org_unit_name:=as.character(org_unit_name)]
org_units[ ,org_unit_url:=as.character(org_unit_url)]

#-----------------------------------------------

#-----------------------------------------------
# extract the data and save! 

#------------------------
# extract the data set and export as a RDS
# click 'plots' tab to watch download progress

extracted_data <- extract_all_data(base_url = base_url, 
                                     data_sets = data_sets[set, ],
                                     org_units = org_units[1:100], 
                                     deb_period = paste0(start_year, '-', start_month, '-01'),
                                     end_period = paste0(end_year, '-', end_month, '-01'),
                                     userID = userID, 
                                     password = password,
                                     pace = 20,
                                     update_date = paste0(update_year, '-01-01'))



# save the data table in its individual folder in 'pre_prep' for merge and prep:
saveRDS(extracted_data, paste0(dir, 'pre_prep/', set_name, '/', set_name, '_', country, 
                                 '_', start_month, '_', start_year, '_', end_month, '_', end_year, '.rds'))


#-------------------------
