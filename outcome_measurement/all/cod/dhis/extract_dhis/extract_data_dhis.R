# DHIS2 Extraction for DRC - SNIS
# Extracts Data from: https://www.snisrdc.com/dhis-web-commons/security/login.action
# Sources dhis_extracting_functions.R on J Drive for dhisextractr package and extraction functions
# Website re-reroutes from https://www.snisdrc.com to https://snisdrc.com

#---------------------------
# Caitlin O'Brien-Carelli
# 1/15/2018

#---------------------------
# Extract single data sets by specifying the data set number
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

dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
setwd(dir)
#---------------------------
# source the necessary functions to download the data 
# change the location of the sourced file to your home directory or source from J

source(paste0(dir, 'code/dhis_extracting_functions.R')) 

# check to make sure the package loaded by viewing a help file
?extract_all_data

#---------------------------
# Input the start year, end year, and output directory

# select the start year and end year for the download
start_year = '2015'
end_year = '2016'
start_month = '01'
end_month = '01' # start month is inclusive, end month is exclusive

# change the update year to before the data begins
update_year = as.character(as.numeric(start_year) - 1)

#identify the data set(s) you want to download by number (list below)
set = 2

# change set_name to the name of the data set you are downloading 
# set_name will change the file names for saving the data
set_name = 'base'

#---------------------------
# available data sets by number: 

# 1	 00 Temporaire 
# 2	 A - Services de Base
# 3	 B - Services Secondaires
# 4	 C1 - SIGL1
# 5	 C2 - SIGL2
# 6	 C. SIGL BCZ_CDR_BCAF
# 7	 C. SIGL FOSA
# 8	 DQI Bureau central de la Zone - Trimestriel
# 9	 DQI Centre de Sante - Trimestriel
# 10 DQI Hospital - Trimestriel
# 11 D - Service Hopital
# 12 E - Banque de Sang et Transfusion
# 13 F - Activites BCZ
# 14 G - Hygiene aux frontieres
# 15 H - Relevee Epidemiologique Hebdomadaire
# 16 I-Surveillance EBOLA
# 17 I-Surveillance Journaliare EBOLA
# 18 MILD_Denombrement
# 19 MILD_Distribution
# 20 OSQD Bureau Central de la ZS
# 21 OSQD Centre de Sante
# 22 OSQD Hospital
# 23 OSQD Verification - Audit
# 24 PATI V - TB Cas enregistre
# 25 PATI V - TB resultat
# 26 PepfarConnect
# 27 PNLP CS Site Sentinelle
# 28 PNLP HGR Site Sentinelle
# 29 PNLS- Canevas Unique FOSA

#---------------------------

#----------------------------------
# set the country, base_url, username, and password 
country = 'drc'
base_url = 'https://snisrdc.com'
userID = 'Bethany_Huntley'
password = 'Snisrdcongo1'

#------------------------------------------------
# import the meta data for the download
# only organizational units and data sets are necessary 
# other meta data is used for the merge 

data_sets = readRDS(paste0(dir, 'meta_data/data_sets.rds'))
org_units = readRDS(paste0(dir, 'meta_data/org_units.rds'))

#-----------------------------------------------

#-----------------------------------------------
# extract the data and save! 

#------------------------
# extract the data set and export as a RDS
# click 'plots' tab to watch download progress
# extract_all_data is a function in the dhisextractr package

extracted_data = extract_all_data(base_url = base_url, 
                                     data_sets = data_sets[set, ],
                                     org_units = org_units, 
                                     deb_period = paste0(start_year, '-', start_month, '-01'),
                                     end_period = paste0(end_year, '-', end_month, '-01'),
                                     userID = userID, 
                                     password = password,
                                     pace = 40,
                                     update_date = paste0(update_year, '-01-01'))


# save the data table in its individual folder in 'pre_prep' for merge and prep:
saveRDS(extracted_data, paste0(dir, 'pre_prep/', set_name, '/', set_name, '_', 
                               start_month, '_', start_year, '_', end_month, '_', end_year, '.rds'))

#-------------------------
