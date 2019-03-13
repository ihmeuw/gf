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
start_year = '2018'
end_year = '2018'
start_month = '02'
end_month = '03' # start month is inclusive, end month is exclusive

# change the update year to before the data begins
update_year = "2009"

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
#------------------------------------------------

#------------------------------------------------
#------------------------------------------------
# TROUBLESHOOTING CODE
# all facilities in the meta data
all_fac <- unique(org_units$org_unit_ID)
all_fac <- as.character(all_fac)

# data downloaded but 
dt_base_2_14 <- readRDS(paste0(dir, 'pre_prep/base/base_01_2018_04_2018_for_troubleshooting.rds'))
dt_base_2_14 <- as.data.table(dt_base_2_14)

month_1_data <- dt_base_2_14[period== "201801", ]
month_1_fac <- unique(month_1_data$org_unit_ID)
month_1_fac <- as.character(month_1_fac)
fac_not_in_month1_fac <- all_fac[!all_fac %in% month_1_fac]

# rerun the data on org units not extracted in the last extraction for month 1 (note: NO data extracted for these org units
# in this month, for any data elements, so we want to see if there just isn't data or if it just didn't download)
org_units <- org_units[org_unit_ID %in% fac_not_in_month1_fac, ]
# ---------------- update dates and run download -------------------

# 421 facilities were downloaded for that month that weren't in the orig 2_14 download
# now try again on this subset to see if there is anymore data. Hoping not!
org_units <- org_units[!org_unit_ID %in% extracted_data$org_unit_ID,]
# ---------------- run download  -------------------
# NO new facilities!

dt_base_2_14_redownload_mo1 <- readRDS(paste0(dir, 'pre_prep/base/base_01_2018_02_2018_for_troubleshooting_redownload_of_fac_not_in_2_14.rds'))
dt_base_2_14_redownload_mo1 <- as.data.table(dt_base_2_14_redownload_mo1)
# facilities that were in the redownload data for mo1 but not the original
redownload_fac_mo1 <- unique(dt_base_2_14_redownload_mo1$org_unit_ID)
redownload_fac_mo1 <- as.character(redownload_fac_mo1)

# now compare month 2 to see if there are different facilities that didn't download
month_2_data <- dt_base_2_14[period == "201802", ]
month_2_fac <- unique(month_2_data$org_unit_ID)
month_2_fac <- as.character(month_2_fac)

month_2_fac[month_2_fac %in% redownload_fac_mo1] # this shows there are 6 facilities that downloaded in the for 02/18 that weren't in the original download for 01/18, but were in DHIS2
fac_not_in_month2 <- all_fac[!all_fac %in% month_2_fac]

# now try re-download on fac not in month 2 (we will compare to see if some of these are in fac 1)
org_units = readRDS(paste0(dir, 'meta_data/org_units.rds'))
org_units <- org_units[org_unit_ID %in% fac_not_in_month2, ]
# ---------------- update dates and run download -------------------

dt_base_2_14_redownload_mo2 <- copy(extracted_data)
# facilities that were in the redownload data for mo2 but not the original
redownload_fac_mo2 <- unique(dt_base_2_14_redownload_mo2$org_unit_ID)
redownload_fac_mo2 <- as.character(redownload_fac_mo2)

# more tests
month_1_fac[month_1_fac %in% redownload_fac_mo2] # 3 facilities
redownload_fac_mo2[redownload_fac_mo2 %in% redownload_fac_mo1] # these weren't in original downloads for either month - 405 facilities (most of them)

# remove the fac in redownload of mo2 and try download again
org_units <- org_units[!org_unit_ID %in% extracted_data$org_unit_ID,]
# ---------------- run download  -------------------


#------------------------------------------------
#------------------------------------------------

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
                                  pace = 10,
                                  update_date = paste0(update_year, '-01-01'))


# save the data table in its individual folder in 'pre_prep' for merge and prep:
saveRDS(extracted_data, paste0(dir, 'pre_prep/', set_name, '/', set_name, '_', 
                               start_month, '_', start_year, '_', end_month, '_', end_year, '_for_troubleshooting_redownload_again.rds'))

#-------------------------
