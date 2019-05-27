# DHIS2 Extraction for DRC - SNIS
# Extracts Data from: https://www.snisrdc.com/dhis-web-commons/security/login.action
# Sources dhis_extracting_functions.R on J Drive for dhisextractr package and extraction functions
# Website re-reroutes from https://www.snisdrc.com to https://snisdrc.com

#---------------------------
# Caitlin O'Brien-Carelli
# 1/15/2018

# Audrey Batzel
# 2/22/19 - updated to work around problem of some facilities not downloading still even with changed pace
#---------------------------
# Extract single data sets by specifying the data set number
# Data will be merged with the meta data to create complete data sets
# You must have the source functions and meta data accessible to run 
#---------------------------

#---------------------------
# To run on the cluster:
# Copy shell script into a qlogin session to open an IDE
# request at least 10 slots for extractions 

# sh /ihme/code/jpy_rstudio/jpy_rstudio_qsub_script.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3501.img -l m_mem_free=4G -l fthread=2 -P proj_pce -q all -t rstudio -l archive=TRUE -l h_rt=72:00:00
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
library(openxlsx)
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
start_month = '01'
end_month = '03' # start month is inclusive, end month is exclusive

# change the update year to before the data begins
update_year = '2009'

#identify the data set(s) you want to download by number (list below)
set = 29

# change set_name to the name of the data set you are downloading 
# set_name will change the file names for saving the data
set_name = 'pnls'

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

# loop through months to extract data
start_date = paste0(start_year, '-', start_month, "-01")
start_date = as.Date(start_date)

end_date = paste0(end_year, '-', end_month, "-01")
end_date = as.Date(end_date)

dates = seq(start_date,end_date, by = 'month')

# NOTE: make sure there is an "intermediate_data" folder within the data set folder to save the data too.  I have set older versions to be "intermediate_data_archive"
# and then created a new "intermediate_data" folder, otherwise it will still work but will overwrite old files (maybe this is okay because we have the combined file from intermediate ones still?)
# TO DO: someday will make this all happen through the code...
# EXTRACTION LOOP-----------------
for (i in 1:((length(dates))-1) ){
  
  start_current_loop = dates[i]
  end_current_loop = dates[i+1]
  
  # extract month of data
  extracted_data = extract_all_data(base_url = base_url, 
                                    data_sets = data_sets[set, ],
                                    org_units = org_units, 
                                    deb_period = start_current_loop,
                                    end_period = end_current_loop,
                                    userID = userID, 
                                    password = password,
                                    pace = 20,
                                    update_date = paste0(update_year, '-01-01'))
  
  save_month_start = month(start_current_loop)
  save_year_start = year(start_current_loop)
  save_month_end = month(end_current_loop)
  save_year_end = year(end_current_loop)
  
  # save intermediate data - 1st iteration
  extracted_data$download_number = 1
  saveRDS(extracted_data, paste0(dir, 'pre_prep/', set_name, '/intermediate_data/', set_name,'_0', save_month_start, '_', 
                                 save_year_start, '_0', save_month_end, '_', save_year_end, '_first_download.rds'))
  
  
  # extract month again on subset of org_units
  extracted_fac = unique(extracted_data$org_unit_ID) %>% as.character
  org_units <- org_units[!org_unit_ID %in% extracted_fac]
  
  extracted_data2 = extract_all_data(base_url = base_url, 
                                     data_sets = data_sets[set, ],
                                     org_units = org_units, 
                                     deb_period = start_current_loop,
                                     end_period = end_current_loop,
                                     userID = userID, 
                                     password = password,
                                     pace = 10,
                                     update_date = paste0(update_year, '-01-01'))
  
  # save intermediate data - 2nd download
  if (nrow(extracted_data2)==0){ 
    print(paste0("skipping iteration ", i, " second download (second download is empty)"))
    next
  } else {
    extracted_data2$download_number = 2
    saveRDS(extracted_data2,  paste0(dir, 'pre_prep/', set_name, '/intermediate_data/', set_name,'_0', save_month_start, '_', 
                                     save_year_start, '_0', save_month_end, '_', save_year_end, '_second_download.rds'))
  }
  print(paste0("Loop ", i, " of ", (length(dates)-1), " complete" ))
}
#------------------------
#------------------------
# read in all files and rbind together to save one file of data
files = list.files( paste0('./pre_prep/', set_name, '/intermediate_data/'), recursive=TRUE)

if (set_name=='base' | set_name=='sigl') {
  keep_vars = read.xlsx(paste0(dir, 'catalogues/data_elements_cod.xlsx'))
  keep_vars = data.table(keep_vars)
  keep_vars[ , keep:=as.numeric(keep)]
  keep_vars = keep_vars[keep==1, element_id]
}

dt = data.table()
# read in the files 
i = 1
for(f in files) {
  #load the RDs file
  vec = f
  current_data = data.table(readRDS(paste0(dir, 'pre_prep/', set_name, '/intermediate_data/', f)))
  current_data[ , file:=vec ]
  
  # subset to only the variables needed for large data sets
  if (set_name=='base' | set_name=='sigl') {
    current_data[ , data_element_ID:=as.character(data_element_ID)]
    current_data = current_data[data_element_ID %in% keep_vars]  
  } 
  
  # append to the full data 
  if(i==1) dt = current_data
  if(i>1)  dt = rbind(dt, current_data)
  print(paste0("Status: ", i))
  i = i+1
}

# save the data table in its individual folder in 'pre_prep' for merge and prep:
saveRDS(extracted_data, paste0(dir, 'pre_prep/', set_name, '/', set_name, '_', 
                               start_month, '_', start_year, '_', end_month, '_', end_year, '_combined_download.rds'))

#-------------------------

