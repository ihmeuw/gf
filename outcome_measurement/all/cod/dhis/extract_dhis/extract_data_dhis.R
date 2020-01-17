#----------------------------------
# Caitlin O'Brien-Carelli
# January 2018-December 2019

# Audrey Batzel
# January 2019-current
#----------------------------------
# Extract single data sets by specifying the data set number
# Data will be merged with the meta data to create complete data sets
# You must have the source functions and meta data accessible to run 

# DHIS2 extraction script for DRC - SNIS
# Extracts Data from: https://www.snisrdc.com/dhis-web-commons/security/login.action
# Sources dhis_extracting_functions.R on J Drive for dhisextractr package and extraction functions
# Website re-reroutes from https://www.snisdrc.com to https://snisdrc.com
#----------------------------------

#----------------------------------
# To run on the cluster:
# Copy shell script into a qlogin session to open an IDE
# Make sure to request enough memory and run time

# sh /ihme/code/jpy_rstudio/jpy_rstudio_qsub_script.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3501.img -l m_mem_free=50G -l fthread=20 -P proj_pce -q all -t rstudio -l archive=TRUE -l h_rt=72:00:00
# qsub -terse -N rst_ide_19_05_14_160329 -q long.q -l fthread=2 -l m_mem_free=60G -l h_rt=85:00:00 -l archive=TRUE -P proj_pce /ihme/code/jpy_rstudio/jpy_rstudio_shell.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3501.img -t rstudio -p 7513 -o 1 -G r
#----------------------------------
# Set up R
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(stringr) 
library(RCurl)
library(XML)
library(plyr)
library(openxlsx)
library(lubridate)
#----------------------------------

#----------------------------------
# Notes on parameters:
#----------------------------------
# select the start year and end year for the download
# start month is inclusive, end month is exclusive
# update_year =  the update year should be before the data begins - default is 2009

# set_subset = identify the data set(s) you want to download by number (list below)

# set_name = change set_name to the name of the data set you are downloading 
# it will determine which folder the data is saved within

# the default is currently set to extract base data for 2019 to date. 
#----------------------------------
# available data sets by number: 
#----------------------------------
# 1: pMbC0FJPkcm A- Services de Base 
# 2: maDtHIFrSHx B- Services Secondaires 
# 3: pePpSnKtAh3 C1- SIGL1 
# 4: s6yd0w2KXWa C2- SIGL2 
# 5: kzug4nJpnis Canevas UNIQUE PNLS 2019 
# 6: l8ppzBjgihH C. SIGL BCZ_CDR_BCAF 
# 7: oTgCLoG5OkY C- SIGL FOSA
# 8: YYIBtkA3LbT  Data PF h
# 9: EbG2JnCIPKD  DQI Bureau central de la Zone - Trimestriel 
# 10: ycHbewznGao DQI Centre de Sant? - Trimestriel 
# 11: cKVdn82G240 DQI H?pital - Trimestriel 
# 12: OeWrFwkFMvf D- Service Hopital 
# 13: uGr4KMkDzav E- Banque de Sang et Transfusion 
# 14: CUa7hLcHtia Export Data TB 
# 15: LmME0nSMLMz Export Data TB Precedent 
# 16: ktBWTI6yjTB F- Activites BCZ 
# 17: iriO2vCt72x G- Hygiene aux frontieres 
# 18: Fo5ux0Ja21i H- Relev? Epid?miologique Hebdomadaire 
# 19: hGTha9836Lk I-Surveillance EBOLA 
# 20: LBXxOUxWLQe I-Surveillance Journali?re EBOLA 
# 21: q2LW2KL8h8O MILD_Denombrement 
# 22: dUg9MGucvYg MILD_Distribution 
# 23: p9OV146elYx OSQD Bureau Central de la ZS 
# 24: dmtwHoZEHpS OSQD Centre de Sant? 
# 25: uhUlD7EQo5Y OSQD H?pital 
# 26: aWLL9odsL7p OSQD V?rification - Audit 
# 27: E39d4kc5aPR PepfarConnect 
# 28: FLATAfhyaHb PNLP CS Site Sentinelle 
# 29: z6oEr8JcJ57 PNLP HGR Site Sentinelle 
# 30: wIMw0dzITTs PNLS- Canevas Unique FOSA 
# 31: jcaxCOQLeQC PNLT -  Rapport Trimestriel Tuberculose 2019 
# 32: mV0r6yDCZy3 Population 
#----------------------------------

#-----------------------------------------------
# extract the data and save 
#-----------------------------------------------
run_extraction_tool = function(start_year = '2019', end_year = as.character(year(Sys.Date())), 
                               start_month = '01', end_month = as.character(month(Sys.Date())), 
                               set_name = set) {

  update_year = '2009'
  
  if (set == 'base') {
    set_subset = 1
  } else if (set == 'sigl'){
    set_subset = c(3:4)
  } else if (set == 'secondary'){
    set_subset = 2
  } else {
    print('This master script is not yet implemented for your chosen data set!')
  }
  
  # Set the directory to download the data
  # detect if operating on windows or on the cluster 
  root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
  # define main directory
  dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
  
  # create a folder to save the download in under the current month/year (date of this download)
  current_year = as.character(year(Sys.Date()))
  current_month = as.character(month(Sys.Date()))
  if(nchar(current_month)==1) current_month = paste0('0', current_month)
  save_dir = paste0(dir, '1_initial_download/', set_name, '/', 'intermediate_data_', current_year, '_', current_month, '/')
  if (!file.exists(save_dir)) dir.create(save_dir)
  
  #----------------------------------
  # set the country, base_url, username, and password 
  country = 'drc'
  base_url = 'https://snisrdc.com'
  userID = 'Bethany_Huntley'
  password = 'Snisrdcongo1'
  
  #----------------------------------
  # import the meta data for the download
  # only organizational units and data sets are necessary 
  # other meta data is used for the merge 
  data_sets = readRDS(paste0(dir, 'meta_data/data_sets.rds'))
  org_units = readRDS(paste0(dir, 'meta_data/org_units.rds'))
  
  #----------------------------------
  # extract the data set and export as a RDS
  # click 'plots' tab to watch download progress
  # extract_all_data is a function in the dhisextractr package
  
  # loop through months to extract data
  start_date = paste0(start_year, '-', start_month, "-01")
  start_date = as.Date(start_date)
  end_date = paste0(end_year, '-', end_month, "-01")
  end_date = as.Date(end_date)
  
  dates = seq(start_date,end_date, by = 'month')

  #----------------------------------
  # EXTRACTION LOOP------------------
  #----------------------------------
  for (i in 1:((length(dates))-1) ){
    
    start_current_loop = dates[i]
    end_current_loop = dates[i+1]
    
    # extract month of data
    extracted_data = extract_all_data(base_url = base_url, 
                                      data_sets = data_sets[set_subset, ],
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
    
    save_file = paste0(save_dir, set_name,'_0', save_month_start, '_', save_year_start, '_0', save_month_end, '_', save_year_end) 
    
    # save intermediate data - 1st iteration
    if (nrow(extracted_data)==0){
      print(paste0('No data extracted for ', save_month_start, '/', save_year_start, '!'))
      next
    } else {
      extracted_data$download_number = 1
    }
    
    saveRDS(extracted_data, paste0(save_file, '_first_download.rds'))
    print(paste0('The first download attempt of ', save_month_start, '/', save_year_start, ' is downloaded and saved.'))
    
    # extract month again on subset of org_units
    extracted_fac = unique(extracted_data$org_unit_ID) %>% as.character
    org_units_to_try_again = org_units[!org_unit_ID %in% extracted_fac]
    
    extracted_data2 = extract_all_data(base_url = base_url, 
                                       data_sets = data_sets[set_subset, ],
                                       org_units = org_units_to_try_again, 
                                       deb_period = start_current_loop,
                                       end_period = end_current_loop,
                                       userID = userID, 
                                       password = password,
                                       pace = 10,
                                       update_date = paste0(update_year, '-01-01'))
    
    # save intermediate data - 2nd download
    if (nrow(extracted_data2)==0){ 
      print(paste0("skipping iteration ", i, " second download (the re-download is empty)"))
      next
    } else {
      extracted_data2$download_number = 2
      saveRDS(extracted_data2, paste0(save_file, '_second_download.rds'))
      print(paste0('The second download attempt of ', save_month_start, '/', save_year_start, ' is downloaded and saved.'))
    }
    print(paste0("Loop ", i, " of ", (length(dates)-1), " complete" ))
  }
}
#-----------------------------------------------


