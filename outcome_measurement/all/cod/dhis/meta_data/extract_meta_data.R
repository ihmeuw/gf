# DHIS Extraction for DRC  - Metadata extraction
# Caitlin O'Brien-Carelli
# 12/11/18

# Calls the extracting functions (dhis_extracting_functions.R)
# Extracts all meta data to run the extraction 

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(reshape)
library(RCurl)
library(XML)
library(plyr)

# --------------------
# shell script to run on the cluster 

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1349 -s 10 -P dhis_download

#----------------------
# determine if the code is being run on the cluster or on home computer
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working directory 
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
setwd(dir)

# source functions 
source (paste0(dir, 'dhis_extracting_functions.R'))

#---------------------------------------------
#'Extract content information from DHIS
#'
#' \code{extract_dhis_content} extracts content information from DHIS
#'
#' @param base_url The base url of the DHIS2 setting
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a list of seven tables :
#'
#' \strong{data_sets} The list of data sets as extracted by
#' \link{extract_dhis_datasets}.
#'
#' \strong{data_elements} The list of data elements as extracted by
#' \link{extract_data_elements}.
#'
#' \strong{data_elements_categories} The list of categories as extracted by
#' \link{extract_categories}.
#'
#' \strong{org_units_list} The list of organization units as extracted by
#' \link{extract_orgunits_list}.
#'
#' \strong{org_units_description} The description of organization units as extracted by
#' \link{extract_org_unit}.
#'
#' \strong{org_units_group} The list of the groups of organization units as extracted by
#' \link{extract_org_unit}.
#'
#' \strong{org_units_report} The list of reports for each organization unit as extracted by
#' \link{extract_org_unit}.
#' 
#---------------------------------------------

#-----------------------
# input the country, base_url, userID, and password for the DRC DHIS2 system
# input as string variables 

country = 'drc'
base_url = 'https://snisrdc.com'
userID = 'Bethany_Huntley'
password = 'Snisrdcongo1'

#---------------------------------------------
#extract_dhis_content function
extract_dhis_content = function(base_url, userID, password) {
  print('Making DHIS urls')
  urls = make_dhis_urls(base_url)
  urls = data.table(urls)
  
  #-----------------------
  # extract data sets
  print('Extracting Data Sets')
  data_sets = extract_dhis_datasets(urls$data_sets_url, userID, password)
  colnames(data_sets) = c('datasets_ID', 'datasets_name', 'datasets_url')
  
  #-----------------------
  # extract data elements 
  print('Extracting Data Elements')
  data_element_list = extract_data_elements(urls$data_elements_url, userID, password)
  
  data_elements = ddply(data_sets, .(datasets_ID, datasets_name),
                        function(data_sets) {
                        out = extract_data_elements_ds(as.character(data_sets$datasets_url),
                                                            userID, password)})
  # merge the data elements and data sets
  data_elements = data.table(data_elements)
  data_elements[ , datasets_name:=NULL]
  interim = merge(data_sets, data_elements, by='datasets_ID')
  
  #change names of data_element_list for the merge
  setnames(data_element_list, c('data_element_id', 'data_element_name', 'data_element_url' ))
  updated_data_elements = merge(interim, data_element_list, by='data_element_id', all=TRUE)
  updated_data_elements = data.table(updated_data_elements)

  #-----------------------
  # extract categories for the data elements (age, sex, etc.)
  print('Extracting Categories')
  data_elements_categories = extract_categories(as.character(urls$data_elements_categories),
                                                  userID, password)
  
  #-----------------------
  # organisational units extraction
  print('Extracting Organisation Units List')
  org_units_list = extract_orgunits_list(as.character(urls$org_units_url),
                                           userID, password)
  
  colnames(org_units_list) = c('org_unit_ID', 'org_unit_name', 'org_unit_url')
  org_units_list = data.table(org_units_list)
  
  # convert factors to strings
  org_units_list[ , org_unit_ID:=as.character(org_unit_ID)]
  org_units_list[ , org_unit_name:=as.character(org_unit_name)]
  org_units_list[ , url:=as.character(org_unit_url)]
  
  # check for duplicate facilities
  org_units_list[duplicated(org_units_list$org_unit_ID)]
  org_units_list[ , org_unit_url:=NULL]
  
  #-----------------------
  # extract meta data associated with organizational units
  # includes data sets, coordinates, opening date, name
  print('Extracting units information')
  extracted_org_units = dlply(org_units_list, .(org_unit_ID),
                              function(org_units_list) {
                              try(extract_org_unit(org_units_list$url, userID, password))},
                              .progress = 'text')  
  
  # screen for timed out and other faulty org units
  lengths = sapply(extracted_org_units, length)
  faults = which(lengths<3)
  if (length(faults)>0) warning('Warning: at least one org unit didn\'t extract correctly')
  extracted_orgunits = extracted_orgunits[-faults]
 
  # save org unit meta data 
  saveRDS(units, file=paste0(dir, 'meta_data/extracted_org_units.rds')) 
  
  #-----------------------
  # return all of the data as a data set
  return(list(data_sets, updated_data_elements, data_elements_categories, org_units_list))
}

#-------------------------------------------------------------------

#------------------------
# run the extraction :)

DRC_extraction = extract_dhis_content(base_url = base_url, userID = userID, password = password)

#-------------------------
# save the data 

# set new output directory
out_dir = paste0(dir, '' )








# 1 - export data sets
data_sets1 <- DRC_extraction[1]
write.csv(data_sets1, paste0(root, '/data_sets.csv'))

# 2 - export data elements with associated data sets
# original code returned data_elements, not updated_data_elements
updated_data_elements1 <- DRC_extraction[2]
write.csv(updated_data_elements1, paste0(root, 'Project/Evaluation/GF/outcome_measurement/cod/dhis_temp/DRC_data_elements_with_ds.csv'))

# 3 - export categories for data elements
data_elements_categories1 <- DRC_extraction[3]
write.csv(data_elements_categories1, paste0(root, 'Project/Evaluation/GF/outcome_measurement/cod/dhis_temp/categories.csv'))

# 4 - export the list of organisational units
org_units1 <- DRC_extraction[4]
write.csv(org_units1, paste0(root, 'Project/Evaluation/GF/outcome_measurement/cod/dhis_temp/org_units.csv'))

# 5 - export the descriptions of the organisational units
org_units_description1 <- DRC_extraction[5]
write.csv(org_units_description1, paste0(root, 'Project/Evaluation/GF/outcome_measurement/cod/dhis_temp/org_units_info.csv'))

#6 - export groups for the organisational units
org_units_group1 <- DRC_extraction[6]
saveRDS(org_units_group1, paste0(out_dir, 'groups.rds'))

write.csv(org_units_group1, paste0(root, 'Project/Evaluation/GF/outcome_measurement/cod/dhis_temp/org_units_group.csv'))

#7 - export the data sets associated with the organisational units - is this set correct?
org_units_report1 <- DRC_extraction[7]
write.csv(org_units_report1, paste0(root, 'Project/Evaluation/GF/outcome_measurement/cod/dhis_temp/org_units_report.csv'))


#-------------------------------------------------------------------