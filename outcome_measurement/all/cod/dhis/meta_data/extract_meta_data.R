# DHIS Extraction for DRC  - Metadata extraction
# Caitlin O'Brien-Carelli
# 1/18/19

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
library(openxlsx)

# --------------------
# shell script to run on the cluster 

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1349 -s 10 -P dhis_download

#----------------------
# determine if the code is being run on the cluster or on home computer
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working directory 
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
setwd(dir)

# set the output directory
out_dir = paste0(dir, 'meta_data/')

# source functions from J Drive - be sure to update from most recent push
source (paste0(dir, 'code/dhis_extracting_functions.R'))

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
  data_sets = data.table(data_sets)
  
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
  data_elements_categories = data.table(data_elements_categories)
  
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
  # return all of the data as a data set
  return(list(data_sets, updated_data_elements, data_elements_categories, org_units_list))
}

#-------------------------------------------------------------------

#------------------------
# run the extraction :)

DRC_extraction = extract_dhis_content(base_url = base_url, userID = userID, password = password)

#------------------------------
# save the data 

# extract data tables from the large list of meta data 
data_sets = DRC_extraction[1][[1]]
updated_data_elements = DRC_extraction[2][[1]]
categories = DRC_extraction[3][[1]]
org_units = DRC_extraction[4][[1]]

# save all the RDS files to the J Drive
saveRDS(data_sets, paste0(out_dir, 'data_sets.rds'))
saveRDS(updated_data_elements, paste0(out_dir, 'updated_data_elements.rds'))
saveRDS(categories, paste0(out_dir, 'data_elements_categories.rds'))
saveRDS(org_units, paste0(out_dir, 'org_units.rds'))

#------------------------------
# translations of the data elements

# export the data elements and translate w google translate
data_elements = data.table(updated_data_elements)
setnames(data_elements, c("data_element_id",  "data_element_name", "datasets_ID", 
           "datasets_name", "datasets_url", "data_element_url"),
         c("element_id",  "element", "data_set_id",
           "data_sets", "data_set_url", "element_url"))

# create a subset to translate (less variables makes translator work better)
sub_elements = data_elements[ ,.(data_set_id, element_id, element)]

# doc translater only works on shorter lists - divide into three sets
rows = nrow(sub_elements)
sub_elements_1 = sub_elements[1:1500]
sub_elements_2 = sub_elements[1501:3000]
sub_elements_3 = sub_elements[3001:rows]
write.xlsx(sub_elements_1, paste0(out_dir, 'translate/data_elements_to_translate_1.xlsx'))
write.xlsx(sub_elements_2, paste0(out_dir, 'translate/data_elements_to_translate_2.xlsx'))
write.xlsx(sub_elements_3, paste0(out_dir, 'translate/data_elements_to_translate_3.xlsx'))

# translate using online document translator - onlinedoctranslator.com
# save translations at file paths below 

# read in the translations and rbind together
trans_elements_1 = read.xlsx(paste0(out_dir, 'translate/data_elements_translations_1.xlsx'))
trans_elements_2 = read.xlsx(paste0(out_dir, 'translate/data_elements_translations_2.xlsx'))
trans_elements_3 = read.xlsx(paste0(out_dir, 'translate/data_elements_translations_3.xlsx'))
trans_elements = rbind(trans_elements_1, trans_elements_2, trans_elements_3)
setnames(trans_elements, 'element', 'element_eng')

# merge in the translations to the original variables list
data_elements = merge(data_elements, trans_elements, by=c('data_set_id', 'element_id'), all=T)

# save the data elements with associated translations
saveRDS(data_elements, paste0(out_dir, 'data_elements.rds'))

#------------------------------





