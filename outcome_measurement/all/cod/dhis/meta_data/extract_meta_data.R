# DHIS Extraction for DRC: Meta data extractopm
# Caitlin O'Brien-Carelli
# Meta data extraction code for the DHIS Extraction tool
# Extracts the names of variables, age/sex categories, data sets, facilities
# 2/20/2020

#---------------------
# This code can be run locally or on the cluster

# Calls the extracting functions (dhis_extracting_functions.R)
# Extracts all meta data to run the extraction 

#--------------------
# Acts as a single function extract_dhis_content:
# this function runs multiple functions to extract:
#

# --------------------
# Set up R
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(reshape)
library(RCurl)
library(XML)
library(profvis)
library(plyr)
library(openxlsx)

# --------------------
# detect the user 

user = Sys.info()[['user']]

#----------------------
# set the working directories and source functions 

# determine if the code is being run on the cluster or on home computer
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working directory 
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
setwd(dir)

# set the output directory
out_dir = paste0(dir, '0_meta_data/')

# library for the dhisextractr package
dirx = paste0(dir, 'packages/')

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
  # this extracts the list of health facilities, but not their associated geographic information 

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
# RUN THE META DATA EXTRACTION
# the main function outputs a list item 

DRC_extraction = extract_dhis_content(base_url = base_url, userID = userID, password = password)


#-----------------------------

#------------------------------
# save the data 

# extract data tables from the large list of meta data 
data_sets = DRC_extraction[1][[1]] # sets where the data live
updated_data_elements = DRC_extraction[2][[1]] # variable names 
categories = DRC_extraction[3][[1]] # age, sex, and inventory categories
org_units = DRC_extraction[4][[1]] # health facility names 

# save all the RDS files to the J Drive

saveRDS(data_sets, paste0(out_dir, 'data_sets.rds'))
saveRDS(updated_data_elements, paste0(out_dir, 'updated_data_elements.rds'))
saveRDS(categories, paste0(out_dir, 'data_elements_categories.rds'))
saveRDS(org_units, paste0(out_dir, 'org_units.rds'))

#--------------------------
