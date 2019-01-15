# DHIS Extraction - extract the meta data for org units
# Provides a list of health facilities and their locations 
# Includes both source functions and download script

# Caitlin O'Brien-Carelli
# 1/1/19

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
library(profvis)
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

#------------------------------
# read in the organisational unit urls to extract
org_units = readRDS(paste0(dir, 'meta_data/org_units.rds' ))

#---------------------------------------------

#--------------------------------------
# arguments for the function - login information for SNIS 

country = 'drc'
base_url = 'https://www.snisrdc.com'
userID = 'Bethany_Huntley'
password = 'Snisrdcongo1'

#--------------------------------------------
# website for bug fixes (use ancestors for higher level units):
# https://www.snisrdc.com/api/organisationUnits/pCfpKXoGBF8.xml

#-----------------------------
# create a function that extracts the information about organisational units
# includes the associated district, health zone, etc. 

# make dhis urls for each unit

make_dhis_urls = function(base_url) {
  data_sets_url = paste(base_url , '/api/dataSets.xml' , sep = '')
  data_elements_url = paste(base_url , '/api/dataElements.xml' , sep = '')
  org_units_url = paste(base_url , '/api/organisationUnits.xml' , sep = '')
  data_elements_categories = paste(base_url , '/api/categoryOptionCombos.xml' , sep = '')
  data.frame(data_sets_url, data_elements_url , data_elements_categories , org_units_url, stringsAsFactors = FALSE)
}

#-----------------------------
# parses xml pages

parse_page = function(url, userID, password) {
  
  # generate arguments for getURLContent and xmlParse
  url = as.character(url)
  userpwd = paste0(userID, ':', password)
  
  # extract_data
  response = getURLContent(url = url, userpwd = userpwd, httpauth = 1L, header=FALSE, ssl.verifypeer = FALSE)
  parsed_page = xmlParse(response)
  root = xmlRoot(parsed_page)
  return(root)
}

#-----------------------------
# extracvt the meta data associated with each unit

extract_org_unit = function(url, userID, password) {
  root = parse_page(url = url, userID = userID, password = password)
  
  #Extract org unit metadata
  id = xmlAttrs(root)[['id']]
  coordinates = xmlValue(root[['coordinates']])
  opening_date = xmlValue(root[['openingDate']])
  name = xmlValue(root[['displayName']])
  
  # to get the associated health areas, health zones and dps
  # transform the xml into a nested list of lists and extract the ancestors list
  tmp = xmlToList(root)
  ancestors = unlist(tmp$ancestors)

  # create a data frame of the meta data and return it
  org_unit_metadata = data.table(id, coordinates, opening_date,
                                 name, ancestors) 

  return(org_unit_metadata) }
  
#-------------------------------

#extract_dhis_content function
extract_dhis_units = function(base_url, userID, password) {
  print('Making DHIS urls')
  urls = make_dhis_urls(base_url)
  
  #extract information about organisational units: coordinates, data sets, etc.
  print('Extracting units information')
  extracted_org_units = dlply(org_units[group==g], .(org_unit_ID),
                              function(org_units) {
                              try(extract_org_unit(org_units$url, userID, password))},
                              .progress = 'text')  
}

#-------------------------------
# create a group variable in order to loop by group
# the website connection breaks if you try to do all 27,000
# loop through 1,000 at a time and pause for a few minutes after

# subset into 28 separate groups to loop over (27,000 facilities)
vec = c(1:28)
org_units[ , group:=rep(vec, 1000)] # warning is ok

#-------------------------------
# run the extraction 
# loop through each group of 1000 units and download meta data 
# save into separate files of 1000 units each

for (g in org_units$group) {

# arguments for the file name and print statement
x = length(unique(org_units$group))
y = g

# extract the meta data and save as a RDS
units = extract_dhis_units(base_url = base_url, userID = userID, password = password)
saveRDS(units, paste0(dir,'meta_data/units/extracted_org_units_', y, '.rds'))

# pause and notify that a new group is starting
pause(600)
print(paste0("Starting group ", g, " of ", x, "!"))
}

#-------------------------------
# rbind the groups together and save

setwd(paste0(dir, 'meta_data/units/'))

# list existing files
files = list.files('./', recursive=TRUE)
length(files)

i = 1
for(f in files) {
  #load the RDs file
  current_data = readRDS(f)
  current_data = rbindlist(current_data)
  
  # append to the full data 
  if(i==1) full_data = current_data
  if(i>1) full_data = rbind(full_data, current_data)
  i = i+1
}

# save the output
saveRDS(full_data, paste0(dir, 'meta_data/master_facilities.rds'))

#-------------------------------





