# DHIS Extraction - extract the meta data for org units
# Provides a list of health facilities and their locations 
# Caitlin O'Brien-Carelli
# 12/12/18

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
# read in the organisational units 
# org_units = fread(paste0(dir, 'meta_data/org_units_list.rds'))

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

# make dhis urls

make_dhis_urls = function(base_url) {
  data_sets_url = paste(base_url , '/api/dataSets.xml' , sep = '')
  data_elements_url = paste(base_url , '/api/dataElements.xml' , sep = '')
  org_units_url = paste(base_url , '/api/organisationUnits.xml' , sep = '')
  data_elements_categories = paste(base_url , '/api/categoryOptionCombos.xml' , sep = '')
  data.frame(data_sets_url, data_elements_url , data_elements_categories , org_units_url, stringsAsFactors = FALSE)
}

#-----------------------------

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
  ancestors = tmp$ancestors

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
# run the extraction - looping through groups of 1000 at a time

# subset into 28 separate groups
vec = c(1:28)
org_units[ , group:=rep(vec, 1000)]

for (g in org_units$group) {
y = g
units = extract_dhis_units(base_url = base_url, userID = userID, password = password)
saveRDS(units, paste0(dir,'meta_data/units/extracted_org_units_', y, '.rds'))
pause(30)

}

#-----------------------

#-----------------------

#--------------------------------------------------------------
# check the download for error messages
# all of the elements in the list should be data frames (T)
# if some are not data frames, they are error messages

# identify the facilities that generated error messages and delete them from the list 
errors <- sapply(units, is.data.frame)
unique(errors) # if F is included, there are errors
units <- units[errors==T]

# rbind the data frames in the list together to create a data table
units <- rbindlist(units, fill=true())
units <- data.table(units)

#------------------------------
# subset to only the relevant org_unit names and merge

org_units <- org_units_list[ ,.(id=org_unit_ID, org_unit_name)]
org_units[ , id:=as.character(id)]
units[ , id:=as.character(id)]

setnames(units, 'name', 'org_unit')
#------------------------------
# convert factor variables to characters 

units[ ,c('coordinates', 'opening_date', 'active', 'parent_id'):=NULL]
units[ ,org_unit:=as.character(org_unit)]
units[ , organisationUnit:=as.character(organisationUnit)]
units[ , organisationUnit.1:=as.character(organisationUnit.1)]
units[ , organisationUnit.2:=as.character(organisationUnit.2)]
units[ , organisationUnit.3:=as.character(organisationUnit.3)]

#change the names of the hierarchy to merge on
setnames(units, c('organisationUnit', 'organisationUnit.1', 'organisationUnit.2', 
                  'organisationUnit.3'),
         c('country_id', 'dps_id', 'health_zone_id', 'health_area_id'))

#-------------------------------
# merge in the names for the hierarchy 

# merge in the countries to all units
units <- merge(units, org_units, by.x='country_id', by.y='id', all.X=TRUE)
setnames(units, 'org_unit_name', 'country')

# join the dps to all units
setnames(org_units, 'id', 'dps_id')
units <- join(units, org_units, by='dps_id', type='left')
setnames(units, 'org_unit_name', 'dps')

# join the health zones to all units
setnames(org_units, 'dps_id', 'health_zone_id')
units <- join(units, org_units, by='health_zone_id',type='left')
setnames(units, 'org_unit_name', 'health_zone')

# join the health areas to all units
setnames(org_units, 'health_zone_id', 'health_area_id')
units <- join(units, org_units, by='health_area_id', type='left')
setnames(units, 'org_unit_name', 'health_area')

#---------------------------------
# convert factors to characters

units[ ,country:=as.character(country)]
units[ ,dps:=as.character(dps)]
units[ ,health_zone:=as.character(health_zone)]
units[ ,health_area:=as.character(health_area)]

#----------------------------------
# put the variables in an intuitive order

units <- units[  ,.(org_unit, id, country, dps, health_zone, health_area, 
                    country_id, dps_id, health_zone_id, health_area_id)]

#----------------------------------
# save the master list of facilities

saveRDS(units, paste0(dir, 'meta_data/master_facilities_pre_prep.rds'))

#---------------------------------



