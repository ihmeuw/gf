# Merge the Base Services, SIGL, and PNLS data downloaded from DHIS2 DRC (SNIS)
# Caitlin O'Brien-Carelli
#
# 6/11/2019
#
# Upload the RDS data from DHIS2 and merge with the meta data 
# prep the data sets for analysis and the Tableau Dashboard
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(openxlsx)
library(lubridate)
library(dplyr)

# increase memory of RStudio session
memory.limit(size = 20000)

# --------------------
# merge on the cluster
# files take a long time to load - merge in a cluster IDE

# script to open a long-lasting large IDE
# qsub -terse -N rst_ide_19_05_14_160329 -q long.q -l fthread=20 -l m_mem_free=20G -l h_rt=70:00:00 -e archive=TRUE -P proj_pce /ihme/code/jpy_rstudio/jpy_rstudio_shell.sh -i /ihme/singularity-images/rstudio/ihme_rstudio_3501.img -t rstudio -p 1247 -o 1 -G r

# ---------------------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
# dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# local directory 
dir = 'C:/Users/ccarelli/Documents/dhis_data/'

# source the merge and prep functions from the J Drive
# source(paste0(dir, 'code/merge_functions.R'))

#---------------------------------

#---------------------------------
# change the folder to the name of the data set you want to merge
# this is the only argument to change 

folder = 'pnls'
#---------------------------------
# create a vector of variables to subset the larger data sets 

if (folder=='base' | folder=='sigl') {
  keep_vars = read.xlsx(paste0(dir, 'catalogues/data_elements_cod.xlsx'))
  keep_vars = data.table(keep_vars)
  keep_vars[ , keep:=as.numeric(keep)]
  keep_vars = keep_vars[keep==1, element_id]
}

#---------------------------------
# drop diacritical marks
# leave in script: sourcing this function on the cluster alters the function

fix_diacritics = function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  replace_me = paste(names(replacement_chars), collapse='')
  replace_with = paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x)) }

#---------------------------------
# set the working directory and read in the files
# setwd(paste0(dir, 'pre_prep/', folder, '/intermediate_data/'))

# local working directory 
setwd('C:/Users/ccarelli/Documents/dhis_data/pre_prep/pnls/intermediate_data')

# list the files in the working directory
files = list.files('./', recursive=TRUE)

#---------------------------------
# merge in categories piecewise as the category merge myseteriously breaks the code 

# load the categories 
categories = data.table(readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')))
categories[ , url_list:=NULL]
categories[ , ID:=as.character(ID)]
categories[ , displayName:=as.character(displayName)]

setnames(categories,'displayName', 'category_name')
setnames(categories,'ID', 'category')

#---------------------------------
# read in the files
i = 1

for(f in files[1:15]) {
  
  #load the RDs file
  file_name = f
  current_data = data.table(readRDS(f))
  current_data[ , file:=file_name]
  
  # skip blank downloads
  if(nrow(current_data)==0) i = i+1
  if(nrow(current_data)==0) next
  
  # add download number if it is not already included
  download = str_split(file_name, '_')[[1]][6]
  if (download=='first') current_data[ , download_number:=1]
  if (download=='second') current_data[ , download_number:=2]

  # subset to only the variables needed for large data sets
  if (folder=='base' | folder=='sigl') {
  current_data[ , data_element_ID:=as.character(data_element_ID)]
  current_data = current_data[data_element_ID %in% keep_vars]
  }

  # add a date variable
  current_data[ , date:=paste0(as.character(period), '01')]
  current_data[ , date:=as.Date(date, format='%Y%m%d')]
  current_data[ , c('period', 'group'):=NULL]
  
  # convert variable types
  current_data[ , data_element_ID:=as.character(data_element_ID)]
  current_data[ , org_unit_ID:=as.character(org_unit_ID)]
  current_data[ , category:=as.character(category)]
  
  # create a date variable based on last update 
  current_data[ , last_update:=as.character(last_update)]
  current_data$last_update = unlist(lapply(str_split(current_data$last_update, 'T'), '[', 1))
  current_data[ ,last_update:=as.Date(last_update, format="%Y-%m-%d")]

  # append to the full data
  if(i==1) dt1 = current_data
  if(i>1)  dt1 = rbind(dt1, current_data)
  
  print(paste("Rbound", file_name, "to the full data"))
  print(paste("Index:", i))
  
  # save the completed rbind for the first half of the data 
  if (i==15) dt1 = merge(dt1, categories, by='category', all.x=T)
  if (i==15) saveRDS(dt1, paste0(dir, 'pre_prep/', folder, '/', folder, '_first_half.rds'))
  i = i+1
}

# start indexing over 
i = 1

for(f in files[16:length(files)]) {
  #load the RDs file
  file_name = f
  current_data = data.table(readRDS(f))
  current_data[ , file:=file_name]
  
  # skip blank downloads
  if(nrow(current_data)==0) i = i+1
  if(nrow(current_data)==0) next
  
  # add download number if it is not already included
  download = str_split(file_name, '_')[[1]][6]
  if (download=='first') current_data[ , download_number:=1]
  if (download=='second') current_data[ , download_number:=2]
  
  # subset to only the variables needed for large data sets
  if (folder=='base' | folder=='sigl') {
    current_data[ , data_element_ID:=as.character(data_element_ID)]
    current_data = current_data[data_element_ID %in% keep_vars]
  }
  
  # add a date variable
  current_data[ , date:=paste0(as.character(period), '01')]
  current_data[ , date:=as.Date(date, format='%Y%m%d')]
  current_data[ , c('period', 'group'):=NULL]
  
  # convert variable types
  current_data[ , data_element_ID:=as.character(data_element_ID)]
  current_data[ , org_unit_ID:=as.character(org_unit_ID)]
  current_data[ , category:=as.character(category)]
  
  # create a date variable based on last update 
  current_data[ , last_update:=as.character(last_update)]
  current_data$last_update = unlist(lapply(str_split(current_data$last_update, 'T'), '[', 1))
  current_data[ ,last_update:=as.Date(last_update, format="%Y-%m-%d")]

  # append to the full data
  if(i==1) dt2 = current_data
  if(i>1)  dt2 = rbind(dt2, current_data)
  
  print(paste("Rbound", file_name, "to the full data"))
  print(paste("Second round index:", i))
  
  # save the completed rbind for the second half of the data 
  # merge in the categories
  if (i==length(files)-15) dt2 = merge(dt2, categories, by='category', all.x=T)
  # if (i==length(files)-15) saveRDS(dt2, paste0(dir, 'pre_prep/', folder, '/', folder, '_second_half.rds'))
  i = i+1
  
}

#---------------------------------
# perform distinct functions on half the data each time 

# bind the two sets together
dt = data.table(rbind(dt1, dt2))

# drop out the additional large files
dt1 = NULL
dt2 = NULL

#---------------------------------
# remove the factoring of value to avoid errors
# introduces some NAs as some values are NULL

dt[ , value:=as.numeric(as.character(value))] 
print(paste0("There are ", sum(is.na(dt)) , " missing values in the raw data."))
dt = dt[!is.na(value)] 

#---------------------------------
# save the interim raw data before the merge with the meta data 

# include the date range in the file name
min_date = dt[ , min(date)]
min_date = gsub('-', '_', min_date)
max_date = dt[ , max(date)]
max_date = gsub('-', '_', max_date)

# save the raw data before the merge 
saveRDS(dt, paste0(dir, 'pre_prep/', folder, '/', folder, min_date, '_', max_date, 'full.rds'))
print("File saved!")

#----------------------------------------------------------------

#----------------------------------------------------------------
# read in the interim file you saved

# file_name1 = paste0(dir, 'pre_prep/', folder, '/', folder, min_date, '_', max_date, 'full.rds')
# dt = readRDS(file_name1)

#---------------------------------
# collapse across the file names 
test_rows = nrow(dt)

byVars = names(dt)[names(dt)!='file' & names(dt)!='download_number' & names(dt)!='value']
dt = dt[ ,.(value=sum(value)), by=byVars]

# unit test of unique identifiers
if (test_rows == nrow(dt)) { print("There is no overlap between downloads.")
} else {print("Houston, we have a problem.")}

#---------------------------------
# merge in the meta data 
# includes english translations to be formatted later

# read in master facilities and variable names
facilities = data.table(readRDS(paste0(dir, 'meta_data/master_facilities.rds')))
data_elements = data.table(readRDS(paste0(dir, 'meta_data/updated_data_elements.rds')))

#---------------------------------
# format health facility information, including geographic location 

# replace dps with the name only
facilities$dps1 = unlist(lapply(strsplit(facilities$dps, " "), "[", 2))
facilities$dps2 = unlist(lapply(strsplit(facilities$dps, " "), "[", 3))
facilities[dps2!='Province', dps:=paste(dps1, dps2)]
facilities[dps2=='Province', dps:=dps1]
facilities[ , c('dps1', 'dps2'):=NULL]

# replace health zone with the name only
facilities$health_zone1 = unlist(lapply(strsplit(facilities$health_zone, " "), "[", 2))
facilities$health_zone2 = unlist(lapply(strsplit(facilities$health_zone, " "), "[", 3))
facilities$health_zone3 = unlist(lapply(strsplit(facilities$health_zone, " "), "[", 4))
facilities[health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone:=paste(health_zone1, health_zone2, health_zone3) ]
facilities[health_zone3=='Zone', health_zone:=paste(health_zone1, health_zone2)]
facilities[health_zone2=='Zone', health_zone:=health_zone1]
facilities[ , c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]

#-------------------
# change the names of the ID variables in elements and categories to match for the merge
setnames(dt, 'org_unit_ID', 'org_unit_id')
setnames(dt, 'data_element_ID', 'data_element_id')

#--------------------------------------------------------
# merge in the meta data 

# merge in the facilities meta data 
dt = merge(dt, facilities, by='org_unit_id', all.x=T)

# merge in the data elements
# some data elements contain duplicate ids - set if statements for these sets
if (folder=='pnls') dt[ , data_set_id:='wIMw0dzITTs']
if (folder=='base') dt[ , data_set_id:='pMbC0FJPkcm']
dt[ , data_set_id:=as.character(data_set_id)]

# subset down the list of elements to the data set being processed
sub_id = dt[ , unique(data_set_id)]
data_elements = data_elements[datasets_ID==sub_id]

# drop unecessary variables
data_elements[ , c('datasets_url', 'data_element_url'):=NULL]

#-------------------------------
# format variable names 

if (folder=='pnls') {

# create a variable that contains only the last word
data_elements[ , data_element_name:=as.character(data_element_name)] # confirm element is a character variable
data_elements[ , last:=word(data_element_name, -1)]
data_elements[ , last:=tolower(last)]

# drop anything containing soutien as the last word
# these elements are duplicates or totals of other variables
data_elements[grep('soutien', last), drop:=TRUE]

# drop out elements that end in 'sex' or 'age'
# these variables are stratified only by sex or age, while others include both sex/age
data_elements[grep('sex', last), drop:=TRUE]
data_elements[grep('age', last), drop:=TRUE]
data_elements[is.na(drop), drop:=FALSE]

#---------------------------------------
# create a data set variable based on element codes
data_elements[ , set:=(unlist(lapply(strsplit(data_element_name, "-"), '[', 2)))]

# translate groupings
data_elements[set=='CDV', set:='VCT']
data_elements[set=='IST', set:='STI']
data_elements[set=='PTME', set:='PMTCT']

#---------------------------------------
# create an element that is easier to grep
data_elements[ , element1:=tolower(data_element_name)]

# run the function to eliminate diacritical marks
data_elements[ , element1:=fix_diacritics(element1)]

#---------------------------------------
# create new variable names without 'PNLS' or set code

# drop the pnls and set codes from the variable names
# in 'co-infected' elements, there are three hyphens
data_elements[ , element_new1:=(unlist(lapply(strsplit(data_element_name, "-"), '[', 3)))]
data_elements[ , element_new2:=(unlist(lapply(strsplit(data_element_name, "-"), '[', 4)))]
data_elements[ , element_new3:=(unlist(lapply(strsplit(data_element_name, "-"), '[', 5)))]

data_elements[!is.na(element_new2) & !is.na(element_new3), element_new:=paste0(element_new1, "-", element_new2, "-", element_new3)]
data_elements[!is.na(element_new2) & is.na(element_new3), element_new:=paste0(element_new1, "-", element_new2)]
data_elements[is.na(element_new2), element_new:=element_new1]

# drop out the variables used to create new elements
data_elements[ , c('last', 'element1','element_new1', 'element_new2', 'element_new3', 'data_element_name'):=NULL]
setnames(data_elements, 'element_new', 'element')

}

#--------------------------------------------
# change name only for other data sets; already changes in pnls
if (folder!='pnls') setnames(data_elements, 'data_element_name', 'element')

#---------------------------------------
# merge in the variable names 
data_elements[, datasets_ID:=NULL]
dt = merge(dt, data_elements, by='data_element_id', all.x=T)

# fix variable types 
dt[ , element:=as.character(element)] # check name is accurate
dt[ , data_element_id:=as.character(data_element_id)]
dt[ , datasets_name:=as.character(datasets_name)]

# drop unecessary variables to simplify
dt[ , c('category', 'opening_date', 'data_set_id'):=NULL] 
setnames(dt, 'category_name', 'category')

print("Metadata merged; beginning to add translations.")

#-------------------
# merge in the english translations
translations = data.table(readRDS(paste0(dir, 'meta_data/data_elements.rds')))
translations = translations[ ,.(data_set_id, data_element_id=element_id, element_eng)]
translations = translations[data_set_id==sub_id]
translations[ ,data_set_id:=NULL]
dt = merge(dt, translations, by='data_element_id', all.x=T)

#--------------------------------------
# change the names to intuitive naming conventions

# create vectors of old and new variable names
old_names = names(dt)
if (folder=='pnls') { new_names = c("element_id", "org_unit_id", "last_update", "date", "category",
              "value", "coordinates", "org_unit", "country", "dps", "health_zone", 
              "health_area", "org_unit_type", "facility_level", "data_set", 
              "drop", "pnls_set", "element", "element_eng")
          } else {new_names = c("element_id", "org_unit_id", "last_update", "date", "category",
                    "value", "coordinates", "org_unit", "country", "dps", "health_zone", 
                    "health_area", "org_unit_type", "facility_level", "data_set",
                    "element", "element_eng")}

# check the vectors are the same length
if (length(old_names)!= length(new_names)) print("Something's up. :(")

# reset the names
setnames(dt, old_names, new_names)

#--------------------------------------
# save the merged rds file 

# arguments for the save
min = dt[ , min(date)]
min = gsub('-', '_', min)
max = dt[ , max(date)]
max = gsub('-', '_', max)

# save a merged rds file 
saveRDS(dt, paste0(dir, 'pre_prep/merged/', folder,'_full_', min, '_', max, '_LOCAL.rds' ))

#------------------------------------------

#-----------------------------------------------------------
# read in the full data set you saved

# dt = readRDS(paste0(dir, 'pre_prep/merged/', folder,'_full_', min, '_', max, '.rds' ))

# local copy of the full data set 
# dt = readRDS("C:/Users/ccarelli/Documents/dhis_data/pre_prep/merged/pnls_full_2017_01_01_2019_04_01.rds")


if (folder=='pnls') dt = dt[drop==FALSE]
if (folder=='pnls') dt[ ,element_eng:=]

  #--------------------------------------
  # save a subsetted version of pnls with only relevant variables
  # variables in pnls are repeated based on stratification 
  # drop duplicates and save

if (folder=='pnls') {
  
  for (s in unique(dt$set)) {
    x = dt[set==s]
    
    
  }
  
  
}
  
  
saveRDS(dt, paste0(dir, 'pre_prep/merged/', folder,'_subset_', min, '_', max, '.rds' ))


#---------------------------------------

