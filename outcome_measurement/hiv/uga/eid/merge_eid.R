# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 2/28/2019
# Bind the Early Infant Diagnosis data sets together
# Merge in the information about facilities
# Creates a usable EID data set 
# ----------------------------------------------

# I AM CHANGING THIS CODE AS AN EXAMPLE

# fix this bad code

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(stringr) 
library(plyr)
library(RColorBrewer)
# --------------------

# -----------------------------------------------
# detect if operating on windows or on the cluster 

# root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
# 
# # set the working directory to loop over the downloaded files
# dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/eid/sex_data/')

# # -----------------
# set files and directories for the uganda viral load data

# set the working directory to loop over the downloaded files
dir = ('/Users/caitlinobrien-carelli/Documents/eid/sex_data')
setwd(dir)

# list existing files
files = list.files('./', recursive=TRUE)
length(files)

# --------------

# ----------------------------------------------
# add identifying variables to the existing data tables using file names
# add year, month, sex

# loop over existing files
i = 1
for(f in files) {
  
  #Load the RDs file
  jsonData = readRDS(f)
  
  # pull out relevant table
  current_data = data.table(jsonData$facility_numbers)
  
  # skip to next if there was no data for this combination
  if (length(current_data)==0) next
  
  # rename the facility id variable 
  setnames(current_data, '_id', 'id')

  # extract meta data from file name
  meta_data = strsplit(f, '_')[[1]]
  current_data[, year:=as.numeric(substr(meta_data[3],1,4))]
  current_data[, month:=as.numeric(substr(meta_data[2],1,2))]
  current_data[, sex:=(meta_data[4])]
  current_data[,pcr:=(meta_data[5])]
  
  current_data[pcr=='FIRSTpcr', pcr:='first']
  current_data[pcr=='SECONDpcr', pcr:='second']
  current_data[pcr=='UNKNOWNpcr', pcr:='repeat']
  
  # add a date variable
  current_data[ ,date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]
  
  # append to the full data 
  if(i==1) full_data = current_data
  if(i>1) full_data = rbind(full_data, current_data)
  i = i+1
}

# view the final product of full_data
str(full_data)

#------------------------------------
# drop unnecessary variables

full_data[ ,c('pcr_one', 'pcr_two', 'month', 'pcr_one_hiv_positive_infants', 
       'pcr_two_hiv_positive_infants', 'pcr_one_art_initiated', 
       'pcr_two_art_initiated'):=NULL]

# shorten the hiv positive variable
setnames(full_data, 'hiv_positive_infants', 'hiv_pos_inf')

#---------------
# rename the sex variable

full_data[sex=='f', sex:='Female']
full_data[sex=='m', sex:='Male']
full_data[sex=='UNKNOWN', sex:=NA]

# ----------------------------------------------
# merge in the meta data on districts and facilities

# merge in facility names
facilities = readRDS('/Users/caitlinobrien-carelli/Documents/eid/facility_names.rds')
full_data = merge(full_data, facilities, by='id', all.x=T)

# full_data = merge(full_data, facilities, by='id', all.x=TRUE)
districts = readRDS('/Users/caitlinobrien-carelli/Documents/eid/district_names.rds')

vl = readRDS('/Users/caitlinobrien-carelli/Documents/eid/uvl_facilities_names.rds')



fac = facilities$facility 
dh = vl$dhis2_name
fac[fac %in% dh]

x = vl$facility
fac[x %in% fac]


eid = facilities$facility
vl = vl$facility

length(eid[eid %in% vl])

# -----------------------------------------------
# save the product

#------------------------------------------------
# save the date range for the file name
min = full_data[ , min(year)]
max = full_data[ , max(year)]

# save the merged data
new_dir = dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/eid')
saveRDS(full_data, paste0(new_dir, '/eid_', min, '_', max, '.rds'))

# ----------------------------------------------
