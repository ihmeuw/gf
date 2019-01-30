# Export an index of all the data sets and elements in DHIS2
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/25/2018
# This code file lists all data elements in DHIS fpor DRC (data points)
# It matches them with their relevant data sets (reports) and exports them
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(dplyr)
library(stringr) 
library(openxlsx)
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# --------------------
# import the list of data elements
elements = readRDS(paste0(dir, 'meta_data/data_elements.rds'))

# import a list of data sets
sets = readRDS(paste0(dir, 'meta_data/data_sets.rds'))
# ----------------------------------------------
# de;lete unecessary variables

elements[ , c('data_set_url', 'element_url'):=NULL]
sets[ , datasets_url:=NULL]

#---------------------------------

#---------------------------------
# export unique lists of data sets and elements with associated sets and ids 
# export as XLSX to capture the special characters

write.xlsx(elements, paste0(dir, 'catalogues/data_elements_cod.xlsx'))
write.xlsx(sets, paste0(dir, 'catalogues/data_sets_cod.xlsx'))

#-------------------------------------
