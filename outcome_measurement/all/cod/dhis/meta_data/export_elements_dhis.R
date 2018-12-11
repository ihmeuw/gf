# Export an index of all the data sets and elements in DHIS2
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/13/2018
# This code file lists all data elements in DHIS fpor DRC (data points)
# It matches them with their relevant data sets (reports) and exports them
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(stringr) 
library(xlsx)
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# import the list of data elements
elements <- readRDS(paste0(dir, 'meta_data/updated_data_elements.rds'))

# ----------------------------------------------
# View the elements and data sets and export a list

# view the first six lines to ensure sets and elements are included
head(elements)

# change it into a data table
elements <- data.table(elements)

# rename the variables
setnames(elements, c("data_element_ID", "datasets_ID", "datasets_name", "displayName", "url_list"),
          c("element_id", "data_set_id", "data_set", "element", "url"))

elements[ , url:=NULL]

#---------------------------------
# create a variable specifying whether the element is relevant to the PCE using old sheet

# merge in the elements to keep
keep <- read.csv(paste0(dir, 'catalogues/archive/elements_to_keep.csv'))
setnames(keep, 'data_element_ID', 'element_id')
elements <- merge(elements, keep, by='element_id', all=T)

elements[is.na(keep), keep:=0]
elements[is.na(tableau), tableau:=0]

#---------------------------------
# export unique lists of data sets and elements with associated ids 
# export as XLSX to capture the special characters

# export a list of data sets separate from the elements
# sets <- elements[ ,.(data_set=unique(data_set), data_set_id=unique(data_set_id))]
# write.xlsx(sets, paste0(dir, 'catalogues/data_sets_cod.xlsx') )


write.xlsx(elements, paste0(dir, 'catalogues/data_elements_cod.xlsx'))

#-------------------------------------
