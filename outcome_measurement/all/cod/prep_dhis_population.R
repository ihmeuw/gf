# Export an index of all the data sets and elements in DHIS2
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/10/2018
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

# import the Population data set and convert to a data table
pop <- read.csv(paste0(dir, 'pre_merge/pop_extraction_drc_2016_2018.csv'))
pop <- data.table(pop)
pop[ , X:=NULL] # drop the random column Excel adds

#--------------------------------

# import the meta data for the merge
data_sets<- data.table(readRDS(paste0(dir, 'meta_data/data_sets.rds'))) # not necessary for the merge
org_units <- data.table(readRDS(paste0(dir, 'meta_data/org_units_list.rds')))
data_elements <- data.table(readRDS(paste0(dir, 'meta_data/updated_data_elements.rds')))
data_elements_categories <- data.table(readRDS(paste0(dir, 'meta_data/data_elements_categories.rds')))
org_units_description <- data.table(readRDS(paste0(dir, 'meta_data/org_units_description.rds')))

# change the names of the ID variables in element categories and descriptions to match for the merge
data_elements[ , element_name:=displayName]
data_elements[ , displayName:=NULL]

data_elements_categories <- data_elements_categories[ ,.(category=ID, category_name=displayName)]
org_units_description <- org_units_description[ ,.(org_unit_ID = id, coordinates, opening_date, parent_id)]


#--------------------------------


#------------------------
# merge in the names of the objects in the data set

# merge on org_unit_ID to get names and descriptions of organisational units
pop <- merge(pop, org_units, by='org_unit_ID', all.x=TRUE)
pop <- merge(pop, org_units_description, by='org_unit_ID', all.x=TRUE)
pop[ ,length(unique(org_unit_ID))] # print the number of organisational units

# merge on data element ID to get data sets and data sets name
pop <- merge(pop, data_elements, by='data_element_ID', all.x=TRUE)

# merge on category id to get age and sex categories for the data elements
pop <- merge(pop, data_elements_categories, by='category', all.x=TRUE)
setnames(pop, c('category', 'category_name'), c('category_id', 'category'))

# drop unnecessary urls
pop[ , org_unit_url:=NULL]
pop[ , url_list:=NULL]
#------------------------


ha <- org_units_description


# Health areas
ha[ , name:=tolower(name)]
health_area <- grep(pattern="aire de santé", x=ha$name)
ha[health_area, level:="Health Area"]
health_area <- grep(pattern="aire de sante", x=ha$name)
ha[health_area, level:="Health Area"]

ha[ ,unique(level)]

ha <- ha[!is.na(level)]

