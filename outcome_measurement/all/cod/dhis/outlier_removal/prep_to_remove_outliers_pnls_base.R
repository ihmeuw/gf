# Prep ARV data for quantile regression
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/31/19
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(openxlsx)
library(RColorBrewer)

# --------------------

# shell script for working on the cluster
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 10 

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# set working directory to git repository
setwd("C:/Users/ccarelli/local/gf/")

# choose the data set you are working with: pnls or base
# set the disease for base servies - malaria or hiv
set = 'base'
if (set=='base') { qr_disease = 'malaria'}

# read in the data 
if (set=='pnls') {dt = readRDS(paste0(dir, 'prepped/pnls_arv.rds'))}
if (set=='base') {dt = readRDS(paste0(dir, 'prepped/base_services_prepped.rds'))}

# convert values to numerics 
dt[ ,value:=as.numeric(as.character(value))]
dt = dt[!is.na(value)] # 52 values are listed as 'null', drop out missing values

# # subset date
# dt = dt[year(date) < 2019]
# ---------------------------------------
# keep only the malaria-related elements in base services
# if you do this, you will drop out anc visits and audrey will be sad
# if (set=='base') { 
#   dt[grepl('Paludisme', element) | grepl('TDR', element) | grepl('MILD', element) | grepl('Sulfadox', element), disease:='malaria']
#   dt[is.na(disease), disease:='hiv']
#   dt = dt[disease==qr_disease]
#   dt[ , disease:=NULL] }

# drop case and additional geographic information
# there is no element_id as the elements are aggregated 
if (set=='pnls') {byvars = c('element', 'org_unit_id', 'date', 'sex', 'age', 'subpop')}
if (set=='base') {byvars = c('element', 'element_id', 'org_unit_id', 'date', 'category')}

if (set == 'pnls') dt = dt[ ,.(value=sum(value)), by=byvars]
# instead of doing this ^ check unique id's:
if (nrow(unique(dt[, byvars, with = FALSE])) != nrow(dt)) stop("check unique ids - you may need to sum across value, but unique ids do not uniquely id rows")

# remove some variables not needed in base
if (set == 'base'){
  dt[, element:= as.character(element)]
  elements = unique(dt$element)
  elements[grepl(elements, pattern = "A 1.9")]
  elements = elements[!grepl(elements, pattern = "A 1.9")]
  elements[grepl(elements, pattern = "A 3.2")]
  elements = elements[!grepl(elements, pattern = "A 3.2")]
  elements[grepl(elements, pattern = "A 4.5")]
  elements = elements[!grepl(elements, pattern = "A 4.5")]
  elements[grepl(elements, pattern = "A 1.8")]
  elements = elements[!grepl(elements, pattern = "A 1.8")]
  elements[grepl(elements, pattern = "A 4.6")]
  elements = elements[!grepl(elements, pattern = "A 4.6")]
  
  dt = dt[element %in% elements, ]
}

# make variable ids
setnames(dt, 'element_id', 'old_element_id')
dt[, element_id:=.GRP, by='element']

# save the prepped file
if (set=='pnls') {saveRDS(dt, paste0(dir, 'pnls_outliers/arvs_to_screen.rds'))}
if (set=='base') {saveRDS(dt, paste0(dir, 'outliers/base/base_to_screen.rds'))}

# ---------------------------------------
# save old element ids for merge 

elements = dt[ ,.(element=unique(element)), by=.(element_id, old_element_id)]
saveRDS(elements, paste0(dir, 'meta_data/elements_fix.rds'))

# ---------------------------------------



