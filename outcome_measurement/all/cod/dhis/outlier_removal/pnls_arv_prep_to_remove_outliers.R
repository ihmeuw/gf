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
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 10 -P snis_download

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
setwd("C:/Users/ccarelli/local/gf/")

# read in the data 
dt = readRDS(paste0(dir, 'prepped/pnls_arv.rds'))

# subset to before August of 2018
dt = dt[date < '2018-09-01']

# ---------------------------------------
# source the standardization functions for the geographic units and apply
source("./core/standardizeHZNames.R")
source("./core/standardizeDPSNames.R")
dt$health_zone = standardizeHZNames(dt$health_zone)
dt$health_zone = standardizeDPSNames(dt$dps)

# ---------------------------------------

# drop case and additional geographic informatioj
dt = dt[ ,.(value=sum(value)),
         by=.(element, org_unit_id, date, sex, age, subpop)]

dt = dt[element=="PLHIV on IPT"]

# make variable ids
dt[, element_id:=.GRP, by='element']

# save the prepped file
saveRDS(dt, paste0(dir, 'pnls_outliers/arvs_to_screen.rds'))

# ---------------------------------------
















