# Prep the DHIS2 data for COD
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 6/13/2018
#
# upload the RDS files scraped from DHIS2
# prep the data sets for analysis and Tableau Dashboard

# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(stringr) # to extract meta data from file names
# --------------------

# --------------------
# detect if operating on windows or on the cluster 

if (Sys.info()[1] == 'Windows') {
  username <- "ccarelli"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}

# --------------------
# Import baser services data set and convert to a data table

base <- readRDS(paste0(root, 'Project/Evaluation/GF/outcome_measurement/cod/dhis/base_services_drc_01_2015_04_2018.rds'))

base <- data.table(base)



