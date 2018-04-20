# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/19/2018
#
# Use the Amelia package to impute missing values
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(stringr) # to help extract meta data from file names
library(ggplot2)

# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
#4/18/2018
# Descriptive statistics and maps for the Uganda Viral Load Dashboard
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)

# -------------------------


# set input/output directory
# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uganda_vl <- readRDS(paste0(dir, "/sex_data.rds"))

# drop out the current month
uganda_vl <- uganda_vl[!(month==3 & year==2018)]

# view the data set and variable names
class(uganda_vl) # check that it is a data table
str(uganda_vl)

# -------------------------
# only working with 2017 for now
 
uganda_vl <- uganda_vl[year==2017]

uganda_vl[,length(unique(facility_id))] #1959 facilities reported in 2017

# -------------------------

facilities <- uganda_vl[,.(facility_id=unique(facility_id), facility_name=unique(facility_name))]
str(facilities)
  

monthly <- uganda_vl[, .(total_facilities=(length(unique(facility_id)))), by=month] 

ggplot (monthly, aes(x=factor(month), y=total_facilities)) + 
  geom_point() + 
  theme_bw()


uganda_vl <- merge(uganda_vl, monthly, all.x=TRUE)
str(uganda_vl)

# -------------------------

uganda_vl[(month==11 & patients_received==1), .(unique(facility_id), patients_received), by=facility_id]









