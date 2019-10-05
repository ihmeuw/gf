# Descriptive analysis of stock outs for 2019 reports
# Caitlin O'Brien-Carelli
# 10/1/2019


# ----------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(plyr)
library(data.table)

# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# working directory to aggregate
dt = readRDS(paste0(dir, 'prepped_data/arv_stockouts_2013_2019.rds'))

# subset dates to exclude 2013 as the data are not complete
dt = dt[year!=2013] 

# art specific data set
art = dt[art_site==TRUE]

# ----------------------




