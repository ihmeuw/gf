# ----------------------------------------------
# Caitlin O'Brien- Carelli
# Visualize the SIGPRO testing data sets

# ----------------------------------------------

#-----------------------------------------------
# Install packages 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(openxlsx)
library(ggplot2)
library(Hmisc)
library(stringr)
library(XLConnect)
#---------------------------------------
# Set up directories 
#----------------------------------------

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')

#----------------------------------------
# load sigpro testing data 

dt = readRDS(paste0(dir, 'prepped/sigpro_testing_transfer_prepped.RDS'))

#----------------------------------------