# ----------------------------------------------
# Caitlin O'Brien- Carelli
# 6/5/2019
#
# Format SIGSA HIV testing data
# Rewrite later as a function - many overlapping processes
# ----------------------------------------------

#-----------------------------------------------
# Load packages 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(ggplot2)
library(Hmisc)
library(stringr)
library(readxl)

#---------------------------------------
# Set up directories 
#----------------------------------------

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')
setwd(paste0(dir, 'sigsa/'))

# to output prepped files
out_dir = paste0(dir, 'prepped/sigpro/')

#-----------------------------------------
# load in the file 

dt = read_excel(paste0(dir,'sigsa/Patient Level Data/Solicitud 0593-2018 SIGSA SIDA 1.2 años 2014 al 2017.xlsx'), skip=1)

#-----------------------------------------
# format the column names and drop unecessary variables 




#-----------------------------------------

