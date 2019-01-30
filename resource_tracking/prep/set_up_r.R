# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Master file for updating resource tracking database. 
# DATE: Last updated November 2018. 
# ----------------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------

# ----------------------------------------------------------------------
# To do list for this code: 
# - add in an option to only rework one file (make database append-only)
# - add in variable creation during the append step to flag current grants. 
# ---------------------------------------------------------------------

rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(tidyr)
library(tools)
library(rlang)
library(zoo)
library(dplyr)

options(scipen=100)

# ---------------------------------------
#Boolean logic switches 
# ---------------------------------------
prep_files <- FALSE 
prep_gos <- TRUE

include_stops = FALSE #Set to true if you would like scripts to stop when errors are found (specifically, module mapping)
verbose = FALSE #Set to true if you would like warning messages printed (helpful for debugging functions). Urgent messages will always be flagged regardless of this switch. 
limit_filelist <- TRUE #Set to TRUE if you want to only run files that will be saved in final budgets and expenditures. 

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#Replace global variables to match what code you want to run. 
user = "elineb" #Change to your username 
country <- c("cod") #Change to the country you want to update. 

#Mark which grants are currently active to save in file - this should be updated every grant period! 
current_gtm_grants <- c('GTM-H-HIVOS', 'GTM-H-INCAP', 'GTM-M-MSPAS', 'GTM-T-MSPAS')
current_gtm_grant_period <- c('2018', '2019-2020', '2018-2020', '2016-2019')

current_cod_grants <- c('COD-C-CORDAID', 'COD-H-MOH', 'COD-T-MOH', 'COD-M-MOH', 'COD-M-SANRU')
current_cod_grant_period <- rep("2018-2020", 5)

current_uga_grants <- c('UGA-C-TASO', 'UGA-H-MoFPED', 'UGA-M-MoFPED', 'UGA-M-TASO', 'UGA-T-MoFPED')
current_uga_grant_period <- rep("2018-2020", 5)

#Filepaths
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')
code_loc = ifelse(Sys.info()[1]=='Windows', paste0('C:/Users/', user, '/Documents/gf/'), paste0('/homes', user, '/gf/'))
code_dir = paste0(code_loc, "resource_tracking/prep/")
combined_output_dir = paste0(j, "resource_tracking/multi_country/mapping")
source(paste0(code_dir, "shared_mapping_functions.R")) 
