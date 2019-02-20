# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Set up R for running resource tracking
# DATE: Last updated November 2018. 
# ----------------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------

library(lubridate)
library(data.table)
library(readxl)
#library(splitstackshape)
library(stats)
library(stringr)
library(tidyr)
library(tools)
library(rlang)
library(zoo)
library(dplyr)

options(scipen=100)

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#J:drive filepaths
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')
master_file_dir = paste0(dir, "resource_tracking/", country, "/grants/")
export_dir = paste0(dir, "resource_tracking/", country, "/prepped/")
combined_output_dir = paste0(dir, "resource_tracking/multi_country/mapping")

#Code filepaths 
code_loc = ifelse(Sys.info()[1]=='Windows', paste0('C:/Users/', user, '/Documents/gf/'), paste0('/homes/', user, '/gf/'))
code_dir = paste0(code_loc, "resource_tracking/prep/")
gf_prep_code = paste0(code_dir, "global_fund_prep/")
budget_pudr_code = paste0(gf_prep_code, "budget_pudr_prep/")
country_code_dir <- paste0(budget_pudr_code, country, "_prep/")
gos_code = paste0(gf_prep_code, "gos_prep/")

#Source shared functions
source(paste0(code_dir, "shared_prep_functions.R"), encoding="UTF-8")
