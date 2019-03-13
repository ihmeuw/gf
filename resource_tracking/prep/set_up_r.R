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
library(foreign)
library(glue)
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
code_loc = ifelse(Sys.info()[1]=='Windows', paste0("C:/Users/", user, "/Documents/gf/"), paste0('/homes/', user, '/gf/'))
code_dir = paste0(code_loc, "resource_tracking/prep/")
gf_prep_code = paste0(code_dir, "global_fund_prep/")
budget_pudr_code = paste0(gf_prep_code, "budget_pudr_prep/")
country_code_dir <- paste0(budget_pudr_code, country, "_prep/")
gos_code = paste0(gf_prep_code, "gos_prep/")

#Source shared functions
source(paste0(code_dir, "shared_prep_functions.R"), encoding="UTF-8")

#Source document prep functions 
setwd(budget_pudr_code)
doc_prep_functions = list.files()
for (file in doc_prep_functions){
  source(file)
}

#------------------------------------------------------
# Set shared values to be used throughout the database. 
#------------------------------------------------------

# ---------------------------------------------------------------------------------------------------
# #Mark which grants are currently active to save in file - this should be updated every grant period! 
# ----------------------------------------------------------------------------------------------------
current_gtm_grants <- c('GTM-H-HIVOS', 'GTM-H-INCAP', 'GTM-M-MSPAS', 'GTM-T-MSPAS')
current_gtm_grant_period <- c('2018', '2019-2020', '2018-2020', '2016-2019')

current_cod_grants <- c('COD-C-CORDAID', 'COD-H-MOH', 'COD-T-MOH', 'COD-M-MOH', 'COD-M-SANRU')
current_cod_grant_period <- rep("2018-2020", 5)

current_uga_grants <- c('UGA-C-TASO', 'UGA-H-MoFPED', 'UGA-M-MoFPED', 'UGA-M-TASO', 'UGA-T-MoFPED')
current_uga_grant_period <- rep("2018-2020", 5)

#---------------------------------------------------------------------------------------------
# Store a list of the IHME internal country codes that correspond to our project's iso codes. 
# To be used throughout the database for consistency. 
#---------------------------------------------------------------------------------------------
code_lookup_tables = data.table(ihme_country_code = c(128, 190,171, 216), iso_code = c('gtm', 'uga', 'cod', 'sen'))

#Given a data table and a column that contains the IHME code, returns a column with the matching ISO code. 
gen_iso_code = function(dt, ihme_code_col){
  for (i in 1:nrow(code_lookup_tables)){
    dt[ihme_code_col == code_lookup_tables$ihme_country_code[i], iso_code:=code_lookup_tables$iso_code[i]]
  }
  return(dt)
}

#Given a data table and a column that contains the country ISO code, returns a column with the matching IHME country code. 
gen_ihme_country_code = function(dt, iso_col){
  for (i in 1:nrow(code_lookup_tables)){
    dt[iso_col == code_lookup_tables$iso_code[i], ihme_country_code:=code_lookup_tables$ihme_country_code[i]]
  }
  return(dt)
}
