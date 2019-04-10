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
library(readstata13)
library(stats)
library(stringr)
library(tidyr)
library(tools)
library(rlang)
library(zoo)
library(dplyr)
library(openxlsx)

options(scipen=100)

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#J:drive filepaths
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/resource_tracking/')
combined_output_dir = paste0(dir, "_gf_files_gos/combined_prepped_data/")
mapping_dir = paste0(dir, "modular_framework_mapping/")

#Code filepaths 
repo = ifelse(Sys.info()[1]=='Windows', paste0("C:/Users/", user, "/Documents/gf/"), paste0('/homes/', user, '/gf/'))
code_dir = paste0(repo, "resource_tracking/prep/")
common_dir = paste0(code_dir, "_common/")

fgh_raw = paste0(dir, "_fgh/raw_data/")
fgh_prepped = paste0(dir, "_fgh/prepped_data/")

gos_raw = paste0(dir, "_gf_files_gos/gos/raw_data/")
gos_prepped = paste0(dir, "_gf_files_gos/gos/prepped_data/")

fgh_ghe_malaria_raw = paste0(dir, "_ghe/fgh_ghe_actuals_malaria/raw_data/")
fgh_ghe_malaria_prepped = paste0(dir, "_ghe/fgh_ghe_actuals_malaria/prepped_data/")

who_raw = paste0(dir, "_ghe/who/raw_data/")
who_prepped = paste0(dir, "_ghe/who/prepped_data/")

sicoin_raw = paste0(dir, "_ghe/sicoin_gtm/raw_data/")
sicoin_prepped = paste0(dir, "_ghe/sicoin_gtm/prepped_data/")

codebook = read.xlsx(paste0(dir, "documentation/RT_Codebook.xlsx"))

#Source shared functions
source(paste0(common_dir, "global_variables.R"))
source(paste0(common_dir, "shared_string_functions.R"), encoding="UTF-8")
source(paste0(common_dir, "shared_calculation_functions.R"))

