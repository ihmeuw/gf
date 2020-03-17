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
library(feather)
library(foreign)
library(ggplot2)
library(glue)
library(janitor)
library(stats)
library(stringr)
library(tidyr)
library(tools)
library(rlang)
library(zoo)
library(dplyr)
library(openxlsx)
library(readxl)
library(splitstackshape)

options(scipen=100)
options(digits=6)

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#J:drive filepaths
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j/')
dir = paste0(j, '/Project/Evaluation/GF/resource_tracking/')
combined_output_dir = paste0(dir, "_gf_files_gos/combined_prepped_data/")
mapping_dir = paste0(dir, "modular_framework_mapping/")

#Code filepaths 
code_dir = "./resource_tracking/prep/"
common_dir = paste0(code_dir, "_common/")

#Box filepaths - these should be used to source raw files, and to save final prepped files. 
box = paste0("C:/Users/",user,"/Box Sync/Global Fund Files/")

#Global fund 
# Budget and PUDR file paths are built dynamically in read_filelist code
gos_raw = paste0(dir, "_gf_files_gos/gos/raw_data/")
gos_prepped = paste0(dir, "_gf_files_gos/gos/prepped_data/")

#J: drive file paths - for storing data
# Other development assistance for health 
odah_raw = paste0(dir, "_odah/raw_data/")
odah_prepped = paste0(dir, "_odah/prepped_data/")

#Government health expenditure 
fgh_ghe_malaria_raw = paste0(dir, "_ghe/fgh_ghe_actuals_malaria/raw_data/")
fgh_ghe_malaria_prepped = paste0(dir, "_ghe/fgh_ghe_actuals_malaria/prepped_data/")
fgh_ghe_tb_raw = paste0(dir, "_ghe/fgh_ghe_actuals_tb/raw_data/")
fgh_ghe_tb_prepped = paste0(dir, "_ghe/fgh_ghe_actuals_tb/prepped_data/")
fgh_ghe_hiv_raw = paste0(dir, "_ghe/fgh_ghe_actuals_hiv/raw_data/")
fgh_ghe_hiv_prepped = paste0(dir, "_ghe/fgh_ghe_actuals_hiv/prepped_data/")

who_raw = paste0(dir, "_ghe/who/raw_data/")
who_prepped = paste0(dir, "_ghe/who/prepped_data/")

sicoin_raw = paste0(dir, "_ghe/sicoin_gtm/raw_data/")
sicoin_prepped = paste0(dir, "_ghe/sicoin_gtm/prepped_data/")

all_ghe_prepped = paste0(dir, "_ghe/combined_prepped_data/")

#Assisting files 
codebook = read.xlsx(paste0(dir, "documentation/RT_Codebook.xlsx"))
pudr_labels = read.xlsx(paste0(dir, "documentation/PUDR Semester Labeling.xlsx"))

#Source shared functions
source(paste0(common_dir, "global_variables.R"))
source(paste0(common_dir, "shared_functions.r"), encoding="UTF-8")
# source(paste0(j, '/Project/IRH/HIV/code/currency_conversion.R')) #FGH team's currency conversion function. #Not sourcing as of 10/3/2019 EL


