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
library(ggplot2)
library(glue)
library(readstata13)
library(stats)
library(stringr)
library(splitstackshape)
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
code_dir = "./resource_tracking/prep/"
common_dir = paste0(code_dir, "/_common/")

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
source(paste0(j, '/Project/IRH/HIV/code/currency_conversion.R')) #FGH team's currency conversion function. 

#How to use the FGH team's currency conversion function 
test <- data.table(iso3 = c('UGA','UGA','GTM','GTM','COD','COD'),
                   year = c(2000, 2001, 2003, 2004, 2003,2004),
                   currency_year = c(2000, 2001, 2003, 2004, 2003,2004),
                   val1 = runif(n = 6,min = 4000, max = 8000),
                   val2 = runif(n = 6, min = 300, max = 1000))

test_new_1 <- currency_conversion(data = test,
                                  col.loc = 'iso3',
                                  col.currency.year = 'currency_year',
                                  currency = 'EUR',
                                  col.value = c('val1','val2'),
                                  base.year = 2018,
                                  base.unit = 'USD',
                                  simplify = T,
                                  converter.version = 3)
# 
# `col.loc` is location column(must be iso3 code) in your data,
# `col.value` is value column that you want to be converted (can take multiple value columns)
# `currency` is the raw currency of the value you want to be converted
# `col.currency.year` is the column for year of the currency
# `base.year` is 2018 in your case
# `base.unit` is usd in your case

