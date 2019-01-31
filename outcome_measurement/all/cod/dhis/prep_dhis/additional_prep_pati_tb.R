# ----------------------------------------------
# Audrey Batzel
#
# 1/23/19
# Prep PATI TB data sets from SNIS
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
setwd('C:/local/gf/')
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/pre_prep/merged/')
out_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/pati_tb/')

# input files
registered <- paste0(dir, "tb_pati_v_registered_2017_01_01_2018_10_01.rds")
result <- paste0(dir,"tb_pati_v_result_2016_01_01_2018_10_01.rds")
#base <- paste0(dir, "base_2016_01_01_2018_12_01.rds")
  
# output files

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ----------------------------------------------

# ----------------------------------------------
# Read in data
# ----------------------------------------------
dt1 <- readRDS(registered)
dt2 <- readRDS(result)
base <- readRDS(base)
# ----------------------------------------------

# ----------------------------------------------
# more prep
# ----------------------------------------------
dt1$value <- as.character(dt1$value)
dt1$value <- as.numeric(dt1$value) # the warning that this introduces NAs can be ignored, because all of those NAs were "NULL" in the original data

dt1$health_zone <- standardizeHZNames(dt1$health_zone)
dt1$dps <- standardizeDPSNames(dt1$dps)

dt2$value <- as.character(dt2$value)
dt2$value <- as.numeric(dt2$value)

dt2$health_zone <- standardizeHZNames(dt2$health_zone)
dt2$dps <- standardizeDPSNames(dt2$dps)
# ----------------------------------------------

# ----------------------------------------------
# separate new-TB cases and relapses by age group and sex variable from the rest of the data since this is the only one broken down by age and sex
# ----------------------------------------------
cases_by_age_sex <- dt1[category != "default", ]
reg <- dt1[category == "default", ]

cases_by_age_sex$category <- as.character(cases_by_age_sex$category)
cases_by_age_sex[, c("age", "sex") := tstrsplit(category, ",", fixed=TRUE)]
cases_by_age_sex$sex <- trimws(cases_by_age_sex$sex)
cases_by_age_sex$age <- trimws(cases_by_age_sex$age)

cases_by_age_sex[, category := NULL]
# vars <- colnames(cases_by_age_sex)
# vars <- vars[!vars %in% c("sex", "value")]
# formula_for_cast <- as.formula(paste(paste(vars, collapse=" + "), "sex", sep= " ~ "))
# cases_by_age_sex <- dcast.data.table(cases_by_age_sex, formula_for_cast, value.var = "value")

# drop unneeded vars
cases_subset <- cases_by_age_sex[, .(date, quarter, dps, health_zone, health_area, org_unit, org_unit_id, org_unit_type, level, element, element_eng, age, sex, value)]

saveRDS(cases_subset, paste0(out_dir, "tb_pati_new_tb_cases_relapses_by_age_sex.rds"))
saveRDS(reg, paste0(out_dir, "tb_pati_cases_registered.rds"))
# ----------------------------------------------










