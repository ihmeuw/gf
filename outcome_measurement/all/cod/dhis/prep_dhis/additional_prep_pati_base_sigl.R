# ----------------------------------------------
# Audrey Batzel
#
# 1/23/19
# Prep PATI TB, base services, and SIGL data sets from SNIS
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
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
out_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')

# input files
registered <- paste0(dir, "tb_pati_v_registered_2017_01_01_2018_10_01.rds")
results <- paste0(dir,"tb_pati_v_result_2016_01_01_2018_10_01.rds")
base_data <- paste0(dir, "base_2018_01_01_2019_01_01.rds")
sigl_data <- paste0(dir, "sigl_2018_01_01_2019_01_01.rds")

# output files
pati_cases <- "pati_tb/tb_pati_new_tb_cases_relapses_by_age_sex.rds"
pati_registered <- "pati_tb/tb_pati_cases_registered.rds"
pati_results <- "pati_tb/tb_pati_case_results.rds"
base_out <- "base_services_prepped.rds"
sigl_out <- "sigl_prepped.rds"

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ----------------------------------------------

# ----------------------------------------------
# Read in data
# ----------------------------------------------
dt1 <- readRDS(registered)
dt2 <- readRDS(results)
base <- readRDS(base_data)
sigl <- readRDS(sigl_data)
# ----------------------------------------------

# ----------------------------------------------
# more prep
# ----------------------------------------------
more_prep <- function(data.table){
  data.table$value <- as.character(data.table$value)
  data.table$value <- as.numeric(data.table$value) 
  data.table$health_zone <- standardizeHZNames(data.table$health_zone)
  data.table$dps <- standardizeDPSNames(data.table$dps)
  return(data.table)
}

base <- more_prep(base)
sigl <- more_prep(sigl)
dt1 <- more_prep(dt1) # the warning that this introduces NAs can be ignored, because all of those NAs were "NULL" in the original data 
dt2 <- more_prep(dt2)
# ----------------------------------------------

# ----------------------------------------------
# PATI data sets:
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
# ----------------------------------------------

# ----------------------------------------------
# cast wide the results data
# ----------------------------------------------
vars <- colnames(dt2)
vars <- vars[!vars %in% c("category", "value")]
formula_for_cast <- as.formula(paste(paste(vars, collapse=" + "), "category", sep= " ~ "))
dt2 <- dcast.data.table(dt2, formula_for_cast, value.var = "value")
dt2 <- dt2[is.na(default), ]
# ----------------------------------------------

# ----------------------------------------------
# save prepped data
# ----------------------------------------------
saveRDS(cases_subset, paste0(out_dir, pati_cases))
saveRDS(reg, paste0(out_dir, pati_registered))
saveRDS(dt2, paste0(out_dir, pati_results))
saveRDS(base, paste0(out_dir, base_out))
saveRDS(sigl, paste0(out_dir, sigl_out))
# ----------------------------------------------










