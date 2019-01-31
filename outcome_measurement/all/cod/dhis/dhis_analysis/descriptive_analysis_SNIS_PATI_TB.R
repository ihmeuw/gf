# ----------------------------------------------
# Audrey Batzel
#
# 1/29/19
# Descriptive analysis of PATI TB prepped data and comparison with PNLT Q1 2018
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
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')
dir2 = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/')

# input files
pati_cases <- paste0(dir, "pati_tb/tb_pati_new_tb_cases_relapses_by_age_sex.rds")
pati_registered <- paste0(dir, "pati_tb/tb_pati_cases_registered.rds")
pati_results <- paste0(dir, "pati_tb/tb_pati_case_results.rds")
pnlt_case_outcomes <- paste0(dir2, 'PNLT_case_outcomes_2018.csv')
pnlt_case_screening <- paste0(dir2,"PNLT_case_screening_2018.csv")
# ----------------------------------------------

# ----------------------------------------------
# Read in data
# ----------------------------------------------
cases <- readRDS(pati_cases)
snis_reg <- readRDS(pati_registered)
snis_res <- readRDS(pati_results)
pnlt_res <- read.csv(pnlt_case_outcomes)
pnlt_reg <- read.csv(pnlt_case_screening)
# ----------------------------------------------

# ----------------------------------------------
# comparison of case results/case outcomes in PNLT and SNIS
# ----------------------------------------------
# first, aggregate snis_res to dps level
sd_cols <- colnames(snis_res)[grepl("TB_", colnames(snis_res))]
snis_res_dps <- snis_res[, lapply(.SD, sum, na.rm=TRUE), by = .(dps, element, element_eng, quarter, date), .SDcols = sd_cols]
# subset to just Q1 2018
snis_res_dps <- snis_res_dps[date == "2018-01-01", ]
snis_res_dps$year <- year(snis_res_dps$date)
# rename vars
names(snis_res_dps) <- c("dps", "element", "element_eng", "quarter", "date", "died", "trt_failed", "healed", "cas_not_eval", "lost_to_followup", "trt_complete", "year")
snis_res_dps$element <- as.character(snis_res_dps$element)
snis_res_dps$element <- trimws(snis_res_dps$element)
snis_res_dps[element== "TB_Enfants 0-14 ans", element_eng:= "children_0to14yrs"]
snis_res_dps[element== "TB_Patients co infectés TB/VIH+ (Np+Rech)", element_eng:= "tbhiv_patients_new_relapse"]
snis_res_dps[element== "TB_Cas de la TB Extra pulmonaire", element_eng:= "tep_case"]
snis_res_dps[element== "TB_Patients  suivis par la communauté", element_eng:= "patients_followed_by_community"]
snis_res_dps[element== "TB_Cas confirmés bactériologiquement (Nouveaux et rechutes)", element_eng:= "tpp_new_relapse"]
snis_res_dps[element== "TB_Cas diagnostiqués cliniquement (Nouveaux et rechutes)", element_eng:= "tpc_new_relapse"]
snis_res_dps[element== "TB_Déjà traité Hors Rechutes", element_eng:= "case_alreadyTreated_notRelapse"]
# ----------------------------------------------









