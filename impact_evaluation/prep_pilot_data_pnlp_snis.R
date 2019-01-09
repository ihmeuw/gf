# Audrey Batzel
# 1-9-19
#
# Prep PNLP/SNIS for pilot data set for impact evaluation
# -----------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(stats)
library(Rcpp)
library(readxl)
library(grid)
library(gridExtra)
library(googlesheets)
library(ggrepel)
library(dplyr)
library(lubridate)

# change working directory to the root of the repo
setwd('C:/local/gf/')
# --------------------


# -----------------------------------------------------------
# DIRECTORIES

# root directory
# change depending on cluster/not
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_pnlp = paste0(j, 'Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')
dir_dhis = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')

# output directory
out_dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/')

# FILES
pnlp = paste0(dir_pnlp, "imputedData_run2_agg_country.rds") # pnlp
snis_base <- paste0(dir_dhis,"base_services_drc_01_2017_09_2018_prepped.rds") # snis base services
snis_sigl <- paste0(dir_dhis,"sigl_drc_01_2015_07_2018_prepped.rds") # snis sigl (supply chain)

# FUNCTIONS

# -------------------------

# ---------------------------------------------------
# Read in data
# ---------------------------------------------------
test <- readRDS(paste0(dir_pnlp, "imputedData_run2_agg_hz.rds"))
dt_pnlp <- readRDS(pnlp) # national level, monthly
dt_base <- readRDS(snis_base) # facility level, monthly
dt_sigl <- readRDS(snis_sigl) # facility level, monthly
# ---------------------------------------------------

# ---------------------------------------------------
# Aggregate to national data
# ---------------------------------------------------
# ---------------------------------------------------

# ---------------------------------------------------
# Aggregate to quarterly data
# ---------------------------------------------------
# ---------------------------------------------------

# ---------------------------------------------------
# Subset to activities / outputs indicators
# ---------------------------------------------------
# ---------------------------------------------------

# ---------------------------------------------------
# calculate completeness measures from 
#       PNLP - imputed variables health facilites
# ---------------------------------------------------
# ---------------------------------------------------
