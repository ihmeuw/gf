setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
rm(list=ls())
library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(readxl)
library(stats)
library(Rcpp)
library(Amelia)
library(DescTools)
# --------------------  

# ----------------------------------------------
# Overview - Files and Directories

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
input_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')

# output files
#imputed_data_long <- "imputed_fullData_noPriors_long.rds"
imputed_data_long <- "imputed_MTK_noPriors_long.rds"
condensed_imputed_data_hz <- "condensed_imputed_fullData_noPriors_hz.rds"
condensed_imputed_data_dps <- "condensed_imputed_MTK_noPriors_dps.rds"
condensed_imputed_data_country <- "condensed_imputed_MTK_noPriors_country.rds"

dt <- readRDS(paste0(input_dir, imputed_data_long))
# -------------------------------


dtHZ <- readRDS(paste0(input_dir, condensed_imputed_data_hz))
dtDPS <- readRDS(paste0(input_dir, condensed_imputed_data_dps))
dtCountry <- readRDS(paste0(input_dir, condensed_imputed_data_country))
