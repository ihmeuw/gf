# Audrey Batzel 
# 1-30-19
#
# Combine PNLP and base data
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
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
library(openxlsx)
# --------------------  

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_pnlp = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/archive/')
dir_dhis = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')
out_dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/')

# input file:
input_dhis_base <- "base_services_prepped.rds"
input_dhis_sigl <- "sigl_prepped.rds"
after_imputation_pnlp <- "imputedData_run2_agg_hz.rds"

# output file:
combined_data <- "base_pnlp_sigl_combined_data_hz_level.rds"
combined_data_malaria <- "snis_pnlp_malaria_hz_level.rds"

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')

# read in variable naming file:
variable_matching = read.xlsx("./outcome_measurement/malaria/cod/match_dhis_names.xlsx")
variable_matching = as.data.table(variable_matching)
variable_matching = variable_matching[!is.na(indicator),]
# ----------------------------------------------

# ----------------------------------------------
# Load data 
# ----------------------------------------------
pnlp <- readRDS(paste0(dir_pnlp, after_imputation_pnlp)) 
base <- readRDS(paste0(dir_dhis, input_dhis_base))
sigl <- readRDS(paste0(dir_dhis, input_dhis_sigl))
# ----------------------------------------------

# ----------------------------------------------
# Clean data
# ----------------------------------------------
# base / sigl - clean column names
    # standardize dps/hz names
    pnlp$health_zone <- standardizeHZNames(pnlp$health_zone)
    base$health_zone <- standardizeHZNames(base$health_zone)
    sigl$health_zone <- standardizeHZNames(sigl$health_zone)
    pnlp$dps <- standardizeDPSNames(pnlp$dps)
    base$dps <- standardizeDPSNames(base$dps)
    sigl$dps <- standardizeDPSNames(sigl$dps)
    
    # trim ws to make sure all of these variables are uniform/match up with what they should
    # base$type <- trimws(base$type)  # for some reason there is both "malaria" and "malaria " in the type unique values
    base$element <- trimws(base$element)
    base$element_eng <- trimws(base$element_eng)
    sigl$element <- trimws(sigl$element)
    sigl$element_eng <- trimws(sigl$element_eng)
    
    # drop columns we don't need
    drop_cols <- c("coordinates", "country", "mtk", "org_unit_id", "element_id", "last_update", "download_number")
    base <- base[ , !(drop_cols), with = FALSE]    
    sigl <- sigl[ , !(drop_cols), with = FALSE]

# Standardize element/indicator names -> moved to a file to read in:
    # NOTE can add variables by adding names to this excel file
  base = merge(base, variable_matching, by.x = c("data_set", "element"), by.y = c("data_set_dhis", "dhis_indicator"), all.x = TRUE)
  sigl = merge(sigl, variable_matching, by.x = c("data_set", "element"), by.y = c("data_set_dhis", "dhis_indicator"), all.x = TRUE)
  
  base = base[!is.na(indicator),]
  sigl = sigl[!is.na(indicator),]
  
  base[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
  sigl[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]

  base[category == "<5 ans" & indicator != "RDT", subpopulation := "under5"]
  base[category == ">5 ans" & indicator != "RDT", subpopulation := "5andOlder"]

# pnlp - standardize indicator names
pnlp[indicator=="ITN", indicator:= "LLIN"]
pnlp[indicator=="newCasesMalariaMild", indicator:= "newCasesMalariaSimpleConf"]
pnlp[indicator=="mildMalariaTreated", indicator:= "simpleConfMalariaTreated"]
pnlp[ indicator == "ArtLum" & subpopulation == "used", indicator := "ALused"]
pnlp[ indicator == "ArtLum" & subpopulation =="received", indicator := "ALreceived"]

setnames(pnlp, "mean", "value")
drop_cols_pnlp <- c("upper", "lower", "variable", "province")
pnlp <- pnlp[ , !(drop_cols_pnlp), with = FALSE] 

# all - add data set names
base[, data_set := "snis_base_services"]
pnlp[, data_set := "pnlp"]
sigl[, data_set := "snis_sigl"]

# agg base and sigl to hz level
base = base[, .(value = sum(value, na.rm = TRUE)), by= .(data_set, date, dps, health_zone, indicator, subpopulation)]
sigl = sigl[, .(value = sum(value, na.rm = TRUE)), by= .(data_set, date, dps, health_zone, indicator, subpopulation)]

dt <- rbindlist(list(pnlp, base, sigl), use.names = TRUE, fill = TRUE)
saveRDS(dt, paste0(out_dir, combined_data))
# ----------------------------------------------

# # ----------------------------------------------
# # Subset/aggregate to the data we want to use
# # ----------------------------------------------
# # in this case, just malaria and 2017 data (overlap between PNLP and base services)
# mal_inds <- unique(pnlp$indicator)
# mal_inds <- c(mal_inds, "ASAQconsumed", "presumedMalariaTreated")
# dt_mal <- dt[indicator %in% mal_inds,]
# dt_mal$year <- year(dt_mal$date)
# dt_mal <- dt_mal[ year >= 2017, ]
# 
# # at health zone level
# dt_mal <- dt_mal[, .(value= sum(value, na.rm=TRUE)), by=c("data_set", "date", "year", "dps", "health_zone", "indicator", "subpopulation")]  
# saveRDS(dt_mal, paste0(out_dir, combined_data_malaria))
# # ----------------------------------------------




