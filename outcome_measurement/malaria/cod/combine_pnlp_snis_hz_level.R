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
library(reshape2) bvnv
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
dir_pnlp = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')
dir_dhis = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')
out_dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/')

# input files:
input_dhis_base <- "base/base_prepped_outliers_replaced.rds"
input_dhis_sigl <- "sigl/sigl_prepped_drugs_received.rds"
after_imputation_pnlp <- "imputedData_run_0_001_aggVars_lagsLeads_condensed_hz_median.rds"
input_dhis_ssc = "ssc_supervisions_prepped.rds"
  
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
ssc = readRDS(paste0(dir_dhis, input_dhis_ssc))
# ----------------------------------------------

# ----------------------------------------------
# Clean data
# ----------------------------------------------
# standardize dps names in base (all others are already done)
  base$dps <- standardizeDPSNames(base$dps)

# trim ws to make sure all of these variables are uniform/match up with what they should
  base$element <- trimws(base$element)
  base$element_eng <- trimws(base$element_eng)
  ssc$element <- trimws(ssc$element)
  ssc$element_eng <- trimws(ssc$element_eng)
  
# sum base and ssc to health zone level:
  base = base[, .(value = sum(value, na.rm = TRUE)), by = c("date", "year", "dps", "health_zone", "data_set", "element", "element_eng", "category")]
  ssc = ssc[, .(value = sum(value, na.rm =TRUE)),  by = c("date", "dps", "health_zone", "data_set", "element", "element_eng") ]
  ssc[, year := year(date)]
  
  sigl = melt.data.table(sigl, id.vars = c("date", "dps", "health_zone", "data_set", "drug") , variable.name = "element", variable.factor = FALSE)
  sigl[ , element := paste0(drug, "_", element)]
  sigl[, drug:= NULL]
  sigl[, year := year(date)]

# Standardize element/indicator names -> moved to a file to read in:
  # NOTE can add variables by adding names to this excel file
  base = merge(base, variable_matching, by.x = c("data_set", "element"), by.y = c("data_set_dhis", "dhis_indicator"), all.x = TRUE)
  sigl = merge(sigl, variable_matching, by.x = c("data_set", "element"), by.y = c("data_set_dhis", "dhis_indicator"), all.x = TRUE)

# pnlp - standardize indicator names
  pnlp[ , c('combine', 'donor','operational_support_partner', 'population', 'lower', 'upper', 'id') := NULL]
  
  pnlp[,  c("indicator", "subpopulation") := tstrsplit(variable, "_", fixed=TRUE)]
  base[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
  sigl[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
    
  pnlp[indicator=="ITN", indicator:= "LLIN"]
  pnlp[indicator=="newCasesMalariaMild", indicator:= "newCasesMalariaSimpleConf"]
  pnlp[indicator=="mildMalariaTreated", indicator:= "simpleConfMalariaTreated"]
  pnlp[ indicator == "ArtLum" & subpopulation == "used", variable := "ALused"]
  pnlp[ indicator == "ArtLum" & subpopulation == "used", indicator := "AL"]
  pnlp[ indicator == "ArtLum" & subpopulation =="received", variable := "ALreceived"]
  pnlp[ indicator == "ArtLum" & subpopulation =="received", indicator := "AL"]
  setnames(pnlp, "variable", "element")
  pnlp[, data_set := "pnlp"]
  pnlp[, year := year(date)]
  
  base[category == "<5 ans" & indicator != "RDT", subpopulation := "under5"]
  base[category == ">5 ans" & indicator != "RDT", subpopulation := "5andOlder"]
  
  base = base[ year >= "2018", ]
  sigl = sigl[ year >= "2018", ]
  ssc = ssc[ year >= "2018", ]
# ----------------------------------------------

# ----------------------------------------------
# combine all data sources together
# ----------------------------------------------
  dt <- rbindlist(list(pnlp, base, sigl, ssc), use.names = TRUE, fill = TRUE)
  
  dt[grepl(data_set, pattern = "Base"), data_set := "base"]
  dt[grepl(data_set, pattern = "SIGL2"), data_set := "sigl2"]
  dt[grepl(data_set, pattern = "SIGL1"), data_set := "sigl1"]
  
  dt[element == "B 10.2 Dont traités selon la politique nationale - Cas de fièvre", element:= "SSCACT"]
  
  # aggregate health zones, based on the decisons we made for Nord Kivu with PNLP
   # Haut-Katanga:
   # do not impute data for Kashobwe before 2012; after imputation add to Kasenga 
    dt[ health_zone == "kashobwe", health_zone := "kasenga" ]
   # Nord Kivu:
   # do not impute Alimbongo before 2012; after imputation add to Lubero
    dt[ health_zone == "alimbongo", health_zone := "lubero"]
   # do not impute Kibirizi before 2016; after imputation add to Rutshuru  
    dt[health_zone == "kibirizi", health_zone := "rutshuru"]
   # do not impute Mabalako before 2013; after imputation add to Beni 
    dt[health_zone == "mabalako", health_zone := "beni"]
   # do not impute Kamango before 2012; after imputation add Kamango to Mutwanga (Oicha?).
    dt[health_zone == "kamango", health_zone := "mutwanga"]
  
    dt = dt[, .(value = sum(value, na.rm = TRUE)), by = .(dps, health_zone, date, year, data_set, element, indicator, subpopulation, element_eng, category)]
  
  saveRDS(dt, paste0(out_dir, combined_data))
  
  nrow(unique(dt[, .(dps, health_zone, date, data_set, element, category)]))
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




