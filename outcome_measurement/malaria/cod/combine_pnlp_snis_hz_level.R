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
dir_pnlp = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')
dir_dhis = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')
out_dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/')

# input files:
input_dhis_base <- "base/base_prepped_outliers_replaced.rds"
input_dhis_base_suspectedCases = "base/base_prepped_outliers_replaced_suspectedCases.rds"
input_dhis_sigl <- "sigl/sigl_prepped_drugs_received.rds"
after_imputation_pnlp <- "imputedData_run_0_001_aggVars_lagsLeads_condensed_hz_median.rds"
input_dhis_ssc = "ssc_supervisions_prepped.rds"
all_sigl_data = "sigl/sigl_prepped.rds"
  
# output file:
combined_data = "base_pnlp_sigl_combined_data_hz_level.rds"

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')

# read in variable naming file:
variable_matching = read.xlsx("./outcome_measurement/malaria/cod/match_dhis_names.xlsx")
variable_matching = as.data.table(variable_matching)
variable_matching = variable_matching

outliers_itns = 'outliers_in_llin_data.rds'
outliers = readRDS(paste0(out_dir, outliers_itns))
# ----------------------------------------------

# ----------------------------------------------
# Load data 
# ----------------------------------------------
pnlp <- readRDS(paste0(dir_pnlp, after_imputation_pnlp)) 
base <- readRDS(paste0(dir_dhis, input_dhis_base))
sigl <- readRDS(paste0(dir_dhis, input_dhis_sigl))
ssc = readRDS(paste0(dir_dhis, input_dhis_ssc))
all_sigl = readRDS(paste0(dir_dhis, all_sigl_data))
base_suspectedCases = readRDS(paste0(dir_dhis, input_dhis_base_suspectedCases))
# ----------------------------------------------

# ----------------------------------------------
# Clean data
# ----------------------------------------------

# standardize dps names in base (all others are already done)
  base[, dps := standardizeDPSNames(dps)]
  base[, health_zone := standardizeHZNames(health_zone)]
  base_suspectedCases[, dps := standardizeDPSNames(dps)]
  base_suspectedCases[, health_zone := standardizeHZNames(health_zone)]
  all_sigl[, health_zone := standardizeHZNames(health_zone)]
  ssc[, health_zone := standardizeHZNames(health_zone)]
  sigl[, health_zone := standardizeHZNames(health_zone)]
  pnlp[, health_zone := standardizeHZNames(health_zone)]
  pnlp[, dps := standardizeDPSNames(dps)]
  
  pnlp = pnlp[,.(value = sum(value, na.rm = FALSE)), by = c("date", "dps", "health_zone", "variable")]
  
# since all SIGL data set is so large, subset that one first:
  sigl_llin = all_sigl[element %in% c("C1 12.1 MIILD - pièce - quantité consommée") ]
  
# trim ws to make sure all of these variables are uniform/match up with what they should
  base$element <- trimws(base$element)
  base$element_eng <- trimws(base$element_eng)
  base_suspectedCases$element <- trimws(base_suspectedCases$element)
  base_suspectedCases$element_eng <- trimws(base_suspectedCases$element_eng)
  ssc$element <- trimws(ssc$element)
  ssc$element_eng <- trimws(ssc$element_eng)
  sigl_llin$element <- trimws(sigl_llin$element)
  sigl_llin$element_eng <- trimws(sigl_llin$element_eng)
  
# sum base, sigl_llin, and ssc to health zone level:
  base = base[, .(value = sum(value, na.rm = FALSE)), by = c("date", "year", "dps", "health_zone", "data_set", "element", "element_eng", "category")]
  base_suspectedCases = base_suspectedCases[, .(value = sum(value, na.rm = FALSE)), by = c("date", "dps", "health_zone", "data_set", "element", "element_eng", "category")]
  base_suspectedCases[ , year := year(date)]
  sigl_llin = sigl_llin[, .(value = sum(value, na.rm = FALSE)),  by = c("date", "dps", "health_zone", "data_set", "element", "element_eng") ]
  sigl_llin[ , year := year(date)]
  
# make sigl drugs data long instead of wide to match other sets
  sigl = melt.data.table(sigl, id.vars = c("date", "dps", "health_zone", "data_set", "drug") , variable.name = "element", variable.factor = FALSE)
  sigl[ , element := paste0(drug, "_", element)]
  sigl[, drug:= NULL]
  sigl[, year := year(date)]
  
  ssc[, year := year(date)]

  # # check unique identifieres in all sets:
  if (nrow(unique(base[, .(date, year, dps, health_zone, element, category)])) != nrow(base) |
      nrow(unique(base_suspectedCases[, .(date, year, dps, health_zone, element, category)])) != nrow(base_suspectedCases) |
    nrow(unique(sigl[, .(date, year, dps, health_zone, element)])) != nrow(sigl) |
    nrow(unique(sigl_llin[, .(date, year, dps, health_zone, element)])) != nrow(sigl_llin) |
    nrow(unique(pnlp[, .(date, dps, health_zone, variable)])) != nrow(pnlp) |
    nrow(unique(ssc[, .(date, year, dps, health_zone, element)])) != nrow(ssc) ) stop ("unique IDs don't uniquely identify rows!")
  
# Standardize element/indicator names -> moved to a file to read in:
  # NOTE can add variables by adding names to this excel file
  base = merge(base, variable_matching, by.x = c("data_set", "element"), by.y = c("data_set_dhis", "dhis_indicator"), all.x = TRUE)
  sigl = merge(sigl, variable_matching, by.x = c("data_set", "element"), by.y = c("data_set_dhis", "dhis_indicator"), all.x = TRUE)
  sigl_llin = merge(sigl_llin, variable_matching, by.x = c("data_set", "element"), by.y = c("data_set_dhis", "dhis_indicator"), all.x = TRUE)
  
  base_suspectedCases[, indicator := "suspectedMalaria"]
  base_suspectedCases[, data_set := "base"]
  
# pnlp - standardize indicator names
  pnlp[,  c("indicator", "subpopulation") := tstrsplit(variable, "_", fixed=TRUE)]
    
  pnlp[indicator=="ITN", indicator:= "LLIN"]
  pnlp[indicator=="newCasesMalariaMild", indicator:= "newCasesMalariaSimpleConf"]
  pnlp[indicator=="mildMalariaTreated", indicator:= "simpleConfMalariaTreated"]
  pnlp[indicator == "ArtLum", indicator := "AL"]
  pnlp[, variable := paste0(indicator, "_", subpopulation)]
  pnlp[variable == "AL_received", variable := "ALreceived"]
  pnlp[variable == "AL_used", variable := "ALused"]
  
  setnames(pnlp, "variable", "element")
  pnlp[, data_set := "pnlp"]
  pnlp[, year := year(date)]

# snis sets - standardize indicator names  
  base[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
  sigl[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
  sigl_llin[, c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
  
  base[ element == "A 1.4 Cas suspect", indicator := "suspectedMalaria" ]
  
  base[category == "<5 ans" & indicator != "RDT", subpopulation := "under5"]
  base[category == ">5 ans" & indicator != "RDT", subpopulation := "5andOlder"]
  base_suspectedCases[category == "<5 ans", subpopulation := "under5"]
  base_suspectedCases[category == ">5 ans", subpopulation := "5andOlder"]
  
  # # for base, need to sum over "category" for RDTs (to match PNLP with agg vars)
  # base = base[, .(value = sum(value, na.rm = FALSE)), by = c("date", "year", "dps", "health_zone", "data_set", "element", "element_eng", "indicator", "subpopulation")]
  base[indicator == "RDT" & subpopulation == "completed", indicator := "RDT_completed"]
  base[indicator == "RDT" & subpopulation == "positive", indicator := "RDT_positive"]
  base[category == "<5 ans", subpopulation := "under5"]
  base[category == ">5 ans", subpopulation := "5andOlder"]
  
# subset snis data to 2018 on  
  base = base[ year >= "2018", ]
  sigl = sigl[ year >= "2018", ]
  ssc = ssc[ year >= "2018", ]
  sigl_llin = sigl_llin[ year >= "2018", ]
  
  ssc[element == "B 10.2 Dont traités selon la politique nationale - Cas de fièvre", element:= "SSCACT"]
  ssc[element == "SSCACT", indicator:= "SSCACT"]
  ssc[element == "B 10.2 TDRs réalisés - Cas de fièvre", element:= "SSCRDT_completed"]
  ssc[element == "SSCRDT_completed", indicator:= "SSCRDT"]
  ssc[element == "B 7.2 Paludisme grave - décès", element:= "malariaDeaths"]
  ssc[element == "malariaDeaths", indicator:= "malariaDeaths"]
  
# check unique identifieres in all sets:
  if (nrow(unique(base[, .(date, year, dps, health_zone, indicator, subpopulation)])) != nrow(base) |
      nrow(unique(base_suspectedCases[, .(date, year, dps, health_zone, indicator, subpopulation)])) != nrow(base_suspectedCases) |
      nrow(unique(sigl[, .(date, year, dps, health_zone, indicator, subpopulation)])) != nrow(sigl) |
      nrow(unique(sigl_llin[, .(date, year, dps, health_zone, indicator, subpopulation)])) != nrow(sigl_llin) |
      nrow(unique(pnlp[, .(date, dps, health_zone, indicator, subpopulation)])) != nrow(pnlp) |
      nrow(unique(ssc[, .(date, year, dps, health_zone, element)])) != nrow(ssc) ) stop ("unique IDs don't uniquely identify rows!")
# ----------------------------------------------

# ----------------------------------------------
# combine all data sources together
# ----------------------------------------------
  dt <- rbindlist(list(pnlp, base, sigl, ssc, sigl_llin, base_suspectedCases), use.names = TRUE, fill = TRUE)
  
  dt[grepl(data_set, pattern = "Base"), data_set := "base"]
  dt[grepl(data_set, pattern = "SIGL2"), data_set := "sigl2"]
  dt[grepl(data_set, pattern = "SIGL1"), data_set := "sigl1"]
  
  if (nrow(unique(dt[, .(date, year, dps, health_zone, element, indicator, subpopulation)])) != nrow(dt)) stop ("unique IDs don't uniquely identify rows!")
  
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

    dt = dt[, .(value = sum(value, na.rm = FALSE)), by = .(dps, health_zone, date, year, data_set, element, indicator, subpopulation, element_eng)]
    
    dt[ data_set == "pnlp" & indicator == "LLIN" & subpopulation != "received", subpopulation:= "consumed"]
    dt[ data_set == "pnlp" & indicator == "LLIN" & subpopulation != "received", element := "LLIN_consumed"]
    dt = dt[, .(value = sum(value, na.rm = FALSE)), by = .(dps, health_zone, date, year, data_set, element, indicator, subpopulation, element_eng)]
  
  # remove LLINs outliers
    outliers[, subpopulation := "consumed"]
    dt = merge(dt, outliers, all = TRUE, by = c("dps", "health_zone", "year", "data_set", "date", "indicator", "subpopulation", "value"))
    dt[outlier == TRUE, .N]
    dt[outlier == TRUE, value := NA]
    dt[, outlier := NULL]
  
  saveRDS(dt, paste0(out_dir, combined_data))
  
  nrow(unique(dt[, .(dps, health_zone, date, data_set, element, indicator, subpopulation)])) == nrow(dt)
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




