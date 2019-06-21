# Audrey Batzel 
# 6/19/19

# manual outlier screen
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
all_sigl_data = "sigl/sigl_prepped.rds"

# output file:
combined_data = "base_pnlp_sigl_combined_data_hz_level.rds"

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
dt = readRDS(paste0(out_dir, combined_data))
# ----------------------------------------------

# ----------------------------------------------
# time series figures to check variables
# ----------------------------------------------
itns = dt[element %in% c("C1 12.1 MIILD - pièce - quantité consommée", "LLIN_distAtPreschool", "LLIN_distAtANC") ]
itns = itns[, .(value = sum(value)), by = .(dps, health_zone, date, year, data_set, indicator)]

plots = lapply(unique(itns$health_zone), function(x) { 
  qplot(date, value, data = itns[ health_zone ==x & indicator == "LLIN", ], main = paste0("LLINs for ", x))
})

pdf('C:/local/ts_llins_by_hz.pdf')
for(p in seq(length(plots))) print(plots[[p]]) 
dev.off()
# -------------------
deaths = dt[element %in% c("malariaDeaths_5andOlder", "malariaDeaths_under5", "malariaDeaths_pregnantWomen", "B 7.2 Paludisme grave - décès") ]
deaths[, indicator := "malariaDeaths"]
deaths = deaths[, .(value = sum(value)), by = .(dps, health_zone, date, year, data_set, indicator)]

plots = lapply(unique(deaths$health_zone), function(x) { 
  qplot(date, value, data = deaths[ health_zone == x, ], main = paste0("Deaths from Malaria for ", x))
})

pdf('C:/local/ts_deaths_by_hz.pdf')
for(p in seq(length(plots))) print(plots[[p]]) 
dev.off()
# -------------------
rdtssc = dt[element %in% c("B 10.2 TDRs réalisés - Cas de fièvre", "SSCRDT_completed") ]
rdtssc = rdtssc[ year >= 2015] # imputed before 2015 but not real data - from rectangularization
rdtssc[, indicator := "SSCRDT"]
rdtssc = rdtssc[, .(value = sum(value)), by = .(dps, health_zone, date, year, data_set, indicator)]

plots = lapply(unique(rdtssc$health_zone), function(x) { 
  qplot(date, value, data = rdtssc[ health_zone ==x, ], main = paste0("RDTs at SSC for ", x))
})

pdf('C:/local/ts_RDT_SSC_by_hz.pdf')
for(p in seq(length(plots))) print(plots[[p]]) 
dev.off()
# -------------------
ssc = dt[ element %in% c("SSCACT", "SSCACT_NA")]
ssc[, indicator := "SSCACT"]
ssc = ssc[ year >= 2015]

plots = lapply(unique(ssc$health_zone), function(x) { 
  qplot(date, value, data = ssc[ health_zone ==x, ], main = paste0("ACT used/cases treated at SSC for ", x))
})

pdf('C:/local/ts_ACT_SSC_by_hz.pdf')
for(p in seq(length(plots))) print(plots[[p]]) 
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# histograms to check variables
# ----------------------------------------------
ssc = readRDS(paste0(dir_dhis, input_dhis_ssc))
all_sigl = readRDS(paste0(dir_dhis, all_sigl_data))
sigl_llin = all_sigl[element %in% c("C1 12.1 MIILD - pièce - quantité consommée") ]


ssc[element == "B 10.2 Dont traités selon la politique nationale - Cas de fièvre", element:= "SSCACT"]
qplot(value, data = ssc[ element == "SSCACT", ])
max(ssc[ element == "SSCACT", value], na.rm = TRUE)
ssc[ value == 6088, ]

plots = lapply(unique(ssc$health_zone), function(x) { 
  qplot(date, value, data = ssc[ health_zone ==x & element =="SSCACT", ])
})

pdf('C:/local/tmp.pdf')
for(p in seq(length(plots))) print(plots[[p]]) 
dev.off()

qplot(value, data = sigl_llin[])
max(sigl_llin[, value], na.rm = TRUE)

llin_hz = sigl_llin[, .(value = sum(value, na.rm =TRUE)),  by = c("date", "dps", "health_zone") ]

qplot(value, data = llin_hz[])
max(llin_hz[, value], na.rm = TRUE)

llin_hz_yr = sigl_llin[, .(value = sum(value, na.rm =TRUE)),  by = c("dps", "health_zone") ]

qplot(value, data = llin_hz_yr[])
max(llin_hz_yr[, value], na.rm = TRUE)

convert_date_to_quarter <- function(dt){
  dt$year <- year(dt$date)
  dt$month <- month(dt$date)
  dt[ month %in% 1:3, quarter:= 1]
  dt[ month %in% 4:6, quarter:= 2]
  dt[ month %in% 7:9, quarter:= 3]
  dt[ month %in% 10:12, quarter:= 4]
  return(dt)}

llin_hz = convert_date_to_quarter(llin_hz)
llin_hz_qtr = llin_hz[, .(value = sum(value, na.rm =TRUE)),  by = c("dps", "health_zone", "quarter", "year") ]

qplot(value, data = llin_hz_qtr[])
max(llin_hz_qtr[, value], na.rm = TRUE)

plots = lapply(unique(llin_hz$health_zone), function(x) { 
  qplot(date, value, data = llin_hz[ health_zone ==x , ])
})

pdf('C:/local/tmp2.pdf')
for(p in seq(length(plots))) print(plots[[p]]) 
dev.off()
# ----------------