# Audrey Batzel 
# 6/19/19

# run quantile regression on LLINs data, full time series with PNLP and SIGL/SNIS data. 
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(ggplot2)
library(lubridate)
library(quantreg)
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
combined_data = "base_pnlp_sigl_combined_data_hz_level.rds"

# output file:
outliers_itns = "outliers_in_llin_data.rds"

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ----------------------------------------------

# ----------------------------------------------
# Load data 
# ----------------------------------------------
dt = readRDS(paste0(out_dir, combined_data))
# ----------------------------------------------

# ----------------------------------------------
# sum over subpopulations in PNLP data (dist at preschool and dist at anc)
# ----------------------------------------------
itns = dt[element %in% c("C1 12.1 MIILD - pièce - quantité consommée", "LLIN_distAtPreschool", "LLIN_distAtANC") ]
itns = itns[, .(value = sum(value, na.rm = TRUE)), by = .(dps, health_zone, date, year, data_set, indicator)]
# ----------------------------------------------

# ----------------------------------------------
# run qr
# ----------------------------------------------
combined_qr_results = data.table()
itns[, combine := paste0(health_zone, "_", dps)]

for (c in unique(itns$combine)) {
  # subset the data further based on loop parameters for qr
  subset = itns[combine == c,] 
  
  # skip cases that will fail
  n = nrow(subset[!is.na(value), ])
  #print(n)
  var = var(subset$value, na.rm=T)
  #print(var)
  nx = length(unique(subset$date))
  #print(nx)
  
  # skip if less than 3 data points or variance is 0
  if(n>=3 & var!=0 & nx>=2) {  
    # create formula
    form = 'value~date'
    
    # # add fixed effect on group if more than one group exist
    form = as.formula(form)
    
    # run quantreg
    quantFit = rq(form, data=subset, tau=0.5)
    summary(quantFit) 
    
    # list the residuals and add them to the out file
    subset[, fitted_value:=predict(quantFit, newdata = subset)]
    subset[, resid:=(fitted_value - value)]
    subset[, skipped_qr := "no"]
    
  } else { 
    subset[, fitted_value:=NA]
    subset[, resid:=NA]
    subset[, skipped_qr := "yes"]
  }
  
  if (nrow(combined_qr_results)==0) {
    combined_qr_results = subset 
  } else { combined_qr_results = rbindlist(list(combined_qr_results, subset), use.names=TRUE, fill = TRUE) }
}
# ----------------------------------------------

# ----------------------------------------------
# identify outliers
# ----------------------------------------------
dt = copy(combined_qr_results)
idVars = c('combine', 'indicator')

threshold = 50

dt[!all(is.na(resid)) , mad_resid := mad(resid, na.rm=TRUE), by = idVars]
dt[!all(is.na(resid)) , sd_resid := sd(resid, na.rm=TRUE), by = idVars]
dt[ , thresh_var := mad_resid]
dt[ , stat_used := "mad"] 
dt[ mad_resid < 1, thresh_var := sd_resid]
dt[ mad_resid < 1, stat_used := "sd"]
dt[ , c('sd_resid', 'mad_resid') := NULL]

# set lower and upper bounds
dt[ , upper := fitted_value + (threshold * thresh_var)]
dt[ , lower := fitted_value - (threshold * thresh_var)]

# select outliers
limit = 7500
# the value is greater than the limit set above and greater than 10 times the mad of residuals 
# or less than 10 times the negative mad of the residuals
dt[, outlier := ifelse( (value > limit & ( value > upper )), TRUE, FALSE) ]
dt[ (value < lower ), outlier :=TRUE ]

# number of outliers
dt[ outlier==TRUE, .N ] 
# ----------------------------------------------


hzs_outliers = dt[outlier==TRUE, unique(health_zone)]

plots = lapply(hzs_outliers, function(x) { 
  qplot(date, value, data = dt[ health_zone ==x & indicator == "LLIN", ], main = paste0("LLINs for ", x), color= outlier )
})

pdf('C:/local/ts_llins_by_hz_outliers.pdf')
for(p in seq(length(plots))) print(plots[[p]]) 
dev.off()

dt = dt[outlier == TRUE, .(dps, health_zone, date, year, data_set, indicator, value, outlier)]

saveRDS(dt, paste0(out_dir, outliers_itns))

