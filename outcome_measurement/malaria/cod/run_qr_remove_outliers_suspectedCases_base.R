# Audrey Batzel 
# 6/24/19

# run QR on suspected cases (element that had gotten dropped out of base data)

setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(quantreg)
# --------------------  

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_dhis = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# input files:
inFile = "outliers/base/base_to_screen_suspectedCases.rds"

# output file:
outFile = "outliers/base/base_quantreg_results_suspectedCases.rds"
# ----------------------------------------------

# ----------------------------------------------
# read in data
# ----------------------------------------------
dt = readRDS(paste0(dir_dhis, inFile))
subset = copy(dt)
# ----------------------------------------------

# ----------------------------------------------
# loop through drug and variable, run quant reg, and then combine results
# ----------------------------------------------
combined_qr_results = data.table()

for (o in unique(subset$org_unit_id)) {
  # subset the data further based on loop parameters for qr
  subset_further = subset[org_unit_id == o,] 
  
  # skip cases that will fail
  n = nrow(subset_further[!is.na(value), ])
  #print(n)
  var = var(subset_further$value, na.rm=T)
  #print(var)
  nx = length(unique(subset_further$date))
  #print(nx)
  
  # skip if less than 3 data points or variance is 0
  if(n>=3 & var!=0 & nx>=2) {  
    # create formula
    form = 'value~date'
    
    # # add fixed effect on group if more than one group exist
    form = as.formula(form)
    
    # run quantreg
    quantFit = rq(form, data=subset_further, tau=0.5)
    summary(quantFit) 
    
    # list the residuals and add them to the out file
    subset_further[, fitted_value:=predict(quantFit, newdata = subset_further)]
    subset_further[, resid:=(fitted_value - value)]
    subset_further[, skipped_qr := "no"]
    
  } else { 
    subset_further[, fitted_value:=NA]
    subset_further[, resid:=NA]
    subset_further[, skipped_qr := "yes"]
  }
  
  if (nrow(combined_qr_results)==0) {
    combined_qr_results = subset_further 
  } else { combined_qr_results = rbindlist(list(combined_qr_results, subset_further), use.names=TRUE, fill = TRUE) }
  print(paste0("completed loop for org_unit_id = ", o))
}
# ----------------------------------------------

# ----------------------------------------------
# save qr data
# ----------------------------------------------
saveRDS(combined_qr_results, paste0(dir_dhis, outFile))
# ----------------------------------------------
