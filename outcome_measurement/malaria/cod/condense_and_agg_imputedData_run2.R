# read in all of the imputed data files by dps created by run_amelia and 
# calculate mean and upper/lower quantiles to condense the rounds of imputation
# do this across the 50 imputations, by hz, dps, and at the country level

# TO DO : healthFacilitiesProportion variable is NA for a lot
# ----------------------------------------------     

# --------------------  
rm(list=ls())
# Set up R / install packages
library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
# --------------------  

# ----------------------------------------------
# Overview - Files and Directories

run_name = "run_0_01_aggVars_lagsLeads"

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')

# input file:
imputed_data = paste0("PNLP_imputedData_", run_name, ".rds")

# output files:
imputed_data_long = paste0("imputedData_", run_name, "_long.rds")
imputed_data_long_corrected = paste0("imputedData_", run_name, "_long_corrected.rds")

condensed_imputed_data_dps = paste0("imputedData_", run_name, "_condensed_dps.rds")
condensed_imputed_data_country = paste0("imputedData_", run_name, "_condensed_country.rds")
condensed_imputed_data_country2 = paste0("imputedData_", run_name, "_condensed_country_byYear.rds")
condensed_imputed_data_hz = paste0("imputedData_", run_name, "_condensed_hz.rds")
# ----------------------------------------------
  
# ---------------------------------------------- 
################### NOTE: can skip down to section that says "START FROM HERE"...
# ---------------------------------------------- 
# Load imputed data
dt <- readRDS(paste0(dir, imputed_data))

# Set up var vectors for manipulation
all_vars = c(colnames(dt))
id_vars = c("id", "dps", "health_zone", "date", "donor", "operational_support_partner", "population", "imputation_number", "combine")
inds = all_vars[!all_vars %in% id_vars] 
# ----------------------------------------------

# ----------------------------------------------    
# Set up a data table for graphing:
# Reshape imputed data long
dt_long = melt.data.table(dt, id.vars = id_vars, variable.factor = FALSE)

# Save data in this format (since it takes a while to run), so it can be used later on if the code breaks somewhere before all versions of the dt are produced
saveRDS(dt_long, paste0(dir, imputed_data_long))
# ----------------------------------------------

# # ----------------------------------------------
# # original data to merge to fix zeroes in the data
# # read in data table prepped by prep_for_MI.R
# input <- "final_data_for_impuation.csv"
# dtOrig <- fread(paste0(dir, input)) 
# dtOrig <- dtOrig[, V1:=NULL]
# 
# for (i in indicators) {
#   dtOrig$i <- as.double(dtOrig$i)
# }
# 
# missMatrixMelt <- melt(dtOrig, id.vars=c(id_vars, "id"))
# missMatrixMelt[, isMissing:=is.na(value)]
# missMatrixMelt$date <- as.Date(missMatrixMelt$date)
# 
# corrected_data <- merge(imputedDataLongSplit, missMatrixMelt, all=T, by= c(id_vars, "variable"))
# 
# # when the original value was 0, set the imputed values to be zero:
# setnames(corrected_data, "value.x", "imp_value")
# setnames(corrected_data, "value.y", "orig_value")
# corrected_data[orig_value==0, imp_value:=0]
# 
# saveRDS(corrected_data, paste0( dir, imputed_data_long_corrected ))
# # ----------------------------------------------

# ----------------------------------------------
################## START FROM HERE.... #################################
# ---------------------------------------------- 
# read in the long imputed data, with zeroes corrected
dt <- readRDS(paste0(dir, imputed_data_long_corrected))

# for some reason there are NAs in these variables (i was just an index var?), but there shouldn't be NAs
# in imputed data, and there can't be to calculate quantile unless na.rm=T; but to make sure there aren't
# NAs present in the other data that we don't, I'll just remove these two variables and not use na.rm=T
dt2 <- dt[variable != "healthFacilitiesProportion"]
dt3 <- dt2[variable != "i"]
# ----------------------------------------------

# ----------------------------------------------
# Calculate mean, upper, and lower across impuations by HEALTH ZONE
# ----------------------------------------------
# compute upper middle and lower for the imputed points for the error bars in the graphs
graphData <- dt3[, .(mean=mean(imp_value),
                    lower=quantile(imp_value, .05),
                    upper=quantile(imp_value, .95)), by=c(id_vars, "variable", "indicator", "subpopulation")]

# get rid of lower and upper values for values that were NOT missing, so these don't show up on the graph
graphData <- graphData[mean==lower, lower:= NA ]
graphData <- graphData[mean==upper, upper:= NA ]

# export graphDataComplete
saveRDS(graphData, paste0(dir, condensed_imputed_data_hz))
# ----------------------------------------------

# ----------------------------------------------
# Aggregate first and then calculate mean and variance across imputations by DPS
# ----------------------------------------------
# aggregate all indicator/intervention data by dps, within each imputation
aggData  <- dt3[, .(aggValue = sum(imp_value)), by=c( "date", "province", "dps", "indicator", "subpopulation", "imputation_number" )]

# then compute the mean, upper and lower across all imputations for each unique dps/date
aggData <- aggData[, .(mean=mean(aggValue), 
                       lower=quantile(aggValue, .05), 
                       upper=quantile(aggValue, .95)), by=c("date", "province", "dps", "indicator", "subpopulation")]

# set upper and lower values to NA where the value was not imputed (where mean==lower and mean==upper)
aggData <- aggData[mean==lower, lower := NA]
aggData <- aggData[mean==upper, upper := NA]

# export data
saveRDS(aggData, paste0(dir, condensed_imputed_data_dps))
# ----------------------------------------------

# ----------------------------------------------
# Aggregate first and then calculate mean and variance across imputations by COUNTRY
# 8/29/18 update: changed country level confidence interval
# ----------------------------------------------
# aggregate all indicator/intervention data by dps, within each imputation
# get rid of dps that is under category 0...
dt3 <- dt3[ dps != "0", ]

fullCountryData  <- dt3[, .(aggValue = sum(imp_value)), by=c( "date", "indicator", "subpopulation", "imputation_number" )]

# then compute the mean, upper and lower across all imputations for each unique dps/date
fullCountryData <- fullCountryData[, .(mean=mean(aggValue), 
                                       lower=quantile(aggValue, .025), 
                                       upper=quantile(aggValue, .975)), by=c("date", "indicator", "subpopulation")]

# set upper and lower values to NA where the value was not imputed (where mean==lower and mean==upper)
fullCountryData <- fullCountryData[mean==lower, lower := NA]
fullCountryData <- fullCountryData[mean==upper, upper := NA]

# export data
saveRDS(fullCountryData, paste0(dir, condensed_imputed_data_country))

# # ----------------------------------------------
## get aggregate yearly values at the country level:
dt3 <- dt3[ dps != "0", ]
dt3$year <- year(dt3$date)
fullCountryData  <- dt3[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"), .(aggValue = sum(imp_value)), by=c( "year", "imputation_number" )]

# then compute the mean, upper and lower across all imputations for each unique dps/date
fullCountryData <- fullCountryData[, .(mean=mean(aggValue), 
                                       lower=quantile(aggValue, .025), 
                                       upper=quantile(aggValue, .975)), by=c("year")]

# set upper and lower values to NA where the value was not imputed (where mean==lower and mean==upper)
fullCountryData <- fullCountryData[mean==lower, lower := NA]
fullCountryData <- fullCountryData[mean==upper, upper := NA]

# export data
saveRDS(fullCountryData, paste0(dir, condensed_imputed_data_country2))
# ----------------------------------------------          
