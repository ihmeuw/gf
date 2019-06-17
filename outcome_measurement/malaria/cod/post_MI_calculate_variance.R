
# calculate mean and upper/lower quantiles to condense the rounds of imputation
# do this across the 50 imputations, by hz, dps, and at the country level
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

run_name = "run_0_001_aggVars_lagsLeads"

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
# initial prep / set up steps
# ---------------------------------------------- 
# Load imputed data
dt <- readRDS(paste0(dir, imputed_data))

# Set up var vectors for manipulation
all_vars = c(colnames(dt))
id_vars = c("id", "dps", "health_zone", "date", "donor", "operational_support_partner", "population", "imputation_number", "combine")
inds = all_vars[!all_vars %in% id_vars] 

if (nrow(unique(dt[,c("id", "dps", "health_zone", "date", "donor", "operational_support_partner", "population", "combine", "imputation_number")])) != (nrow(dt))) stop("unique identifiers don't uniquely identify rows")
if (nrow(unique(dt[,c("id", "dps", "health_zone", "date", "donor", "operational_support_partner", "population", "combine")])) != (nrow(dt)/50)) stop("unique identifiers don't uniquely identify rows")

# use healthFacilitiesProportion to calculate healthFacilities_numReporting
# first, need to calculate health facilities reporting, before summing over rows
dt[, healthFacilities_numReporting := healthFacilitiesProportion * healthFacilities_total]
dt[, healthFacilitiesProportion := NULL]

inds = inds[!inds %in% "healthFacilitiesProportion"]
inds = c(inds, "healthFacilities_numReporting")
# ----------------------------------------------

# ----------------------------------------------
# Calculate MEDIAN, upper, and lower across impuations by HEALTH ZONE
# ----------------------------------------------
id_vars = id_vars[!id_vars %in% "imputation_number"]

median = dt[, lapply(.SD, median), by = id_vars, .SDcols = inds]
lower = dt[, lapply(.SD, quantile, .05), by = id_vars, .SDcols = inds]
upper = dt[, lapply(.SD, quantile, .95), by = id_vars, .SDcols = inds]

median = melt.data.table(median, id.vars = id_vars, variable.factor = FALSE, value.name = "value")
lower = melt.data.table(lower, id.vars = id_vars, variable.factor = FALSE, value.name = "lower")
upper = melt.data.table(upper, id.vars = id_vars, variable.factor = FALSE, value.name = "upper")

agg_hz = merge(median, lower, by = c(id_vars, "variable"))
agg_hz = merge(agg_hz, upper, by = c(id_vars, "variable"))

# get rid of lower and upper values for values that were NOT missing, so these don't show up on the graph
agg_hz[upper==lower, lower := NA ]
agg_hz[is.na(lower), upper := NA ]

if ((nrow(agg_hz[ is.na(lower) & !is.na(upper), ]) != 0) | (nrow(agg_hz[ !is.na(lower) & is.na(upper), ]) != 0)) stop("when lower or upper is NA, both should be NA")

# export graphDataComplete
saveRDS(agg_hz, paste0(dir, condensed_imputed_data_hz))
# ----------------------------------------------

# ----------------------------------------------
# Aggregate first and then calculate median and variance across imputations by DPS
# ----------------------------------------------
id_vars = c("dps", "date")

# aggregate all indicator/intervention data by dps, within each imputation
agg_dps = dt[, lapply(.SD, sum), by = c(id_vars, "imputation_number"), .SDcols = inds]
if ( nrow(agg_dps) != (nrow(unique(dt[, c("date", "dps")]))*50)) stop("unique identifiers don't uniquely identify rows")

# then compute the median, upper and lower across all imputations for each unique dps/date (the same way as for hz, only unique id's are different)
median = agg_dps[, lapply(.SD, median), by = id_vars, .SDcols = inds]
lower = agg_dps[, lapply(.SD, quantile, .05), by = id_vars, .SDcols = inds]
upper = agg_dps[, lapply(.SD, quantile, .95), by = id_vars, .SDcols = inds]

median = melt.data.table(median, id.vars = id_vars, variable.factor = FALSE, value.name = "value")
lower = melt.data.table(lower, id.vars = id_vars, variable.factor = FALSE, value.name = "lower")
upper = melt.data.table(upper, id.vars = id_vars, variable.factor = FALSE, value.name = "upper")
                        
dps_level = merge(median, lower, by = c(id_vars, "variable"))
dps_level = merge(dps_level, upper, by = c(id_vars, "variable"))

# set upper and lower values to NA where the value was not imputed (where median==lower and median==upper)
dps_level[upper==lower, lower := NA ]
dps_level[is.na(lower), upper := NA ]

if ((nrow(dps_level[ is.na(lower) & !is.na(upper), ]) != 0) | (nrow(dps_level[ !is.na(lower) & is.na(upper), ]) != 0)) stop("when lower or upper is NA, both should be NA")

# export data
saveRDS(dps_level, paste0(dir, condensed_imputed_data_dps))
# ----------------------------------------------

# ----------------------------------------------
# Aggregate first and then calculate median and variance across imputations by COUNTRY
# 8/29/18 update: changed country level confidence interval
# ----------------------------------------------
id_vars = c("date")

# first, aggregate all indicator/intervention data by dps, within each imputation
agg_natl = dt[, lapply(.SD, sum), by = c(id_vars, "imputation_number"), .SDcols = inds]
if ( nrow(agg_natl) != ( nrow(unique(dt[, c("date")]))*50 )) stop("unique identifiers don't uniquely identify rows")

# then compute the median, upper and lower across all imputations for each unique dps/date
median = agg_natl[, lapply(.SD, median), by = id_vars, .SDcols = inds]
lower = agg_natl[, lapply(.SD, quantile, .05), by = id_vars, .SDcols = inds]
upper = agg_natl[, lapply(.SD, quantile, .95), by = id_vars, .SDcols = inds]

median = melt.data.table(median, id.vars = id_vars, variable.factor = FALSE, value.name = "value")
lower = melt.data.table(lower, id.vars = id_vars, variable.factor = FALSE, value.name = "lower")
upper = melt.data.table(upper, id.vars = id_vars, variable.factor = FALSE, value.name = "upper")

natl_level = merge(median, lower, by = c(id_vars, "variable"))
natl_level = merge(natl_level, upper, by = c(id_vars, "variable"))

# set upper and lower values to NA where the value was not imputed (where median==lower and median==upper)
natl_level[upper==lower, lower := NA ]
natl_level[is.na(lower), upper := NA ]

if ((nrow(natl_level[ is.na(lower) & !is.na(upper), ]) != 0) | (nrow(natl_level[ !is.na(lower) & is.na(upper), ]) != 0)) stop("when lower or upper is NA, both should be NA")

# export data
saveRDS(natl_level, paste0(dir, condensed_imputed_data_country))
# ----------------------------------------------



# # ----------------------------------------------
# ## get aggregate yearly values at the country level:
# dt3 <- dt3[ dps != "0", ]
# dt3$year <- year(dt3$date)
# fullCountryData  <- dt3[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere"), .(aggValue = sum(imp_value)), by=c( "year", "imputation_number" )]
# 
# # then compute the mean, upper and lower across all imputations for each unique dps/date
# fullCountryData <- fullCountryData[, .(mean=mean(aggValue), 
#                                        lower=quantile(aggValue, .025), 
#                                        upper=quantile(aggValue, .975)), by=c("year")]
# 
# # set upper and lower values to NA where the value was not imputed (where mean==lower and mean==upper)
# fullCountryData <- fullCountryData[mean==lower, lower := NA]
# fullCountryData <- fullCountryData[mean==upper, upper := NA]
# 
# # export data
# saveRDS(fullCountryData, paste0(dir, condensed_imputed_data_country2))
# # ----------------------------------------------          
