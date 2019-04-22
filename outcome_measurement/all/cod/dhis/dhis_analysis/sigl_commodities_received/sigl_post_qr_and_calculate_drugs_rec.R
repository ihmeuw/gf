setwd('C:/local/gf/')
# Audrey Batzel

# 4/18/19 split this off into it's own file because it was getting to overwhelming/complex
# this file is for manipulating the results of QR on SIGL drugs available, consumed, and lost data, to include:
  # - removing outliers
  # - fixing bad QR results (where fitted value is negative, replace with 0)
  # - replace > 99.5 percentile with missing where QR was skipped (some of these look to be outliers, but since
  #     QR didn't run on them, they won't be caught with the other outlier method.)
  # - handle values still missing with median imputation

# Then aggregate to hz level and calculate drugs/commodities received
# ----------------------------------------------

# --------------------
# Set up R / install packages
## -------------------
rm(list=ls())
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
# -------------------------
# Files and directories
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set directories
data_path <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')
catalogue_path <- paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/catalogues/")

# input files
data <-"sigl_prepped.rds"
cat <- "data_elements_cod.csv"
imputed_data = paste0(data_path, 'sigl_quantreg_imputation_results.rds')

# output files 
prepped_drug_data <- "drugs_consumed_lost_available_update_3_6_19.rds"
prepped_for_qr = "sigl_for_qr.rds"
post_qr = "sigl_quantreg_imputation_results.rds"
drugs_dist = "drugs_dist_update_3_28_19.rds"

# # functions
# source('./core/standardizeHZNames.R')
# -------------------------# load in imputed data:
dt = readRDS(paste0(data_path, post_qr))
dt[, date := as.Date(date, origin = "1970-01-01")]

# # Figure out how much of the missingness could be from a facility not stocking certain drugs:
# # total missing values = 6566509
# missing = dt[is.na(value), .(num_missing= .N), by = c("org_unit_id", "drug")] # this will be across all time/all variables of a drug...
# # right now all missing would be = 39 NAs
# sum(missing[num_missing == 39, num_missing]) / sum(missing$num_missing) # 62 % of all missing is due to facilities with no data at all (whole time series) for the drug (both avail and consumed)
# # Then, the same, excluded the variable "lost"
# missing = dt[is.na(value), .(num_missing= .N), by = c("org_unit_id", "drug", "variable")] 
# missing[ variable == "lost", sum(num_missing)] # = 2654006, so 40% of total missing is from the "lost" variable
# missing = dt[is.na(value) & variable != "lost", .(num_missing= .N), by = c("org_unit_id", "drug")] 
# sum(missing[num_missing == 26, num_missing]) # 2752906 = the number missing the full time series for drugs not including the lost variable
# # 2752906 + 2654006 = 5406912 missing due to facility missing all data for the drug or the lost variable missing
# # 5406912 / 6566509 * 100 = 82% of the missing data that is still missing could potentially be account for other ways...
# # 6566509 - 5406912 = 1159597 data points still missing

# now, exclude org_unit_id - drug where all values are NA by facility and drug, across time and specific variable (realized I could probably have used all(is.na(x))...) 
# missing = dt[is.na(value), .(num_missing= .N), by = c("org_unit_id", "drug")] 
# exclude_from_rect = unique( missing[num_missing == 39, .(org_unit_id, drug)] )
# dt_for_calc = anti_join(dt, exclude_from_rect, by = c("org_unit_id", "drug"))
# dt_for_calc = as.data.table(dt_for_calc)
dt[, completely_missing:=all(is.na(value)), by=c('org_unit_id','drug')]
dt = dt[completely_missing==FALSE]

# verification of results step - compare median of observed values by id vars to the median of imputed values
# should be similar, so note any cases that are really different
check1= dt[is.na(got_imputed) & skipped_qr == "no", .(med_observed = median(value, na.rm = TRUE)), by = .(org_unit_id, level, drug, variable)]
check2= dt[got_imputed == "yes" & skipped_qr == "no", .(med_imputed = median(value, na.rm = TRUE)), by = .(org_unit_id,level,  drug, variable)]
check3= dt[skipped_qr == "no", .(med_fitted = median(fitted_value, na.rm = TRUE)), by = .(org_unit_id, level, drug, variable)]

dt[ got_imputed== "yes", value := NA ]
# want to try changing minimum # of values needed to run QR from 3 to 4..
# get the number of values that were present before QR imputation
check4 = dt[!is.na(value) & skipped_qr == "no", .N, by = .(org_unit_id, level, drug, variable) ]

check = merge(check1, check2, all = TRUE)
check = merge(check, check3, all = TRUE)
check = merge(check, check4, all = TRUE)

check[, med_imputed := round(med_imputed, digits = 2)]
check[, abs_diff := abs(med_observed - med_imputed) ]

dt = merge(dt, check, all = TRUE, by = c('org_unit_id', 'drug', 'variable'))

# make figures that show an example of good fit and bad fit
o = 'kVjIG3QXgoX'
d = 'RH_150mg+75mg'

ex = ggplot( dt[ org_unit_id == o & drug == d, ], aes(x=date, y=value)) +
  geom_point(size = 2) +
  geom_line(aes(x=date, y=fitted_value), alpha = 0.5) +
  geom_point(data= dt[  org_unit_id == o & drug == d & got_imputed == "yes" ], aes(x=date, y = fitted_value), color='#d73027', alpha=0.8) +
  facet_wrap(~ variable, scales = "free") + 
  theme_bw()
ex

# set fitted values back to value where imputation was true
dt[ got_imputed == "yes", value := fitted_value]
# ---------------------------------------------------

# ---------------------------------------------------
# Calculate drugs received(/distributed?) using the formula:
# received(n) = available(n) + consumed(n) + lost(n) - avialable(n-1)
# ---------------------------------------------------
# aggregate to hz level first to improve calculation.
dt_for_calc = dt[, c("variable_id", "element_id", "got_imputed", "skipped_qr", "fitted_value", "resid", "med_imputed",
                     "med_fitted", "med_observed") := NULL]

dt_hz = dt_for_calc[, .(value = sum(value, na.rm = TRUE)), by= .(health_zone, dps, date, drug, variable)]

# cast wide
# create formula for cast
all_vars <- colnames(dt_hz)
vars_for_cast <- all_vars[!all_vars %in% c("variable", "value")]
f <- as.formula(paste(paste(vars_for_cast, collapse = " + "), "~ variable"))
# cast variable wide so we can add/subtract vars
dt_for_calc_wide <- dcast.data.table(dt_hz, f, value.var = "value")

# identify where previous date (by month) is missing in the data,
# by unique identifiers
calc <- setorderv(dt_for_calc_wide, c("health_zone", "drug", "date"))
calc <- calc[, previous_date := (date - months(1))]
calc <- calc[, actual_previous_date := data.table::shift(date, 1L, type="lag"), by=c('health_zone', 'drug')] #by=c('org_unit_id', 'drug', 'category')]
calc <- calc[, include_in_calculation := ifelse(previous_date == actual_previous_date, TRUE, FALSE), ]

# calculate received, only for dates where the previous month exists
# use shift to get the value for the previous date of the available variable
calc[is.na(lost), lost:= 0]
calc[include_in_calculation==TRUE, received := (available + consumed + lost - ( data.table::shift(available, 1L, type="lag") ))]

# # aggregate to health zone values
# sd_cols = c("available", "consumed", "lost", "received")
# hz_level <- calc[, lapply(.SD, sum, na.rm=TRUE), by=c("date", "dps", "health_zone", "drug", "category"), .SDcols = sd_cols]

# save a copy of the data with drugs distributed for analysis/comparison to PNLP
saveRDS(calc, paste0(data_path, drugs_dist))
# calc <- readRDS(paste0(data_path, "drugs_dist_update_3_28_19.rds"))

# # where level/org unit type is health zone, the actual field for health zone is blank
# # fill this in so that we can aggregate to health zone level
# calc[is.na(health_zone), edit_hz_string:= TRUE]
# calc[org_unit_type =="health zone", health_zone:= org_unit]
# calc[, health_zone:= tolower(health_zone)]
# 
# calc[edit_hz_string== TRUE, health_zone:= sapply(str_split(health_zone, " ", 2),'[', 2)]
# calc[edit_hz_string== TRUE, health_zone := str_squish(health_zone)]
# calc[edit_hz_string== TRUE, health_zone := trimws(health_zone)]
# calc[edit_hz_string== TRUE, health_zone := gsub(" zone de santé", "", health_zone)]
# 
# calc[, health_zone:= gsub(" ", "-", health_zone)]
# 
# calc[, health_zone := standardizeHZNames(health_zone)]
# ---------------------------------------------------


