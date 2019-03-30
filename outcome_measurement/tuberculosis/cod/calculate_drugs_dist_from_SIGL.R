setwd('C:/local/gf/')
# Audrey Batzel

# add 3/15/19 - results from QR imputation
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
# -------------------------

# ---------------------------------------------------
# DATA PREP
# ---------------------------------------------------
  # # Load data
  # dt <- readRDS(paste0(data_path, data))
  # catalogue <- read.csv(paste0(catalogue_path, cat))
  # 
  # # subset DHIS data to just the data on drugs that we want to assess by first grabbing those variables from the catalogue 
  # # and then subsetting to just those in dt
  # drugs_consumed <- catalogue[grepl("consumed", catalogue$element_eng) & catalogue$keep == 1,]
  # drugs_avail <- catalogue[grepl("available", catalogue$element_eng) & catalogue$keep == 1,]
  # drugs_lost <- catalogue[grepl("lost", catalogue$element_eng) & catalogue$keep == 1,]
  # 
  # vars_to_keep <- rbind(drugs_consumed, drugs_avail, drugs_lost)
  # vars_to_keep$element_id <- as.character(vars_to_keep$element_id)
  # vars_to_keep <- vars_to_keep$element_id
  # 
  # dt$element_id <- as.character(dt$element_id)
  # dt <- dt[element_id %in% vars_to_keep, ]
  # dt$element <- as.character(dt$element)
  # 
  # dt[grep("perdue", dt$element), variable:= "lost"]
  # dt[grep("consommée", dt$element), variable:= "consumed"]
  # dt[grep("utilisable", dt$element), variable:= "available"]
  # 
  # dt[grep("RHE", element, fixed=TRUE), drug:= "RHE"]
  # dt[grep("RHZ", element, fixed=TRUE), drug:= "RHZ"]
  # dt[grep("RHZE", element, fixed=TRUE), drug:= "RHZE"]
  # dt[grep("(RH) 150mg+75mg", element, fixed=TRUE), drug:= "RH_150mg+75mg"]
  # dt[grep("(RH) 60mg+30mg", element, fixed=TRUE), drug:= "RH_60mg+30mg"]
  # dt[grep("Amodiaquine (6-13 ans", element, fixed=TRUE), drug:= "ASAQ_6to13yrs"]
  # dt[grep("Amodiaquine (+14 ans", element, fixed=TRUE), drug:= "ASAQ_14yrsAndOlder"]
  # dt[grep("Amodiaquine (2-11 mois)", element, fixed=TRUE), drug:= "ASAQ_2to11mos"]
  # dt[grep("Amodiaquine (12-59 mois)", element, fixed=TRUE), drug:= "ASAQ_12to59mos"]
  # dt[grep("Sulfadoxine", element, fixed=TRUE), drug:= "SP"]
  # dt[grep("Artemether 80mg+480mg", element, fixed=TRUE), drug:= "AL_80mg+480mg"]
  # dt[grep("Artemether 40mg+240mg", element, fixed=TRUE), drug:= "AL_40mg+240mg"]
  # dt[grep("Artesunate 200mg Suppositoire", element, fixed=TRUE), drug:= "ASAQ_supp_200mg"]
  # dt[grep("Artesunate 400mg Suppositoire", element, fixed=TRUE), drug:= "ASAQ_supp_400mg"]
  # dt[grep("Artesunate 60mg Injectable", element, fixed=TRUE), drug:= "ASAQ_inj"]
  # dt[grep("Quinine base 500mg Injectable", element, fixed=TRUE), drug:= "Quinine_inj"]
  # dt[grep("Quinine base 500 mg Cès", element, fixed=TRUE), drug:= "Quinine_tab"]
  # 
  # dt <- dt[!is.na(drug)] # other drugs / commodities we aren't as interested in for now (might want to go back and add in HIV drugs/or other malaria commodities like RDTs and LLINs)
  # 
  # saveRDS(dt, paste0(data_path, prepped_drug_data))
# ---------------------------------------------------

# ---------------------------------------------------
# Load prepped data
# ---------------------------------------------------
dt <- readRDS(paste0(data_path, prepped_drug_data))
# ---------------------------------------------------

# ---------------------------------------------------
# Continue data prep for calculating drugs dist
# ---------------------------------------------------
# snis data is more accurate from 2017 on for drc
dt <- dt[ date >= "2017-01-01"]
dt <- dt[, c("coordinates", "mtk", "last_update", "download_number", "country", "category", "element_id", "element", "element_eng") := NULL]

# check unique identifiers
if( nrow(unique(dt[, .(date, org_unit_id, drug, variable)])) != nrow(dt) ) stop( "unique identifiers do not uniquely ID rows")

# create formula for cast
all_vars <- colnames(dt)
vars_for_cast <- all_vars[!all_vars %in% c("variable", "value")]
f <- as.formula(paste(paste(vars_for_cast, collapse = " + "), "~ variable"))

# cast variable wide so we can add/subtract vars
dt_wide <- dcast.data.table(dt, f, value.var = "value")

# # OPTIONAL - sum by DPS here before calculation, then do calculation. Testing what happens when I try this
# sdcols <- c('available', 'consumed', 'lost')
# dt_wide <- dt_wide[, lapply(.SD, sum, na.rm=TRUE), .SDcols= sdcols, by= c('dps','date', 'type', 'drug')]

# rectangularize data set
drugs <- unique(dt_wide$drug)
dates <- unique(dt_wide$date)
org_unit_ids <- unique(dt_wide$org_unit_id)
rect <- expand.grid(dates, drugs, org_unit_ids)
names(rect)= c("date", "drug", "org_unit_id")

# merge other id vars onto rect so they aren't NA when merging back to other data
org_unit_id_vars <- unique(dt_wide[, .(org_unit_id, org_unit, org_unit_type, level, dps, health_zone, health_area)])
rect <- merge(rect, org_unit_id_vars, by = c("org_unit_id"), all = TRUE)

data_id_vars <- unique(dt_wide[, .(data_set, drug)])
rect <- merge(rect, data_id_vars, by = c("drug"), all = TRUE)

rect <- as.data.table(rect)

id_vars = names(dt_wide)[!names(dt_wide) %in% c("available", "consumed", "lost")]
# merge back to data set so we have a fully rectangularized data set with all of our data
dt_rect <- merge(rect, dt_wide, by = id_vars, all = TRUE)

saveRDS(dt_rect_long, paste0(data_path, prepped_for_qr))

#------IMPUTATION HERE with run_quantreg_parallel.R----------------

# load in imputed data:
dt = readRDS(paste0(data_path, post_qr))
dt[, date := as.Date(date, origin = "1970-01-01")]

# Figure out how much of the missingness could be from a facility not stocking certain drugs:
# total missing values = 6566509
missing = dt[is.na(value), .(num_missing= .N), by = c("org_unit_id", "drug")] # this will be across all time/all variables of a drug...
# right now all missing would be = 39 NAs
sum(missing[num_missing == 39, num_missing]) / sum(missing$num_missing) # 62 % of all missing is due to facilities with no data at all (whole time series) for the drug (both avail and consumed)
# Then, the same, excluded the variable "lost"
missing = dt[is.na(value), .(num_missing= .N), by = c("org_unit_id", "drug", "variable")] 
missing[ variable == "lost", sum(num_missing)] # = 2654006, so 40% of total missing is from the "lost" variable
missing = dt[is.na(value) & variable != "lost", .(num_missing= .N), by = c("org_unit_id", "drug")] 
sum(missing[num_missing == 26, num_missing]) # 2752906 = the number missing the full time series for drugs not including the lost variable
# 2752906 + 2654006 = 5406912 missing due to facility missing all data for the drug or the lost variable missing
# 5406912 / 6566509 * 100 = 82% of the missing data that is still missing could potentially be account for other ways...
# 6566509 - 5406912 = 1159597 data points still missing

# now, exclude org_unit_id - drug where all values are NA by facility and drug, across time and specific variable (realized I could probably have used all(is.na(x))...) 
missing = dt[is.na(value), .(num_missing= .N), by = c("org_unit_id", "drug")] 
exclude_from_rect = unique( missing[num_missing == 39, .(org_unit_id, drug)] )
dt_for_calc = anti_join(dt, exclude_from_rect, by = c("org_unit_id", "drug"))
dt_for_calc = as.data.table(dt_for_calc)

# verification of results step - compare median of observed values by id vars to the median of imputed values
# should be similar, so note any cases that are really different
check1= dt_for_calc[is.na(got_imputed), .(med_observed = median(value, na.rm = TRUE)), by = .(org_unit_id, drug, variable)]
check2= dt_for_calc[got_imputed == "yes", .(med_imputed = median(value, na.rm = TRUE)), by = .(org_unit_id, drug, variable)]
check3= dt_for_calc[, .(med_fitted = median(fitted_value, na.rm = TRUE)), by = .(org_unit_id, drug, variable)]
check = merge(check1, check2, all = TRUE)
check = merge(check, check3, all = TRUE)
check[, med_imputed := round(med_imputed, digits = 2)]
check[, abs_diff := abs(med_observed - med_imputed) ]
# ---------------------------------------------------

# ---------------------------------------------------
# Calculate drugs received(/distributed?) using the formula:
# received(n) = available(n) + consumed(n) + lost(n) - avialable(n-1)
# ---------------------------------------------------
# aggregate to hz level first to improve calculation.
dt_for_calc[, c("variable_id", "element_id", "got_imputed", "skipped_qr", "fitted_value", "resid", "med_imputed",
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


  