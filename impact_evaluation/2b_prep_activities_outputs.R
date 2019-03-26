# Audrey Batzel
# 1-9-19
#
# Prep PNLP/SNIS for pilot data set for impact evaluation
# The current working directory should be the root of this repo (set manually by user)
# -----------------------------------------------------------

# ---------------------------------------------------
library(dplyr)
# FUNCTIONS
convert_date_to_quarter <- function(dt){
  dt$year <- year(dt$date)
  dt$month <- month(dt$date)
  dt[ month %in% 1:3, quarter:= 1]
  dt[ month %in% 4:6, quarter:= 2]
  dt[ month %in% 7:9, quarter:= 3]
  dt[ month %in% 10:12, quarter:= 4]
  return(dt)
}

convert_quarter_to_decimal <- function(dt){
  dt$quarter <- as.character(dt$quarter)
  dt[ quarter == '1', quarter:= '.00']
  dt[ quarter == '2', quarter:= '.25']
  dt[ quarter == '3', quarter:= '.50']
  dt[ quarter == '4', quarter:= '.75']
  dt$year <- as.character(dt$year)
  dt[, date := paste0(year, quarter)]
  dt[, quarter:= NULL]
  dt[, year := NULL]
  dt$date <- as.numeric(dt$date)
  return(dt)
}
source('./core/standardizeHZNames.R')
# ---------------------------------------------------

# ---------------------------------------------------
# Read in data
# ---------------------------------------------------
dt <- readRDS(combinedFile)
sigl_comp <- readRDS(comp_sigl_file)
base_comp <- readRDS(comp_base_file)
pnlp_comp <- readRDS(pnlpHZFile)

# standardize health zone names in SNIS files - because of the three that we combined, will need to avg across health_zones
base_comp[ , health_zone := standardizeHZNames(health_zone)]
sigl_comp[ , health_zone := standardizeHZNames(health_zone)]

base_comp = base_comp[, .(completeness = mean(completeness)), by = .(dps, dps_code, health_zone, date, year, quarter)]
sigl_comp = sigl_comp[, .(completeness = mean(completeness)), by = .(dps, dps_code, health_zone, date, year, quarter)]

if (nrow(base_comp[duplicated(base_comp[, .(health_zone, dps, year, quarter)])]) != 0 
    & nrow(sigl_comp[duplicated(sigl_comp[, .(health_zone, dps, year, quarter)])]) != 0){
  stop ( "Unique identifiers do not uniquely idenitfy rows!")}
# ---------------------------------------------------

# ---------------------------------------------------
# Change variable names / other set up
# ---------------------------------------------------
# Subset DHIS2 data sets to 2018 data onward (use PNLP up to 2017); for now, stop at Q4 2018
remove_data <- dt[ (data_set == "snis_base_services" & date < "2018-01-01") | (data_set == "snis_base_services" & date > "2018-12-01"),]
remove_data <- rbind(remove_data, dt[ (data_set == "snis_sigl" & date < "2018-01-01") | (data_set == "snis_base_services" & date > "2018-12-01"),])
dt <-  anti_join(dt, remove_data)
dt <- as.data.table(dt)

dt <- convert_date_to_quarter(dt)
# ---------------------------------------------------

# ---------------------------------------------------
# Aggregate to quarterly data 
# ---------------------------------------------------
dt <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(year, quarter, dps, health_zone, data_set, indicator, subpopulation)]
# ---------------------------------------------------

# ---------------------------------------------------
# Merge completeness measure from SNIS dashboard  - NOTE: they need to be re-downloaded at a quarterly time point
# ---------------------------------------------------
base_comp[, data_set := "snis_base_services"]
sigl_comp[, data_set := "snis_sigl"]

base_comp[, year := as.numeric(year)]
sigl_comp[, year := as.numeric(year)]
dt[, year := as.numeric(year)]

base_comp[, quarter := as.numeric(quarter)]
sigl_comp[, quarter := as.numeric(quarter)]
dt[, quarter := as.numeric(quarter)]

dt_base <- merge(dt[data_set == "snis_base_services", ], base_comp, by = c("data_set", "year", "quarter", "dps", "health_zone"), all.x = TRUE)
dt_sigl <- merge(dt[data_set == "snis_sigl", ], sigl_comp, by = c("data_set", "year", "quarter", "dps", "health_zone"), all.x = TRUE)
dt_pnlp <- dt[data_set == "pnlp"]
# ---------------------------------------------------

# ---------------------------------------------------
# Completeness from PNLP --> USE pnlp_comp after running this block of code
# Calculate completeness from the hz level pnlp data
# (not indicator-specific unfortunately - just date specific at natl level)
# ---------------------------------------------------
pnlp_comp$variable <- as.character(pnlp_comp$variable)

pnlp_fac <- pnlp_comp[variable %in% c("healthFacilities_total", "healthFacilitiesProduct"), .(dps, health_zone, date, variable, mean)]
setnames(pnlp_fac, "mean", "value")

pnlp_fac <- dcast.data.table(pnlp_fac, dps + health_zone + date ~ variable)
pnlp_fac[, healthFacilities_reporting := healthFacilitiesProduct / healthFacilities_total]
pnlp_fac[, completeness := healthFacilities_reporting / healthFacilities_total]

# correction for where proportion reporting is > 1
pnlp_fac[completeness > 1, healthFacilities_reporting := healthFacilities_total] 

# sum numerator and denominator quarterly
pnlp_fac <- convert_date_to_quarter(pnlp_fac)
pnlp_comp <- pnlp_fac[, .(healthFacilities_reporting = sum(healthFacilities_reporting),
                          healthFacilities_total = sum(healthFacilities_total)),
                          by = .(dps, health_zone, year, quarter) ]
pnlp_comp[ , completeness:= healthFacilities_reporting / healthFacilities_total]
pnlp_comp = pnlp_comp[, .(dps, health_zone, year, quarter, completeness)]
pnlp_comp[, data_set := "pnlp"]
# ---------------------------------------------------

# ---------------------------------------------------
# Subset to years where data wasn't fully imputed and just the indicators we want for activities/outputs
# ---------------------------------------------------
# remove vars from years where they were missing in the original data (they are in here because they were imputed when the data was rectangularized)
dt[ data_set =="pnlp" & grepl(indicator, pattern = 'AL') & year < 2015, value := NA]

# keep SSCACT with subpop = NA for 2015 and 2016, keep with subpops <5 and >5 for 2017
dt[ data_set == "pnlp" & indicator == "SSCACT" & is.na(subpopulation) & year < 2015, value := NA]
dt[ data_set == "pnlp" & indicator == "SSCACT" & is.na(subpopulation) & year > 2016, value := NA]
dt[ data_set == "pnlp" & indicator == "SSCACT" & !is.na(subpopulation) & year < 2017, value := NA]

dt <- dt[!is.na(value)]

dt <- dt[ indicator %in% c("LLIN", "ASAQreceived", "SP", "ALreceived", "SSCACT", "simpleConfMalariaTreated", "severeMalariaTreated", "presumedMalariaTreated", "RDT") 
          & !subpopulation %in% c("lost", "available", "stockOutDays", "positive"), ]
dt <- dt[! (indicator %in% "RDT" & subpopulation %in% "consumed")]
dt <- dt[! (data_set %in% "snis_base_services" & indicator %in% "LLIN"), ]
# ---------------------------------------------------

# ---------------------------------------------------
# Sum together variables to create the ones needed for the model
# ---------------------------------------------------
# because we have some that need to be summed that have different subpopulations and some that shouldn't be summed with different subpopulations
# I'm doing a slightly hacky thing here-by changing the name of the subpopulation to all be the same on the ones that should be summed, we can then
# sum by indicator and subpopulation to preserve the ones (like RDT completed vs RDT received) that shouldn't just be summed on indicator
dt[ data_set == "pnlp" & indicator == "LLIN" & subpopulation != "received", subpopulation := "consumed"]
dt[ indicator == "ASAQreceived", subpopulation := "none"]
dt[ indicator == "SP", subpopulation := "none"]
dt[ indicator == "SSCACT", subpopulation := "none"]
dt[ indicator == "severeMalariaTreated", subpopulation := "none"]
dt[ indicator == "simpleConfMalariaTreated", subpopulation := "none"]

dt <-  dt[, .(value = sum(value, na.rm=TRUE)), by=.(year, quarter, dps, health_zone, data_set, indicator, subpopulation, completeness)]

# we need to create variable for totalPatientsTreated and a variable for ACT_received, will do this separately then rbind back together
acts_rec <- dt[ indicator %in% c("ALreceived", "ASAQreceived") ]
acts_rec <- acts_rec[, .(value = sum(value, na.rm=TRUE)), by=.(year, quarter, dps, health_zone, data_set, completeness)]
acts_rec[, indicator := "ACT_received"]
acts_rec[, subpopulation := "none"]

patients_treated <- dt[ grepl("treated", indicator, ignore.case = TRUE), ]
patients_treated <- patients_treated[, .(value = sum(value, na.rm=TRUE)), by=.(year, quarter, dps, health_zone, data_set, completeness)]
patients_treated[, indicator := "totalPatientsTreated"]
patients_treated[, subpopulation := "none"]

dt_final <- rbindlist(list(acts_rec, patients_treated, dt), use.names = TRUE)

# remove variables we don't need for impact model
dt_final <- dt_final[!indicator %in% c("simpleConfMalariaTreated", "ASAQreceived", "ALreceived", "presumedMalariaTreated")]

# rename pnlp variables to match to impact model
dt_final[ indicator == "LLIN" & subpopulation == "consumed", indicator := "ITN_consumed" ]
dt_final[ indicator == "LLIN" & subpopulation == "received", indicator := "ITN_received" ]
dt_final[ indicator == "RDT" & subpopulation == "completed", indicator := "RDT_completed" ]
dt_final[ indicator == "RDT" & subpopulation == "received", indicator := "RDT_received" ]
dt_final[ indicator == "SSCACT", indicator := "ACTs_SSC" ]
dt_final <- dt_final[,.(year, quarter, dps, health_zone, indicator, value, completeness)]

dt_final = convert_quarter_to_decimal(dt_final)

saveRDS(dt_final, outputFile2b)
dt_final <- readRDS(outputFile2b)
archive(dt_final, outputFile2b)
# ---------------------------------------------------

# ---------------------------------------------------
# switch data to wide format
# ---------------------------------------------------
# dt <- readRDS(outputFile2b)
# dt_final_wide <- dcast.data.table(dt_final, date + dps + health_zone ~ indicator, value.var = c("value"))
dt_final_wide <- dcast.data.table(dt_final, date + dps + health_zone ~ indicator, value.var = c("value", "completeness"))

saveRDS(dt_final_wide, outputFile2b_wide)
archive(dt_final_wide, outputFile2b_wide)
# ---------------------------------------------------
