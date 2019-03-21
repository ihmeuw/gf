# Audrey Batzel
# 1-9-19
#
# Prep PNLP/SNIS for pilot data set for impact evaluation
# The current working directory should be the root of this repo (set manually by user)
# -----------------------------------------------------------

# ---------------------------------------------------
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
# ---------------------------------------------------

# ---------------------------------------------------
# Read in data
# ---------------------------------------------------
# pnlp_hz <- readRDS(pnlpHZFile) # hz level, monthly - get completeness from this
# dt_pnlp <- readRDS(pnlpFile) # national level, monthly
#   setnames(dt_pnlp, "mean", "value")
# dt_base <- readRDS(snisBaseFile) # facility level, monthly
# dt_sigl <- readRDS(snisSiglFile) # facility level, monthly
dt <- readRDS(combinedFile)
# ---------------------------------------------------

# ---------------------------------------------------
# Change variable names / other set up
# ---------------------------------------------------
# Subset DHIS2 data sets to 2018 data onward (use PNLP up to 2017)
remove_data <- dt[ data_set == "snis_base_services" & date < "2018-01-01",]
remove_data <- rbind(remove_data, dt[ data_set == "snis_sigl" & date < "2018-01-01",])
dt <-  anti_join(dt, remove_data)
dt <- as.data.table(dt)

dt <- convert_date_to_quarter(dt)
# ---------------------------------------------------

# # ---------------------------------------------------
# # Calculate completeness measure from SNIS data --> USE comp_base and comp_sigl after running this block of code
# # ---------------------------------------------------
# # BASE SERVICES DATA------------>
# # calculate the number of unique facilities reporting by indicator, indicator-month, and indicator-year
# # since we are just using 2018 for now, number of unique facilities by indicator-year will be the same as by indicator-ever
# # not using this one now... # dt_comp_base <- dt_base[, .(num_fac_per_indicator_ever = length(unique(org_unit))), by=c("element_eng", "indicator", "subpopulation", "category")]
# dt_comp_base2 <- dt_base[, .(num_fac_per_indicator_month = length(unique(org_unit))), by=c("element_eng", "indicator", "subpopulation", "category", "date", "year", "quarter")]
# dt_comp_base3 <- dt_base[, .(num_fac_per_indicator_year = length(unique(org_unit))), by=c("element_eng", "indicator", "subpopulation", "category", "year")]
# 
# comp_base <- merge(dt_comp_base2, dt_comp_base3, by=c("element_eng", "indicator", "subpopulation", "category", "year"), all = TRUE)
# 
# # calculate completeness over category and subpopulations by QUARTER, year, and indicator - sum numerator and denominator and then calculate  
# comp_base_totals <- comp_base[subpopulation != "positive"] # first - need to remove RDT_positive because we just want to use RDT_completed in the model for now. 
# # Other indicators can be averaged across subpopulations
# comp_base_totals <- comp_base_totals[, .(num = sum(num_fac_per_indicator_month),
#                                   denom = sum(num_fac_per_indicator_year)),
#                               by=c("indicator", "year", "quarter")]
# comp_base_totals[, completeness:= num/denom]
# 
# # separate calculation for totalPatientsTreated, since this invloves a combination of indicators
# comp_patientsTreated <- comp_base[indicator %in% c("mildMalariaTreated", "severeMalariaTreated", "presumedMalariaTreated")]
# comp_patientsTreated <- comp_patientsTreated[, .(num = sum(num_fac_per_indicator_month),
#                                                            denom = sum(num_fac_per_indicator_year)),
#                                                        by=c("year", "quarter")]
# comp_patientsTreated[, completeness:= num/denom]
# comp_patientsTreated[, indicator := "totalPatientsTreated"]
# 
# # bind totalPatientsTreated values for completeness back to full data set of completeness measures
# comp_base <- rbindlist(list(comp_base_totals, comp_patientsTreated), use.names= TRUE)
# 
# # SIGL (supply chain) DATA------------>
# dt_comp_sigl <- dt_sigl[, .(num_fac_per_indicator_year = length(unique(org_unit))), by=c("element_eng", "indicator", "subpopulation", "category", "year")]
# dt_comp_sigl2 <- dt_sigl[, .(num_fac_per_indicator_month = length(unique(org_unit))), by=c("element_eng", "indicator", "subpopulation", "category", "year", "date")]
# comp_sigl <- merge(dt_comp_sigl, dt_comp_sigl2, by=c("element_eng", "indicator", "subpopulation", "category", "year"), all = TRUE)
# comp_sigl[, indicator_month_comp := num_fac_per_indicator_month / num_fac_per_indicator_year]
# comp_sigl <- convert_date_to_quarter(comp_sigl)
# 
# # quarterly????
# # ---------------------------------------------------
# 
# # ---------------------------------------------------
# # Completeness from PNLP --> USE pnlp_comp after running this block of code
# # Calculate completeness from the hz level pnlp data
# # (not indicator-specific unfortunately - just date specific at natl level)
# # ---------------------------------------------------
# pnlp_hz$variable <- as.character(pnlp_hz$variable)
# 
# pnlp_fac <- pnlp_hz[variable %in% c("healthFacilities_numReportedWithinDeadline", "healthFacilities_total", "healthFacilitiesProduct"), .(dps, health_zone, date, variable, mean)]
# setnames(pnlp_fac, "mean", "value")
# 
# pnlp_fac <- dcast.data.table(pnlp_fac, dps + health_zone + date ~ variable)
# pnlp_fac[, healthFacilities_reporting := healthFacilitiesProduct / healthFacilities_total]
# pnlp_fac[, healthFacilities_proportionReporting := healthFacilities_reporting / healthFacilities_total]
# 
# pnlp_fac[healthFacilities_proportionReporting > 1, healthFacilities_reporting := healthFacilities_total] # this will make the proportions = 1, but we want 
# # to sum and then divide (I think?) rather than average 
# # so the proportions are weighted by total # of facilities
# pnlp_fac <- convert_date_to_quarter(pnlp_fac)
# 
# # sum numerator and denominator to national level, quarterly
# pnlp_comp <- pnlp_fac[, .(healthFacilities_reporting = sum(healthFacilities_reporting),
#                               healthFacilities_total = sum(healthFacilities_total)),
#                           by = c("quarter", "year")]
# pnlp_comp[ , completeness:= healthFacilities_reporting / healthFacilities_total]
# # ---------------------------------------------------
# 
# # ---------------------------------------------------
# # Aggregate to national data
# # ---------------------------------------------------
# base_natl <- dt_base[, .(value = sum(value, na.rm=TRUE)), by=c("date", "year", "type", "element", "element_eng", "category", "quarter", "indicator", "subpopulation")]
# sigl_natl <- dt_sigl[, .(value = sum(value, na.rm=TRUE)), by=c("date", "year", "type", "element", "element_eng", "category", "quarter", "indicator", "subpopulation")]
# # ---------------------------------------------------
# 
# ---------------------------------------------------
# Aggregate to quarterly data / can sum over category in SNIS
# ---------------------------------------------------
# dt_pnlp[ is.na(subpopulation), subpopulation:= "none"]
# pnlp_natl_qtr <-  dt_pnlp[, .(value = sum(value, na.rm=TRUE)), by=c("year", "quarter", "indicator", "subpopulation")]
# pnlp_natl_qtr$data_source = "PNLP"
# 
# base_natl_qtr <-  base_natl[, .(value = sum(value, na.rm=TRUE)), by=c("quarter", "year", "type", "element", "element_eng", "indicator", "subpopulation")]
# base_natl_qtr$data_source = "SNIS_base_services"
# 
# sigl_natl_qtr <-  sigl_natl[, .(value = sum(value, na.rm=TRUE)), by=c("quarter", "year", "type", "element", "element_eng", "indicator", "subpopulation")]
# sigl_natl_qtr$data_source = "SNIS_sigl"

# quarterly combined data, by data set. 
dt <- dt[, year := year(date)]
dt <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(year, quarter, dps, health_zone, data_set, indicator, subpopulation)]
# -------------------------------------------------

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

dt <-  dt[, .(value = sum(value, na.rm=TRUE)), by=.(year, quarter, dps, health_zone, data_set, indicator, subpopulation)]

# we need to create variable for totalPatientsTreated and a variable for ACT_received, will do this separately then rbind back together
acts_rec <- dt[ indicator %in% c("ALreceived", "ASAQreceived") ]
acts_rec <- acts_rec[, .(value = sum(value, na.rm=TRUE)), by=.(year, quarter, dps, health_zone, data_set)]
acts_rec[, indicator := "ACT_received"]
acts_rec[, subpopulation := "none"]

patients_treated <- dt[ grepl("treated", indicator, ignore.case = TRUE), ]
patients_treated <- patients_treated[, .(value = sum(value, na.rm=TRUE)), by=.(year, quarter, dps, health_zone, data_set)]
patients_treated[, indicator := "totalPatientsTreated"]
patients_treated[, subpopulation := "none"]

dt_final <- rbindlist(list(acts_rec, patients_treated, dt), use.names = TRUE)

# remove variables we don't need for pilot dataset
dt_final <- dt_final[!indicator %in% c("simpleConfMalariaTreated", "ASAQreceived", "ALreceived", "presumedMalariaTreated")]

# rename pnlp variables to match to pilot dataset
dt_final[ indicator == "LLIN" & subpopulation == "consumed", indicator := "ITN_consumed" ]
dt_final[ indicator == "LLIN" & subpopulation == "received", indicator := "ITN_received" ]
dt_final[ indicator == "RDT" & subpopulation == "completed", indicator := "RDT_completed" ]
dt_final[ indicator == "RDT" & subpopulation == "received", indicator := "RDT_received" ]
dt_final[ indicator == "SSCACT", indicator := "ACTs_SSC" ]
dt_final <- dt_final[,.(year, quarter, dps, health_zone, indicator, value)]

saveRDS(dt_final, outputFile2b)
# ---------------------------------------------------

# # ---------------------------------------------------
# # Put it all together!
# # ---------------------------------------------------
# # final clean up of completeness data
# pnlp_comp <- pnlp_comp[, .(year, quarter, completeness)]
# 
# sigl_comp <- comp_sigl[ indicator == "ITN", ]
# sigl_comp <- sigl_comp[, .(completeness = mean(indicator_month_comp)), by = c("year", "quarter", "indicator") ]  # can do a simple avg here since denom is all the same
# sigl_comp <- sigl_comp[ indicator == "ITN", indicator := "ITN_consumed"]
# 
# base_indicators <- unique(base_final$indicator)
# comp_base <- comp_base[ indicator == "RDT", indicator := "RDT_completed"]
# base_comp <- comp_base[indicator %in% base_indicators,]
# base_comp <- base_comp[, .(year, quarter, indicator, completeness)]
# 
# # merge completeness measures with data sets
# pnlp_final <- merge(pnlp_final, pnlp_comp, by = c("year", "quarter"))
# pnlp_final[, data_source := "PNLP"]
# base_final <- merge(base_final, base_comp, by = c("year", "quarter", "indicator"))
# base_final[, data_source := "SNIS_base"]
# sigl_final <- merge(sigl_final, sigl_comp, by = c("year", "quarter", "indicator"))
# sigl_final[, data_source := "SNIS_sigl"]
#     
# # bind all data sets together into final data set for pilot
# pilot_dataset <- rbindlist(list(pnlp_final, base_final, sigl_final), use.names = TRUE)
# 
# # save dataset
# saveRDS(pilot_dataset, outputFile2b)
# # ---------------------------------------------------

# ---------------------------------------------------
# switch data to wide format
# ---------------------------------------------------
# dt <- readRDS(outputFile2b)
dt_final <- dcast.data.table(dt_final, year + quarter + dps + health_zone ~ indicator, value.var = c("value"))
# dt_final <- dcast.data.table(dt_final, year + quarter + dps + health_zone ~ indicator, value.var = c("value", "completeness"))

dt_final = convert_quarter_to_decimal(dt_final)
saveRDS(dt_final, outputFile2b_wide)
# ---------------------------------------------------
