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
source('./core/standardizeDPSNames.r')
# ---------------------------------------------------

# ---------------------------------------------------
# Read in data
# ---------------------------------------------------
dt = readRDS(combined_data_file)
snis_comp = readRDS(snis_comp_file)
pnlp_comp = readRDS(pnlp_hz_file)
# ---------------------------------------------------

# ---------------------------------------------------
# Change variable names / other set up
# ---------------------------------------------------
dt = dt[ date <= "2019-03-01"]
dt = convert_date_to_quarter(dt)
# ---------------------------------------------------

# ---------------------------------------------------
# Aggregate to quarterly data 
# ---------------------------------------------------
dt = dt[, .(value = sum(value, na.rm = TRUE)), by = .(year, quarter, dps, health_zone, data_set, element, category, indicator, subpopulation)]
# ---------------------------------------------------

# ---------------------------------------------------
# Merge completeness measure from SNIS dashboard  (downloaded at hz - quarterly level)
# ---------------------------------------------------
snis_comp[, year := as.numeric(year)]
dt[, year := as.numeric(year)]
snis_comp[, quarter := as.numeric(quarter)]
dt[, quarter := as.numeric(quarter)]
setnames(snis_comp, "set", "data_set")

dt = merge(dt, snis_comp, by = c("data_set", "year", "quarter", "dps", "health_zone"), all.x = TRUE)
dt[, date := NULL]

dt[ , completeness := completeness/100]
# ---------------------------------------------------

# ---------------------------------------------------
# Completeness from PNLP 
# Calculate completeness from the hz level pnlp data
# (not indicator-specific unfortunately - just date specific at natl level)
# ---------------------------------------------------
pnlp_comp$variable <- as.character(pnlp_comp$variable)

pnlp_comp = pnlp_comp[variable %in% c("healthFacilities_total", "healthFacilities_numReporting"), .(dps, health_zone, date, variable, value)]

# health zone changes - include these in completeness calculation (sum over facilities)
# Haut Katanga:
# do not impute data for Kashobwe before 2012; after imputation add to Kasenga 
pnlp_comp[ health_zone == "kashobwe", health_zone := "kasenga" ]
# Nord Kivu:
# do not impute Alimbongo before 2012; after imputation add to Lubero
pnlp_comp[ health_zone == "alimbongo", health_zone := "lubero"]
# do not impute Kibirizi before 2016; after imputation add to Rutshuru  
pnlp_comp[health_zone == "kibirizi", health_zone := "rutshuru"]
# do not impute Mabalako before 2013; after imputation add to Beni 
pnlp_comp[health_zone == "mabalako", health_zone := "beni"]
# do not impute Kamango before 2012; after imputation add Kamango to Mutwanga (Oicha?).
pnlp_comp[health_zone == "kamango", health_zone := "mutwanga"]

pnlp_comp = convert_date_to_quarter(pnlp_comp)

# sum over these health zone changes, and date in year-quarters (rather than year-months)
pnlp_comp = pnlp_comp[, .(value = sum(value)), by = .(dps, health_zone, year, quarter, variable)]
# calculate completeness by finding the percentage of facilities reporting of the total
pnlp_comp = dcast.data.table(pnlp_comp, dps + health_zone + year + quarter ~ variable, value.var = "value")
pnlp_comp[, completeness := healthFacilities_numReporting / healthFacilities_total]

# correction for where proportion reporting is > 1
pnlp_comp[completeness > 1, completeness := 1] 

pnlp_comp[, data_set := "pnlp"]

dt = merge(dt, pnlp_comp, by= c('year', 'quarter', 'dps', 'health_zone', 'data_set'), all.x = TRUE)
setnames(dt, 'completeness.x', 'completeness')
dt[is.na(completeness) & !is.na(completeness.y), completeness := completeness.y]
dt[, c('completeness.y', 'healthFacilities_numReporting', 'healthFacilities_total') := NULL]
# ---------------------------------------------------

# ---------------------------------------------------
# Subset to years where data wasn't fully imputed and just the indicators we want for activities/outputs
# ---------------------------------------------------
# remove vars from years where they were missing in the original data (they are in here because they were imputed when the data was rectangularized)
dt[ data_set =="pnlp" & grepl(indicator, pattern = 'AL') & year < 2015, value := NA]

# keep SSCACT with subpop = NA for 2015 and 2016, keep with subpops <5 and >5 for 2017
dt[ data_set == "pnlp" & indicator == "SSCACT" & year < 2015, value := 0] # iccm didn't exist prior to 2014
# commenting these out for now because the current version of the data had subpops aggregated for imputation (we have 
# a version of them separate)
# dt[ data_set == "pnlp" & indicator == "SSCACT" & is.na(subpopulation) & year == 2017, value := NA]
# dt[ data_set == "pnlp" & indicator == "SSCACT" & !is.na(subpopulation) & year < 2017, value := NA]
dt[ data_set == "pnlp" & indicator == "SSCACT" & year < 2015, completeness := 1]

dt <- dt[is.na(value)]

dt <- dt[ indicator %in% c("LLIN", "ASAQreceived", "SP", "ALreceived", "SSCACT", "simpleConfMalariaTreated", "severeMalariaTreated", "presumedMalariaTreated", "RDT") 
          & !subpopulation %in% c("lost", "available", "stockOutDays", "positive"), ]
dt <- dt[! (indicator %in% "RDT" & subpopulation %in% "consumed")]
dt <- dt[! (data_set %in% "snis_base_services" & indicator %in% "LLIN"), ]
# ---------------------------------------------------


# ---------------------------------------------------
# Set aside child-specific variables

# possible child-specific subpops (not all are present)
subpops = c('1to5yrs','2to11mos','under5','completedUnder5','0to11mos')
under5 = dt[subpopulation %in% subpops]

# rename the indicator to reflect subpopulation
under5[, indicator:=paste0(indicator, '_under5')]

# aggregate across infants/children
byVars = c('year', 'quarter', 'dps', 'health_zone', 'data_set', 'indicator', 'completeness')
under5 = under5[, .(value = sum(value, na.rm=TRUE)), by=byVars]

# append to data
dt = rbind(dt, under5, fill=TRUE)
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
# NOTE: do not have to calculate completeness separately, because completeness is the same for each indicator (varies across time and health zones)
acts_rec <- dt[ indicator %in% c("ALreceived", "ASAQreceived") ]
acts_rec <- acts_rec[, .(value = sum(value, na.rm=TRUE)), by=.(year, quarter, dps, health_zone, data_set, completeness)]
acts_rec[, indicator := "ACT_received"]
acts_rec[, subpopulation := "none"]

# rename the under5 ASAQreceived variable to be consistent with ACT_received variable name
dt[indicator=='ASAQreceived_under5', indicator:='ACT_received_under5']

patients_treated <- dt[ grepl("treated", indicator, ignore.case = TRUE), ]
children_treated = patients_treated[subpopulation %in% subpops]
patients_treated <- patients_treated[, .(value = sum(value, na.rm=TRUE)), by=.(year, quarter, dps, health_zone, data_set, completeness)]
children_treated <- children_treated[, .(value = sum(value, na.rm=TRUE)), by=.(year, quarter, dps, health_zone, data_set, completeness)]
patients_treated[, indicator := "totalPatientsTreated"]
children_treated[, indicator := "totalPatientsTreated_under5"]
patients_treated = rbind(patients_treated, children_treated)
patients_treated[, subpopulation := "none"]

dt_final <- rbindlist(list(acts_rec, patients_treated, dt), use.names = TRUE)

# remove variables we don't need for impact model
dt_final <- dt_final[!indicator %in% c("simpleConfMalariaTreated", "simpleConfMalariaTreated_under5", "ASAQreceived", "ALreceived", "presumedMalariaTreated", "presumedMalariaTreated_under5")]

# rename pnlp variables to match to impact model
dt_final[ indicator == "LLIN" & subpopulation == "consumed", indicator := "ITN_consumed" ]
dt_final[ indicator == "LLIN" & subpopulation == "received", indicator := "ITN_received" ]
dt_final[ indicator == "RDT" & subpopulation == "completed", indicator := "RDT_completed" ]
dt_final[ indicator == "RDT" & subpopulation == "received", indicator := "RDT_received" ]
dt_final[ indicator == "SSCACT", indicator := "ACTs_SSC" ]
dt_final[ indicator == "SSCACT_under5", indicator := "ACTs_SSC_under5" ]
dt_final <- dt_final[,.(year, quarter, dps, health_zone, indicator, value, completeness)]

dt_final = convert_quarter_to_decimal(dt_final)

# add in SSC data from SNIS
# dt_final = readRDS(outputFile2b)
ssc = readRDS(ssc_file)
dt_final <- rbindlist(list(dt_final, ssc), use.names=TRUE, fill = TRUE)
  
saveRDS(dt_final, outputFile2b)
archive(outputFile2b)
# ---------------------------------------------------

# ---------------------------------------------------
# switch data to wide format
# ---------------------------------------------------
# dt <- readRDS(outputFile2b)
# dt_final_wide <- dcast.data.table(dt_final, date + dps + health_zone ~ indicator, value.var = c("value"))
dt_final_wide <- dcast.data.table(dt_final, date + dps + health_zone ~ indicator, value.var = c("value", "completeness"))

saveRDS(dt_final_wide, outputFile2b_wide)
archive(outputFile2b_wide)
# ---------------------------------------------------
