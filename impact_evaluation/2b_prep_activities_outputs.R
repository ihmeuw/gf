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
# ---------------------------------------------------


# ---------------------------------------------------
# Read in data
# ---------------------------------------------------
pnlp_hz <- readRDS(pnlpHZFile) # hz level, monthly - get completeness from this
dt_pnlp <- readRDS(pnlpFile) # national level, monthly
setnames(dt_pnlp, "mean", "value")
dt_base <- readRDS(snisBaseFile) # facility level, monthly
dt_sigl <- readRDS(snisSiglFile) # facility level, monthly
# ---------------------------------------------------

# ---------------------------------------------------
# Change variable names / other set up
# ---------------------------------------------------
# subset to post-2017 data (before that is really incomplete)
dt_base <- dt_base[ year >= 2017, ]
dt_sigl <- dt_sigl[ year >= 2017, ]

dt_base$type <- trimws(dt_base$type)  # for some reason there is both "malaria" and "malaria " in the type unique values
dt_base <- dt_base[ type == "malaria", ]
dt_base$element <- trimws(dt_base$element)
dt_base$element_eng <- trimws(dt_base$element_eng)

dt_base[element== "A 2.1 MILD distribués a la CPN2+", element_eng := "ITN_distAtANC"]
dt_base[element== "A 2.1 MILD distribués a la CPN1", element_eng := "ITN_distAtANC"]
dt_base[element== "A 1.4 TDR positif", element_eng := "RDT_positive"]
dt_base[element== "A 1.4 TDR réalisé", element_eng := "RDT_completed"]
dt_base[element== "A 2.1 Sulfadox. + Pyrimét 1ère dose reçue", element_eng := "SP_1st"]
dt_base[element== "A 2.1 Sulfadox. + Pyrimét 2ème dose reçue", element_eng := "SP_2nd"]
dt_base[element== "A 2.1 Sulfadox. + Pyrimét 3ème dose reçue", element_eng := "SP_3rd"]
dt_base[element== "A 2.1 Sulfadox. + Pyrimét 4ème dose reçue", element_eng := "SP_4th"]
dt_base[element_eng== "A 1.4 Severe malaria treated", element_eng := "severeMalariaTreated"]
dt_base[element_eng== "A 1.5 Severe malaria treated - pregnant woman", element_eng := "severeMalariaTreated_pregnantWomen"]
dt_base[element_eng== "A 1.4 Confirmed simple malaria treated", element_eng := "mildMalariaTreated"]
dt_base[element_eng== "A 1.5 Confirmed simple malaria treated - pregnant woman", element_eng := "mildMalariaTreated_pregnantWomen"]
dt_base[element_eng== "A 1.4 Severe malaria", element_eng := "newCasesMalariaSevere"]
dt_base[element_eng== "A 1.5 Severe malaria - pregnant woman", element_eng := "newCasesMalariaSevere_pregnantWomen"]
dt_base[element_eng== "A 1.4 Confirmed simple malaria", element_eng := "newCasesMalariaMild"]
dt_base[element_eng== "A 1.5 Confirmed simple malaria - pregnant woman", element_eng := "newCasesMalariaMild_pregnantWomen"]
dt_base[element_eng== "A 1.4 Presumed malaria", element_eng := "presumedMalaria"]
dt_base[element_eng== "A 1.4 Presumed malaria treated", element_eng := "presumedMalariaTreated"]
dt_base[element_eng== "A 1.4 Suspected malaria case", element_eng := "suspectedMalaria"]

dt_sigl$type <- trimws(dt_sigl$type)  # for some reason there is both "malaria" and "malaria " in the type unique values
dt_sigl <- dt_sigl[ type == "malaria", ]
dt_sigl$element <- trimws(dt_sigl$element)
dt_sigl$element_eng <- trimws(dt_sigl$element_eng)

dt_sigl[element== "C1 12.1 Artesunate-Amodiaquine (+14 ans, 6 cés) 100mg+270mg Comprimé - quantité consommée", 
              element_eng := "ASAQconsumed_14yrsAndOlder"]
dt_sigl[element== "C2 12.2 Artesunate 60mg Injectable - Quantité consommée", 
              element_eng := "ASAQconsumed_inj"]
dt_sigl[element== "C1 12.1 Artesunate 400mg Suppositoire - quantité consommée", 
              element_eng := "ASAQconsumed_supp400"]
dt_sigl[element== "C1 12.1 Artesunate-Amodiaquine (6-13 ans, 3 cés) 100mg+270mg Comprimé - quantité consommée", 
              element_eng := "ASAQconsumed_6to13yrs"]
dt_sigl[element== "C1 12.1 Artesunate-Amodiaquine (2-11 mois) 25mg+67,5mg Comprimé - quantité consommée", 
              element_eng := "ASAQconsumed_2to11mos"]
dt_sigl[element== "C1 12.1 Artesunate-Amodiaquine (12-59 mois) 50mg+135mg Comprimé - quantité consommée", 
              element_eng := "ASAQconsumed_1to5yrs"]
dt_sigl[element== "C1 12.1 Lumefantrine+ Artemether 40mg+240mg Comprimé - quantité consommée", 
              element_eng := "ArtLumConsumed_240+40"]
dt_sigl[element== "C1 12.1 Lumefantrine+ Artemether 80mg+480mg Comprimé - quantité consommée", 
              element_eng := "ArtLumConsumed_480+80"]
dt_sigl[element== "C1 12.1 Sulfadoxine + Pyriméthamine 500mg+25mg Cés - quantité consommée", 
              element_eng := "SP_consumed"]
dt_sigl[element== "C1 12.1 Artesunate 200mg Suppositoire - quantité consommée", 
              element_eng := "ASAQconsumed_supp200"]
dt_sigl[element== "C1 12.1 MIILD - pièce - quantité consommée", 
              element_eng := "ITN_consumed"]

dt_pnlp <- convert_date_to_quarter(dt_pnlp)
dt_base <- convert_date_to_quarter(dt_base)
dt_sigl <- convert_date_to_quarter(dt_sigl)

dt_base[, c("indicator", "subpopulation") := tstrsplit(element_eng, "_", fixed=TRUE)]
dt_base[is.na(subpopulation), subpopulation:="none"]
dt_sigl[, c("indicator", "subpopulation") := tstrsplit(element_eng, "_", fixed=TRUE)]
dt_sigl[is.na(subpopulation), subpopulation:="none"]
# ---------------------------------------------------

# ---------------------------------------------------
# Completeness measure from SNIS --> USE comp_base_totals and comp_sigl
# ---------------------------------------------------
# FOR BASE DATA
dt_comp_base <- dt_base[, .(num_fac_per_indicator_ever = length(unique(org_unit))), by=c("element_eng", "indicator", "subpopulation", "category")]
dt_comp_base2 <- dt_base[, .(num_fac_per_indicator_month = length(unique(org_unit))), by=c("element_eng", "indicator", "subpopulation", "category", "date", "year", "quarter")]
# dt_comp_base3 <- dt_base[, .(num_fac_per_indicator_year = length(unique(org_unit))), by=c("element_eng", "category", "year")]
comp_base <- merge(dt_comp_base, dt_comp_base2, by=c("element_eng", "indicator", "subpopulation", "category"), all = TRUE)
# comp_base <- merge(comp_base, dt_comp_base3, by=c("element_eng", "category", "year"), all = TRUE)
# comp_base[, indicator_month_comp := num_fac_per_indicator_month / num_fac_per_indicator_ever]

# weighted average over category and subpopulations by quarter year and indicator
# first - need to remove RDT_positive because we just want to use RDT_completed in the model for now. Other indicators can be averaged across subpopulations
comp_base_totals <- comp_base[subpopulation != "positive"]
comp_base_totals <- comp_base_totals[, .(num = sum(num_fac_per_indicator_month),
                                  denom = sum(num_fac_per_indicator_ever)),
                              by=c("indicator", "year", "quarter")]
comp_base_totals[, completeness:= num/denom]
  # UNTIL WE GET NEW DATA - remove 2018 Q3 due to very low completeness
    dt_base <- dt_base[date <= "2018-06-01"]
    
# separate calculation for totalPatientsTreated
comp_base_patientsTreated <- comp_base[indicator %in% c("mildMalariaTreated", "severeMalariaTreated", "presumedMalariaTreated")]
comp_base_patientsTreated <- comp_base_patientsTreated[, .(num = sum(num_fac_per_indicator_month),
                                                           denom = sum(num_fac_per_indicator_ever)),
                                                       by=c("year", "quarter")]
comp_base_patientsTreated[, completeness:= num/denom]
comp_base_patientsTreated[, indicator := "totalPatientsTreated"]

# attach totalPatientsTreated values for completeness back to full data set of completeness measures
comp_base_totals <- rbindlist(list(comp_base_totals, comp_base_patientsTreated), use.names= TRUE)

# FOR SIGL DATA
dt_comp_sigl <- dt_sigl[, .(num_fac_per_indicator_ever = length(unique(org_unit))), by=c("element_eng", "indicator", "subpopulation", "category")]
dt_comp_sigl2 <- dt_sigl[, .(num_fac_per_indicator_month = length(unique(org_unit))), by=c("element_eng", "indicator", "subpopulation", "category", "date")]
comp_sigl <- merge(dt_comp_sigl, dt_comp_sigl2, by=c("element_eng", "indicator", "subpopulation", "category"), all = TRUE)
comp_sigl[, indicator_month_comp := num_fac_per_indicator_month / num_fac_per_indicator_ever]
comp_sigl <- convert_date_to_quarter(comp_sigl)
# ---------------------------------------------------

# ---------------------------------------------------
# Completeness from PNLP --> USE pnlp_fac_natl_qtr
# (get/agg from hz level) 
# (not indicator-specific unfortunately - just date specific at natl level)
# ---------------------------------------------------
pnlp_hz$variable <- as.character(pnlp_hz$variable)

pnlp_fac <- pnlp_hz[variable %in% c("healthFacilities_numReportedWithinDeadline", "healthFacilities_total", "healthFacilitiesProduct"), .(dps, health_zone, date, variable, mean)]
setnames(pnlp_fac, "mean", "value")

pnlp_fac <- dcast.data.table(pnlp_fac, dps + health_zone + date ~ variable)
pnlp_fac[, healthFacilities_reporting := healthFacilitiesProduct / healthFacilities_total]
pnlp_fac[, healthFacilities_proportionReporting := healthFacilities_reporting / healthFacilities_total]

pnlp_fac[healthFacilities_proportionReporting > 1, healthFacilities_reporting := healthFacilities_total] # this will make the proportions = 1, but we want 
# to sum and then divide (I think?) rather than average 
# so the proportions are weighted by total # of facilities
pnlp_fac_natl <- pnlp_fac[, .(healthFacilities_reporting = sum(healthFacilities_reporting),
                              healthFacilities_total = sum(healthFacilities_total)),
                          by = "date"]

pnlp_fac_natl <- convert_date_to_quarter(pnlp_fac_natl)
pnlp_fac_natl_qtr <- pnlp_fac_natl[, .(healthFacilities_reporting = sum(healthFacilities_reporting),
                                       healthFacilities_total = sum(healthFacilities_total)),
                                   by = c("quarter", "year")]
pnlp_fac_natl_qtr[ , prop_fac_reporting:= healthFacilities_reporting / healthFacilities_total]
# ---------------------------------------------------

# ---------------------------------------------------
# Aggregate to national data
# ---------------------------------------------------
base_natl <- dt_base[, .(value = sum(value, na.rm=TRUE)), by=c("date", "year", "type", "element", "element_eng", "category", "quarter", "indicator", "subpopulation")]
sigl_natl <- dt_sigl[, .(value = sum(value, na.rm=TRUE)), by=c("date", "year", "type", "element", "element_eng", "category", "quarter", "indicator", "subpopulation")]
# ---------------------------------------------------

# ---------------------------------------------------
# Aggregate to quarterly data / can sum over category in SNIS
# ---------------------------------------------------
dt_pnlp[ is.na(subpopulation), subpopulation:= "none"]
pnlp_natl_qtr <-  dt_pnlp[, .(value = sum(value, na.rm=TRUE)), by=c("year", "quarter", "indicator", "subpopulation")]
pnlp_natl_qtr$data_source = "PNLP"

base_natl_qtr <-  base_natl[, .(value = sum(value, na.rm=TRUE)), by=c("quarter", "year", "type", "element", "element_eng", "indicator", "subpopulation")]
base_natl_qtr$data_source = "SNIS_base_services"

sigl_natl_qtr <-  sigl_natl[, .(value = sum(value, na.rm=TRUE)), by=c("quarter", "year", "type", "element", "element_eng", "indicator", "subpopulation")]
sigl_natl_qtr$data_source = "SNIS_sigl"
# ---------------------------------------------------

# ---------------------------------------------------
# Subset to years where data wasn't fully imputed and just the indicators we want for activities/outputs
# ---------------------------------------------------
# remove vars from years where they were missing in the original data (they are in here because they were imputed when the data was rectangularized)
pnlp_natl_qtr[ indicator == "ArtLum" & year < 2015, value := NA]
  # keep SSCACT with subpop = NA for 2015 and 2016, keep with subpops <5 and >5 for 2017
pnlp_natl_qtr[ indicator == "SSCACT" & subpopulation =="none" & year < 2015, value := NA]
pnlp_natl_qtr[ indicator == "SSCACT" & subpopulation =="none" & year > 2016, value := NA]
pnlp_natl_qtr[ indicator == "SSCACT" & subpopulation !="none" & year < 2017, value := NA]
# keep SSCACT with subpop = NA for 2015 and 2016, keep with subpops <5 and >5 for 2017
pnlp_natl_qtr <- pnlp_natl_qtr[!is.na(value)]

pnlp_natl_qtr <- pnlp_natl_qtr[ indicator %in% c("ASAQreceived", "ITN", "ArtLum", "RDT", "SSCACT", "SP", "mildMalariaTreated", "severeMalariaTreated") & subpopulation != "used" & subpopulation !="positive", ]
base_natl_qtr <- base_natl_qtr[ indicator %in% c("RDT", "SP", "mildMalariaTreated", "severeMalariaTreated", "presumedMalariaTreated") & subpopulation != "positive", ]
sigl_natl_qtr <- sigl_natl_qtr[ indicator %in% c("ITN"), ]
# ---------------------------------------------------

# ---------------------------------------------------
# Sum together variables to create the ones needed for pilot
# ---------------------------------------------------
# PNLP DATA
# because we have some that need to be summed that have different subpopulations and some that shouldn't be summed with different subpopulations
# I'm doing a slightly hacky thing here-by changing the name of the subpopulation to all be the same on the ones that should be summed, we can then
# sum by indicator and subpopulation to preserve the ones (like RDT completed vs RDT received) that shouldn't just be summed on indicator
pnlp_natl_qtr[ indicator == "ITN" & subpopulation != "received", subpopulation := "consumed"]
pnlp_natl_qtr[ indicator == "ASAQreceived", subpopulation := "none"]
pnlp_natl_qtr[ indicator == "SP", subpopulation := "none"]
pnlp_natl_qtr[ indicator == "SSCACT", subpopulation := "none"]
pnlp_natl_qtr[ indicator == "severeMalariaTreated", subpopulation := "none"]
pnlp_natl_qtr[ indicator == "mildMalariaTreated", subpopulation := "none"]

pnlp_natl_qtr <-  pnlp_natl_qtr[, .(value = sum(value, na.rm=TRUE)), by=c("year", "quarter", "indicator", "subpopulation")]

# we need to create variable for totalPatientsTreated and a variable for ACT_received, will do this separately then merge back on
acts_rec <- pnlp_natl_qtr[ indicator %in% c("ASAQreceived", "ArtLum")]
acts_rec <- acts_rec[, .(value = sum(value, na.rm=TRUE)), by=c("year", "quarter")]
acts_rec[, indicator := "ACT_received"]
acts_rec[, subpopulation := "none"]

patients_treated <- pnlp_natl_qtr[ indicator %in% c("mildMalariaTreated", "severeMalariaTreated")]
patients_treated <- patients_treated[, .(value = sum(value, na.rm=TRUE)), by=c("year", "quarter")]
patients_treated[, indicator := "totalPatientsTreated"]
patients_treated[, subpopulation := "none"]

pnlp_final <- rbindlist(list(acts_rec, patients_treated, pnlp_natl_qtr), use.names = TRUE)

# remove variables we don't need for pilot dataset
pnlp_final <- pnlp_final[!indicator %in% c("mildMalariaTreated", "ASAQreceived", "ArtLum")]
# rename pnlp variables to match to pilot dataset
pnlp_final[ indicator == "ITN" & subpopulation == "consumed", indicator := "ITN_consumed" ]
pnlp_final[ indicator == "ITN" & subpopulation == "received", indicator := "ITN_received" ]
pnlp_final[ indicator == "RDT" & subpopulation == "completed", indicator := "RDT_completed" ]
pnlp_final[ indicator == "RDT" & subpopulation == "received", indicator := "RDT_received" ]
pnlp_final[ indicator == "SSCACT", indicator := "ACTs_CHWs" ]
pnlp_final <- pnlp_final[,.(year, quarter, indicator, value)]

# BASE DATA
# sum variables in base by indicator
base_natl_qtr <-  base_natl_qtr[, .(value = sum(value, na.rm=TRUE)), by=c("year", "quarter", "indicator")]
# rename base variables to match to pilot dataset
base_natl_qtr[ indicator == "RDT", indicator := "RDT_completed" ]
# create variable for totalPatientsTreated
patients_treated2 <- base_natl_qtr[ indicator %in% c("mildMalariaTreated", "severeMalariaTreated", "presumedMalariaTreated")]
patients_treated2 <- patients_treated2[, .(value = sum(value, na.rm=TRUE)), by=c("year", "quarter")]
patients_treated2[, indicator := "totalPatientsTreated"]

base_final <- rbindlist(list(base_natl_qtr, patients_treated2), use.names = TRUE)
base_final <- base_final[!indicator %in% c("mildMalariaTreated", "presumedMalariaTreated")]

# SIGL DATA
sigl_natl_qtr[ indicator == "ITN", indicator := "ITN_consumed" ]
sigl_final <- sigl_natl_qtr[, .(year, quarter, indicator, value)]
# ---------------------------------------------------

# ---------------------------------------------------
# Put it all together!
# ---------------------------------------------------
# final clean up of completeness data
pnlp_comp <- pnlp_fac_natl_qtr[, .(year, quarter, prop_fac_reporting)]
setnames(pnlp_comp, "prop_fac_reporting", "completeness")

sigl_comp <- comp_sigl[ indicator == "ITN", ]
sigl_comp <- sigl_comp[, .(completeness = mean(indicator_month_comp)), by = c("year", "quarter", "indicator") ]  # can do a simple avg here since denom is all the same
sigl_comp <- sigl_comp[ indicator == "ITN", indicator := "ITN_consumed"]

base_indicators <- unique(base_final$indicator)
comp_base_totals <- comp_base_totals[ indicator == "RDT", indicator := "RDT_completed"]
base_comp <- comp_base_totals[indicator %in% base_indicators,]
base_comp <- base_comp[, .(year, quarter, indicator, completeness)]

# merge completeness measures with data sets
pnlp_final <- merge(pnlp_final, pnlp_comp, by = c("year", "quarter"))
pnlp_final[, data_source := "PNLP"]
base_final <- merge(base_final, base_comp, by = c("year", "quarter", "indicator"))
base_final[, data_source := "SNIS_base"]
sigl_final <- merge(sigl_final, sigl_comp, by = c("year", "quarter", "indicator"))
sigl_final[, data_source := "SNIS_sigl"]
    
# bind all data sets together into final data set for pilot
pilot_dataset <- rbindlist(list(pnlp_final, base_final, sigl_final), use.names = TRUE)

# clean up dates, so we have only SNIS after 2017 (remove 2017 pnlp where there is SNIS data) 
# and make sure we don't have SNIS after 2018 Q2 (at least now, 1/16/19 while it's not complete)
remove_rows <- pilot_dataset[indicator %in% c("SP", "severeMalariaTreated", "RDT_completed", "totalPatientsTreated", "ITN_consumed") & data_source == "PNLP" & year == 2017, ]
pilot_dataset <- pilot_dataset[!remove_rows, on= colnames(pilot_dataset)]

# save dataset
saveRDS(pilot_dataset, outputFile2b)
# ---------------------------------------------------
