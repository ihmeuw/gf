# Audrey Batzel 
# 8-16-18
#
# Compare DHIS2 SNIS data with PNLP data
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(readxl)
library(stats)
library(Rcpp)
library(Amelia)
# --------------------  


# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# source in variable names
# variable_names <-"./outcome_measurement/malaria/cod/variable_names.R"
# source(variable_names)

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/')
dir_pnlp = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
dir_dhis = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/prepped_data/')

# input file:
input_pnlp <- "final_data_for_imputation.csv"
input_dhis_base <- "base.rds"
input_dhis_sigl <- "sigl.rds"

standardized_hzs <- "standardized_hzs.csv"
# ----------------------------------------------


# ----------------------------------------------
# Load data and hz file
# ----------------------------------------------
pnlp <- read.csv(paste0(dir_pnlp, input_pnlp))
pnlp <- as.data.table(pnlp)

dhis_base <- readRDS(paste0(dir_dhis, input_dhis_base))
# dhis_sigl <- readRDS(paste0(dir_dhis, input_dhis_sigl))

hzs <- read.csv(paste0(dir, standardized_hzs))
hzs <- as.data.table(hzs)
# ----------------------------------------------


# ----------------------------------------------
# Standardize pnlp with snis data to merge them together and subset to just 2017
# ----------------------------------------------
# make var for year in pnlp
pnlp$year <- year(pnlp$date)

# subset to just 2017
pnlp <- pnlp[year==2017 & dps != "0",]
pnlp$dps <- gsub(" ", "-", pnlp$dps)
pnlp[ dps== "bas-congo", dps := "kongo-central" ]

# subset to 2017 and to type = malaria for dhis
dhis_base <- dhis_base[year==2017 & type=="malaria" & keep==1,]
  # setnames(dhis_base, "dps", "dps_snis")
  # setnames(dhis_base, "health_zone", "hz_snis")

# aggregate dhis data to health zone level
dhis_base_hz <- dhis_base[, .(value = sum(value)), by=c("dps", "health_zone", "date", "month", "year", "element", "element_eng", "category", "age")]

# subset to the columns we want to use in dhis
dhis_base_subset <- dhis_base[, .(dps, health_zone, health_area, date, month, year, element, element_eng, category, age, value)]

hzs_snis <- hzs[!is.na(hz_snis), ]
  
# merge hzs with dhis_base data
dhis <- merge(dhis_base_subset, hzs, by.x=c("dps", "health_zone"), by.y=c("dps_snis", "hz_snis"), all=TRUE, allow.cartesian = TRUE) # not sure about this??



# clean dhis dps and hz strings
dhis_base$dps <- sapply(str_split(dhis_base$dps, " ", 2), '[', 2)
dhis_base$dps <- gsub(" Province", "", dhis_base$dps)
dhis_base$dps <- tolower(dhis_base$dps)
dhis_base$dps <- gsub(" ", "-", dhis_base$dps)

dhis_base$health_zone <- sapply(str_split(dhis_base$health_zone, " ", 2),'[', 2)
dhis_base$health_zone <- gsub(" Zone de Santé", "", dhis_base$health_zone)
dhis_base$health_zone <- tolower(dhis_base$health_zone)
dhis_base$health_zone <- gsub(" ", "-", dhis_base$health_zone)

dhis_base$health_area <- sapply(str_split(dhis_base$health_area, " ", 2),'[', 2)
dhis_base$health_area <- gsub(" Aire de Santé", "", dhis_base$health_area)
dhis_base$health_area <- tolower(dhis_base$health_area)
dhis_base$health_area <- gsub(" ", "-", dhis_base$health_area)


# # aggregate by dps
# # PNLP
#   # get id vars/sd cols
#   all_vars_pnlp <- colnames(pnlp)
#   id_vars_pnlp <- colnames(pnlp)[1:6]
#   id_vars_pnlp <- c(id_vars_pnlp, "year")
#   sdcols_pnlp <- all_vars_pnlp[!all_vars_pnlp %in% id_vars_pnlp]
#   # sum to dps level
#   pnlp_2017_dps <- pnlp_2017[, lapply(.SD, sum), .SDcols=sdcols_pnlp, by=c("year", "date", "province", "dps")]
# # DHIS BASE
#   dhis_base_2017_dps <- dhis_base_2017[, .(dpsValue = sum(value)), by=c("date", "year", "month", "dps", "element_eng")]
# # DHIS SIGL
#   # dhis_sigl_2017_dps <- dhis_sigl_2017[, .(dpsValue = sum(value)), by=c("date", "year", "month", "dps", "element_eng")]

# change relevant var names in dhis data
# NOTE: just realized that dhis data has malaria cases split by <5 and >5 too.. not sure if the split is the same for where age 5 is included
  dhis_base_subset[element_eng== "A 2.1 LLINs distributed has ANC2 +", element_eng := "ITN_distAtANC2"]
  dhis_base_subset[element_eng== "A 2.1 LLINs distributed to the CPN1", element_eng := "ITN_distAtANC1"]
  dhis_base_subset[element_eng== "A 1.4 RDT positive", element_eng := "RDT_positive"]
  dhis_base_subset[element_eng== "A 1.4 RDT performed", element_eng := "RDT_completed"]
  dhis_base_subset[element_eng== "A 1.4 presumed Malaria", element_eng := "presumed_mal"]
  dhis_base_subset[element_eng== "A 1.4 Suspected case", element_eng := "suspected_mal"]
  dhis_base_subset[element_eng== "A 2.1 Sulfadox. + Pyrimét first dose", element_eng := "SP_1st"]
  dhis_base_subset[element_eng== "A 2.1 Sulfadox. + Pyrimét 2nd dose", element_eng := "SP_2nd"]
  dhis_base_subset[element_eng== "A 2.1 Sulfadox. + Pyrimét 3rd dose", element_eng := "SP_3rd"]
  dhis_base_subset[element_eng== "A 1.5 Severe malaria FE", element_eng := "newCasesMalariaSevere_pregnantWomen"]
  dhis_base_subset[element_eng== "A 1.5 Severe malaria FE treated", element_eng := "severeMalariaTreated_pregnantWomen"]
  dhis_base_subset[element_eng== "A 1.5 Confirmed simple malaria - pregnant woman", element_eng := "newCasesMalariaMild_pregnantWomen"]
  dhis_base_subset[element_eng== "A 1.5 Confirmed simple malaria treated - pregnant woman", element_eng := "mildMalariaTreated_pregnantWomen"]
  dhis_base_subset[element_eng== "A 1.4 Severe malaria", element_eng := "severe_mal"]
  dhis_base_subset[element_eng== "A 1.4 Severe malaria treated", element_eng := "severe_mal_treated"]
  dhis_base_subset[element_eng== "A 1.5 Confirmed simple malaria treated", element_eng := "simple_mal_treated"]
  
# sum variables in pnlp for comparison
  pnlp_2017_dps[, presumed_mal := rowSums(.SD, na.rm=TRUE), .SDcols = c("presumedMalaria_5andOlder" , "presumedMalaria_pregnantWomen" , "presumedMalaria_under5")]
  pnlp_2017_dps[, suspected_mal := rowSums(.SD, na.rm=TRUE), .SDcols = c('suspectedMalaria_5andOlder' , 'suspectedMalaria_pregnantWomen' , 'suspectedMalaria_under5')]
  pnlp_2017_dps[, severe_mal := rowSums(.SD, na.rm=TRUE), .SDcols = c('newCasesMalariaSevere_under5' , 'newCasesMalariaSevere_5andOlder')]
  pnlp_2017_dps[, severe_mal_treated := rowSums(.SD, na.rm=TRUE), .SDcols = c('severeMalariaTreated_5andOlder' , 'severeMalariaTreated_under5')]
  pnlp_2017_dps[, simple_mal_treated := rowSums(.SD, na.rm=TRUE), .SDcols = c('mildMalariaTreated_5andOlder' , 'mildMalariaTreated_under5')]

# sum variables in dhis base for comparison
  var_for_comp <- c("RDT_completed", "RDT_positive", "ITN_distAtANC", "ITN_distAtANC1", "ITN_distAtANC2", "presumed_mal", "suspected_mal", "SP_1st", 
                    "SP_2nd", "SP_3rd", "newCasesMalariaSevere_pregnantWomen", "newCasesMalariaMild_pregnantWomen",
                    "mildMalariaTreated_pregnantWomen", "severe_mal", "severe_mal_treated", "simple_mal_treated",
                    "severeMalariaTreated_pregnantWomen")
  
  dhis <- dhis_base_2017_dps[element_eng %in% var_for_comp,]
  dhis[, data_source := "dhis"]
  
  # cast wide in order to add vars together
  dhis_wide <- dcast(dhis, date+year+month+dps+data_source ~ element_eng, value.var = 'dpsValue')
  dhis_wide <- as.data.table(dhis_wide)
  # sum ITN vars
  dhis_wide[, ITN_distAtANC := rowSums(.SD, na.rm=TRUE), .SDcols = c('ITN_distAtANC1' , 'ITN_distAtANC2')]
  # melt back to long
  dhis_long <- melt(dhis_wide, 
                    id.vars= c("date", "year", "month", "dps", "data_source"))
  
  # update vars for comparison - remove ITN_distATANC1 and ITN_distAtANC2 
  var_for_comp <- c("RDT_completed", "RDT_positive", "ITN_distAtANC", "presumed_mal", "suspected_mal", "SP_1st", 
                    "SP_2nd", "SP_3rd", "newCasesMalariaSevere_pregnantWomen", "newCasesMalariaMild_pregnantWomen",
                    "mildMalariaTreated_pregnantWomen", "severe_mal", "severe_mal_treated", "simple_mal_treated",
                    "severeMalariaTreated_pregnantWomen")
  
# melt pnlp to match dhis
  pnlp_long <- melt(pnlp_2017_dps, 
                    id.vars=colnames(pnlp_2017_dps)[colnames(pnlp_2017_dps) %in% id_vars_pnlp])

# create variables to join on
  # vars to compare
  pnlp_long[ variable %in% var_for_comp ]
  pnlp_long[, data_source := "pnlp"]
  
  
