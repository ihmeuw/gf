# Audrey Batzel 
# 1-30-19
#
# Combine PNLP and base data
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
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_pnlp = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
dir_dhis = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')
out_dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/')

# input file:
input_dhis_base <- "base_services_prepped.rds"
input_dhis_sigl <- "sigl_prepped.rds"
after_imputation_pnlp <- "post_imputation/imputedData_run2_agg_hz.rds"

# output file:
combined_data <- "base_pnlp_sigl_combined_data_hz_level.rds"
combined_data_malaria <- "snis_pnlp_malaria_hz_level.rds"

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ----------------------------------------------

# ----------------------------------------------
# Load data 
# ----------------------------------------------
pnlp <- readRDS(paste0(dir_pnlp, after_imputation_pnlp)) 
base <- readRDS(paste0(dir_dhis, input_dhis_base))
sigl <- readRDS(paste0(dir_dhis, input_dhis_sigl))
# ----------------------------------------------

# ----------------------------------------------
# Clean data
# ----------------------------------------------
# base / sigl - clean column names
    # standardize dps/hz names
    pnlp$health_zone <- standardizeHZNames(pnlp$health_zone)
    base$health_zone <- standardizeHZNames(base$health_zone)
    sigl$health_zone <- standardizeHZNames(sigl$health_zone)
    pnlp$dps <- standardizeDPSNames(pnlp$dps)
    base$dps <- standardizeDPSNames(base$dps)
    sigl$dps <- standardizeDPSNames(sigl$dps)
    
    # trim ws to make sure all of these variables are uniform/match up with what they should
    base$type <- trimws(base$type)  # for some reason there is both "malaria" and "malaria " in the type unique values
    base$element <- trimws(base$element)
    base$element_eng <- trimws(base$element_eng)
    sigl$element <- trimws(sigl$element)
    sigl$element_eng <- trimws(sigl$element_eng)
    
    # drop columns we don't need
    drop_cols <- c("coordinates", "country", "mtk", "org_unit_id", "element_id")
    base <- base[ , !(drop_cols), with = FALSE]    
    sigl <- sigl[ , !(drop_cols), with = FALSE]
    
    setnames(base, "element_eng", "indicator")
    setnames(base, "category", "subpopulation")
    setnames(sigl, "element_eng", "indicator")
    setnames(sigl, "category", "subpopulation")

# base - standardize element/indicator names
  base[element== "A 2.1 MILD distribués a la CPN2+",  c("indicator", "subpopulation") := list("LLIN", "distAtANC1")]
  base[element== "A 2.1 MILD distribués a la CPN1", c("indicator", "subpopulation") := list("LLIN", "distAtANC2")]
  base[element== "A 1.4 TDR positif", c("indicator", "subpopulation") := list("RDT", "positive")]
  base[element== "A 1.4 TDR réalisé", c("indicator", "subpopulation") := list("RDT", "completed")]
  base[element== "A 2.1 Sulfadox. + Pyrimét 1ère dose reçue", c("indicator", "subpopulation") := list("SP", "1st")]
  base[element== "A 2.1 Sulfadox. + Pyrimét 2ème dose reçue", c("indicator", "subpopulation") := list("SP", "2nd")]
  base[element== "A 2.1 Sulfadox. + Pyrimét 3ème dose reçue", c("indicator", "subpopulation") := list("SP", "3rd")]
  base[element== "A 2.1 Sulfadox. + Pyrimét 4ème dose reçue", c("indicator", "subpopulation") := list("SP", "4th")]

  base[element== "A 1.4 Paludisme grave traité" & subpopulation == "<5 ans", c("indicator", "subpopulation") := list("severeMalariaTreated", "under5")]
  base[element== "A 1.4 Paludisme grave traité" & subpopulation == ">5 ans", c("indicator", "subpopulation") := list("severeMalariaTreated", "5andOlder")]
  base[element== "A 1.5 Paludisme grave traité FE", c("indicator", "subpopulation") := list("severeMalariaTreated", "pregnantWomen") ]
  
  base[element== "A 1.4 Paludisme grave" & subpopulation == "<5 ans", c("indicator", "subpopulation") := list("newCasesMalariaSevere", "under5")]
  base[element== "A 1.4 Paludisme grave" & subpopulation == ">5 ans", c("indicator", "subpopulation") := list("newCasesMalariaSevere", "5andOlder")]
  base[element== "A 1.5 Paludisme grave FE", c("indicator", "subpopulation") := list("newCasesMalariaSevere", "pregnantWomen") ]
  
  base[element== "A 1.4 Paludisme simple confirmé traité [PN]" & subpopulation == "<5 ans", c("indicator", "subpopulation") := list("simpleConfMalariaTreated", "under5")]
  base[element== "A 1.4 Paludisme simple confirmé traité [PN]" & subpopulation == ">5 ans", c("indicator", "subpopulation") := list("simpleConfMalariaTreated", "5andOlder")]
  base[element== "A 1.5 Paludisme simple confirmé traité selon PN-FE", c("indicator", "subpopulation") := list("simpleConfMalariaTreated", "pregnantWomen") ]
  
  base[element== "A 1.4 Paludisme simple confirmé" & subpopulation == "<5 ans", c("indicator", "subpopulation") := list("newCasesMalariaSimpleConf", "under5")]
  base[element== "A 1.4 Paludisme simple confirmé" & subpopulation == ">5 ans", c("indicator", "subpopulation") := list("newCasesMalariaSimpleConf", "5andOlder")]
  base[element== "A 1.5 Paludisme simple confirmé FE", c("indicator", "subpopulation") := list("newCasesMalariaSimpleConf", "pregnantWomen") ]
  
  base[element== "A 1.4 Paludisme présumé traité" & subpopulation == "<5 ans", c("indicator", "subpopulation") := list("presumedMalariaTreated", "under5")]
  base[element== "A 1.4 Paludisme présumé traité" & subpopulation == ">5 ans", c("indicator", "subpopulation") := list("presumedMalariaTreated", "5andOlder")]
  base[element== "A 1.4 Paludisme présumé" & subpopulation == "<5 ans", c("indicator", "subpopulation") := list("presumedMalaria", "under5")]
  base[element== "A 1.4 Paludisme présumé" & subpopulation == ">5 ans", c("indicator", "subpopulation") := list("presumedMalaria", "5andOlder")]
  
  base[element== "A 1.4 Confirmed simple malaria", indicator := "newCasesMalariaMild"]
  base[element== "A 1.4 Confirmed simple malaria treated", indicator := "mildMalariaTreated"]
  base[element== "A 1.5 Confirmed simple malaria - pregnant woman", c("indicator", "subpopulation") := list("newCasesMalariaMild", "pregnantWomen") ]
  base[element== "A 1.5 Confirmed simple malaria treated - pregnant woman", c("indicator", "subpopulation") := list("mildMalariaTreated", "pregnantWomen") ]

# sigl - standardize element/indicator names
sigl[element== "C1 12.1 Artesunate-Amodiaquine (+14 ans, 6 cés) 100mg+270mg Comprimé - quantité consommée", 
               c("indicator", "subpopulation") := list("ASAQconsumed", "14yrsAndOlder") ]
sigl[element== "C2 12.2 Artesunate 60mg Injectable - Quantité consommée", 
               c("indicator", "subpopulation") := list("ASAQconsumed", "inj")]
sigl[element== "C1 12.1 Artesunate 400mg Suppositoire - quantité consommée", 
               c("indicator", "subpopulation") := list("ASAQconsumed", "supp400mg")]
sigl[element== "C1 12.1 Artesunate-Amodiaquine (6-13 ans, 3 cés) 100mg+270mg Comprimé - quantité consommée", 
               c("indicator", "subpopulation") := list("ASAQconsumed", "6to13yrs")]
sigl[element== "C1 12.1 Artesunate-Amodiaquine (2-11 mois) 25mg+67,5mg Comprimé - quantité consommée", 
               c("indicator", "subpopulation") := list("ASAQconsumed", "2to11mos")]
sigl[element== "C1 12.1 Artesunate-Amodiaquine (12-59 mois) 50mg+135mg Comprimé - quantité consommée", 
               c("indicator", "subpopulation") := list("ASAQconsumed", "1to5yrs")]
sigl[element== "C1 12.1 Lumefantrine+ Artemether 40mg+240mg Comprimé - quantité consommée", 
               c("indicator", "subpopulation") := list("ArtLum", "consumed(240+40)")]
sigl[element== "C1 12.1 Lumefantrine+ Artemether 80mg+480mg Comprimé - quantité consommée", 
               c("indicator", "subpopulation") := list("ArtLum", "consumed(480+80)")]
sigl[element== "C1 12.1 Sulfadoxine + Pyriméthamine 500mg+25mg Cés - quantité consommée", 
               c("indicator", "subpopulation") := list("SP", "consumed")]
sigl[element== "C1 12.1 Artesunate 200mg Suppositoire - quantité consommée", 
               c("indicator", "subpopulation") := list("ASAQconsumed", "supp200mg")]
sigl[element== "C1 12.1 MIILD - pièce - quantité consommée", 
               c("indicator", "subpopulation") := list("LLIN", "consumed")]

# pnlp - standardize indicator names
pnlp[indicator=="ITN", indicator:= "LLIN"]
pnlp[indicator=="newCasesMalariaMild", indicator:= "newCasesMalariaSimpleConf"]
pnlp[indicator=="mildMalariaTreated", indicator:= "simpleConfMalariaTreated"]
setnames(pnlp, "mean", "value")
drop_cols_pnlp <- c("upper", "lower", "variable", "province")
pnlp <- pnlp[ , !(drop_cols_pnlp), with = FALSE] 
pnlp$type = "malaria"

# all - add data set names
base[, data_set := "dhis_base_services"]
pnlp[, data_set := "pnlp"]
sigl[, data_set := "dhis_sigl"]

dt <- rbindlist(list(pnlp, base, sigl), use.names = TRUE, fill = TRUE)
saveRDS(dt, paste0(out_dir, combined_data))
# ----------------------------------------------


# ----------------------------------------------
# Subset/aggregate to the data we want to use
# ----------------------------------------------
# in this case, just malaria and 2017 data (overlap between PNLP and base services)
mal_inds <- unique(pnlp$indicator)
mal_inds <- c(mal_inds, "ASAQconsumed", "presumedMalariaTreated")
dt_mal <- dt[indicator %in% mal_inds,]
dt_mal$year <- year(dt_mal$date)
dt_mal <- dt_mal[ year >= 2017, ]
# at health zone level
dt_mal <- dt_mal[, .(value= sum(value, na.rm=TRUE)), by=c("data_set", "date", "year", "dps", "health_zone", "indicator", "subpopulation")]  
saveRDS(dt_mal, paste0(out_dir, combined_data_malaria))
# ----------------------------------------------




