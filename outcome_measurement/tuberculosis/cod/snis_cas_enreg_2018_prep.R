# ----------------------------------------------
# Audrey Batzel
#
# 4/24/19
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(raster)
library(rgeos)
library(ggplot2)
library(maptools)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
snis_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')

pati_registered = paste0(snis_dir, "pati_tb/tb_pati_cases_registered.rds")
outData = 

# functions
source('./core/standardizeDPSNames.R')
source('./core/standardizeHZNames.R')
# ----------------------------------------------

# ----------------------------------------------
# Read in data
# ----------------------------------------------
snis_reg <- readRDS(pati_registered)
# ----------------------------------------------

# ----------------------------------------------
# further prep
# ----------------------------------------------
snis_reg = snis_reg[date >= "2018-01-01"] 
snis_reg = snis_reg[date <= "2018-07-01"]  # temporary until we download new data
snis_reg[, element := as.character(element)]
snis_reg[, health_zone := standardizeHZNames(health_zone)]
snis_reg[, dps := standardizeDPSNames(dps)]

snis_reg = snis_reg[, .(data_set, date, quarter, dps, health_zone, health_area, org_unit, org_unit_id, level, element, element_eng, element_id, value)]

snis_reg[ element == "TB-Patients (nouveaux et rechutes) avec résultat de test VIH connu", element_eng := "TB_patients_known_HIV_test_result"]
snis_reg[ element == "TB-Patients (nouveaux et rechutes) séropositifs pour le VIH", element_eng := "TB_patients_positive_HIV"]
snis_reg[ element == "TB-Patients séropositifs pour le VIH sous traitement antirétroviral (TAR)", element_eng := "TB_patients_positive_HIV_on_ART"]
snis_reg[ element == "TB-Rechute Cas de tuberculose pulmonaire, diagnostiqués cliniquement", element_eng := "tpc_relapse"]
snis_reg[ element == "TB-Rechute Cas de tuberculose pulmonaire, confirmés bactériologiquement", element_eng := "tpp_relapse"]
snis_reg[ element == "TB-Rechute Cas de tuberculose extrapulmonaire, confirmés bactériologiquement ou diagnostiqués cliniquement", element_eng := "tep_relapse"]
snis_reg[ element == "TB-Nouveau Cas de tuberculose pulmonaire, diagnostiqués cliniquement", element_eng := "tpc_new"]
snis_reg[ element == "TB-Nouveau Cas de tuberculose pulmonaire, confirmés bactériologiquement", element_eng := "tpp_new"]
snis_reg[ element == "TB-Nouveau Cas de tuberculose extrapulmonaire, confirmés bactériologiquement ou diagnostiqués cliniquement", element_eng := "tep_new"]

saveRDS(snis_reg, paste0(snis_dir, "pati_tb/snis_tb_cas_enreg_2018.rds"))
write.csv(snis_reg, paste0(snis_dir, "pati_tb/snis_tb_cas_enreg_2018.csv"))
