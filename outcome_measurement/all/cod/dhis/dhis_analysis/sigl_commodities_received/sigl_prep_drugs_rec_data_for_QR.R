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
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# input files
data = paste0(dir, "prepped/sigl/sigl_prepped.rds")
catalogue =  paste0(dir, "catalogues/data_elements_cod.csv")
imputed_data = paste0(data_path, 'sigl_quantreg_imputation_results.rds')

# output files 
prepped_drug_data = paste0(dir, "prepped/sigl/drugs_consumed_lost_available_update_6_16_19.rds")
prepped_for_qr = paste0(dir, "prepped/sigl/sigl_for_qr.rds")

# functions
source('./core/standardizeHZNames.R')
-------------------------

# ---------------------------------------------------
# DATA PREP
# ---------------------------------------------------
  # Load data
  dt = readRDS(data)
  catalogue = read.csv(catalogue)

  # subset DHIS data to just the data on drugs that we want to assess by first grabbing those variables from the catalogue
  # and then subsetting to just those in dt
  drugs_consumed <- catalogue[grepl("consumed", catalogue$element_eng) & catalogue$keep == 1,]
  drugs_avail <- catalogue[grepl("available", catalogue$element_eng) & catalogue$keep == 1,]
  drugs_lost <- catalogue[grepl("lost", catalogue$element_eng) & catalogue$keep == 1,]

  vars_to_keep <- rbind(drugs_consumed, drugs_avail, drugs_lost)
  vars_to_keep$element_id <- as.character(vars_to_keep$element_id)
  vars_to_keep <- vars_to_keep$element_id

  dt$element_id <- as.character(dt$element_id)
  dt <- dt[element_id %in% vars_to_keep, ]
  dt$element <- as.character(dt$element)

  dt[grep("perdue", dt$element), variable:= "lost"]
  dt[grep("consommée", dt$element), variable:= "consumed"]
  dt[grep("utilisable", dt$element), variable:= "available"]

  dt[grep("RHE", element, fixed=TRUE), drug:= "RHE"]
  dt[grep("RHZ", element, fixed=TRUE), drug:= "RHZ"]
  dt[grep("RHZE", element, fixed=TRUE), drug:= "RHZE"]
  dt[grep("(RH) 150mg+75mg", element, fixed=TRUE), drug:= "RH_150mg+75mg"]
  dt[grep("(RH) 60mg+30mg", element, fixed=TRUE), drug:= "RH_60mg+30mg"]
  dt[grep("Amodiaquine (6-13 ans", element, fixed=TRUE), drug:= "ASAQ_6to13yrs"]
  dt[grep("Amodiaquine (+14 ans", element, fixed=TRUE), drug:= "ASAQ_14yrsAndOlder"]
  dt[grep("Amodiaquine (2-11 mois)", element, fixed=TRUE), drug:= "ASAQ_2to11mos"]
  dt[grep("Amodiaquine (12-59 mois)", element, fixed=TRUE), drug:= "ASAQ_12to59mos"]
  dt[grep("Sulfadoxine", element, fixed=TRUE), drug:= "SP"]
  dt[grep("Artemether 80mg+480mg", element, fixed=TRUE), drug:= "AL_80mg+480mg"]
  dt[grep("Artemether 40mg+240mg", element, fixed=TRUE), drug:= "AL_40mg+240mg"]
  dt[grep("Artesunate 200mg Suppositoire", element, fixed=TRUE), drug:= "ASAQ_supp_200mg"]
  dt[grep("Artesunate 400mg Suppositoire", element, fixed=TRUE), drug:= "ASAQ_supp_400mg"]
  dt[grep("Artesunate 60mg Injectable", element, fixed=TRUE), drug:= "ASAQ_inj"]
  dt[grep("Quinine base 500mg Injectable", element, fixed=TRUE), drug:= "Quinine_inj"]
  dt[grep("Quinine base 500 mg Cès", element, fixed=TRUE), drug:= "Quinine_tab"]

  dt = dt[!is.na(drug)] # other drugs / commodities we aren't as interested in for now (might want to go back and add in HIV drugs/or other malaria commodities like RDTs and LLINs)

  dt = dt[, c("coordinates", "mtk", "last_update", "download_number", "country", "category", "element_id", "element", "element_eng") := NULL]
  
  # check unique identifiers
  if( nrow(unique(dt[, .(date, org_unit_id, drug, variable)])) != nrow(dt) ) stop( "unique identifiers do not uniquely ID rows")
  
  saveRDS(dt, prepped_drug_data)
# ---------------------------------------------------

# # ---------------------------------------------------
# # Load prepped data
# # ---------------------------------------------------
# dt <- readRDS(paste0(data_path, prepped_drug_data))
# # ---------------------------------------------------

# ---------------------------------------------------
# Continue data prep for calculating drugs dist
# ---------------------------------------------------
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

dt_wide[, c("category_id", "opening_date", "data_set_id", "year") := NULL]
id_vars = names(dt_wide)[!names(dt_wide) %in% c("available", "consumed", "lost")]

# merge back to data set so we have a fully rectangularized data set with all of our data
dt_rect <- merge(rect, dt_wide, by = id_vars, all = TRUE)

dt_melt = melt.data.table(dt_rect, id.vars = id_vars)

# remove rectangularization where a drug is completely missing in an org unit across all variables
dt_melt[, completely_missing:=all(is.na(value)), by=c('org_unit_id','drug')]
dt_melt = dt_melt[completely_missing==FALSE]

saveRDS(dt_melt, prepped_for_qr)

#------RUN IMPUTATION with run_quantreg_parallel.R----------------

