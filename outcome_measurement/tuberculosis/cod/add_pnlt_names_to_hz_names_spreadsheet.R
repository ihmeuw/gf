# ----------------------------------------------
# Audrey Batzel
# 10/25/18
#
# add PNLT health zone names to spreadsheet of differences
#
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
## -------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(lubridate)
library(readxl)
library(stats)
library(tidyr)
library(dplyr)
library(openxlsx)
library(raster)
library(rgeos)
library(maptools)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_data = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/')
dir_pnlt <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/")

# input file
standardized_names <- "standardized_hzs_final.xlsx"
pnlt_data <- "PNLT_totalCases_2016.rds"
pnlt_names <- "standardized_pnlt_hzs.xlsx"

# output file
output_stand_names <- 'standardized_hzs_final_inc_pnlt.csv'
# ----------------------------------------------

# ----------------------------------------------
# read in the data
names <- data.table(read_excel(paste0(dir_data, standardized_names)))
pnlt <- readRDS(paste0(dir_pnlt, pnlt_data))
# ----------------------------------------------

# ----------------------------------------------
# PNLT
# get unique combos of health zone and dps names
pnlt <- unique(pnlt[, .(dps, hz)])
# save a version of the names as they appear in the data
pnlt$hz_pnlt <- pnlt$hz
pnlt$dps_pnlt <- pnlt$dps


# Now make modifications to standardize dps and hz for merge
pnlt[dps=="kongo-central-est", dps:= "kongo-central"]
pnlt[dps=="kongo-central-ouest", dps:= "kongo-central"]
# standardize original names to be merged together
pnlt$hz <- gsub(" ", "-", pnlt$hz)
pnlt$dps <- gsub(" ", "-", pnlt$dps)
pnlt$dps <- tolower(pnlt$dps)
pnlt$hz <- tolower(pnlt$hz)
# ----------------------------------------------

# ----------------------------------------------
# keep all hz names as potential names for pnlt to merge on

#--># first add a variable for the cleaned version of the SNIS name
    names$hz_snis_cleaned <- names$hz_snis
    # then clean that var to strip away other symbols (this will actually be good for the standardizing
    # names function too, as another possible name... so we have the original SNIS name for standardizing the
    # SNIS names with the different formatting AND another variation of the name to merge on.)
    names$hz_snis_cleaned <- sapply(str_split(names$hz_snis_cleaned, " ", 2),'[', 2)
    names$hz_snis_cleaned <- gsub(" Zone de Santé", "", names$hz_snis_cleaned)
    names$hz_snis_cleaned <- tolower(names$hz_snis_cleaned)
    names$hz_snis_cleaned <- gsub(" ", "-", names$hz_snis_cleaned)

# this just subsets to the ones that would be possible to match on - but keep the full data table
names_abbrev <- names[, .(health_zone, dps, hz_shp1, hz_shp2, hz_snis_cleaned, hz_pnlp)]
# take the subset data table and melt so that each name is a potential "alternate name" that matches to a standardized name
names_long <- melt.data.table(names_abbrev, id.vars=c("health_zone", "dps"), variable.name= "source", value.name= "alternate_name")

# standardize all alternate names, in terms of punctuation, so that they can be matched on. 
# (Note: this is just to match up the PNLT names to the equivalent "standardized name" - the health_zone variable - but we saved
# the full data set, including the original versions of the PNLT vars, so we can keep the original versions of all sources)
names_long <- names_long[ !is.na(alternate_name), ]
names_long$alternate_name <- tolower(names_long$alternate_name)
names_long$alternate_name <- gsub(" ", "-", names_long$alternate_name)
# don't need to keep track of the source here, so take unique values of alternate names, with the dps/hz they match too.
names_long <- unique(names_long[, .(dps, health_zone, alternate_name)])

names_long <- names_long[ !is.na(dps) & !is.na(health_zone)]
setnames(names_long, "health_zone", "hz_standard")

# merge standardized pnlt names with alternate names in long names dt
dt <- merge(names_long, pnlt, by.y=c("dps", "hz"), by.x=c("dps", "alternate_name"), all.x=TRUE, all.y=TRUE)

check_mismatched<- dt[is.na(hz_standard) | is.na(hz_pnlt)]

# ----------------------------------------------
# IN EXCEL - matched up the unmatched hz names.. saved as an excel doc.  Read in that data to add to the original spreadsheet and resave it.
# read in the excel sheet
pnlt_names_standardized <- data.table(read_excel(paste0(dir_pnlt, pnlt_names)))

# take the hz_pnlt and hz_standard vars from dt
data_to_bind <- unique(dt[ !is.na(hz_pnlt) & !is.na(hz_standard), .(hz_pnlt, hz_standard)])
pnlt_names_standardized <- rbind(pnlt_names_standardized, data_to_bind)

# merge pnlt_names_standardized back with original names spreadsheet on the hz_standard var
names_check <- merge(names, pnlt_names_standardized, by.x="health_zone", by.y="hz_standard", all=TRUE)

# save this in both standard folder, and "core folder"
write.csv(names_check, paste0(dir_data, output_stand_names))
write.csv(names_check, 'C:/local/gf/core/standardized_hzs_final_inc_pnlt.csv')
# ----------------------------------------------


