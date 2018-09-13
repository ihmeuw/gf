# ----------------------------------------------
# Audrey Batzel
#
# 8/23/18
# 
# Compare provinces involved in PBF to those not doing PBF; make other graphs for TERG slides
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
## Set up R / install packages
# --------------------
rm(list=ls())

library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(stats)
library(Rcpp)
library(readxl)
# --------------------  


# ----------------------------------------------
## Overview - Files and Directories
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_data = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
dir_pop = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/worldpop_data/')
dir_cod = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/')
dir_pbf = paste0(dir_cod, 'PBF/')
dir_cod_data = paste0(dir_cod, 'prepped_data/')

# input files
hz_data <- "imputedData_run2_condensed_hz.rds"
funders <- "fullData_dps_standardized.csv"
funder_change <- "funders_data.xlsx"
hzs_pbf <- "Update_Cartographie FBP RDC23012017_083018.xlsx"
std_hz_names <- "alternate_hz_spellings.csv"
fac_file <- "J:/Project/Evaluation/GF/outcome_measurement/cod/dhis/all_units/health_zone_counts_wide.rds"

# output files
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_analysis/')
output_dt <- "dt_for_matching_analysis_of_pbf.csv"
# ----------------------------------------------


# -----------------------------
## Load data
# -----------------------------
# load the imputed data at the hz level
dt <- readRDS(paste0(dir_data, hz_data))
dt$date <- as.Date(dt$date)

# read in data for what health zones are pbf
hz_pbf <- read_excel(paste0(dir_pbf, hzs_pbf)) 
hz_pbf <- as.data.table(hz_pbf)

# load in full data where dps/hz was standardized to get data on funders from PNLP
funder_data <- read.csv(paste0(dir_data, funders))
funder_data <- as.data.table(funder_data)

# counts of type of facility from Caitlin
fac_counts <- readRDS(fac_file)

# file to merge fac counts/types with data
std_names <- read.csv(paste0(dir_cod_data, std_hz_names))
# -----------------------------


# ----------------------------------------------
## Clean dps and hz names in dt
# ----------------------------------------------
dt$dps <- gsub(" ", "-", dt$dps)
dt[dps=="bas-congo", dps:= "kongo-central"]
dt <- dt[dps!="0",]

dt$health_zone <- gsub(" ", "-", dt$health_zone)
# ----------------------------------------------


# ----------------------------------------------
## Clean dps and hz names in hz_pbf
# ----------------------------------------------
hz_pbf <- hz_pbf[-c(1:7), -c(1,7,9)]
colnames(hz_pbf) <- c("dps", "health_zone", "HGR", "official_AS", "pop_covered", "year_start_pbf")
hz_pbf$dps <- tolower(hz_pbf$dps)
hz_pbf$health_zone <- tolower(hz_pbf$health_zone)
hz_pbf$dps <- gsub(" ", "-", hz_pbf$dps)
hz_pbf$health_zone <- gsub(" ", "-", hz_pbf$health_zone)
hz_pbf$dps <- gsub("---", "-", hz_pbf$dps)
hz_pbf <- hz_pbf[!is.na(dps) & !is.na(health_zone),]

hz_pbf <- hz_pbf[dps == "equateur" & health_zone == "mankanza", health_zone := "makanza"]
hz_pbf <- hz_pbf[dps == "equateur" & health_zone == "lilanga-bobangi", health_zone := "lilanga-bobanga"]
hz_pbf <- hz_pbf[dps == "kwango" & health_zone == "kasongolunda", health_zone := "kasongo-lunda"]
hz_pbf <- hz_pbf[dps == "kwango" & health_zone == "mwela-lemba", health_zone := "mwela-lembwa"]
hz_pbf <- hz_pbf[dps == "kwilu" & health_zone == "yasabonga", health_zone := "yasa-bonga"]
hz_pbf <- hz_pbf[dps == "mai-ndombe" & health_zone == "bandjow-moke", health_zone := "bandjow"]
hz_pbf <- hz_pbf[dps == "mai-ndombe" & health_zone == "ntandembelo", health_zone := "ntandembele"]
hz_pbf <- hz_pbf[dps == "mai-ndombe" & health_zone == "pendjaw", health_zone := "pendjwa"]
hz_pbf <- hz_pbf[dps == "mongala" & health_zone == "bongandanga", health_zone := "bongandanganda"]
hz_pbf <- hz_pbf[dps == "mongala" & health_zone == "boso-mondanda", health_zone := "bosomondanda"]
hz_pbf <- hz_pbf[dps == "mongala" & health_zone == "manzi", health_zone := "bosomanzi"]
hz_pbf <- hz_pbf[dps == "sud-ubangi" & health_zone == "bogosenubea", health_zone := "bogosenusebea"]
hz_pbf <- hz_pbf[dps == "tshuapa" & health_zone == "busanga", health_zone := "bosanga"]

hz_pbf <- hz_pbf[dps == "haut-katanga" & health_zone == "sakania", health_zone := "sakanya"]
hz_pbf <- hz_pbf[dps == "haut-lomami" & health_zone == "kabondo-dianda", health_zone := "kabond-dianda"]
hz_pbf <- hz_pbf[dps == "kolwezi", dps := "lualaba"]
hz_pbf <- hz_pbf[dps == "sud-maniema", dps := "maniema"]
hz_pbf <- hz_pbf[dps == "maniema" & health_zone == "saramabila", health_zone := "saramabil"]
hz_pbf <- hz_pbf[dps == "nord-kivu" & health_zone == "kirotshé", health_zone := "kirotshe"]
hz_pbf <- hz_pbf[dps == "sud-kivu" & health_zone == "kimbi-lulenge", health_zone := "lulenge"]
hz_pbf <- hz_pbf[dps == "sankuru" & health_zone == "wembonyama", health_zone := "wembo-nyama"]
hz_pbf <- hz_pbf[dps == "lualaba" & health_zone == "kazenze", health_zone := "kanzenze"]
hz_pbf <- hz_pbf[dps == "lualaba" & health_zone == "lualaba", health_zone := "lwalaba"]
# ----------------------------------------------


# ----------------------------------------------
## Clean funder data
# ----------------------------------------------
# clean dps and hz names
funder_data$dps <- gsub(" ", "-", funder_data$dps)
funder_data[dps=="bas-congo", dps:= "kongo-central"]
funder_data <- funder_data[dps!="0",]

funder_data$health_zone <- gsub(" ", "-", funder_data$health_zone)

# subset to just the relevant vars
funder_data <- funder_data[, .(dps, health_zone, donor, year, date)]

# clean variables
funder_data$date <- as.Date(funder_data$date)
funder_data$dps <- as.character(funder_data$dps)
funder_data$health_zone <- as.character(funder_data$health_zone)
# ----------------------------------------------


# ----------------------------------------------
## Merge hz_pbf and dt and mark whether or not each hz is in PBF
# ----------------------------------------------
hz_pbf <- hz_pbf[, pbf:="yes"]
dt_pbf <- merge(dt, hz_pbf, all=TRUE, by=c("dps", "health_zone"))
dt_pbf <- dt_pbf[is.na(pbf), pbf:= "no"]
dt_pbf$year <- year(dt_pbf$date)
# ----------------------------------------------


# ----------------------------------------------
## subset to only certain indicators and cast wide/sum variables
# ----------------------------------------------
dt_match_analysis <- dt_pbf[indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere", "mildMalariaTreated", "severeMalariaTreated"), ]
setnames(dt_match_analysis, "mean", "value")

id_vars <- c("dps","health_zone", "year", "pbf", "year_start_pbf")

dt_match_analysis <- dt_match_analysis[, .(indicator_value = sum(value)), by=c(id_vars, "indicator")]

dt_wide <- data.table(dcast(dt_match_analysis, dps + health_zone +  year + pbf + year_start_pbf ~ indicator, value.var="indicator_value"))

dt_wide <- dt_wide[, conf_cases := newCasesMalariaMild + newCasesMalariaSevere]
dt_wide <- dt_wide[, cases_treated := mildMalariaTreated + severeMalariaTreated]

dt_wide <- dt_wide[, c(id_vars, "conf_cases", "cases_treated"), with=FALSE]

# for bijombo, we want to sum conf_cases and cases_treated prior to merge
# change bijombo to uvira
# then sum by health zone
dt_wide <- dt_wide[health_zone=="bijombo", health_zone:="uvira"]
sd_cols <- c("conf_cases", "cases_treated")

dt_wide <- dt_wide[, lapply(.SD, sum), by=id_vars, .SDcols = sd_cols]  # do before merging on facility names?

# for katoyi and haut-plateau, we want to sum facilities info in fac_counts prior to merge
fac_counts[health_zone=="sk Haut Plateau Zone de Santé" | health_zone=='sk Uvira Zone de Santé', health_zone:="sk Uvira Zone de Santé"]
fac_counts[health_zone=="nk Katoyi Zone de Santé" | health_zone=='nk Masisi Zone de Santé', health_zone:="nk Masisi Zone de Santé"]

all_cols <- colnames(fac_counts)
id_cols <- c("health_zone", "dps")
sd_cols <- all_cols[!all_cols %in% id_cols]

fac_counts <- fac_counts[, lapply(.SD, sum), by=id_cols, .SDcols = sd_cols]  #not working for some reason?

setnames(fac_counts, "health_zone", "hz_snis")
setnames(fac_counts, "dps", "dps_snis")
# ----------------------------------------------


# ----------------------------------------------
## merge dt for matching analysis with health facility info from Caitlin using standardized hz names
# ----------------------------------------------
# need to clean dps_ and hz_pnlp std_names the same way that dt from pnlp data was cleaned
std_names <- as.data.table(std_names)
std_names$dps_pnlp <- gsub(" ", "-", std_names$dps_pnlp)
std_names[dps_pnlp=="bas-congo", dps_pnlp:= "kongo-central"]
std_names <- std_names[dps_pnlp!="0",]

std_names$hz_pnlp <- gsub(" ", "-", std_names$hz_pnlp)
std_names <- std_names[, .(dps_snis, hz_snis, hz_pnlp, dps_pnlp)]
std_names <- unique(std_names)

std_names <- std_names[hz_pnlp != "bijombo",]
std_names <- std_names[hz_snis != "nk Katoyi Zone de Santé",]
std_names <- std_names[hz_snis != "sk Haut Plateau Zone de Santé",]

dt_std <- merge(dt_wide, std_names, all=TRUE, by.x=c("dps", "health_zone"), by.y=c("dps_pnlp", "hz_pnlp"))

# now, merge with Caitlin's facility counts data:
dt <- merge(dt_std, fac_counts, all=TRUE, by=c("dps_snis", "hz_snis"))
dt <- dt[, -c("dps_snis", "hz_snis")]
dt <- setnames(dt, "conf_cases", "cases")
# ----------------------------------------------


# ----------------------------------------------
## merge funder data to dt_pbf
# ----------------------------------------------
# funder_data <- unique(funder_data[, .(dps, health_zone, donor, year)])
# dt2 <- merge(dt, funder_data, all=TRUE, by=c("dps", "health_zone", "year"))
# 
# # mark where gf was a donor
# setnames(dt2, "donor", "funder")
# dt2[!is.na(funder), GF:= ifelse(grepl("FM", funder), "yes", "no") ]
dps_names <- unique(dt$dps)
gf_dps <- c("equateur", "kongo-central", "kinshasa", "kwilu", "kwango", "mai-ndombe", "tshuapa", "sud-ubangi", "nord-ubangi", "mongala", "tshopo", "maniema", "bas-uele",
            "haut-uele", "ituri", "nord-kivu")

dt[dps %in% gf_dps, funder_2017:="GF"]
dt[dps=="kasai", funder_2017:="DFID"]
dt[!dps %in% c(gf_dps, "kasai"), funder_2017:="PMI"]

write.csv(dt, file=paste0(dir_cod_data, output_dt))
# ----------------------------------------------


