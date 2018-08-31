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
colnames(hz_pbf) <- c("dps", "health_zone", "HGR", "official_AS", "pop_covered", "year_start_PBF")
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

dt_match_analysis <- dt_match_analysis[, .(indicator_value = sum(value)), by=c("dps","health_zone", "year", "pbf", "year_start_pbf", "funder", "GF", "indicator")]

dt_wide <- data.table(dcast(dt_match_analysis, dps + health_zone +  year + pbf + year_start_pbf + funder + GF ~ indicator, value.var="indicator_value"))

dt_wide <- dt_wide[, conf_cases := newCasesMalariaMild + newCasesMalariaSevere]
dt_wide <- dt_wide[, cases_treated := mildMalariaTreated + severeMalariaTreated]

dt_wide <- dt_wide[, c("year", "dps", "health_zone", "pbf", "year_start_pbf", "funder", "GF", "conf_cases", "cases_treated")]
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

dt_std <- merge(dt_wide, std_names, all=TRUE, by.x=c("dps", "health_zone"), by.y=c("dps_pnlp", "hz_pnlp"))

# merge health fac info from caitlin on dps_snis and hz_snis
setnames(fac_counts, "dps", "dps_snis")
setnames(fac_counts, "health_zone", "hz_snis")

# for bijombo, since it is in PNLP but not SNIS, we want to sum conf_cases and cases_treated (the data in PNLP)
# and we want to change bijombo to uvira (?)
# do this BEFORE merge, so it's easier to sum over facilities
# doing this next part manually because I can't figure it out...

dt_std[health_zone=="bijombo", conf_cases:= sum(conf_cases), by="year"]
dt_std[health_zone=="bijombo", cases_treated:= sum(cases_treated), by="year"]

dt_subset <- dt_std[health_zone == "bijombo" | health_zone == "uvira",]

dt_subset <- dt_subset[, add_var := rep(1:2)]

# add bijombo values to uvira
dt_std[health_zone=="uvira", conf_cases:= conf_cases + 6380.559]
dt_std[health_zone=="uvira", cases_treated:= cases_treated + 6250.308]



# get rid of bijombo lines so it's just uvira
dt_std <- dt_std[!health_zone=="bijombo",]

# now, merge with Caitlin's facility counts data:
dt <- merge(dt_std, fac_counts, all=TRUE, by=c("dps_snis", "hz_snis"))


# for katoyi and haut-plateau, we want to sum facilities info since it is in SNIS but not PNLP (so we don't want to sum cases since it is duplicated)
all_cols <- colnames(dt)
dt[,dps_snis:=NULL]
dt[,hz_snis:=NULL]
id_vars <- c("year", "dps", "health_zone", "pbf", "year_start_pbf", "funder", "GF", "conf_cases", "cases_treated")
sd_cols <- all_cols[!all_cols %in% id_vars]

dt_test <- dt[, lapply(.SD, sum), by=id_vars, .SDcols = sd_cols]
# ----------------------------------------------


# ----------------------------------------------
## merge funder data to dt_pbf
# ----------------------------------------------
dt2 <- merge(dt_pbf, funder_data, all=TRUE, by=c("dps", "health_zone", "date", "year"))

# mark where gf was a donor
setnames(dt2, "donor", "funder")
dt2 <- dt2[!is.na(funder), GF:= ifelse(grepl("FM", funder), "yes", "no") ]
setnames(dt2, "year_start_PBF", "year_start_pbf")
dt_pbf <- dt2[, c("dps", "health_zone", "date", "year", "pbf", "year_start_pbf", "funder", "GF", "variable", "indicator", "subpopulation", "mean")]
# ----------------------------------------------




