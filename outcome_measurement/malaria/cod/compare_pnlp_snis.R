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
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/')

# input file:
input_pnlp <- "final_data_for_imputation.csv"
input_dhis_base <- "base.rds"
input_dhis_sigl <- "sigl.rds"

standardized_hzs <- "standardized_hzs.csv"

# output file:
comparison <- "snis_pnlp_comparison.pdf"
comparison_at_dps <- "snis_pnlp_comparison_at_dps_level.pdf"
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
# Subset data to just the data being compared
# ----------------------------------------------
# PNLP
# make var for year in pnlp
pnlp$year <- year(pnlp$date)

# subset to just 2017
pnlp <- pnlp[year==2017 & dps != "0",]
pnlp$dps <- gsub(" ", "-", pnlp$dps)
pnlp[ dps== "bas-congo", dps := "kongo-central" ]

# SNIS
# subset to 2017 and to type = malaria
dhis_base <- dhis_base[year==2017 & type=="malaria" & keep==1,]

# subset to the columns we want to use in snis
dhis_base_subset <- dhis_base[, .(dps, health_zone, health_area, date, month, year, element, element_eng, category, age, value)]

# aggregate snis data to health zone and dps level
dhis_base_hz <- dhis_base_subset[, .(value = sum(value, na.rm=TRUE)), by=c("dps", "health_zone", "date", "month", "year", "element", "element_eng", "category", "age")]
dhis_base_dps <- dhis_base_subset[, .(value = sum(value, na.rm=TRUE)), by=c("dps", "date", "month", "year", "element", "element_eng", "category", "age")]
# ----------------------------------------------


# ----------------------------------------------
# Standardize hzs and dps in pnlp and snis data
# ----------------------------------------------
hzs <- hzs[, .(health_zone, dps, hz_snis, dps_snis, hz_pnlp, dps_pnlp)]
setnames(hzs, "health_zone", "standardized_hz")
setnames(hzs, "dps", "standardized_dps")
hzs_complete <- hzs[complete.cases(hzs)]
hzs_complete <- unique(hzs_complete)

# hzs_merge_dps <- hzs_complete[, .(dps, dps_snis, dps_pnlp)]
# hzs_merge_dps <- unique(hzs_merge_dps)

# merge hzs with dhis_base data
dhis_standardized <- merge(dhis_base_hz, hzs_complete, by.x=c("dps", "health_zone"), by.y=c("dps_snis", "hz_snis"), all=TRUE)

# dhis_dps <- merge(dhis_base_dps, hzs_merge_dps, by.x=c("dps"), by.y=c("dps_snis"), all=TRUE)

# merge hzs with pnlp data
pnlp_standardized <- merge(pnlp, hzs_complete, by.x=c("dps", "health_zone"), by.y=c("dps_pnlp", "hz_pnlp"), all.x=TRUE)
# ----------------------------------------------


# ----------------------------------------------
# Standardize variables for comparison
# ----------------------------------------------
# standardize variables for comparison & subset to just those variables
# change relevant var names in dhis data
# NOTE: just realized that dhis data has malaria cases split by <5 and >5 too.. not sure if the split is the same for where age 5 is included
dhis_standardized[element_eng== "A 2.1 LLINs distributed has ANC2 +", element_eng := "ITN_distAtANC2"]
dhis_standardized[element_eng== "A 2.1 LLINs distributed to the CPN1", element_eng := "ITN_distAtANC1"]
dhis_standardized[element_eng== "A 1.4 RDT positive", element_eng := "RDT_positive"]
dhis_standardized[element_eng== "A 1.4 RDT performed", element_eng := "RDT_completed"]
dhis_standardized[element_eng== "A 1.4 presumed Malaria", element_eng := "presumed_mal"]
dhis_standardized[element_eng== "A 1.4 Suspected case", element_eng := "suspected_mal"]
dhis_standardized[element_eng== "A 2.1 Sulfadox. + Pyrimét first dose", element_eng := "SP_1st"]
dhis_standardized[element_eng== "A 2.1 Sulfadox. + Pyrimét 2nd dose", element_eng := "SP_2nd"]
dhis_standardized[element_eng== "A 2.1 Sulfadox. + Pyrimét 3rd dose", element_eng := "SP_3rd"]
dhis_standardized[element_eng== "A 1.5 Severe malaria FE", element_eng := "newCasesMalariaSevere_pregnantWomen"]
dhis_standardized[element_eng== "A 1.5 Severe malaria FE treated", element_eng := "severeMalariaTreated_pregnantWomen"]
dhis_standardized[element_eng== "A 1.5 Confirmed simple malaria - pregnant woman", element_eng := "newCasesMalariaMild_pregnantWomen"]
dhis_standardized[element_eng== "A 1.5 Confirmed simple malaria treated - pregnant woman", element_eng := "mildMalariaTreated_pregnantWomen"]
dhis_standardized[element_eng== "A 1.4 Severe malaria", element_eng := "severe_mal"]
dhis_standardized[element_eng== "A 1.4 Severe malaria treated", element_eng := "severe_mal_treated"]
dhis_standardized[element_eng== "A 1.5 Confirmed simple malaria treated", element_eng := "simple_mal_treated"]

# sum variables in pnlp for comparison
pnlp_standardized[, presumed_mal := rowSums(.SD, na.rm=TRUE), .SDcols = c("presumedMalaria_5andOlder" , "presumedMalaria_pregnantWomen" , "presumedMalaria_under5")]
pnlp_standardized[, suspected_mal := rowSums(.SD, na.rm=TRUE), .SDcols = c('suspectedMalaria_5andOlder' , 'suspectedMalaria_pregnantWomen' , 'suspectedMalaria_under5')]
pnlp_standardized[, severe_mal := rowSums(.SD, na.rm=TRUE), .SDcols = c('newCasesMalariaSevere_under5' , 'newCasesMalariaSevere_5andOlder')]
pnlp_standardized[, severe_mal_treated := rowSums(.SD, na.rm=TRUE), .SDcols = c('severeMalariaTreated_5andOlder' , 'severeMalariaTreated_under5')]
pnlp_standardized[, simple_mal_treated := rowSums(.SD, na.rm=TRUE), .SDcols = c('mildMalariaTreated_5andOlder' , 'mildMalariaTreated_under5')]

# sum variables in dhis base for comparison
var_for_comp <- c("RDT_completed", "RDT_positive", "ITN_distAtANC", "ITN_distAtANC1", "ITN_distAtANC2", "presumed_mal", "suspected_mal", "SP_1st", 
                  "SP_2nd", "SP_3rd", "newCasesMalariaSevere_pregnantWomen", "newCasesMalariaMild_pregnantWomen",
                  "mildMalariaTreated_pregnantWomen", "severe_mal", "severe_mal_treated", "simple_mal_treated",
                  "severeMalariaTreated_pregnantWomen")

dhis_standardized <- dhis_standardized[element_eng %in% var_for_comp,]
dhis_standardized[, data_source := "dhis"]

dhis_standardized <- dhis_standardized[!is.na(health_zone),]
dhis_standardized <- dhis_standardized[, value:= sum(value), by=.(dps, health_zone, date, element_eng)]
dhis_standardized <- dhis_standardized[, .(data_source, standardized_dps, standardized_hz, date, year, element_eng, value)]
setnames(dhis_standardized, "element_eng", "variable")

# subset variable in pnlp to just the ones to compare with snis
var_for_comp <- c("RDT_completed", "RDT_positive", "ITN_distAtANC", "presumed_mal", "suspected_mal", "SP_1st",
                  "SP_2nd", "SP_3rd", "newCasesMalariaSevere_pregnantWomen", "newCasesMalariaMild_pregnantWomen",
                  "mildMalariaTreated_pregnantWomen", "severe_mal", "severe_mal_treated", "simple_mal_treated",
                  "severeMalariaTreated_pregnantWomen")

pnlp_standardized <- pnlp_standardized[, c("standardized_dps", "standardized_hz", "date", var_for_comp), with=FALSE]

# melt pnlp
pnlp_standardized <- melt(pnlp_standardized, id.vars=c("standardized_dps", "standardized_hz", "date"))
pnlp_standardized[, data_source := "pnlp"]
pnlp_standardized$year <- year(pnlp_standardized$date)
# ----------------------------------------------


# ----------------------------------------------
# rbind standardized pnlp and dhis
# ----------------------------------------------
dhis_standardized$year <- as.numeric(dhis_standardized$year)
pnlp_standardized$date <- as.Date(pnlp_standardized$date)
dhis_standardized$date <- as.Date(dhis_standardized$date)

dt <- rbind(pnlp_standardized, dhis_standardized)
# ----------------------------------------------


# ----------------------------------------------
#graph
# ----------------------------------------------
var_to_graph <- c("RDT_completed", "RDT_positive", "presumed_mal", "suspected_mal", "SP_1st",
                  "SP_2nd", "SP_3rd", "newCasesMalariaSevere_pregnantWomen", "newCasesMalariaMild_pregnantWomen",
                  "mildMalariaTreated_pregnantWomen", "severe_mal", "severe_mal_treated",
                  "severeMalariaTreated_pregnantWomen")

# dps and national level values
dt_dps <- dt[, .(dpsValue = sum(value, na.rm=TRUE)), by=.(standardized_dps, date, year, variable, data_source)]
dt_cod <- dt[, .(countryValue = sum(value, na.rm=TRUE)), by=.(date, year, variable, data_source)]

# national level comparison
pdf(paste0(output_dir, comparison), height = 9, width = 11)
for (v in var_to_graph){
  
  g <- ggplot(dt_cod[ variable == v, ], aes(x=date, y=countryValue, color=data_source)) + 
      theme_bw()+ geom_point() + geom_line() +
      ggtitle(paste0("Snis and PNLP comparison at the national level for ", v)) +
      ylab("Value") + xlab("Date") + ylim(0, NA)
  print(g)
    
}
dev.off()

# dps level comparison
dt$standardized_dps <- as.character(dt$standardized_dps)

dps <- unique(dt$standardized_dps)
dps <- dps[!is.na(dps)]

pdf(paste0(output_dir, comparison_at_dps), height = 9, width = 11)
for (v in var_to_graph) {
  for (d in dps){
    
    g <- ggplot(dt_dps[ standardized_dps == d & variable == v, ], aes(x=date, y=dpsValue, color=data_source)) + 
      theme_bw()+ geom_point() + geom_line() +
      ggtitle(paste0("Snis and PNLP comparison in ", d, " for ", v)) +
      ylab("Value") + xlab("Date") + ylim(0, NA)
    print(g)
    
  }
}
dev.off()

dt$standardized_hz <- as.character(dt$standardized_hz)
health_zones <- unique(dt$standardized_hz)

pdf(paste0(output_dir, "snis_pnlp_comparison_hz.pdf"), height = 9, width = 11)
for (v in var_to_graph){
  for( h in health_zones[1:20]) {
    
    g <- ggplot(dt[ standardized_hz == h & variable == v, ], aes(x=date, y=value, color=data_source)) + 
      theme_bw()+ geom_point() + geom_line() +
      ggtitle(paste0("Snis and PNLP comparison in ", h , " for ", v)) +
      ylab("Value") + xlab("Date") + ylim(0, NA)
    print(g)
    
  }
  }
dev.off()
# ----------------------------------------------


# ----------------------------------------------
# Code I ended up not using...

# # clean dhis dps and hz strings
# dhis_base$dps <- sapply(str_split(dhis_base$dps, " ", 2), '[', 2)
# dhis_base$dps <- gsub(" Province", "", dhis_base$dps)
# dhis_base$dps <- tolower(dhis_base$dps)
# dhis_base$dps <- gsub(" ", "-", dhis_base$dps)
# 
# dhis_base$health_zone <- sapply(str_split(dhis_base$health_zone, " ", 2),'[', 2)
# dhis_base$health_zone <- gsub(" Zone de Santé", "", dhis_base$health_zone)
# dhis_base$health_zone <- tolower(dhis_base$health_zone)
# dhis_base$health_zone <- gsub(" ", "-", dhis_base$health_zone)
# 
# dhis_base$health_area <- sapply(str_split(dhis_base$health_area, " ", 2),'[', 2)
# dhis_base$health_area <- gsub(" Aire de Santé", "", dhis_base$health_area)
# dhis_base$health_area <- tolower(dhis_base$health_area)
# dhis_base$health_area <- gsub(" ", "-", dhis_base$health_area)

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

# # cast wide in order to add vars together
# dhis_wide <- dcast(dhis, date+year+month+dps+data_source ~ element_eng, value.var = 'dpsValue')
# dhis_wide <- as.data.table(dhis_wide)
# # sum ITN vars
# dhis_wide[, ITN_distAtANC := rowSums(.SD, na.rm=TRUE), .SDcols = c('ITN_distAtANC1' , 'ITN_distAtANC2')]
# # melt back to long
# dhis_long <- melt(dhis_wide, id.vars= c("date", "year", "month", "dps", "data_source"))
# update vars for comparison - remove ITN_distATANC1 and ITN_distAtANC2 

  
