# Audrey Batzel 
# 3/28/19
#
# prep SSC data from Constant's manual download
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
library(openxlsx)
library(zoo)
# --------------------  

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_dhis = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
ieDir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/')

# input file:
inFile = paste0(dir_dhis, 'pre_prep/SSC_data_2017_2018.rds')
combinedFile = paste0(ieDir, 'base_pnlp_sigl_combined_data_hz_level.rds')

# output file:
outFile = paste0(dir_dhis, 'prepped/SSC_data_2017_2018_prepped.rds')

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')

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
# ----------------------------------------------

# ----------------------------------------------
# load data
# ----------------------------------------------
dt <- readRDS(inFile)
dt <- as.data.table(dt)

# for now, just subset to ACTs_SSC
dt = dt[element == "B 10.2 Dont traités selon la politique nationale - Cas de fièvre", ]
# ----------------------------------------------

# ----------------------------------------------
# prep data
# ----------------------------------------------
names(dt) = c('date', 'dps', 'health_zone', 'element', 'value')
dt[element == "B 10.2 Dont traités selon la politique nationale - Cas de fièvre", element:= "ACTs_SSC"]
dt[ , c("indicator", "subpopulation"):= tstrsplit(element, "_")]
dt[, date := as.Date(date)]
dt[, data_set := "snis_secondary_scc"]

# standardize names
dt$health_zone1 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 2))
dt$health_zone2 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 3))
dt$health_zone3 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 4))
dt[health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone:=paste(health_zone1, health_zone2, health_zone3) ]
dt[health_zone3=='Zone', health_zone:=paste(health_zone1, health_zone2)]
dt[health_zone2=='Zone', health_zone:=health_zone1]
dt[ , c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]

dt[, health_zone := standardizeHZNames(health_zone)]
dt[, dps := standardizeDPSNames(dps)]
# ----------------------------------------------

# ----------------------------------------------
# agg to quarterly/save for adding to model data
# ----------------------------------------------
dt[, quarter:= quarter(date)]
dt[, year:= year(date)]
dt_qtr <- convert_quarter_to_decimal(dt)

dt_qtr = dt_qtr[, .(value= sum(value, na.rm=TRUE)), by = .(date, dps, health_zone, indicator, subpopulation, data_set)] # note, now date is quarterly
dt_qtr[, indicator := paste0(indicator, '_',subpopulation)]
dt_qtr[, subpopulation := NULL]
dt_qtr = dt_qtr[ date >= 2018.00, ]
saveRDS(dt_qtr, outFile)
# ----------------------------------------------

# ----------------------------------------------
# compare with PNLP SSC acts data:
# ----------------------------------------------
comb <- readRDS(combinedFile)
pnlp <- comb[ data_set == "pnlp" & indicator == "SSCACT", ]
pnlp[, year:= year(date)]
pnlp[ data_set == "pnlp" & indicator == "SSCACT" & is.na(subpopulation) & year < 2015, value := NA]
pnlp[ data_set == "pnlp" & indicator == "SSCACT" & is.na(subpopulation) & year > 2016, value := NA]
pnlp[ data_set == "pnlp" & indicator == "SSCACT" & !is.na(subpopulation) & year < 2017, value := NA]
pnlp <- pnlp[!is.na(value)]

dt = dt[, c("quarter", "element"):= NULL]
ssc_act <- rbindlist(list(dt, pnlp), use.names = TRUE)
ssc_act[ , indicator := 'ACTs_SSC']
ssc_act[ , subpopulation:= NULL]
ssc_act_natl = ssc_act[, .(value = sum(value, na.rm= TRUE)), by = c("date", "data_set", "indicator")]

g <- ggplot(ssc_act_natl, aes(x=date, y=value, color=data_set)) + 
  geom_point() + geom_line() +
  ggtitle(paste0("Time series showing comparison between PNLP and SNIS for ACTs at SSC")) +
  ylab("Count") + xlab("Date") + theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  scale_y_continuous( label= scales :: comma ) + 
  guides(color = guide_legend(title= "Data Source:"))
print(g)
# ----------------------------------------------

