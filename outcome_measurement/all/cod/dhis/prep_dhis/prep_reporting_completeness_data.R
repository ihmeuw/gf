# Audrey Batzel 
# 3/21/19
#
# prep reporting completeness data from SNIS pivot tables for use in impact model
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

data_set = "sigl1"

# input file:
inFile = paste0(dir_dhis, 'completeness_reports/', data_set, '_reporting_completeness_hz.xls')
  
# output file:
outFile = paste0(dir_dhis, 'completeness_reports/prepped/', data_set, '_reporting_completeness_hz_prepped.rds')

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ----------------------------------------------

# ----------------------------------------------
# load data
# ----------------------------------------------
dt <- read_xls(inFile)
dt <- as.data.table(dt)
# ----------------------------------------------

# ----------------------------------------------
# prep data
# ----------------------------------------------
colnames(dt) <- unlist(dt[2, ])
dt <- dt[-(1:2),]
colnames(dt)[1] = 'health_zone'
dt$dps_code = unlist(lapply(strsplit(dt$health_zone, " "), "[", 1))

dt$health_zone1 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 2))
dt$health_zone2 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 3))
dt$health_zone3 = unlist(lapply(strsplit(dt$health_zone, " "), "[", 4))
dt[health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone:=paste(health_zone1, health_zone2, health_zone3) ]
dt[health_zone3=='Zone', health_zone:=paste(health_zone1, health_zone2)]
dt[health_zone2=='Zone', health_zone:=health_zone1]
dt[ , c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]

dt$health_zone = standardizeHZNames(dt$health_zone)

# get dps names merged to completeness data
facilities = data.table(readRDS(paste0(dir_dhis, 'meta_data/master_facilities.rds')))
dps_code_matching = unique(facilities[, .(health_zone, dps)])
dps_code_matching$dps_code = unlist(lapply(strsplit(dps_code_matching$dps, " "), "[", 1))
dps_code_matching = unique(dps_code_matching[, .(dps, dps_code)])

dps_code_matching$dps1 = unlist(lapply(strsplit(dps_code_matching$dps, " "), "[", 2))
dps_code_matching$dps2 = unlist(lapply(strsplit(dps_code_matching$dps, " "), "[", 3))
dps_code_matching[dps2 != 'Province', dps:=paste(dps1, dps2) ]
dps_code_matching[dps2=='Province', dps:=dps1]
dps_code_matching[ , c('dps1', 'dps2'):=NULL]

dps_code_matching$dps = standardizeDPSNames(dps_code_matching$dps)
dps_code_matching = dps_code_matching[!is.na(dps)]

dt = merge(dt, dps_code_matching, by = "dps_code")

dt = melt.data.table(dt, id.vars = c("dps", "dps_code", "health_zone"), variable.name = "date", value.name = "completeness")
dt[, c("month", "year"):= tstrsplit(date, " ")]
mos = unique(dt[, .(month, year)])
mos[, month_number := rep(seq(1:12))]
mos = unique(mos[year!="2019", .(month, month_number)])
dt = merge(dt, mos, by = "month", all = TRUE)
dt[, month:= NULL]
setnames(dt, "month_number", "month")
dt[, date := as.Date(as.yearmon(paste(year, month, sep = "-")))]
dt[, c("year", "month", "dps_code") := NULL]
dt$completeness = as.numeric(dt$completeness)

saveRDS(dt, outFile)
# ----------------------------------------------

# ----------------------------------------------
# simple graphs of completeness
# ----------------------------------------------
g <- ggplot(dt, aes(x=date, y=completeness)) + 
  geom_point(position = "jitter") + 
  ggtitle("Completeness of SIGL1 data at the health zone level over time") +
  ylab("Percent complete") + xlab("Date") + theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) 
print(g)

subset_hz_high = dt[ date == "2018-01-01" & completeness == 100, ]
subset_hz_high = subset_hz_high[ sample(nrow(subset_hz_high), 5), ]
subset_test = dt[ health_zone %in% subset_hz_high$health_zone, ]

g <- ggplot(subset_test, aes(x=date, y=completeness, color = health_zone)) + 
  geom_point() + geom_line() +
  ggtitle("Completeness of Base Services data at the health zone level over time") +
  ylab("Percent complete") + xlab("Date") + theme_bw() + ylim(0, 100) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) 
print(g)

subset_hz_low = dt[ date == "2018-01-01" & completeness < 50, ]
subset_hz_low = subset_hz_low[ sample(nrow(subset_hz_low), 5), ]
subset_test = dt[ health_zone %in% subset_hz_low$health_zone, ]

g <- ggplot(subset_test, aes(x=date, y=completeness, color = health_zone)) + 
  geom_point() + geom_line() +
  ggtitle("Completeness of Base Services data at the health zone level over time") +
  ylab("Percent complete") + xlab("Date") + theme_bw() +  ylim(0, 100) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) 
print(g)
# ----------------------------------------------
