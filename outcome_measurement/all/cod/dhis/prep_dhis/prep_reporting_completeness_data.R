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

# input file:
files = list.files( paste0(dir_dhis, 'completeness_reports/'), recursive=TRUE)
files = files[!grepl(files, pattern = "archive")]
files = files[grepl(files, pattern = "hz")]
files = files[grepl(files, pattern = "quarterly")]

# output file:
outFile = paste0(dir_dhis, 'prepped/all_data_sets_completeness_hz_quarterly.rds')

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ----------------------------------------------

# ----------------------------------------------
# use meta data to get dps names matched to dps codes
# ----------------------------------------------
# meta data for getting names
facilities = data.table(readRDS(paste0(dir_dhis, 'meta_data/master_facilities.rds')))
# get dps names merged to completeness data based on the dps code in the health zone name
dps_code_matching = unique(facilities[, .(health_zone, dps)])
dps_code_matching$dps_code = unlist(lapply(strsplit(dps_code_matching$dps, " "), "[", 1))
dps_code_matching = unique(dps_code_matching[, .(dps, dps_code)])

dps_code_matching$dps1 = unlist(lapply(strsplit(dps_code_matching$dps, " "), "[", 2))
dps_code_matching$dps2 = unlist(lapply(strsplit(dps_code_matching$dps, " "), "[", 3))
dps_code_matching[dps2 != 'Province', dps:=paste(dps1, dps2) ]
dps_code_matching[dps2=='Province', dps:=dps1]
dps_code_matching[ , c('dps1', 'dps2'):=NULL]
dps_code_matching = dps_code_matching[ !is.na(dps)]
if(nrow(dps_code_matching) != 26) stop("something went wrong, there should be 26 DPS!")

dps_code_matching$dps = standardizeDPSNames(dps_code_matching$dps)
# ----------------------------------------------

# ----------------------------------------------
# load files of completeness data
# ----------------------------------------------
# set up a list to store all the data in
dtList = list()

# loop over files (note: for DEP this takes about an hour on my computer)
i=1
for (f in files) { 
  # display the name of the current file to monitor progress
  print(f)
  
  # load the sheet and convert to data.table for convenience
  currentFile = read_excel(paste0(dir_dhis, 'completeness_reports/', f))
  currentFile = data.table(currentFile)

  # identify which file the data came from
  currentFile[, file:=f]
  
  # store the data in the list
  dtList[[i]] = currentFile
  i=i+1
}
# ----------------------------------------------

# ----------------------------------------------
# prep data - health zone/quarterly level only, so far!
# ----------------------------------------------
# set up a dt to store all the data in
cleanedData = data.table()

# loop over sheets
for(d in seq(length(dtList))) {
  tmp = copy(dtList[[d]])
  
  # remove file name so it doesn't mess up reshaping the data long:
  data_set = unique(unlist(lapply(strsplit(tmp$file, "_"), "[", 1))) # first save data set name
  tmp[, file := NULL]
  
  # clean data
  # clean up unneccessary first couple rows and column names
  colnames(tmp) <- unlist(tmp[2, ])
  tmp <- tmp[-(1:2),]
  colnames(tmp)[1] = 'health_zone'
  
  # clean up health zone and dps names
  tmp$dps_code = unlist(lapply(strsplit(tmp$health_zone, " "), "[", 1))
  
  # standardize health zone names
  tmp$health_zone1 = unlist(lapply(strsplit(tmp$health_zone, " "), "[", 2))
  tmp$health_zone2 = unlist(lapply(strsplit(tmp$health_zone, " "), "[", 3))
  tmp$health_zone3 = unlist(lapply(strsplit(tmp$health_zone, " "), "[", 4))
  tmp[health_zone3 != 'Zone' & health_zone2 != 'Zone', health_zone:=paste(health_zone1, health_zone2, health_zone3) ]
  tmp[health_zone3=='Zone', health_zone:=paste(health_zone1, health_zone2)]
  tmp[health_zone2=='Zone', health_zone:=health_zone1]
  tmp[ , c('health_zone1', 'health_zone2', 'health_zone3'):=NULL]
  tmp[, health_zone := standardizeHZNames(health_zone)]
  
  # merge dps names onto tmp
  tmp = merge(tmp, dps_code_matching, by = "dps_code", all.x=TRUE)
  
  # edit dates to be usable in the format we want - depends on whether input file is quarterly or monthly
  tmp = melt.data.table(tmp, id.vars = c("dps", "dps_code", "health_zone"), variable.name = "date", value.name = "completeness")
  tmp[, date := as.character(date)]
  tmp[, completeness := as.character(completeness)] # do as.character() first **just in case** it was a factor, to prevent it from changing values
  tmp[, completeness := as.numeric(completeness)]
  
  # for quarterly data - add on monthly later to make this script more functional
  tmp[, year := sapply(strsplit(date, " "), tail, 1)]
  tmp[ grepl(date, pattern= "Jan", ignore.case = TRUE), quarter := "1"]
  tmp[ grepl(date, pattern= "Avr", ignore.case = TRUE), quarter := "2"]
  tmp[ grepl(date, pattern= "Juil", ignore.case = TRUE), quarter := "3"]
  tmp[ grepl(date, pattern= "Oct", ignore.case = TRUE), quarter := "4"]
  
  tmp[, set := data_set]
  
  # store data in final dt
  cleanedData = rbindlist(list(cleanedData, tmp), use.names = TRUE, fill = TRUE)
}

# because of standardization, some are duplicated, so take the average of those
dt = cleanedData[, .(completeness = mean(completeness)), by = .(dps, health_zone, date, year, quarter, set)]

if (nrow(dt[duplicated(dt[, .(health_zone, dps, year, quarter, set)])]) != 0 ) stop ( "Unique identifiers do not uniquely identify rows!")

saveRDS(dt, outFile)
# ----------------------------------------------

# # for monthly data:
# } else {
#   dt[, c("month", "year"):= tstrsplit(date, " ")]
#   mos = unique(dt[, .(month, year)])
#   mos[, month_number := rep(seq(1:12))]
#   mos = unique(mos[year!="2019", .(month, month_number)])
#   dt = merge(dt, mos, by = "month", all = TRUE)
#   dt[, month:= NULL]
#   setnames(dt, "month_number", "month")
#   dt[, date := as.Date(as.yearmon(paste(year, month, sep = "-")))]
#   dt[, c("year", "month", "dps_code") := NULL]
# }


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
