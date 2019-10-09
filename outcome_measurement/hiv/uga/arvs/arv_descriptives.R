# Descriptive analysis of stock outs for 2019 reports
# Caitlin O'Brien-Carelli
# 10/1/2019

# ----------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(plyr)
library(data.table)

# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# working directory to aggregate
og = readRDS(paste0(dir, 'prepped_data/arv_stockouts_2013_2019.rds'))

# subset dates to exclude 2013 as the data are not complete
og = og[year!=2013] 

# ----------------------
# create a function that reads in the data

# art specific data set
art = og[art_site==TRUE]
test = copy(og)

# reset the name of the outcome variable to be comparable
setnames(art, 'arvs', 'value')
setnames(test, 'test_kits', 'value')

# ----------------------  
# select arv or test kit data - 'art' or 'test' 

type = 'art'
dt = copy(art) # change to fit outcome of interest

# ---------------------------------------
# create tables

# subset to 2017 and 2018
dt = dt[year==2017 | year==2018]

# for tables showing 2018 and 2019, Jan - Sept (inclusive)
# dt = dt[year==2018 | year==2019]
# dt = dt[month(date)!=10 & month(date)!=11 & month(date)!=12]

# ----------------------  
# calculate the columns

# number of facilities by year
facilities = dt[ ,.(facilities=length(unique(facility))), by=.(region, year)]

# percent reporting
reports = dt[!is.na(test_kits),.(reporting=length(unique(facility))), by=.(region, date, year)]
reports = merge(reports, facilities, by=c('year', 'region'), all.x=T)
reports[ , percent_reporting:=100*(reporting/facilities)]
reports = reports[ ,.(mean_reports = mean(percent_reporting)), by=.(region, year)]
reports[ ,mean_reports:=round(mean_reports, 1)]

# merge facilities and reporting
reports = merge(facilities, reports, by=c('year', 'region'), all=T)

# ----------------------  
# reshape wide with columns for each separate year

f17 = reports[year==2017]
f18 = reports[year==2018]

setnames(f17, c('facilities', 'mean_reports'), 
         c('facilities17', 'mean_reports17'))
setnames(f18, c('facilities', 'mean_reports'), 
         c('facilities18', 'mean_reports18'))
f17[ ,year:=NULL]
f18[ ,year:=NULL]

# final merged reshape
reports = merge(f17, f18, by='region')

# reorder - total facilities and mean weekly percent reporting 
reports = reports[ ,.(facilities17, facilities18, mean_reports17, mean_reports18),
                   by=region]

# -------------------------------------------------
# mean weeks out, number of stock outs, and pecent of time out 

# mean weeks out of stock per facility
total_out = dt[ ,.(out=sum(value, na.rm=T)), by=.(year, region)]
total_out = merge(facilities, total_out, by=c('year', 'region'))
total_out[ ,mean_out:=round(out/facilities, 1)]

# -------------------------------------------------

#--------------------------------
# duration of stock outs 



