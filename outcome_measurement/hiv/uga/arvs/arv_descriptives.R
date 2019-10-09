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
# reset the number of stockouts and the duration to be comparable 
setnames(art, 'arvs', 'value')
setnames(test, 'test_kits', 'value')
setnames(art, c('adur', 'astock'), c('dur', 'stock'))
setnames(test, c('tdur', 'tstock'), c('dur', 'stock'))
# ----------------------  
# select arv or test kit data - 'art' or 'test' 

type = 'test'
dt = copy(test) # change to fit outcome of interest

# choose 2017/18 or 2018/19
now = TRUE

#----------------------------
# for 2017 and 18 tables
if (now == FALSE) {
  dt = dt[year==2017 | year==2018]
  year1 = 2017
  year2 = 2018
} else {
  # for tables showing 2018 and 2019, Jan - Sept (inclusive)
  dt = dt[year==2018 | year==2019]
  dt = dt[month(date) < 9] 
  year1 = 2018
  year2 = 2019 }

# ---------------------------------------
# create tables
# ----------------------  
# calculate the columns

# number of facilities by year
facilities = dt[ ,.(facilities=length(unique(facility))), by=.(region, year)]

# percent reporting
reports = dt[!is.na(value),.(reporting=length(unique(facility))), by=.(region, date, year)]
reports = merge(reports, facilities, by=c('year', 'region'), all.x=T)
reports[ , percent_reporting:=100*(reporting/facilities)]
reports = reports[ ,.(mean_reports = mean(percent_reporting)), by=.(region, year)]
reports[ ,mean_reports:=round(mean_reports, 1)]

# merge facilities and reporting
reports = merge(facilities, reports, by=c('year', 'region'), all=T)

# ----------------------  
# reshape wide with columns for each separate year

f1 = reports[year==year1]
f2 = reports[year==year2]

setnames(f1, c('facilities', 'mean_reports'), 
         c('facilities1', 'mean_reports1'))
setnames(f2, c('facilities', 'mean_reports'), 
         c('facilities2', 'mean_reports2'))
f1[ ,year:=NULL]
f2[ ,year:=NULL]

# final merged reshape
reports = merge(f1, f2, by='region')

# reorder - total facilities and mean weekly percent reporting 
reports = reports[ ,.(facilities1, facilities2, mean_reports1, mean_reports2),
                   by=region]

# -------------------------------------------------
# mean weeks out, number of stock outs, and pecent of time out 

# mean weeks out of stock per facility
total_out = dt[ ,.(out=sum(value, na.rm=T)), by=.(year, region)]
total_out = merge(facilities, total_out, by=c('year', 'region'))
total_out[ ,mean_out:=round(out/facilities, 1)]

#--------------------------------
# mean annual count and duration of stock outs 

#----------
# mean annual number of stock outs
# take the maximum value of sequentially counted stock outs per facility
count = dt[!is.na(stock), .(total_stockouts = max(stock, na.rm=T)), 
           by=.(facility, year, region)]

# take the mean of the total count of stock outs per facility
count = count[ ,.(mean_number_of_stockouts=mean(total_stockouts)), by=.(region, year)]
count[ , mean_number_of_stockouts:=round(mean_number_of_stockouts, 1)]

#----------
# mean duration of stock outs

# stock out duration for each stock out
dur = dt[!is.na(dur), .(dur_of_each = max(dur, na.rm=T)),
         by=.(stock, facility, region, year)]

# mean duration per region
dur = dur[ ,.(mean_duration=round(mean(dur_of_each), 1)), by=.(region, year)]

#----------
# combine them

count_dur = merge(count, dur, by=c('region', 'year'), all=T)
count_dur = merge(total_out, count_dur, by=c('region', 'year'), all=T)
count_dur[ ,c('facilities', 'out'):=NULL]
#------------
# reshape 

c1 = count_dur[year==year1]
c2 = count_dur[year==year2]  
c1[ ,year:=NULL]
c2[ ,year:=NULL]

# reset the names to reflect the year
setnames(c1, c("region", "mean_out1", "mean_number_of_stockouts1", "mean_duration1"))
setnames(c2, c("region", "mean_out2", "mean_number_of_stockouts2", "mean_duration2"))

# merge and reorder the columns
count_dur = merge(c1, c2, by='region', all=T)
count_dur = count_dur[ ,.(mean_out1, mean_out2, mean_number_of_stockouts1,
                          mean_number_of_stockouts2, mean_duration1, mean_duration2), 
                       by=region]

#-----------------------------------------
# merge it all together
final = merge(reports, count_dur, by='region')

#-----------------------------------------
# export the table

min_year = dt[ , min(year(date))]
max_year = dt[ , max(year(date))]

write.csv(final, paste0(dir, 'outputs/', type, 
  '_stockout_stats_by_region_', min_year, '_', max_year, '.csv'))

#-----------------------------------------
# create lines of totals and export separately 

# total facilities
fac_tot = dt[, .(facilities=length(unique(facility))), by=year]

# mean weekly percent reporting
report_tot = dt[!is.na(value),.(reporting=length(unique(facility))), by=.(date, year)]
report_tot = merge(report_tot, fac_tot, by='year', all.x=T)
report_tot[ , percent_reporting:=100*(reporting/facilities)]
report_tot = report_tot[ ,.(mean_reports = round(mean(percent_reporting), 1)), by=year]
tots = merge(fac_tot, report_tot, by='year', all=T)

# mean weeks out of stock per facility
out_tot = dt[ ,.(out=sum(value, na.rm=T)), by=year]
tots = merge(tots, out_tot, by='year', all=T)
tots[ ,mean_out:=round(out/facilities, 1)]
tots[ , out:=NULL]

# mean annual number of stock outs
# take the maximum value of sequentially counted stock outs per facility
# then take the mean of those maximums (mean of the total number of stock outs per facility)
count_tot = dt[!is.na(stock), .(total_stockouts = max(stock, na.rm=T)), 
           by=.(facility, year)]
count_tot = count_tot[ ,.(mean_number_of_stockouts=round(mean(total_stockouts), 1)),
                       by=year]

# mean duration of stock outs
dur_tot = dt[!is.na(dur), .(dur_of_each = max(dur, na.rm=T)),
           by=.(stock, facility, year)]
dur_tot = dur_tot[ ,.(mean_duration=round(mean(dur_of_each), 1)), by=year]

# merge in the last two metrics
tots = merge(tots, count_tot, by='year', all=T)
tots = merge(tots, dur_tot, by='year', all=T)

# facility-weeks out of stock 
fac_wks_tot = dt[!is.na(value) ,.(out=sum(value, na.rm=T), 
              report=length(value)), by=year]
fac_wks_tot[ ,perc_wks_out:=round(100*(out/report), 1)]
fac_wks_tot[ ,c('out', 'report'):=NULL]

# merge in the last set of totals
tots = merge(tots, fac_wks_tot, by='year', all=T)

#-----------------------------------------
# reshape the totals to be appended to the region-stratified tables




















