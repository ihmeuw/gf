# ARV stockouts by facility - visualize the data 

# Caitlin O'Brien-Carelli
# 12/18/2018
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
# dt = readRDS(paste0(dir, 'arv_stockouts_2013_2018.rds'))

dt = readRDS("C:/Users/ccarelli/Documents/arv_stockout_analyses/arv_stockouts_2013_2018.rds")

# subset dates to before November 2018
dt = dt[year!=2013] 

#---------------------------------------------
# ARV stockout table
art = dt[art_site==TRUE & (year==2017 | year==2018)]

#--------------
# facilities reporting, by region and total
art[!is.na(arvs) & year==2017 | year==2018, .(report=length(unique(facility))), by=region][order(region)]
art[!is.na(arvs) & year==2017 | year==2018, .(report=length(unique(facility)))]

#--------------
# mean monthly percent of facilities reporting
report = art[!is.na(arvs) & year==2017 | year==2018, .(report=length(unique(facility))), by=.(month, year, region)]
all_sites = art[year==2017 | year==2018, .(sites=length(unique(facility))), by=.(year, region)]
report = merge(report, all_sites, by=c('year', 'region'), all.x=TRUE)
report[ , ratio:=round(100*(report/sites), 1)]

# mean annual ratio 
report[ ,.(avg=round(mean(ratio), 1)), by=year] # take the total annual reporting ratio first
report = report[ ,.(avg=round(mean(ratio), 1)), by=.(year, region)][order(region)]
report[year==2017]
report[year==2018]

#--------------
# mean weeks stocked out per site

# subset to 2017/18 and only months Jan - Nov
wks = dt[art_site==TRUE & month!=12 & (year==2017 | year==2018)]

# calculate total stock out weeks and total facilities reporting
total_wks = wks[!is.na(arvs),.(arvs=sum(arvs, na.rm=T), facilities=length(unique(facility))), by=year]
total_wks[ ,ratio:=round(arvs/facilities, 1)]
total_wks

# calculate stock out weeks by facilities reporting by region
wks = wks[!is.na(arvs),.(arvs=sum(arvs, na.rm=T), facilities=length(unique(facility))), by=.(region, year)]
wks[ , ratio:=round(arvs/facilities, 1)]
wks[year==2017][order(region)]
wks[year==2018][order(region)]

#--------------
# percent of facility-weeks stocked out 
art[!is.na(arvs), report:=TRUE]
art[is.na(report), report:=FALSE]

# calculate regional percentage of facility-weeks stocked out
fac = art[!is.na(arvs) & (year==2017 | year==2018),.(arvs=sum(arvs, na.rm=T), report=sum(report)), by=.(region, year)]
fac[ , ratio:=round(100*arvs/report, 1)]
fac[year==2017][order(region)]
fac[year==2018][order(region)]

# percent of total facility-weeks stocked out 
art[!is.na(arvs) & (year==2017 | year==2018),.(100*(sum(arvs, na.rm=T)/sum(report))), by=year]

#--------------

#---------------------------------------------
# test kit stockout table
tk = dt[year==2017 | year==2018]

#--------------
# facilities reporting, by region and total
tk[!is.na(test_kits), .(report=length(unique(facility))), by=region][order(region)]
tk[!is.na(test_kits), .(report=length(unique(facility)))]

#--------------
# mean monthly percent of facilities reporting
t_report = tk[!is.na(test_kits), .(report=length(unique(facility))), by=.(month, year, region)]
t_sites = tk[ , .(sites=length(unique(facility))), by=.(year, region)]
t_report = merge(t_report, t_sites, by=c('year', 'region'), all.x=TRUE)
t_report[ , ratio:=100*(report/sites)]

# mean annual ratio 
t_report[ ,.(avg=round(mean(ratio), 1)), by=year] # take the total annual reporting ratio first
t_report = t_report[ ,.(avg=round(mean(ratio), 1)), by=.(year, region)][order(region)]
t_report[year==2017]
t_report[year==2018]

#--------------
# mean weeks stocked out per site

# subset to 2017/18 and only months Jan - Nov
twks = dt[month!=12 & (year==2017 | year==2018)]

# calculate total stock out weeks and total facilities reporting
ttotal_wks = twks[!is.na(test_kits),.(test_kits=sum(test_kits, na.rm=T), facilities=length(unique(facility))), by=year]
ttotal_wks[ ,ratio:=round(test_kits/facilities, 1)]
ttotal_wks

# calculate stock out weeks by facilities reporting by region
twks = twks[!is.na(test_kits),.(test_kits=sum(test_kits, na.rm=T), facilities=length(unique(facility))), by=.(region, year)]
twks[ , ratio:=round(test_kits/facilities, 1)]
twks[year==2017][order(region)]
twks[year==2018][order(region)]

#--------------
# percent of facility-weeks stocked out 
tk[!is.na(test_kits), report:=TRUE]
tk[is.na(report), report:=FALSE]

# calculate regional percentage of facility-weeks stocked out
fac = tk[!is.na(test_kits) ,.(test_kits=sum(test_kits, na.rm=T), report=sum(report)), by=.(region, year)]
fac[ , ratio:=round(100*test_kits/report, 1)]
fac[year==2017][order(region)]
fac[year==2018][order(region)]

# percent of total facility-weeks stocked out 
tk[!is.na(test_kits),.(100*(sum(test_kits, na.rm=T)/sum(report))), by=year]

#--------------

#------------------------------------------------
# descriptives in the text

# districts in which stock outs increased
# measure by weeks and mean weeks stocked out per facility


# mean weeks stocked out per site

# subset to 2017/18 and only months Jan - Nov
wks = dt[art_site==TRUE & month!=12 & (year==2017 | year==2018)]

# calculate total stock out weeks and total facilities reporting
total_wks = wks[!is.na(arvs),.(arvs=sum(arvs, na.rm=T), facilities=length(unique(facility))), by=year]
total_wks[ ,ratio:=round(arvs/facilities, 1)]
total_wks

# calculate stock out weeks by facilities reporting by region
wks = wks[!is.na(arvs),.(arvs=sum(arvs, na.rm=T), facilities=length(unique(facility))), by=.(region, year)]
wks[ , ratio:=round(arvs/facilities, 1)]
wks[year==2017][order(region)]
wks[year==2018][order(region)]







