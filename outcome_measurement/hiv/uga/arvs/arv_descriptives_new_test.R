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
dt[ ,month:=month(date)]

#---------------------------------------------
# ARV descriptives in the text
art = dt[art_site==TRUE & (year==2017 | year==2018)]

#---------------------------------------------
# facilities with 0 weeks out of stock

out = art[month!=12 & year==2018]
out = out[!is.na(arvs),.(arvs = sum(arvs, na.rm=T)), by=facility]
out = out[ ,.(facilities=length(unique(facility))), by=arvs]

# no stock outs
out[arvs==0, sum(facilities)]
out[4 <= arvs, sum(facilities)]
out[ ,sum(facilities)]

#--------------------------------------
# percentage of facility-weeks stocked out by facility
art[!is.na(arvs) , arv_report:=TRUE]
art[is.na(arvs), arv_report:=FALSE]

# percent of facility weeks
dec = art[month!=12,.(arvs=sum(arvs, na.rm=T), arv_report=sum(arv_report)), 
          by=.(facility, year)]

dec = dec[order(facility)]

# check that there are two entries for each facility
dec[ , count:=.N, by=facility] # six facilities reported only in 2018
dec = dec[count==2]

# calculate the roc
dec[ ,arv_ratio:=arvs/arv_report]
dec[year==2018, arv_roc:=(arv_ratio - shift(arv_ratio))]


# facilities that experienced a stockout
elim = art[ month!=12,.(test=sum(arvs, na.rm=T)), by=facility]
elim = elim[test!=0]
dec = dec[facility %in% elim$facility]

new[year==2018 & 0 < arv_roc ]

#-------------------------------------
# percent of facilities out of stock

# arvs
month = art[year==2018 & arvs==TRUE, .(fac=length(unique(facility))), by=month ]
rep = art[year==2018 & arv_report==TRUE, .(rep=length(unique(facility))), by=month ]
rep = merge(month, rep, by='month')
rep[ , ratio:=100*(fac/rep)]

# test kits
tmonth = dt[year==2018 & test_kits==TRUE, .(fac=length(unique(facility))), by=month]
trep = dt[year==2018 & !is.na(test_kits), .(rep=length(unique(facility))), by=month]
trep = merge(tmonth, trep, by='month')





