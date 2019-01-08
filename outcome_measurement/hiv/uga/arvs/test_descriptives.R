# ARV stockouts by facility - visualize the data 

# Caitlin O'Brien-Carelli
# 12/19/2018
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

# subset to only 2017 and 2018
dt = dt[year==2017 | year==2018]
dt[ , month:=month(date)]

#------------------------------------------------
# descriptives in the text

# districts in which stock outs increased
# measure by weeks and mean weeks stocked out per facility

#--------------------
# mean weeks stocked out

# arvs
# subset to 2017/18 and only months Jan - Nov
wks = dt[art_site==TRUE & month!=12]

# calculate stock out weeks by facilities reporting by region
wks = wks[!is.na(arvs),.(arvs=sum(arvs, na.rm=T), facilities=length(unique(facility))), by=.(district, year)]
wks[ , ratio:=round(arvs/facilities, 1)]
wks[ , roc:=(ratio - shift(ratio))]
wks = wks[year==2018]

# test kits
twks = dt[month!=12]

# calculate stock out weeks by facilities reporting by region
twks = twks[!is.na(test_kits),.(test_kits=sum(test_kits, na.rm=T), facilities=length(unique(facility))), by=.(district, year)]
twks[ , ratio:=round(test_kits/facilities, 1)]
twks[ , roc:=(ratio - shift(ratio))]
twks = twks[year==2018]

wks = wks[ ,.(district, roc)]
twks = twks[ ,.(district, troc=roc)]
wks = merge(wks, twks, by='district')

# positive change in stockout rates
wks[0 < roc ] # 33
wks [0 < troc] # 40


wks[0 < roc | 0 < troc] # 54
wks[0 < roc & 0 < troc] # 19

# decrease in stockouts (2018 minus 2017 is negative)
wks[troc < 0]
wks[ ,length(unique(district))]

# by region


#--------------------
# facilities experiencing at least one stock out

# decrease in facilities with at least one stock out
dt[arvs==T,.(length(unique(facility))), by=year]
dt[test_kits==T & month!=12,.(length(unique(facility))), by=year]

#arvs
one = dt[arvs==T,.(facility=length(unique(facility))), by=.(district, year)]
one[ ,roc:=(facility - shift(facility))]
one[year==2018 & 0 < roc]

#test kits
tone = dt[test_kits==T,.(facility=length(unique(facility))), by=.(district, year)]
tone[ ,roc:=(facility - shift(facility))]
tone[year==2018 & 0 < roc]

#-------------------------------
# percentage of facility-weeks per district - roc

dt[!is.na(test_kits), test_report:=TRUE]
dt[is.na(test_kits), test_report:=FALSE]

dt[!is.na(arvs) & art_site==TRUE, arv_report:=TRUE]
dt[is.na(arvs), arv_report:=FALSE]

new = dt[ ,.(arvs=sum(arvs, na.rm=T), test_kits=sum(test_kits, na.rm=T),
                      test_report=sum(test_report), arv_report=sum(arv_report)), 
                    by=.(district, year)]

# one district is in 2018 but not 2017
x = unique(new[year==2018]$district)
y = unique(new[year==2017]$district)
x[!(x %in% y)]
new = new[district!='Ssembabule']

new[ ,arv_ratio:=arvs/arv_report]
new[ ,test_ratio:=test_kits/test_report]
new[ ,arv_roc:=(arv_ratio - shift(arv_ratio))]
new[ ,test_roc:=(test_ratio - shift(test_ratio))]

new[year==2018 & test_roc < 0]

#-----------------------------
# per region


new2 = dt[ ,.(arvs=sum(arvs, na.rm=T), test_kits=sum(test_kits, na.rm=T),
             test_report=sum(test_report), arv_report=sum(arv_report)), 
          by=.(region, year)]



new2[ ,arv_ratio:=arvs/arv_report]
new2[ ,test_ratio:=test_kits/test_report]
new2[ ,arv_roc:=(arv_ratio - shift(arv_ratio))]
new2[ ,test_roc:=(test_ratio - shift(test_ratio))]

new2[year==2018 & test_roc < 0]



#-------------------------------
# prolonged or frequent stock outs

# number of facilities with four or more weeks stocked out
out = dt[month!=12]
out = out[ ,.(test_kits = sum(test_kits, na.rm=T)), by=.(facility, year)]
out = out[ ,.(facilities=length(unique(facility))), by=.(test_kits, year)]

out[8 <= test_kits & year==2017, sum(facilities)]
out[8 <= test_kits & year==2018, sum(facilities)]

# no stock outs
out[0 == test_kits & year==2018, sum(facilities)]

# at least one stock out
out[8 <= test_kits & year==2018, sum(facilities)]
#----------------------------------------
# test kit stockout duration

for (f in unique(dt$facility)) {
  dt[facility==f, count:=seq_len(.N), by=rleid(test_kits)]
  dt[facility==f, group:=rleid(test_kits)]
  dt[test_kits!=T, group:=NA] 
  dt[facility==f & !is.na(group), duration:=max(count), by=group]
  dt[ , count:=NULL]
}

dt[is.na(test_kits) | test_kits==FALSE, duration:=NA]
dt[month!=12, max(duration, na.rm=T), by=year]

#----------------------------------------
# districts with the highest mean stockouts per district

# arvs
awks = dt[art_site==T & year==2018 & !is.na(arvs),.(arvs=sum(arvs, na.rm=T), 
            facilities=length(unique(facility))), by=.(district, year)]
awks[ , ratio:=round(arvs/facilities, 1)]

# test kits
tdist = dt[year==2018 & !is.na(test_kits),.(test_kits=sum(test_kits, na.rm=T), 
                        facilities=length(unique(facility))), by=.(district, year)]
tdist[ , ratio:=round(test_kits/facilities, 1)]

#----------------------------------------
# districts with the highest percentage of facility weeks stocked out

high = dt[ ,.(arvs=sum(arvs, na.rm=T), test_kits=sum(test_kits, na.rm=T),
             test_report=sum(test_report), arv_report=sum(arv_report)), 
          by=.(district, year)]

# calculate the percent of facility-weeks stocked out by district
high[ ,test_ratio:=round(100*test_kits/test_report, 1)]

# view highest percentage facilities
View(high[year==2018])

#------------------------------------------
# stock out frequency and duration 

# create a data set of just stockouts
dur= dt[year==2018 & month!=12 & test_kits==T]

# create a single maximum value
# shows the number of stockouts per facility and the duration of each
dur = dur[ ,.(length=unique(duration)), by=.(facility, group, district)]


dur[ , mean(length)]

#--------------
# intro paragraph on test kits


test = dt[test_kits==TRUE & month!=12 & (year==2017 | year==2018)]
test[ ,length(unique(facility)), by=year]


