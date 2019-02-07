# ARV stockouts by facility - visualize the data 

# Caitlin O'Brien-Carelli
# 1/8/2019
# Order of code should be updated to reflect text

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
dt = readRDS(paste0(dir, 'arv_stockouts_2013_2018.rds'))

# subset dates to before November 2018
dt = dt[year!=2013] 
dt[ , month:=month(date)]

# art specific data set
art = dt[art_site==TRUE]

#---------------------------------------------------------------
# ARV STOCKOUT TABLE

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
# TEST KIT STOCKOUT TABLE 
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

#---------------------

#-----------------------------------------
# DESCRIPTIVES IN THE TEXT

#---------------------------------------------
# calculate rates of change in weeks stocked out by year, district
new = dt[ ,.(test_kits=sum(test_kits, na.rm=T)), by=.(year, district)]
new = new[year==2017 | year==2018]
new = dcast(new, district~year)
setnames(new, (c('district', 'y2017', 'y2018')))
new[ ,roc:=(y2018 - y2017)]

#---------------------------------------------
# facilities with 0 weeks out of stock

out = art[month!=12 & year==2018]
out = out[!is.na(arvs),.(arvs = sum(arvs, na.rm=T)), by=facility]
out = out[ ,.(facilities=length(unique(facility))), by=arvs]

# no stock outs
out[arvs==0, sum(facilities)]
out[4 <= arvs, sum(facilities)] # less than a month stocked out
out[ , sum(facilities)]

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

new[0 < roc] # positive rate of change between 

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

#----------------------------------------------
# rates of change in facility-weeks per year
stockout[ , year2:=paste0('n', year)]
roc = dcast(data = stockout, id ~ year2)
roc[ , change:=(n2018 - n2017)]
roc_map = merge(coord, roc, by='id')

# only districts with more stockouts in 2018 than 2017
roc_map_alt = merge(coord, roc, by='id')
roc_map_alt[change <=0, change:=NA]

# percentage of weeks stocked out
stock = dt[art_site==TRUE, .(weeks_out=sum(arvs, na.rm=T)), by=.(year, id)]
dt[art_site==TRUE & !is.na(arvs), reported:=TRUE]
stock_add = dt[art_site==TRUE , .(total_weeks=sum(reported, na.rm=T)), by=.(year, id)]
stock = merge(stock, stock_add, by=c('year', 'id'))
stock[ , percent_out:=round(100*(weeks_out/total_weeks), 1)]
stock = merge(stock, coord_ann, by=c('id', 'year'))


# descriptive stats

x = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01' &  art_site==TRUE, .(weeks=sum(arvs, na.rm=T)), by=.(year, facility)]

x[weeks > 4, length(unique(facility)), by=year]

# mean weeks stocked out
dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]
dt[year==2017 & month < '2017-10-01' & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]

out = dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility))), by=district]
out[ , ratio:=arvs/V2]
View(out)


dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]
dt[year==2017 & month < '2017-10-01' & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]

out = dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility))), by=district]
out[ , ratio:=arvs/V2]

setnames(out, c('arvs', 'V2', 'ratio'), c('weeks_stocked_out_of_ARVs', 'art_sites_reporting', 'mean_weeks_stocked_out_per_site'))


# mean weeks stocked out 
# number of weeks of stockout divided by art sites reporting 
art_sites = dt[!is.na(arvs) & art_site==TRUE, .(art_sites=length(unique(facility))), byyear, id)]
art_sites = merge(stockout, art_sites)
art_sites[ , mean_weeks:=round((value/art_sites), 1)]
arv_map_norm = merge(art_sites, coord_ann, by=c('id', 'year'), all.y=TRUE)





# mean weeks stocked out
dt[year==2018, .(tks=sum(test_kits, na.rm=T), length(unique(facility)))]
dt[year==2017 & month < '2017-10-01', .(tks=sum(test_kits, na.rm=T), length(unique(facility)))]

out2 = dt[year==2018 , .(tks=sum(test_kits, na.rm=T), length(unique(facility))), by=district]
out2[ , ratio:=tks/V2]
setnames(out2, c('tks', 'V2', 'ratio'), c('weeks_stocked_out_of_test_kits', 'facilities_reporting', 'mean_weeks_stocked_out_per_facility'))


dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]
dt[year==2017 & month < '2017-10-01' & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility)))]

out = dt[year==2018 & art_site==TRUE, .(arvs=sum(arvs, na.rm=T), length(unique(facility))), by=district]
out[ , ratio:=arvs/V2]
View(out)

out = merge(out, out2, by='district')

write.csv(out, paste0(dir, 'mean_weeks_stocked_out.csv'))


#-------------------------------------------
# STOCK OUT DURATION 

#--------------------------------
# duration of stock outs 
cols = c('facility', 'date')
setorderv(dt, cols)

for (f in unique(dt$facility)) {
  dt[facility==f, count:=seq_len(.N), by=rleid(test_kits)]
  dt[facility==f, group:=rleid(test_kits)]
  dt[test_kits!=T, group:=NA] 
  dt[facility==f & !is.na(group), duration:=max(count), by=group]
  dt[ ,c('count', 'group'):=NULL]
}

#------------------------------------
regions = fread(paste0(root, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
regions = regions[ ,.(region = region10_name, district = dist112_name)]
regions = regions[!duplicated(district)]
dt = merge(dt, regions, by='district', all.x=T)

#-------------------------------------
# descrtipve statistics

# percentage of weeks stocked out
stock = dt[art_site==TRUE, .(weeks_out=sum(arvs, na.rm=T)), by=.(year, id)]
dt[art_site==TRUE & !is.na(arvs), reported:=TRUE]
stock_add = dt[art_site==TRUE , .(total_weeks=sum(reported, na.rm=T)), by=.(year, id)]
stock = merge(stock, stock_add, by=c('year', 'id'))
stock[ , percent_out:=round(100*(weeks_out/total_weeks), 1)]
stock = merge(stock, coord_ann, by=c('id', 'year'))

art[!is.na(arvs), reported:=TRUE]
y = art[ ,.(stockout=sum(arvs, na.rm=T), total=sum(reported, na.rm=T)), by=.(region, year)]
y = y[order(region)]
y[ ,ratio:=round(100*stockout/total, 1)]
y[year==2017]

art[!is.na(arvs), reported:=TRUE]
z = art[ ,.(stockout=sum(arvs, na.rm=T), total=sum(reported, na.rm=T)), by=.(region, date)]
z = z[order(region)]
z[ , ratio:=100*stockout/total]

#-------------------------------
# test kits table

# sort by region to make tables in regional order
dt = dt[order(region)]

# total number of health facilities per region
dt[ , length(unique(facility)), by=region]

# mean monthly % of facilities reporting 
tot = dt[ , .(total=length(unique(facility))), by=region]
rep = dt[!is.na(test_kits), .(reporting=length(unique(facility))), by=.(month, year, region)]
rep = merge(rep, tot, by='region', all.x=T)
rep[ , percent_report:=100*(reporting/total)]

rep[year==2017, round(mean(percent_report), 1), by=region]
rep[year==2018, round(mean(percent_report), 1), by=region]

# all regions 
rep[ ,mean(percent_report), by=year]
#-------------------
# mean weeks stocked out per facility

# subset to the same weeks in 2017/18
wk = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01' ]

# calculate the number of weeks stocked out per facility
out = wk[ ,.(stockout=sum(test_kits, na.rm=T)), by=.(region, year)]
fac = wk[ ,.(facilities=(length(unique(facility)))), by=.(region, year)]

out = merge(out, fac, by=c('region', 'year'), all.x=T)
out[ , weeks_per_facility:=round(stockout/facilities, 1)]

out[year==2017]
out[year==2018]

# totals - mean weeks stocked out per facility, all regions
out[ ,.(sum(stockout), sum(facilities),
        round(sum(stockout)/sum(facilities), 1) ), by=year]
#---------------------
# percentage of facility-weeks stocked out 

# weeks in which the facility reported
wk[!is.na(test_kits), reported:=TRUE]
report = wk[ ,.(reported=sum(reported, na.rm=T)), by=.(region, year)]

# weeks in which the facilities were stocked out
out2 = out[ , .(region, year, stockout)]

fac_weeks = merge(out2, report, by=c('region', 'year'))
fac_weeks[ , weeks_out:=round(100*(stockout/reported), 1)]

fac_weeks[year==2017]
fac_weeks[year==2018]

# all regions
fac_weeks[ ,.(sum(stockout), sum(reported), (100*sum(stockout/sum(reported)))), by=year]

#------------------------------------
# intro paragraph

total_out = wk[ ,.(weeks_out=sum(test_kits, na.rm=T)), by=.(year, facility, district)]
total_out[weeks_out==0 ,.(length(unique(facility)), zeroes=(100*length(unique(facility))/1564)), by=year]
total_out[0 < weeks_out & weeks_out <= 3,.(length(unique(facility)), zeroes=(100*length(unique(facility))/1564)), by=year]
total_out[12 <= weeks_out ,.(length(unique(facility)), (100*length(unique(facility))/1564)), by=year]
total_out[year==2018 & 1 <= weeks_out & weeks_out <= 3, length(unique(facility)) ]

bad = total_out[4 <= weeks_out & year!=2016 ] 

#-----------------------------------
# highest districts 

# mean weeks stocked out per facility

# subset to the same weeks in 2017/18
wk = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01' ]

# calculate the number of weeks stocked out per facility
out = wk[ ,.(stockout=sum(test_kits, na.rm=T)), by=.(district, year)]
fac = wk[ ,.(facilities=(length(unique(facility)))), by=.(district, year)]

out = merge(out, fac, by=c('district', 'year'), all.x=T)
out[ , weeks_per_facility:=round(stockout/facilities, 1)]

eight = out[year==2018]

# weeks in which the facility reported
wk[!is.na(test_kits), reported:=TRUE]
report = wk[ ,.(reported=sum(reported, na.rm=T)), by=.(district, year)]

# weeks in which the facilities were stocked out
out2 = out[ , .(district, year, stockout)]

fac_weeks = merge(out2, report, by=c('district', 'year'))
fac_weeks[ , weeks_out:=round(100*(stockout/reported), 1)]

#----------------------------------------------------
# ART

art = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01' & art_site==TRUE ]
art[!is.na(arvs), reported:=TRUE]

total_out = art[year!=2016 ,.(weeks_out=sum(arvs, na.rm=T)), by=.(year, facility, district)]


total_out[weeks_out==0 ,.(length(unique(facility)), zeroes=(100*length(unique(facility))/1286)), by=year]
total_out[ 0 < weeks_out,.(length(unique(facility)), zeroes=(100*length(unique(facility))/1286)), by=year]

total_out[year==2018 & 4 <= weeks_out , length(unique(facility))]
total_out[year==2018 & 8 <= weeks_out , length(unique(facility))]
total_out[ 8 <= weeks_out, mean(weeks_out) ]

outs = total_out[weeks_out > 0]
outs[year==2018,length(unique(facility)), by=weeks_out]
outs[4 <= weeks_out & year==2018, length(unique(facility)) ]


total_out[0 < weeks_out & weeks_out <= 3,.(length(unique(facility)), zeroes=(100*length(unique(facility))/1564)), by=year]


#-----------------------------------------------
# district stock outs 

art = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01' & art_site==TRUE ]
art[!is.na(arvs), reported:=TRUE]

out = art[year!=2016 ,.(weeks_out=sum(arvs, na.rm=T), facilities=length(unique(facility)) ), by=.(year, district)]

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

#------------------------------------------







