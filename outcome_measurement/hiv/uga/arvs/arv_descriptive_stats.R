# Descriptive statistics - ARV stock outs

# Caitlin O'Brien-Carelli
# 10/30/2018
# ----------------------
# Set up R
rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(maptools)
library(plyr)

# ----------------------
# home drive 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(root,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# working directory to aggregate
dt = readRDS(paste0(dir, 'arv_stockouts_2016_2018.rds'))

# subset dates to before september 30, 2018
dt = dt[year!=2013]

#------------------------------
# merge in regions
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
# descrtipve statiss

x = dt[!is.na(test_kits), .(reporting=length(unique(facility))), by=.(date, month, year)]
x[ , mean(reporting), by=year]
y = x[year==2018, mean(reporting), by=date]
y[ ,percent:=V1/1564]
y[order(V1)]

count = dt[!is.na(test_kits) & month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01', .(reporting=length(unique(facility))), by=.(month, year)]
count[, percent:=reporting/1564 ]
count[order(percent)]
count[ ,mean(reporting), by=year]

art= dt[art_site==T]
x = art[!is.na(arvs), .(reporting=(length(unique(facility)))), by=.(date, year)]
x[ ,mean(reporting), by=year]

y = art[!is.na(arvs), .(reporting=(length(unique(facility)))), by=.(month, year)]

art = dt[art_site==T & !is.na(arvs), .(reporting=(length(unique(facility)))), by=.(date, year)]

dt[all(is.na(arvs)), unique(facility)]

z = art[!is.na(arvs) & month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01', .(reporting=(length(unique(facility)))), by=.(month, year)]

#-------------------------------------------


art = dt[art_site==T]

x = art[!is.na(arvs) ,.(sites=length(unique(facility))), by=.(month, year, region)]
y = art[ ,.(total=length(unique(facility))), by=.(month, year, region)]
x = merge(x, y, by=c('month', 'year', 'region'))

p = x[ ,.(round(100*sum(sites)/sum(total), 1)), by=.(region, year)]
p[year==2017, mean(V1), by=region]


y = x[year==2017, mean(sites), by=region]
y[ order(region)]


stock = art [art_site==T & !is.na(arvs) & month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01', .(weeks=sum(arvs), facilities=length(unique(facility))), by=.(region, year)]
stock[ ,ratio:=round(weeks/facilities, 1)]
stock = stock[order(region)]


stock[year==2017]


art[ ,nrow]


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


z = melt(z, id.vars=c('region', 'date'))

ggplot(z, aes(x=date, y=ratio, color=region)) +
  geom_line() + 
  geom_point()




x = dt[,length(unique(facility)), by=region]
x = x[order(region)]

z = dt[!is.na(test_kits), .(reported=length(unique(facility))), by=.(month, year, region)]
r = dt[, .(total=length(unique(facility))), by=.(month, year, region)]
z = merge(z, r, by=c('month', 'year', 'region'))

z[ ,ratio:=round(100*reported/total, 1)]

z = z[order(region)]

z[year==2017]


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



bad = total_out[4 <= weeks_out & year!=2016 ] 



wks = art[year!=2016 ,.(weeks_out=sum(arvs, na.rm=T), reported=sum(reported, na.rm=T)), by=.(year, facility, district)]

wks[ , fac_ratio:=(100*weeks_out/reported)]

sev = wks[year==2017]
setnames(sev, c('weeks_out', 'reported', 'fac_ratio'), c('weeks_out17', 'reported17', 'fac_ratio17'))
eig = wks[year==2018]
setnames(eig, c('weeks_out', 'reported', 'fac_ratio'), c('weeks_out18', 'reported18', 'fac_ratio18'))

sev[ ,year:=NULL]
eig[ ,year:=NULL]


comp = merge(sev, eig, by=c('facility', 'district'), all=TRUE)

comp[ ,roc:=fac_ratio18 - fac_ratio17]

# comp = comp[!(weeks_out17==0 & weeks_out18==0)]

comp[roc < 0 ]
comp[roc==0]
comp[0 < roc]




x = art[year!=2016 ,.(weeks_out=sum(arvs, na.rm=T), reported=sum(reported, na.rm=T)), by=.(year, district)]

x[ , ratio:=(100*weeks_out/reported)]

sev = x[year==2017]
setnames(sev, c('weeks_out', 'reported', 'ratio'), c('weeks_out17', 'reported17', 'ratio17'))
eig = x[year==2018]
setnames(eig, c('weeks_out', 'reported', 'ratio'), c('weeks_out18', 'reported18', 'ratio18'))

sev[ ,year:=NULL]
eig[ ,year:=NULL]

dist = merge(sev, eig, by='district')
dist[ ,roc:=ratio17 - ratio18]
dist[ roc < 0]

#-----------------------------------------------
# district stock outs 

art = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01' & art_site==TRUE ]
art[!is.na(arvs), reported:=TRUE]

out = art[year!=2016 ,.(weeks_out=sum(arvs, na.rm=T), facilities=length(unique(facility)) ), by=.(year, district)]

out[ , mean_weeks:=weeks_out/facilities]

# percentage of facility-weeks stocked out 


new = art[year==2018,.(out=sum(arvs, na.rm=TRUE), reported=sum(reported, na.rm=TRUE)), by=district]
new[ ,ratio:=100*(out/reported)]
View(new)






# scatter plots (facility level)
dt[ level=='HC II', level2:=2]
dt[ level=='HC III', level2:=3]
dt[ level=='HC IV', level2:=4]
dt[ level=='Hospital', level2:=5]

setnames(dt, c('level', 'level2'), c('level_og', 'level'))

scatter = dt[year==2018 ,.(arvs=sum(arvs, na.rm=T), test_kits=sum(test_kits, na.rm=T)), by=.(facility, level, art_site)]
scatter[art_site==FALSE, arvs:=NA]

scatter = melt(scatter, id.vars=c('facility', 'level', 'art_site')) 


scatter$variable = factor(scatter$variable, c('arvs', 'test_kits'), c('ARVs', 'HIV test kits'))

# test kit stockouts by level, year       
ggplot(scatter, aes(x=level, y=value)) +
  geom_jitter(width=0.25) + 
  facet_wrap(~variable) +
  labs(title='Weeks stocked out by health facility level*', x='Facility level', 
       y='Weeks stocked out', caption='*Level 5 corresponds to hospitals; all other levels are Health Centers') +
  theme_bw()


arvs = dt[year==2018 & art_site==TRUE,.(stockout_weeks=sum(arvs, na.rm=T)), by=.(facility, level, art_site)]

chi2(arvs$days, arvs$level)

glm(stockout_weeks ~ factor(level), family='poisson', data=arvs)

#---------------------------------------


art = dt[art_site==TRUE]


x = art[arvs==TRUE,length(unique(facility)), by=month]

ggplot(x, aes(x=month, y=V1)) +
  geom_point() + geom_line()

y = art[!is.na(arvs),length(unique(facility)), by=month]


art = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01' &  art_site==TRUE]
art[ , year:=year(date)]

art[arvs==TRUE, length(unique(facility)), by=year ]



