# ARV stockouts by facility - visualize the data 

# Caitlin O'Brien-Carelli
# 12/14/2018
# ----------------------
# Set up R
rm(list=ls())
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(maptools)
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

#--------------------------------
# import the shape file 

# set working directory
setwd('J:/Project/Evaluation/GF/mapping/uga/')

# load the shapefile
shapeData = shapefile('uga_dist112_map.shp')

# create a list of names
shape_names = data.table(district=shapeData@data$dist112_na, id=shapeData@data$dist112)

# add id #s for maps
dt = merge(dt, shape_names, by='district')

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coord = data.table(fortify(shapeData, region='dist112')) 
coord[, id:=as.numeric(id)]

# coordinates by year for faceting 
coord_ann = rbind(coord, coord, coord, coord, coord)
coord_ann[, year:=rep(2014:2018, each=nrow(coord))]

#---------------------------------
# reporting completeness data prep - graphs 1, 2

# total facilities/art sites and whether they reported
report = dt[ ,.(facilities=length(unique(facility)), art_sites=(sum(art_site, na.rm=T))), by=date]
tk = dt[!is.na(test_kits), .(test_kits=length(unique(facility))), by=date]
at = dt[!is.na(arvs) & art_site==TRUE, .(arvs=length(unique(facility))), by=date]
report = merge(report, tk, by='date', all.x=TRUE)
report = merge(report, at, by='date', all.x=TRUE)

report[is.na(test_kits), test_kits:=0]
report[is.na(arvs), arvs:=0]
report[ , arv_ratio:=100*(arvs/art_sites)]
report[ , test_ratio:=100*(test_kits/facilities)]

# convert to numerics and round
report = report[ , lapply(.SD, as.numeric), by = date, .SDcols = 2:7]
report = report[ ,lapply(.SD, round, 1), .SDcols = 6:7, 
                 by=.(date, facilities, art_sites, test_kits, arvs)]

# shape long
report = melt(report, id.vars='date')
report[variable=='art_sites' | variable=='arvs' | variable=='arv_ratio', indicator:='ART reporting']
report[variable=='facilities' | variable=='test_kits'| variable=='test_ratio', indicator:='HIV test kit reporting']
report[grep('ratio', variable), ratio:=TRUE]
report[!grep('ratio', variable), ratio:=FALSE]

# label the variables
report$variable = factor(report$variable, c('art_sites', 'arvs', 'facilities',  'test_kits',
                                             'test_ratio', 'arv_ratio'), 
                         c('Total ART sites', 'Reported about ART stock', 'Total health facilities', 'Reported about HIV test kit stock',
                           '% of facilities reporting', '% of ART sites reporting'))

#-----------------------------
# stock outs of ARVs - 3, 4, 5

arv = dt[ , .(date=(unique(date)))]
arv2 = dt[art_site==TRUE & !is.na(arvs), .(art_sites_reporting=as.numeric(length(unique(facility))), 
                                           art_stockout=as.numeric(sum(arvs, na.rm=T))), by=date]  
arv = merge(arv, arv2, by='date', all=T)
arv[is.na(art_sites_reporting), art_sites_reporting:=0]
arv[ ,ratio:=round(100*(art_stockout/art_sites_reporting), 2)]

# calculate 50% of ART sites to graph above a reporting threshold
n = dt[art_site==T, length(unique(facility))/2 ]
arv_thresh = arv[art_sites_reporting > n]

arv = melt(arv, id.vars='date')
arv$variable = factor(arv$variable, c('art_sites_reporting', 'art_stockout', 'ratio'),
                      c('ART sites reporting', 'ART sites with a stockout', 
                        'Percentage of ART sites stocked out of ARVs'))

# run the same code on the threshold subset
arv_thresh = melt(arv_thresh, id.vars='date')
arv_thresh$variable = factor(arv_thresh$variable, c('art_sites_reporting', 'art_stockout', 'ratio'),
                      c('ART sites reporting', 'ART sites with a stockout', 
                        'Percentage of ART sites stocked out of ARVs'))

#-----------------------------------
# ARV stockout weeks bar graphs - 6, 7

# Number of weeks of stockout by facility
arv_weeks = dt[art_site==TRUE, .(weeks=sum(arvs, na.rm=T)), by=.(year, facility)]
arv_weeks = arv_weeks[1 < weeks]
arv_weeks = arv_weeks[ ,.(facilities=length(unique(facility))), by=.(weeks, year)]

# labels 
labels = arv_weeks[ ,.(total=sum(facilities)), by=year]
l14 = paste0('2014 (n=', labels[year==2014]$total, ')')
l15 = paste0('2015 (n=', labels[year==2015]$total, ')') 
l16 = paste0('2016 (n=', labels[year==2016]$total, ')') 
l17 = paste0('2017 (n=', labels[year==2017]$total, ')')
l18 = paste0('2018 (n=', labels[year==2018]$total, ')') 
labels_vec = c(l14, l15, l16, l17, l18)

arv_weeks$year = factor(arv_weeks$year, c(2014, 2015, 2016, 2017, 2018), 
                       labels_vec)

# same bar graph of stockouts, comparable time periods
arv_weeks2 = dt[month(date)!='12' & art_site==TRUE, .(weeks=sum(arvs, na.rm=T)), by=.(year, facility)]
arv_weeks2 = arv_weeks2[1 < weeks]
arv_weeks2 = arv_weeks2[ ,.(facilities=length(unique(facility))), by=.(weeks, year)]

# labels 
labels2 = arv_weeks2[ ,.(total=sum(facilities)), by=year]
l142 = paste0('2014 (n=', labels2[year==2014]$total, ')')
l152 = paste0('2015 (n=', labels2[year==2015]$total, ')') 
l162 = paste0('2016 (n=', labels2[year==2016]$total, ')') 
l172 = paste0('2017 (n=', labels2[year==2017]$total, ')')
l182 = paste0('2018 (n=', labels2[year==2018]$total, ')') 
labels_vec2 = c(l142, l152, l162, l172, l182)

arv_weeks2$year = factor(arv_weeks2$year, c(2014, 2015, 2016, 2017, 2018), 
                        labels_vec2)

#---------------------------------------
# ARV stockout maps - 8:12

# total facility-weeks of stock outs 
# exclude the months that are not in all years (e.g. december 2018)
stockout = dt[month(date)!=12 &  art_site==TRUE, .(value=sum(arvs, na.rm=T)), by=.(year, id)]
arv_map = merge(stockout, coord_ann, by=c('id', 'year'), all.y=TRUE)

# mean weeks stocked out 
# number of weeks of stockout divided by art sites reporting 
art_sites = dt[!is.na(arvs) & art_site==TRUE, .(art_sites=length(unique(facility))), by=.(year, id)]
art_sites = merge(stockout, art_sites)
art_sites[ , mean_weeks:=round((value/art_sites), 1)]
arv_map_norm = merge(art_sites, coord_ann, by=c('id', 'year'), all.y=TRUE)

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


#---------------------------------------
# TEST KITS

# test kit stock out line graphs 13:15

# test kit stockouts
test = dt[ , .(date=(unique(date)))]
test2 = dt[!is.na(test_kits), .(reporting=length(unique(facility)), stockout=sum(test_kits, na.rm=T)), by=date]  
test = merge(test, test2, by='date', all.x=T)

# calculate the percent of facilities stocked out in a given week
test[ , ratio:=round(100*(stockout/reporting), 1)]
test[ , reporting:=as.numeric(reporting)]
test[ , stockout:=as.numeric(stockout)]

# label the variable
test = melt(test, id.vars='date')
test$variable = factor(test$variable, c('reporting', 'stockout', 'ratio'),
                      c('Health facilities reporting', 'Facilities with a stockout of test kits', 
                        'Percentage of facilities stocked out of test kits'))

# comparison of percent stocked out - test kits and arvs
compare = test[variable == 'Percentage of facilities stocked out of test kits']
compare_add = arv[variable == 'Percentage of ART sites stocked out of ARVs']
compare = rbind(compare, compare_add)

#-----------------------------------
# test kit stockout weeks bar graphs - 16, 17

# Number of weeks of stockout by facility
tk_weeks = dt[ , .(weeks=sum(test_kits, na.rm=T)), by=.(year, facility)]
tk_weeks = tk_weeks[1 < weeks]
tk_weeks = tk_weeks[ ,.(facilities=length(unique(facility))), by=.(weeks, year)]

# labels 
tlabels = tk_weeks[ ,.(total=sum(facilities)), by=year]
tl14 = paste0('2014 (n=', tlabels[year==2014]$total, ')')
tl15 = paste0('2015 (n=', tlabels[year==2015]$total, ')') 
tl16 = paste0('2016 (n=', tlabels[year==2016]$total, ')') 
tl17 = paste0('2017 (n=', tlabels[year==2017]$total, ')')
tl18 = paste0('2018 (n=', tlabels[year==2018]$total, ')') 
t_labels_vec = c(tl14, tl15, tl16, tl17, tl18)

tk_weeks$year = factor(tk_weeks$year, c(2014, 2015, 2016, 2017, 2018), 
                        t_labels_vec)

# same bar graph of stockouts, for comparable time periods
tk_weeks2 = dt[month(date)!='12', .(weeks=sum(test_kits, na.rm=T)), by=.(year, facility)]
tk_weeks2 = tk_weeks2[1 < weeks]
tk_weeks2 = tk_weeks2[ ,.(facilities=length(unique(facility))), by=.(weeks, year)]

# labels 
t_labels2 = tk_weeks2[ ,.(total=sum(facilities)), by=year]
tl142 = paste0('2014 (n=', t_labels2[year==2014]$total, ')')
tl152 = paste0('2015 (n=', t_labels2[year==2015]$total, ')') 
tl162 = paste0('2016 (n=', t_labels2[year==2016]$total, ')') 
tl172 = paste0('2017 (n=', t_labels2[year==2017]$total, ')')
tl182 = paste0('2018 (n=', t_labels2[year==2018]$total, ')') 
tlabels_vec2 = c(tl142, tl152, tl162, tl172, tl182)

tk_weeks2$year = factor(tk_weeks2$year, c(2014, 2015, 2016, 2017, 2018), 
                         tlabels_vec2)

#-----------------------------------
# TEST KIT STOCKOUT MAPS

#--------------------------
# map of facility-weeks of stock outs 
tk_stockout = dt[month!='2017-12-01', .(value=sum(test_kits, na.rm=T)), by=.(year, id)]

# merge with coordinates
tk_map = merge(tk_stockout, coord_ann, by=c('id', 'year'), all.y=TRUE)

# number of weeks of stockout divided by facilities reporting 
tk_sites = dt[!is.na(test_kits), .(tk_sites=length(unique(facility))), by=.(year, id)]
tk_sites = merge(tk_stockout, tk_sites)
tk_sites[ , mean_weeks:=round((value/tk_sites), 1)]
tk_map_norm = merge(tk_sites, coord_ann, by=c('id', 'year'), all.y=TRUE)

# rates of change in facility-weeks per year
tk_stockout[ , year2:=paste0('n', year)]
tk_roc = dcast(tk_stockout, id ~ year2)
tk_roc[ , change:=(n2018 - n2017)]
tk_roc_map = merge(coord, tk_roc, by='id')

# only districts with more stockouts in 2018 than 2017
tk_roc_map_alt = merge(coord, tk_roc, by='id')
tk_roc_map_alt[change <=0, change:=NA]

# percentage of weeks stocked out
tk_stock = dt[ , .(weeks_out=sum(test_kits, na.rm=T)), by=.(year, id)]
dt[!is.na(test_kits), reported:=TRUE]
tk_stock_add = dt[ , .(total_weeks=sum(reported, na.rm=T)), by=.(year, id)]
tk_stock = merge(tk_stock, tk_stock_add, by=c('year', 'id'))
tk_stock[ , percent_out:=round(100*(weeks_out/total_weeks), 1)]
tk_stock = merge(tk_stock, coord_ann, by=c('id', 'year'))

#-------------------------------------------
# scatter plots (facility level)

dt[ level=='HC II', level2:=2]
dt[ level=='HC III', level2:=3]
dt[ level=='HC IV', level2:=4]
dt[ level=='Hospital', level2:=5]

scatter = dt[ ,.(arvs=sum(arvs, na.rm=T), test_kits=sum(test_kits, na.rm=T)), by=.(facility, level2, art_site)]
scatter2 = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01',.(arvs=sum(arvs, na.rm=T), 
                                      test_kits=sum(test_kits, na.rm=T)), by=.(facility, level2, year, art_site)]

#---------------------------------------
# finale maps - categorical arv stockouts 

final = dt[art_site==TRUE,.(arvs=sum(arvs, na.rm=T)) , by=.(facility, year, id) ]
final = final[ ,.(facilities=length(unique(facility))), by=.(arvs, year, id)]
final[ ,months:=(arvs/4)]
final[months==0, category:='no_stock_out']
final[0 < months & months <= 1, category:='one_week_2_mos']
final[1 < months & months <= 2, category:='two_4_mos']
final[2 < months, category:='four_months']
final = final[ ,.(value=sum(facilities)), by=.(year, id, variable=category)]
final = dcast(final, year+id ~ variable)

final[is.na(no_stock_out), no_stock_out:=0]
final[is.na(one_week_2_mos), one_week_2_mos:=0]
final[is.na(two_4_mos), two_4_mos:=0]
final[is.na(four_months), four_months:=0]

final = merge(final, coord_ann, by=c('id', 'year'), all.y=TRUE)
final = melt(final, id.vars=c('year', 'id', 'long', 'lat', 'order', 'hole',
                              'piece', 'group'))

final$variable = factor(final$variable, c('no_stock_out', 'one_week_2_mos',
                                          'two_4_mos', 'four_months'), c('No stock outs reported',
                                                                         '1 week - 1 month ', '1+ - 2 months ', '2+ months'))

# ------------------------------------------------------
# color palettes

two = c('#bd0026', '#91bfdb')
ratio_colors = brewer.pal(8, 'Spectral')
results_colors = brewer.pal(6, 'Blues')
sup_colors = brewer.pal(6, 'Reds')
ladies = brewer.pal(11, 'RdYlBu')
gents = brewer.pal(9, 'Purples')

graph_colors = c('#bd0026', '#fecc5c', '#74c476','#3182bd', '#8856a7')
tri_sex = c('#bd0026', '#74c476', '#3182bd')
wrap_colors = c('#3182bd', '#fecc5c', '#bd0026', '#74c476', '#8856a7', '#f768a1')
sex_colors = c('#bd0026', '#3182bd', '#74c476', '#8856a7') # colors by sex plus one for facilities
single_red = '#bd0026'

# ------------------------------------------------------
# SOURCE THE GRAPH FUNCTION

# export a pdf of the graphs
source('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/arvs/arv_graphs.R')

# ------------------------------------------------------




