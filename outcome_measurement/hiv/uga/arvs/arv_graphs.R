# ARV and test kit stockouts by facility - visualize the data 
# create data tables for distinct graphs
#
# Caitlin O'Brien-Carelli
# 10/7/2019
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
library(data.table)

# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# working directory to aggregate
dt = readRDS(paste0(dir, 'prepped_data/arv_stockouts_2013_2019.rds'))

# save the main data set in memory
full = copy(dt)

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

# coordinates by year for faceting 
coord_ann_full = rbind(coord, coord, coord, coord, coord, coord)
coord_ann_full[, year:=rep(2014:2019, each=nrow(coord))]

#---------------------------------

# ------------------------------------------------------
# color palettes

two = c('#91bfdb', '#bd0026')
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
bar_color = brewer.pal(5, 'RdYlBu') 

# ------------------------------------------------------

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
report[variable=='art_sites' | variable=='arvs' | variable=='arv_ratio', indicator:='ART reporting*']
report[variable=='facilities' | variable=='test_kits'| variable=='test_ratio', indicator:='HIV test kit reporting']
report[grep('ratio', variable), ratio:=TRUE]
report[!grep('ratio', variable), ratio:=FALSE]

# label the variables
report$variable = factor(report$variable, c('art_sites', 'arvs', 'facilities',  'test_kits',
                                             'test_ratio', 'arv_ratio'), 
                         c('Total ART sites', 'Reported about ART stock',
                           'Total health facilities', 'Reported about HIV test kit stock',
                           '% of all health facilities reporting', '% of ART sites reporting'))

#-----------------------------
# stock outs of ARVs - 3, 4, 5

# expand to all dates
arv_dates = dt[ , .(date=(unique(date)))]
arv2 = dt[art_site==TRUE & !is.na(arvs), .(art_sites_reporting=as.numeric(length(unique(facility))), 
                                           art_stockout=as.numeric(sum(arvs, na.rm=T))), by=date]  
arv = merge(arv_dates, arv2, by='date', all=T)
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
arv_weeks = dt[art_site==TRUE & year!=2019, .(weeks=sum(arvs, na.rm=T)), by=.(year, facility)]
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

#-------------
# same bar graph of stockouts for 2019 only
arv_weeks2 = dt[year==2019 & art_site==TRUE, .(weeks=sum(arvs, na.rm=T)), by=.(facility, level)]
arv_weeks2 = arv_weeks2[ ,.(facilities=length(unique(facility))), by=.(weeks, level)]

# label the totals for each bar
lab_facilities = arv_weeks2[ ,.(facilities=sum(facilities)), by=weeks]

# for the color
arv_weeks2[ , year:=2019]
#------------
# g6b - comparing 2018 and 2019, same time period
bad_months = c(9:12)
arv_weeks3 = dt[(year==2019 | year==2018) & !(month(date) %in% bad_months) & art_site==T]
arv_weeks3 = arv_weeks3[ , .(weeks=sum(arvs, na.rm=T)), by=.(year, facility)]
arv_weeks3 = arv_weeks3[ ,.(facilities=length(unique(facility))), by=.(weeks, year)]

# labels 
labels3 = arv_weeks3[ ,.(total=sum(facilities)), by=year]
l183 = paste0('2018 (n=', labels3[year==2018]$total, ')')
l193 = paste0('2019 (n=', labels3[year==2019]$total, ')')
labels_vec3 = c(l183, l193)

arv_weeks3$year = factor(arv_weeks3$year, c(2018, 2019), 
                        labels_vec3)

#---------------------------------------
# ARV stockout maps - 8:12

# total facility-weeks of stock outs 
# exclude 2019
stockout = dt[art_site==TRUE, .(value=sum(arvs, na.rm=T)), by=.(year, id)]
arv_map = merge(stockout, coord_ann, by=c('id', 'year'), all.y=TRUE) # 

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

#--------------------------
# categorical stock out map - g11a

roc_map[change < 0, roc_cat:=2] # less
roc_map[change == 0, roc_cat:=3]
roc_map[change > 0, roc_cat:=1]# more

roc_map$roc_cat = factor(roc_map$roc_cat, c(1, 2, 3),
                         c('More stock outs', 'Less stock outs',
                           'Same number of stock outs'))

#--------------------------

#--------------------------------------------------------------
# TEST KITS

# test kit stock out line graphs 16:18

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
# test kit stockout weeks bar graphs - 15, 19

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

#-----------------------------------
# same bar graph of stockouts, 2019 only

tk2019 = dt[year==2019, .(weeks=sum(test_kits, na.rm=T)), by=.(year, facility)]
tk2019 = tk2019[ ,.(facilities=length(unique(facility))), by=.(weeks, year)]

# labels 
tlabels19 = tk2019[ ,.(total=sum(facilities)), by=year]
tl19 = paste0('2019 (n=', tlabels19$total, ')')
tk2019[ ,year:=NULL]
tk2019[, year:=tl19]

#-----------------------------------
# TEST KIT STOCKOUT MAPS

#--------------------------
# map of facility-weeks of stock outs 
tk_stockout = dt[ , .(value=sum(test_kits, na.rm=T)), by=.(year, id)]

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

#--------------------------
# categorical stock out map - g26a

tk_roc_map[change < 0, roc_cat:=2] # less
tk_roc_map[change == 0, roc_cat:=3]
tk_roc_map[change > 0, roc_cat:=1] # more

tk_roc_map$roc_cat = factor(tk_roc_map$roc_cat, c(1, 2, 3),
                         c('More stock outs', 'Less stock outs',
                           'Same number of stock outs'))
#--------------------------

#-------------------------------------------
# scatter plots (facility level)

# g21 - scatter plot by year
scat = dt[ ,.(test_kits=sum(test_kits, na.rm=T)), by=.(facility, year, level)]

dt[ level=='HC II', level2:=2]
dt[ level=='HC III', level2:=3]
dt[ level=='HC IV', level2:=4]
dt[ level=='Hospital', level2:=5]
dt[ level=='TASO', level2:=6]

dt$level2 = factor(dt$level2, c(2:6), 
                   c('HC II', 'HC III', 'HC IV', 'Hospital', 'TASO'))

scatter = dt[year==2017 | year==2018,.(arvs=sum(arvs, na.rm=T), test_kits=sum(test_kits, na.rm=T)),
             by=.(facility, level2, art_site)]

scatter2 = dt[,.(arvs=sum(arvs, na.rm=T), 
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
                                          'two_4_mos', 'four_months'),
                                           c('No stock outs reported',
                                          '1 week - 1 month ', '1+ - 2 months ', '2+ months'))

# for graphs with 0 depicted in grey
final[ ,alter:=value]
final[alter==0, alter:=NA] 

# ------------------------------------------------------
# SOURCE THE GRAPH FUNCTION
# export a pdf of the graphs

# read in all of the maps and graphs
source('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/arvs/arv_visuals_to_source.R')

# export the maps and graphs as a pdf
pdf(paste0(dir, 'outputs/stockout_descriptives_2013_2019.pdf'), height=6, width=12)

g_opener
g1
g2
g3
g4
g5
g5a
g6
g7
g7a
g6a
g8
g9
g10
g11
g11a
g12
g13
g14
g16 # introduce the test kit slides
g15
g17
g18
g19
g20
g21
g22
g23
g24
g25
g26
g26a
g27
g28
g29
g30 
g31
g32
g32a
g33
g34
g35

dev.off()


# ------------------------------------------------------ 
