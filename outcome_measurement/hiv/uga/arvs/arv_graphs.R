# ARV stockouts by facility - data visualization

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
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# working directory to aggregate
dt = readRDS(paste0(dir, 'arv_stockouts_2017_2018.rds'))

# subset dates to before september 30, 2018
dt = dt[date < '2018-10-01']

#--------------------------------
# shape file 

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

# coordinates by year for faceting (repeat 5 times for 5 years of data)
coord_ann = rbind(coord, coord)
coord_ann[, year:=rep(2017:2018, each=nrow(coord))]

#-------------------------------------------------------------
# reporting completeness data prep

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
                         c('ART sites', 'Reported about ART stock', 'Total facilities', 'Reported about HIV test kit stock',
                           '% of facilities reporting', '% of ART sites reporting'))




#----------------------------
# drop out the 12 facilities that never reported 
missing = dt[ , .(check=all(is.na(arvs)), check_t=all(is.na(test_kits))), by=facility]
missing = missing[check==TRUE & check_t==TRUE]
dt = dt[!facility %in% missing$facility]

#-----------------------------
# stock outs of ARVs

arv = dt[ , .(date=(unique(date)))]
arv2 = dt[art_site==TRUE & !is.na(arvs), .(art_sites_reporting=length(unique(facility)), art_stockout=sum(arvs, na.rm=T)), by=date]  
arv = merge(arv, arv2, by='date', all.x=T)
arv[is.na(art_sites_reporting), art_sites_reporting:=0]
arv[is.na(art_stockout), art_stockout:=0]
arv[ ,ratio:=round(100*(art_stockout/art_sites_reporting), 2)]
arv = melt(arv, id.vars='date')

arv$variable = factor(arv$variable, c('art_sites_reporting', 'art_stockout', 'ratio'),
                      c('ART sites reporting', 'ART sites with a stockout', 
                        'Percentage of ART sites stocked out of ARVs'))

#-----------------------------------
# ARV stockout weeks bar graphs

# Number of weeks of stockout by facility
arv_weeks = dt[art_site==TRUE, .(weeks=sum(arvs, na.rm=T)), by=.(year, facility)]
arv_weeks = arv_weeks[ ,.(facilities=length(unique(facility))), by=.(weeks, year)]
arv_weeks$year = factor(arv_weeks$year, c('2017', '2018'), 
                        c('2017 (n=328)', '2018 (n=235)'))

# same graph, comparable time periods
arv_weeks2 = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01' &  art_site==TRUE, .(weeks=sum(arvs, na.rm=T)), by=.(year, facility)]
arv_weeks2 = arv_weeks2[ ,.(facilities=length(unique(facility))), by=.(weeks, year)]
arv_weeks2$year = factor(arv_weeks2$year, c('2017', '2018'), 
                        c('2017 (n=273)', '2018 (n=235)'))

#---------------------------------------
# ARV stockout maps 

#--------------------------
# map of facility-weeks of stock outs 
stockout = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01' &  art_site==TRUE, .(weeks=sum(arvs, na.rm=T)), by=.(year, id)]

# merge with coordinates
arv_map = merge(stockout, coord_ann, by=c('id', 'year'), all.y=TRUE)

# number of weeks of stockout divided by art sites reporting 
art_sites = dt[!is.na(arvs) & art_site==TRUE, .(art_sites=length(unique(facility))), by=.(year, id)]
art_sites = merge(stockout, art_sites)
art_sites[ , mean_weeks:=round((weeks/art_sites), 1)]
arv_map_norm = merge(art_sites, coord_ann, by=c('id', 'year'), all.y=TRUE)

# rates of change 
s1 = stockout[year==2017, .(n2017 = weeks ), by=id]
s2 = stockout[year==2018, .(n2018 = weeks ), by=id]
roc = merge(s1, s2, by='id')
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

# test kit stockouts
test = dt[ , .(date=(unique(date)))]
test2 = dt[!is.na(test_kits), .(reporting=length(unique(facility)), stockout=sum(test_kits, na.rm=T)), by=date]  
test = merge(test, test2, by='date', all.x=T)

test[is.na(reporting), reporting:=0]
test[is.na(stockout), stockout:=0]
test[ , ratio:=round(100*(stockout/reporting), 2)]
test = melt(test, id.vars='date')

test$variable = factor(test$variable, c('reporting', 'stockout', 'ratio'),
                      c('Health facilities reporting', 'Facilities with a stockout of test kits', 
                        'Percentage of facilities stocked out of test kits'))

#-----------------------------------
# test kit stockout weeks bar graphs

# Number of weeks of stockout by facility
tk_weeks = dt[ , .(weeks=sum(test_kits, na.rm=T)), by=.(year, facility)]
tk_weeks = tk_weeks[ ,.(facilities=length(unique(facility))), by=.(weeks, year)]
tk_weeks$year = factor(tk_weeks$year, c('2017', '2018'), 
                        c('2017 (n=699)', '2018 (n=613)'))

# number of facilities that reported at least one stockout weeek
tk_weeks[weeks > 0,sum(facilities), by=year]

# same graph, comparable time periods
tk_weeks2 = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01', .(weeks=sum(test_kits, na.rm=T)), by=.(year, facility)]
tk_weeks2 = tk_weeks2[ ,.(facilities=length(unique(facility))), by=.(weeks, year)]
tk_weeks2$year = factor(tk_weeks2$year, c('2017', '2018'), 
                         c('2017 (n=652)', '2018 (n=613)'))

# number of facilities that reported at least one stockout weeek - same time period
tk_weeks2[weeks > 0,sum(facilities), by=year]

#-----------------------------------
# TEST KIT STOCKOUT MAPS

#--------------------------
# map of facility-weeks of stock outs 
tk_stockout = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01', .(weeks=sum(test_kits, na.rm=T)), by=.(year, id)]

# merge with coordinates
tk_map = merge(tk_stockout, coord_ann, by=c('id', 'year'), all.y=TRUE)

# number of weeks of stockout divided by art sites reporting 
tk_sites = dt[!is.na(test_kits), .(tk_sites=length(unique(facility))), by=.(year, id)]
tk_sites = merge(tk_stockout, tk_sites)
tk_sites[ , mean_weeks:=round((weeks/tk_sites), 1)]
tk_map_norm = merge(tk_sites, coord_ann, by=c('id', 'year'), all.y=TRUE)

# rates of change 
s1 = tk_stockout[year==2017, .(n2017 = weeks ), by=id]
s2 = tk_stockout[year==2018, .(n2018 = weeks ), by=id]
tkroc = merge(s1, s2, by='id')
tkroc[ , change:=(n2018 - n2017)]
tk_roc_map = merge(coord, tkroc, by='id')

# only districts with more stockouts in 2018 than 2017
tk_roc_map_alt = merge(coord, tkroc, by='id')
tk_roc_map_alt[ change <= 0, change:=NA ]

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
# final map - categorical arv stockouts 

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

# store colors
ratio_colors <- brewer.pal(8, 'Spectral')
results_colors <- brewer.pal(6, 'Blues')
sup_colors <- brewer.pal(6, 'Reds')
ladies <- brewer.pal(11, 'RdYlBu')
gents <- brewer.pal(9, 'Purples')

# red colors for bar graph
bar_colors <- c('Not Suppressed'='#de2d26', 'Suppressed'='#fc9272')

graph_colors <- c('#bd0026', '#fecc5c', '#74c476','#3182bd', '#8856a7')
tri_sex <- c('#bd0026', '#74c476', '#3182bd')
wrap_colors <- c('#3182bd', '#fecc5c', '#bd0026', '#74c476', '#8856a7', '#f768a1')
sex_colors <- c('#bd0026', '#3182bd', '#74c476', '#8856a7') # colors by sex plus one for facilities
single_red <- '#bd0026'

# breaks for log transformation legends
breaks <- c(1, 20, 400, 8100)


#------------------------------------------------
# PDF VISUALS 

pdf(paste0(dir, '/outputs/stockout_descriptives.pdf'), height=6, width=9)

#----------------------------------------
# reporting completeness graphs

# count of facilities and art sites reporting
ggplot(report[ratio==FALSE], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~indicator) +
  labs(x='Date', y='Number of health facilities', title='Number of health facilities and ART sites reporting stock out information',
       subtitle='2017 - September 2018', color="")

# ratio of facilities reporting
ggplot(report[ratio==TRUE], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable) +
  labs(x='Date', y='% of facilities', title='Percentage of health facilities and ART sites reporting stock out information',
       subtitle='2017 - September 2018', color='% Reporting')

#-----------------------------------
# arv stockout graphs 

# arv stockout counts
ggplot(arv[variable!='Percentage of ART sites stocked out of ARVs'], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  labs(title='Number of ART sites that were stocked out of ARVs in a given week', 
       y='Number of facilities', x='Date', color="")

# percentage of art sites that reported that were stocked out
ggplot(arv[variable=='Percentage of ART sites stocked out of ARVs'], aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Percentage of ART sites that were stocked out of ARVs in a given week', 
       x='Number of facilities', y='%')

# stacked bar of weeks stocked out 
ggplot(arv_weeks[weeks!=0 ], aes(x=weeks, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  labs(title = "Facilities stocked out of ARVs for at least one week by total weeks stocked out", x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year',
       subtitle='January 2017 - September 2018')

# stacked bar of weeks stocked out 
ggplot(arv_weeks2[weeks!=0 ], aes(x=weeks, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  labs(title = "Facilities stocked out of ARVs for at least one week by total weeks stocked out", 
       subtitle='Same time period: January - September', x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year')

#-----------------------
# ARV stockout maps 

# map of facility-weeks of stock outs 
ggplot(arv_map, aes(x=long, y=lat, group=group, fill=weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title="Total facility-weeks of ARV stockouts by district, Uganda", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Facility-weeks") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# number of weeks of stockout divided by art sites reporting 
ggplot(arv_map_norm, aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title="Mean number of weeks stocked out of ARVs per ART site by district", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Mean weeks per facility") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# rate of change 
ggplot(roc_map, aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=brewer.pal(9, 'Purples')) + 
  theme_void() +
  labs(title="Rate of change: facility-weeks of ARV stockouts in 2018 minus 2017", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Difference in weeks (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# facilities with more stockouts
ggplot(roc_map_alt, aes(x=long, y=lat, group=group, fill=change)) + 
coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=brewer.pal(9, 'Purples')) + 
  theme_void() +
  labs(title="Facilities with more facility-weeks of ARV stockouts in 2018 than 2017 ", caption="The number of ART sites remained the same from 2017 to 2018", 
       subtitle='Same time period: January - September', fill="Difference in weeks (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# percentage of weeks stocked out
ggplot(stock, aes(x=long, y=lat, group=group, fill=percent_out)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=rev(ratio_colors)) + 
  theme_void() +
  labs(title="Percentage of facility-weeks stocked out of ARVs", subtitle="Weeks ART sites were stocked out/Total weeks in which ART sites reported", 
       caption='Source: HMIS', fill="% of weeks stocked out") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


#------------------------------
# test kits

# test kit stockout counts
ggplot(test[variable!='Percentage of facilities stocked out of test kits'], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  labs(title='Number of facilities that were stocked out of HIV test kits in a given week', 
       y='Number of facilities', x='Date', color="")

# percentage of facilities that reported that were stocked out of test kits
ggplot(test[variable=='Percentage of facilities stocked out of test kits'], aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Percentage of facilities that were stocked out of HIV test kits in a given week', 
       x='Number of facilities', y='%')

# stacked bar of weeks stocked out 
ggplot(tk_weeks[weeks!=0 ], aes(x=weeks, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  labs(title = "Facilities stocked out of HIv test kits for at least one week by total weeks stocked out", x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year',
       subtitle='January 2017 - September 2018')

# stacked bar of weeks stocked out 
ggplot(tk_weeks2[weeks!=0 ], aes(x=weeks, y=facilities, fill=factor(year))) + 
  geom_bar(stat='identity', position='dodge') +
  theme_minimal() +
  labs(title = "Facilities stocked out of HIV test kits for at least one week by total weeks stocked out", 
       subtitle='Same time period: January - September', x='Number of weeks out of stock*', 
       y="Number of facilities", caption="*Does not include facilities stocked out for 0 weeks", fill='Year')

#------------------------------------
# test kit maps

# map of facility-weeks of stock outs 
ggplot(tk_map, aes(x=long, y=lat, group=group, fill=weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  labs(title="Total facility-weeks of test kit stockouts by district, Uganda", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Facility-weeks stocked out of tests") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# number of weeks of stockout divided by art sites reporting 
ggplot(tk_map_norm, aes(x=long, y=lat, group=group, fill=mean_weeks)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  labs(title="Mean number of weeks stocked out of HIV test kits per facility by district", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Mean weeks per facility") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# rate of change 
ggplot(tk_roc_map, aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=brewer.pal(9, 'BuGn')) + 
  theme_void() +
  labs(title="Rate of change: facility-weeks of test kit stockout in 2018 minus 2017", caption="Source: HMIS", 
       subtitle='Same time period: January - September',fill="Difference in weeks (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# facilities with more stockouts
ggplot(tk_roc_map_alt, aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=brewer.pal(9, 'BuGn')) + 
  theme_void() +
  labs(title="Facilities with more facility-weeks of test kit stockouts in 2018 than 2017 ", 
       subtitle='Same time period: January - September', fill="Difference in weeks (2018 - 2017)") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# percentage of weeks stocked out
ggplot(tk_stock, aes(x=long, y=lat, group=group, fill=percent_out)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=rev(ratio_colors)) + 
  theme_void() +
  labs(title="Percentage of facility-weeks stocked out", subtitle="Weeks stocked out at ART sites/Total weeks reporting from ART sites", 
       caption='Source: HMIS', fill="% of weeks stocked out") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


#--------------------------------
# facility level 

# arv stockouts by level
ggplot(scatter[art_site==TRUE], aes(x=level2, y=arvs)) +
  geom_jitter(width=0.25) + theme_bw() + 
  labs(title='Weeks stocked out of ARVs by facility level (ART sites)', subtitle='2017 - 2018', x='Facility level',
       y='Weeks stocked out of ARVs')

# arv stockouts by level, year       
ggplot(scatter2[art_site==TRUE], aes(x=level2, y=arvs)) +
  geom_jitter(width=0.25) + 
  facet_wrap(~year) +
  labs(title='Weeks stocked out of ARVs by facility level (ART sites)', x='Facility level', 
       y='Weeks stocked out of ARVs', subtitle='Same time period: January - September') +
  theme_bw()

# test kit stockouts by level, year       
ggplot(scatter, aes(x=level2, y=test_kits)) +
  geom_jitter(width=0.25) + 
  labs(title='Weeks stocked out of HIV test kits by facility level', x='Facility level', 
       y='Weeks stocked out of HIV test kits') +
  theme_bw()

# test kit stockouts by level, year       
ggplot(scatter2, aes(x=level2, y=test_kits)) +
  geom_jitter(width=0.25) + 
  facet_wrap(~year) +
  labs(title='Weeks stocked out of HIV test kits by facility level', x='Facility level', 
       y='Weeks stocked out of HIV test kits', subtitle='Same time period: January - September') +
  theme_bw()



#--------------------------------
# finale maps

# Number of weeks stocked out, categorical
ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) + 
  theme_void() +
  labs(title="Number of facilities stocked out of ARVs by time stocked out, 2017", 
       subtitle="Cumulative: one month is equal to four weeks stocked out of ARVs", 
       caption='Source: HMIS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# at least one stockout
ggplot(final[year==2017 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlGnBu')) + 
  theme_void() +
  labs(title="Number of facilities stocked out of ARVs by time stocked out, 2017", 
       subtitle="Minimum one week of stockout", 
       caption='Source: HMIS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


# Number of weeks stocked out, categorical
ggplot(final[year==2018], aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) + 
  theme_void() +
  labs(title="Number of facilities stocked out of ARVs by time stocked out, 2018", 
       subtitle="Cumulative: one month is equal to four weeks stocked out of ARVs", 
       caption='Source: HMIS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# at least one stockout
ggplot(final[year==2018 & variable!='No stock outs reported'], aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~variable) +
  scale_fill_gradientn(colors=brewer.pal(9, 'YlOrBr')) + 
  theme_void() +
  labs(title="Number of facilities stocked out of ARVs by time stocked out, 2018", 
       subtitle="Minimum one week of stockout", 
       caption='Source: HMIS', fill="Number of facilities") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


#---------------------------
dev.off()

#------------------------------


