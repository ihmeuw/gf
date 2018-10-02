# Prep & analyze the COD DHIS2 PNLS Viral Load data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 10/1/2018
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr) 
library(rgeos)
library(raster)
library(maptools)

# --------------------

#------------------------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

vl <- readRDS(paste0(dir, 'prepped/viral_load_pnls_interim.rds'))

#-------------------------------------------
# VISUALIZATIONS

# print a PDF of all of the visualizations
pdf(file=paste0(dir, 'viral_load/viral_load.pdf'), width=9, height=6)

#-------------------------
# facilities reporting
facilities <- vl[ ,.(facilities=length(unique(org_unit))), by=date]
facilities[ , fac:='Facilities reporting (total)']
facilities2 <- vl[value>0, .(facilities=length(unique(org_unit))), by=date]
facilities2[ , fac:='Facilities reporting at least one VL test performed']
facilities <- rbind(facilities, facilities2)

ggplot(facilities, aes(x=date, y=facilities, color=fac, group=fac)) +
  geom_point() +
  geom_line() +
  labs(title='Facilities reporting, viral load testing', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Facilities', x='Date', color='Facilities') +
  theme_bw() 

#----------------
# facilities reporting and tests performed 

facilities[ ,indicator:='Facilities reporting']
setnames(facilities, c('facilities', 'fac'), c('value', 'variable'))

tests = vl[variable=='PLHIV who received a VL test', .(value=sum(value)), by=.(date, variable)]
tests[ ,indicator:='VL tests performed']

reporting <- rbind(facilities, tests)

ggplot(reporting, aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~indicator, scales='free_y') +
  labs(title='Facilities reporting and VL tests performed', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 


#----------------
# facilities reporting and tests performed with undetectables

tests2 = vl[ , .(value=sum(value)), by=.(date, variable)]
tests2[ ,indicator:='Viral load testing']
reporting2 <- rbind(facilities, tests2)

ggplot(reporting2, aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~indicator, scales='free_y') +
  labs(title='Facilities reporting and VL tests performed', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 


#----------------
# facilities reporting by level
fac1 <- vl[ ,.(facilities=length(unique(org_unit))), by=.(date, level)]
fac1[ , fac:='Facilities reporting (total)']
fac2 <- vl[value>0, .(facilities=length(unique(org_unit))), by=.(date, level)]
fac2[ , fac:='Facilities reporting at least one VL test performed']
fac3 <- rbind(fac1, fac2)

ggplot(fac3[!is.na(level)], aes(x=date, y=facilities, color=level, group=level)) +
  geom_point() +
  geom_line() +
  facet_wrap(~fac) +
  labs(title='Facilities reporting by facility level, viral load testing', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Facilities', x='Date', color='Health facility level') +
  theme_bw() 

#----------------
# tests performed and undetectable by level 

tests_l = vl[ , .(value=sum(value)), by=.(date, variable, level)]

ggplot(tests_l[variable=='PLHIV who received a VL test' & !is.na(level)], aes(x=date, y=value, color=level, group=level)) +
  geom_point() +
  geom_line() +
  labs(title='VL testing by health facility level', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 

ggplot(tests_l, aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~level, scales='free_y') +
  ylim(0, NA) +
  labs(title='VL tests performed by facility level', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 

#------------------------
# VIRAL SUPPRESSION - outcomes - national trends 

all <- vl[ ,.(value=sum(value)), by=.(date, variable)]

ggplot(all, aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  labs(title='Viral load tests performed and undetectable results, DRC', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 

# outcomes by risk group
all_group <- vl[ ,.(value=sum(value)), by=.(date, variable, group)]

ggplot(all_group, aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~group, scales='free_y') +
  labs(title='VL tests performed and VL test results, DRC', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 

#----------------------
# viral suppression ratios

# proportions
prop <- all
prop[variable=='PLHIV who received a VL test', variable:='test']
prop[variable=='PLHIV with undetectable VL', variable:='und']
prop <- data.table(dcast(prop, date ~ variable))
prop[ , ratio:=(100*(und/test))]
prop[ , und:=NULL]
prop <- melt(prop, id.vars='date')

prop$variable <- factor(prop$variable, c('test', 'ratio'), c('VL Tests (denominator)', 'Percent virally suppressed'))

ggplot(prop, aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Viral load tests performed & percent virally suppressed, DRC', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='', x='Date', color='Variable') +
  theme_bw() 

#------------------------
prop1 <- vl[ ,.(value=sum(value)), by=.(date, variable, case)]

prop1[variable=='PLHIV who received a VL test', variable:='test']
prop1[variable=='PLHIV with undetectable VL', variable:='und']
prop1 <- data.table(dcast(prop1, date + case ~ variable))
prop1[ , ratio:=(100*(und/test))]
prop1[ , und:=NULL]
prop1 <- melt(prop1, id.vars=c('date', 'case'))

prop1$case = factor(prop1$case, c('Old', 'New'), c('Previously enrolled', 'Newly diagnosed'))


ggplot(prop1, aes(x=date, y=value, color=case, group=case)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Viral load tests performed & percent virally suppressed, DRC', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 

#----------------------------------
# viral suppression ratios - old cases only 
old = vl[case=='Old']
old = old[, .(value=sum(value)), by=.(date, dps, mtk, variable)]

ggplot(old, aes(x=date, y=value, color=dps, group=dps)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~variable) +
  theme_bw()

ggplot(old[mtk=='Yes'], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~dps, scales='free_y') +
  theme_bw() 

old1 = old[variable=='PLHIV who received a VL test', .(value=sum(value)), by=dps]
old1[order(value, decreasing=T)]  
old1 = old[dps=='Haut Katanga' | dps=='Kinshasa' |dps=='Lualaba' ]

ggplot(old1, aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~dps, scales='free_y') +
  theme_bw() +
  labs(title='Received a VL test & undetectable results, Top 3 provinces for tests performed', x='Date', y='Count')


ggplot(old[mtk=='Yes'], aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~dps, scales='free_y') +
  theme_bw() 

#-----------------------------------------------
# viral suppression ratio
und <- old[variable=='PLHIV with undetectable VL']
setnames(und, 'value', 'und')
und[ ,variable:=NULL]

tests <- old[variable=="PLHIV who received a VL test"]
setnames(tests, 'value', 'test')
tests[ ,variable:=NULL]

rat <- merge(und, tests, by=c('date', 'dps', 'mtk'), all=T)
rat[ ,prop:=(100*und/test)]

ggplot(rat, aes(x=date, y=prop, color=dps, group=dps)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  labs(title='Percent virally suppressed by DPS (missing if no VL tests performed)', x='Date', y='Percent(%)', color='DPS')

rat2 = rat[dps=='Haut Katanga' | dps=='Kinshasa' |dps=='Lualaba' ]

ggplot(rat2, aes(x=date, y=prop, color=dps, group=dps)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  labs(title='Percent virally suppressed by DPS', x='Date', y='Percent(%)', color='DPS')

rat3 = rat2
rat3 = rat3[dps=='Haut Katanga' | dps=='Kinshasa' |dps=='Lualaba']
rat3[ ,und:=NULL]
rat3 = melt(rat3, id.vars=c('date', 'dps', 'mtk'))

rat3$variable = factor(rat3$variable, c('test', 'prop'), c('VL tests performed', 'Percent (%) with undetectable VL'))

ggplot(rat3, aes(x=date, y=value, color=dps, group=dps)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  labs(x='Date', y='', color='DPS') +
  theme_bw()


dev.off()

#-----------------------------------------
# MAPS

# import the shape file
shapeData = shapefile(paste0(root, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/gadm36_COD_shp/gadm36_COD_1.shp"))
class(shapeData)

# plot the shape file in the base package
plot(shapeData)

# identify the variable that contains district names and codes
shapeData@data %>% as_tibble()
unique(shapeData@data$NAME_1)

# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(dps_name=shapeData@data$NAME_1, id=shapeData@data$GID_1)
str(shape_names)

#---------------------------------------
# create data sets to map

# remove old cases and create a year variable
vl = vl[case=='Old']
vl[date < '2018-01-01', year:='2017']
vl[date >= '2018-01-01', year:='2018']

# tests by year
map_yr <- vl[ ,.(value=sum(value)), by=.(dps, variable, year)]

# tests by year by sex
map_yr_sex <- vl[ ,.(value=sum(value)), by=.(dps, variable, year, sex)]

# tests by year by group
map_yr_group <- vl[ ,.(value=sum(value)), by=.(dps, variable, year, group)]

# suppression ratio by year
und = map_yr[variable=='PLHIV with undetectable VL']
setnames(und, 'value', 'und')
test = map_yr[variable=='PLHIV who received a VL test']
setnames(test, 'value', 'test')
und[,variable:=NULL]
test[,variable:=NULL]

map_yr_ratio = merge(und, test,all=T)
map_yr_ratio[ ,ratio:=(100*und/test)]

# suppression ratio by sex by year
und = map_yr_sex[variable=='PLHIV with undetectable VL']
setnames(und, 'value', 'und')
test = map_yr_sex[variable=='PLHIV who received a VL test']
setnames(test, 'value', 'test')
und[,variable:=NULL]
test[,variable:=NULL]

map_yr_ratio_sex = merge(und, test,all=T)
map_yr_ratio_sex[ ,ratio:=(100*und/test)]







# suppression ratio by group by year


























