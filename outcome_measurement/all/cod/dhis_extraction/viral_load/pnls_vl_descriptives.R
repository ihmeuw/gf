# Prep & analyze the COD DHIS2 PNLS Viral Load data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 9/24/2018
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr) 
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

ggplot(tests_l, aes(x=date, y=value, color=level, group=level)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable) +
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
prop1 <- melt(prop, id.vars='date')

ggplot(prop1, aes(x=date, y=value, color=case, group=case)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Viral load tests performed & percent virally suppressed, DRC', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 

#----------------------------------
# group ratios

und <- vl[variable=='PLHIV with undetectable VL']
setnames(und, 'value', 'und')
und[ ,variable:=NULL]

tests <- vl[variable=="PLHIV who received a VL test"]
setnames(tests, 'value', 'test')
tests[ ,variable:=NULL]

rat <- merge(und, tests, all=T)

rat[is.na(und) & !is.na(test), length(unique(org_unit))]


prop1[variable=='PLHIV who received a VL test', variable:='test']
prop1[variable=='PLHIV with undetectable VL', variable:='und']





prop1 <- data.table(dcast(prop1, date + case ~ variable))
prop1[ , ratio:=(100*(und/test))]
prop1[ , und:=NULL]
prop1 <- melt(prop, id.vars='date')


#-----------------------------------------
# Maps


map <- vl[ ,.(value=sum(value)), by=.(dps, variable)]






















