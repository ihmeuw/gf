# ----------------------------------------------
# Caitlin O'Brien-Carelli 
# Estimates of mortality and incidence from GBD
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(stringr)
options(scipen=999)
# --------------------

#--------------------------------
# set directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/impact_over_time_gbd/')

#--------------------------------
# upload the data sets

dt = fread(paste0(dir, 'ihme_gbd_incidence_deaths.csv'))

#-------------------------
# calculate annualized rates of change - 2000 to 2017

rates = dt[sex=='Both' & metric=='Rate' & (year==2000 | year==2017),.(measure, location, cause, year, val)]
rates = dcast(rates, measure+location+cause~year)
setnames(rates, c('2000', '2017'), c('y2000', 'y2017'))
rates[ , roc:=round((log(y2017/y2000)/17), 3)]
rates[ ,c('y2000', 'y2017'):=NULL]

# merge in annualized roc
dt = merge(dt, rates, by=c('measure', 'location', 'cause'), all=T)

# label the locations with associated rates of change
dt[ , label:=paste0(location, ' (', roc, ')')]

#-------------------------
# divide into incidence and deaths 

deaths = dt[measure =='Deaths']
inc = dt[measure=='Incidence']

#-------------------------
# source table 

source("C:/Users/ccarelli/local/gf/impact_evaluation/gbd_epidemiology/mort_inc_all_pce_countries_table.R")

#-----------------------------------------------------
# MORTALITY GRAPHS

# HIV/AIDS
#----------------------
# rates 

# total deaths as a rate
ggplot(deaths[cause=='HIV/AIDS' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths per 100,000 population, PCE countries, 2000 - 2017',
       subtitle = 'Includes HIV/TB mortality', caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths as a rate with annualized rates of change

ggplot(deaths[cause=='HIV/AIDS' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths per 100,000 population, PCE countries, 2000 - 2017',
       subtitle = 'Annualized rates of change in parentheses', 
       caption = 'Source: IHME Global Burden of Disease; includes HIV/TB mortality',
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths by sex as a rate
ggplot(deaths[cause=='HIV/AIDS' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths per 100,000 population, PCE countries, 2000 - 2017 by sex',
       subtitle = 'Includes HIV/TB mortality', caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='deaths per 100,000 population')

#----------------------
# counts

# total deaths as an absolute number 
ggplot(deaths[cause=='HIV/AIDS' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related , PCE countries, 2000 - 2017',
       subtitle = 'Includes HIV/TB mortality', caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Number of deaths')

# total deaths by sex as an absolute number 
ggplot(deaths[cause=='HIV/AIDS' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths, PCE countries, 2000 - 2017 by sex',
       subtitle = 'Includes HIV/TB mortality', caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='Number of deaths')

#------------------------------------------
# TUBERCULOSIS

#----------------------
# rates 

# total deaths as a rate
ggplot(deaths[cause=='Tuberculosis' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis-related deaths per 100,000 population, PCE countries, 2000 - 2017',
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths as a rate with annualized rates of change

ggplot(deaths[cause=='Tuberculosis' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberulosis-related deaths per 100,000 population, PCE countries, 2000 - 2017',
       subtitle = 'Annualized rates of change in parentheses', 
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths by sex as a rate
ggplot(deaths[cause=='Tuberculosis' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths per 100,000 population, PCE countries, 2000 - 2017 by sex',
       subtitle = 'Includes HIV/TB mortality', caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='deaths per 100,000 population')

#----------------------
# counts

# total deaths as an absolute number 
ggplot(deaths[cause=='HIV/AIDS' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related , PCE countries, 2000 - 2017',
       subtitle = 'Includes HIV/TB mortality', caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Number of deaths')

# total deaths by sex as an absolute number 
ggplot(deaths[cause=='HIV/AIDS' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths, PCE countries, 2000 - 2017 by sex',
       subtitle = 'Includes HIV/TB mortality', caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='Number of deaths')










