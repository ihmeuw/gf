# ----------------------------------------------
# Caitlin O'Brien-Carelli 
# Estimates of mortality and incidence from GBD/WHO/UNAIDS
# Set function arguments to determine which data sources
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
# detect the user 

user = Sys.info()[['user']]

#--------------------------------
# set directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/synthesis_epidemiology/')

code_dir = paste0('C:/Users/', user, '/local/gf/impact_evaluation/gbd_epidemiology/')

#--------------------------------
# determine if the data set is gbd or who/unaids

# select 'gbd' or 'who_unaids' as the data set to visualize 
set = 'who_unaids'

#--------------------------------
# upload the data set

#--------------------------------
# Import GBD data and format appropriately 

if (set=='gbd') { 
  
dt = fread(paste0(dir, 'raw_data/gbd/ihme_age_standardized_2017.csv'))

# subset to age standardized rates and all ages counts
dt = dt[!(metric=='Rate' & age=='All Ages')]

# set the caption
cap = 'Source: IHME Global Burden of Disease'
hiv_cap = 'Source: IHME Global Burden of Disease; includes HIV/TB mortality'
tb_cap = 'Source: IHME Global Burden of Disease; includes drug susceptible and drug resistant TB'
years = '1990 - 2017'
mal_years = years

} else { 
  
  dt = readRDS(paste0(dir, 'prepped_data/who_unaids_prepped.RDS'))
  hiv_cap = 'Source: UNAIDS; includes HIV/TB mortality'
  tb_cap = 'Source: WHO; includes drug susceptible and drug resistant TB'
  cap = 'Sources: WHO; UNAIDS'
  years = '2000 - 2018'
  mal_years = '2010 - 2018'
}


#--------
# calculate annualzied rates of change 2010 to 2017
if (set=='who_unaids') {
rates = dt[sex=='Both' & metric=='Rate' & location!='Sudan' & (year==2010 | year==2017), .(measure, location, cause, year, val)]
rates = dcast(rates, measure+location+cause~year)
setnames(rates, c('2010', '2017'), c('y2010', 'y2017'))
rates[ , roc:=round((log(y2017/y2010)/17), 3)]
rates[ ,roc:=roc*100]
rates[ ,c('y2010', 'y2017'):=NULL] 

# rates for sudan - only 2011 to present
s_rates = dt[sex=='Both' & metric=='Rate' & location=='Sudan' & (year==2011 | year==2017), .(measure, location, cause, year, val)]
s_rates = dcast(s_rates, measure+location+cause~year)
setnames(s_rates, c('2011', '2017'), c('y2011', 'y2017'))
s_rates[ , roc:=round((log(y2017/y2011)/16), 3)]
s_rates[ ,roc:=roc*100]
s_rates[ ,c('y2011', 'y2017'):=NULL] 

# bind in the sudanese rates
rates = rbind(rates, s_rates)

}

#----------
# calculate annualized rates of change - 2000 to 2017

if (set=='gbd') {
rates = dt[sex=='Both' & metric=='Rate' & (year==2000 | year==2017),.(measure, location, cause, year, val)]
rates = dcast(rates, measure+location+cause~year)
setnames(rates, c('2000', '2017'), c('y2000', 'y2017'))
rates[ , roc:=round((log(y2017/y2000)/17), 3)]
rates[ ,roc:=roc*100]
rates[ ,c('y2000', 'y2017'):=NULL]

}

# merge in annualized roc
dt = merge(dt, rates, by=c('measure', 'location', 'cause'), all=T)

# label the locations with associated rates of change
dt[!is.na(roc), label:=paste0(location, ' (', roc, '%)')] 

#-------------------------
# reset the order for facet wrapped graphs
dt$location = factor(dt$location, c("Cambodia", "Democratic Republic of the Congo",
                                    "Guatemala", "Mozambique", "Myanmar", "Senegal", "Sudan", "Uganda", "Global"), 
                     c("Cambodia", "DRC", "Guatemala", "Mozambique", "Myanmar", "Senegal", 
                       "Sudan", "Uganda", "Global Trend"))  

#-------------------------
# divide into incidence and deaths 

deaths = dt[measure =='Deaths']
inc = dt[measure=='Incidence']

#-------------------------
# # source outside code for tables and figures
# 
# # works on caitlin's computer - change to relevant directory
# source(paste0(code_dir, 'mort_inc_all_pce_countries_table.R'))
# 
# # works on caitlin's computer - change to relevant directory
# source(paste0(code_dir, "trend_figures_synthesis.R"))
# 
# #-----------------------------------------------------
# MORTALITY GRAPHS

pdf(paste0(dir, 'outputs/mortality_pce_countries_', set, '.pdf'), height=9, width=12)

# HIV/AIDS
#----------------------
# rates 

# total deaths as a rate
ggplot(deaths[cause=='HIV/AIDS' & metric=='Rate' & sex=='Both'], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths per 100,000 population, PCE countries',
       subtitle = years,
   caption = hiv_cap,
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths as a rate with annualized rates of change

ggplot(deaths[cause=='HIV/AIDS' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths per 100,000 population, PCE countries',
       subtitle = paste0('Annualized rates of change in parentheses, ', years), 
       caption = hiv_cap,
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths by sex as a rate

if (set=='gbd') {
ggplot(deaths[cause=='HIV/AIDS' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = hiv_cap,
       color='Sex', x='Year', y='deaths per 100,000 population') }

#----------------------
# counts

# total deaths as an absolute number 
ggplot(deaths[cause=='HIV/AIDS' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths, PCE countries',
       subtitle = years, caption = hiv_cap,
       x='Year', y='Number of deaths')

if (set=='gbd') {

# total deaths by sex as an absolute number 
ggplot(deaths[cause=='HIV/AIDS' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV/AIDS-related deaths, PCE countries, 2000 - 2017 by sex',
       caption = hiv_cap,
       color='Sex', x='Year', y='Number of deaths') }

#------------------------------------------
# TUBERCULOSIS

#----------------------
# rates 

# total deaths as a rate
ggplot(deaths[cause=='Tuberculosis' & metric=='Rate' & sex=='Both'], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis deaths per 100,000 population, PCE countries',
       subtitle = years, caption = tb_cap,
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths as a rate with annualized rates of change

ggplot(deaths[cause=='Tuberculosis' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberulosis deaths per 100,000 population, PCE countries',
       subtitle = paste0('Annualized rates of change in parentheses, ', years), 
       caption = tb_cap,
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths by sex as a rate

if (set=='gbd') {
ggplot(deaths[cause=='Tuberculosis' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis deaths per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = tb_cap,
       color='Sex', x='Year', y='deaths per 100,000 population')}

#----------------------
# counts

# total deaths as an absolute number 
ggplot(deaths[cause=='Tuberculosis' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis deaths, PCE countries',
       subtitle = years, caption = tb_cap,
       x='Year', y='Number of deaths')

# total deaths by sex as an absolute number 

if (set=='gbd'){
ggplot(deaths[cause=='Tuberculosis' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis deaths, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       color='Sex', x='Year', y='Number of deaths')}

#--------------------------------------
# MALARIA 

#----------------------
# rates 

# total deaths as a rate
ggplot(deaths[cause=='Malaria' & metric=='Rate' & sex=='Both'], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title=paste0('Malaria deaths per 100,000 population, ', mal_years),
       caption = cap,
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths as a rate with annualized rates of change

ggplot(deaths[cause=='Malaria' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria deaths per 100,000 population, PCE countries',
       subtitle = paste0('Annualized rates of change in parentheses, ', mal_years), 
       caption = cap,
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths by sex as a rate

if (set=='gbd') {
ggplot(deaths[cause=='Malaria' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria deaths per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='deaths per 100,000 population') }

#----------------------
# counts

# total deaths as an absolute number 
ggplot(deaths[cause=='Malaria' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title=paste0('Malaria deaths, PCE countries, ', mal_years),
       caption = cap,
       x='Year', y='Number of deaths')

# total deaths by sex as an absolute number 
if (set=='gbd'){
ggplot(deaths[cause=='Malaria' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria deaths, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='Number of deaths')}

dev.off()

#-----------------------------------------------------------------


#-----------------------------------------------------
# INCIDENCE GRAPHS

pdf(paste0(dir, 'outputs/incidence_pce_countries_', set, '.pdf'), height=9, width=12)

# HIV/AIDS
#----------------------
# rates 

# total incidence as a rate
ggplot(inc[cause=='HIV/AIDS' & metric=='Rate' & sex=='Both'], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title=paste0('HIV incidence per 100,000 population, PCE countries, ', years),
      caption = hiv_cap,
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incidence as a rate with annualized rates of change

ggplot(inc[cause=='HIV/AIDS' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title=paste0('HIV incidence per 100,000 population, PCE countries,  ', years),
       subtitle = 'Annualized rates of change in parentheses', 
       caption = cap,
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incidence by sex as a rate

if (set=='gbd') {
ggplot(inc[cause=='HIV/AIDS' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV incidence per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='Incidence per 100,000 population') }

#----------------------
# counts

# total incidence as an absolute number 
ggplot(inc[cause=='HIV/AIDS' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title=paste0('HIV incident cases, PCE countries, ', years),
     caption = hiv_cap,
       x='Year', y='Number of new cases')

# total inc by sex as an absolute number 
if (set=='gbd') {
ggplot(inc[cause=='HIV/AIDS' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV incident cases, PCE countries, 2000 - 2017 by sex',
    caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='Number of new cases') }

#------------------------------------------
# TUBERCULOSIS

#----------------------
# rates 

# total incidence as a rate
ggplot(inc[cause=='Tuberculosis' & metric=='Rate' & sex=='Both'], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title=paste0('Tuberculosis incidence per 100,000 population, PCE countries, ', years),
       caption =tb_cap,
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incidence as a rate with annualized rates of change

ggplot(inc[cause=='Tuberculosis' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title=paste0('Tuberculosis incidence per 100,000 population, PCE countries, ', years),
       subtitle = 'Annualized rates of change in parentheses', 
       caption = tb_cap,
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incidence by sex as a rate
if (set=='gbd') {
ggplot(inc[cause=='Tuberculosis' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis incidence per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       color='Sex', x='Year', y='Incidence per 100,000 population') }

#----------------------
# counts

# total incidence as an absolute number 
ggplot(inc[cause=='Tuberculosis' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title=paste0('Tuberculosis incidence, PCE countries, ', years),
       caption = tb_cap,
       x='Year', y='Number of new cases')

# total incidence by sex as an absolute number 
if (set=='gbd') {
ggplot(inc[cause=='Tuberculosis' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis incidence, PCE countries, 2000 - 2017 by sex',
       caption = tb_cap,
       color='Sex', x='Year', y='Number of inc') }

#--------------------------------------
# MALARIA 

#----------------------
# rates 

# total incidence as a rate
ggplot(inc[cause=='Malaria' & metric=='Rate' & sex=='Both' ], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title=paste0('Malaria incidence per 100,000 population, PCE countries, ', mal_years),
       caption = cap,
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incincidence as a rate with annualized rates of change

ggplot(inc[cause=='Malaria' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria incidence per 100,000 population, PCE countries, 2000 - 2017',
       subtitle = 'Annualized rates of change in parentheses', 
       caption = cap,
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incidence by sex as a rate
if (set=='gbd') {
ggplot(inc[cause=='Malaria' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria incidence per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = cap,
       color='Sex', x='Year', y='Incidence per 100,000 population') }

#----------------------
# counts

# total incidence as an absolute number 
if (set=='gbd') {
ggplot(inc[cause=='Malaria' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria incidence, PCE countries, 2000 - 2017',
       caption = cap,
       x='Year', y='Number of new cases') }

# total incidence by sex as an absolute number 
if (set=='gbd') {
ggplot(inc[cause=='Malaria' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria incidence, PCE countries, 2000 - 2017 by sex',
       caption = cap,
       color='Sex', x='Year', y='Number of new cases') }

dev.off()
