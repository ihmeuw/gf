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
# detect the user 

user = Sys.info()[['user']]

#--------------------------------
# set directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/impact_over_time_gbd/')

code_dir = paste0('C:/Users/', user, '/local/gf/impact_evaluation/gbd_epidemiology/')

#--------------------------------
# upload the data sets

dt = fread(paste0(dir, 'ihme_age_standardized_2017.csv'))

#--------
# subset to age standardized rates and all ages counts
dt = dt[!(metric=='Rate' & age=='All Ages')]

# reset the order for facet wrapped graphs
dt$location = factor(dt$location, c("Cambodia", "Democratic Republic of the Congo",
      "Guatemala", "Mozambique", "Myanmar", "Senegal", "Sudan", "Uganda", "Global"), 
      c("Cambodia", "DRC", "Guatemala", "Mozambique", "Myanmar", "Senegal", 
        "Sudan", "Uganda", "Global Trend"))
                        
#--------


# rates = dt[sex=='Both' & metric=='Rate' & (year==2010 | year==2017),.(measure, location, cause, year, val)]
# rates = dcast(rates, measure+location+cause~year)
# setnames(rates, c('2010', '2017'), c('y2010', 'y2017'))
# rates[ , roc:=round((log(y2017/y2010)/17), 3)]
# rates[ ,roc:=roc*100]
# rates[ ,c('y2010', 'y2017'):=NULL]


#-------------------------
# calculate annualized rates of change - 2000 to 2017

rates = dt[sex=='Both' & metric=='Rate' & (year==2000 | year==2017),.(measure, location, cause, year, val)]
rates = dcast(rates, measure+location+cause~year)
setnames(rates, c('2000', '2017'), c('y2000', 'y2017'))
rates[ , roc:=round((log(y2017/y2000)/17), 3)]
rates[ ,roc:=roc*100]
rates[ ,c('y2000', 'y2017'):=NULL]

# merge in annualized roc
dt = merge(dt, rates, by=c('measure', 'location', 'cause'), all=T)

# label the locations with associated rates of change
dt[ , label:=paste0(location, ' (', roc, '%)')]

#-------------------------
# divide into incidence and deaths 

deaths = dt[measure =='Deaths']
inc = dt[measure=='Incidence']

#-------------------------
# source outside code for tables and figures

# works on caitlin's computer - change to relevant directory
source(paste0(code_dir, 'mort_inc_all_pce_countries_table.R'))

# works on caitlin's computer - change to relevant directory
source(paste0(code_dir, "trend_figures_synthesis.R"))

#-----------------------------------------------------
# MORTALITY GRAPHS

pdf(paste0(dir, 'outputs/mortality_pce_countries.pdf'), height=9, width=12)

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
  labs(title='HIV/AIDS-related deaths per 100,000 population, PCE countries, 1990 - 2017',
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
  labs(title='HIV/AIDS-related deaths, PCE countries, 2000 - 2017',
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
ggplot(deaths[cause=='Tuberculosis' & metric=='Rate' & sex=='Both'], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis deaths per 100,000 population, PCE countries, 1990 - 2017',
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
  labs(title='Tuberulosis deaths per 100,000 population, PCE countries, 2000 - 2017',
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
  labs(title='Tuberculosis deaths per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       color='Sex', x='Year', y='deaths per 100,000 population')

#----------------------
# counts

# total deaths as an absolute number 
ggplot(deaths[cause=='Tuberculosis' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis deaths, PCE countries, 2000 - 2017',
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       x='Year', y='Number of deaths')

# total deaths by sex as an absolute number 
ggplot(deaths[cause=='Tuberculosis' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis deaths, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       color='Sex', x='Year', y='Number of deaths')

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
  labs(title='Malaria deaths per 100,000 population, PCE countries, 1990 - 2017',
       caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths as a rate with annualized rates of change

ggplot(deaths[cause=='Malaria' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria deaths per 100,000 population, PCE countries, 2000 - 2017',
       subtitle = 'Annualized rates of change in parentheses', 
       caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Deaths per 100,000 population')

#-------------------------------
# total deaths by sex as a rate
ggplot(deaths[cause=='Malaria' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria deaths per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='deaths per 100,000 population')

#----------------------
# counts

# total deaths as an absolute number 
ggplot(deaths[cause=='Malaria' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria deaths, PCE countries, 2000 - 2017',
       caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Number of deaths')

# total deaths by sex as an absolute number 
ggplot(deaths[cause=='Malaria' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria deaths, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='Number of deaths')

dev.off()

#-----------------------------------------------------------------


#-----------------------------------------------------
# INCIDENCE GRAPHS

pdf(paste0(dir, 'outputs/incidence_pce_countries.pdf'), height=9, width=12)

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
  labs(title='HIV incidence per 100,000 population, PCE countries, 1990 - 2017',
      caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incidence as a rate with annualized rates of change

ggplot(inc[cause=='HIV/AIDS' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV incidence per 100,000 population, PCE countries, 2000 - 2017',
       subtitle = 'Annualized rates of change in parentheses', 
       caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incidence by sex as a rate

ggplot(inc[cause=='HIV/AIDS' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV incidence per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='Incidence per 100,000 population')

#----------------------
# counts

# total incidence as an absolute number 
ggplot(inc[cause=='HIV/AIDS' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV incident cases, PCE countries, 2000 - 2017',
     caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Number of new cases')

# total inc by sex as an absolute number 
ggplot(inc[cause=='HIV/AIDS' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='HIV incident cases, PCE countries, 2000 - 2017 by sex',
    caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='Number of new cases')

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
  labs(title='Tuberculosis incidence per 100,000 population, PCE countries, 1990 - 2017',
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incidence as a rate with annualized rates of change

ggplot(inc[cause=='Tuberculosis' & metric=='Rate' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~label, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberulosis incidence per 100,000 population, PCE countries, 2000 - 2017',
       subtitle = 'Annualized rates of change in parentheses', 
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incidence by sex as a rate
ggplot(inc[cause=='Tuberculosis' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis incidence per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       color='Sex', x='Year', y='Incidence per 100,000 population')

#----------------------
# counts

# total incidence as an absolute number 
ggplot(inc[cause=='Tuberculosis' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis incidence, PCE countries, 2000 - 2017',
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       x='Year', y='Number of new cases')

# total incidence by sex as an absolute number 
ggplot(inc[cause=='Tuberculosis' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Tuberculosis incidence, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease; includes drug suscpetible and drug resistant TB',
       color='Sex', x='Year', y='Number of inc')

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
  labs(title='Malaria incidence per 100,000 population, PCE countries, 1990 - 2017',
       caption = 'Source: IHME Global Burden of Disease',
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
       caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Incidence per 100,000 population')

#-------------------------------
# total incidence by sex as a rate
ggplot(inc[cause=='Malaria' & metric=='Rate' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria incidence per 100,000 population, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='Incidence per 100,000 population')

#----------------------
# counts

# total incidence as an absolute number 
ggplot(inc[cause=='Malaria' & metric=='Number' & sex=='Both' & 1999 < year], aes(x=year, y=val))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='#f0f0f0')+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria incidence, PCE countries, 2000 - 2017',
       caption = 'Source: IHME Global Burden of Disease',
       x='Year', y='Number of new cases')

# total incidence by sex as an absolute number 
ggplot(inc[cause=='Malaria' & metric=='Number' & sex!='Both' & 1999 < year], aes(x=year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill='#f0f0f0', linetype=0)+
  geom_point() + 
  facet_wrap(~location, scales='free_y') +
  geom_line() +
  theme_bw() +
  labs(title='Malaria incidence, PCE countries, 2000 - 2017 by sex',
       caption = 'Source: IHME Global Burden of Disease',
       color='Sex', x='Year', y='Number of new cases')

dev.off()
