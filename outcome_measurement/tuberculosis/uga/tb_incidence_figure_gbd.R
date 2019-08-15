# Prep the COD DHIS2 PNLS data 
# Drug-susceptible TB incidence graph

# ----------------------------------------------
# Caitlin O'Brien-Carelli

# ----------------------------------------------

# ------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 

# ------------------------------
# read in the data 

dt = fread("J:/Project/Evaluation/GF/outcome_measurement/uga/tb_gbd/ds_tb_incidence.csv")


# create a plot of drug susceptible tb incidence by sex with uncertainty
ggplot(dt[metric=='Rate'], aes(x=year, y=val, color=sex))+
  geom_ribbon(data=dt[metric=='Rate'], aes(ymin=lower, ymax=upper), alpha=0.7, fill='#f0f0f0')+
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(text=element_text(size=18))+
  labs(x='Year', y='Cases per 100,000', 
       title='Incidence rate for drug-susceptible TB: cases per 100,000 population', 
       color='Sex')


# click export as pdf
# ------------------------------