# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/18/2019
# Rbind the UVL data sets together
# Run dist_facilities_uvl.R to download facility and district names
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(stringr) 
library(plyr)
library(RColorBrewer)
library(gplots)
library(corrplot)
library(boot)
library(stargazer)
# --------------------
# detect if operating on windows or on the cluster 

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
# ------------------------------
# set directories and upload data 

inDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/prepped/')
outDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/age_analyses/')

# upload the data 
dt = readRDS(paste0(inDir, 'uvl_prepped_2014_2018_.rds'))

#-----------------------------
# import the regions 
regions = fread(paste0(j, '/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv'))
regions = regions[ ,.(dist112_name, region10_alt)]
setnames(regions, c('district', 'region'))
regions = regions[!duplicated(regions)]

# merge the regions into the data 
dt = merge(dt, regions, by='district', all.x=T)

#--------------------------------
# create a year variable

dt[ ,year:=year(date)]

# ------------------------------
# subset to successes and trials

dt = dt[ ,.(facility, district, region, date, sex, age, year, 
            successes = suppressed, trials = valid_results)]

dt[ ,successes:=round(successes)]
dt[ ,trials:=round(trials)]
dt[trials < successes, successes:=trials]
#------------------------------
# sum to the right level - annual facility level

dt = dt[ ,.(successes=sum(successes), trials=sum(trials)), 
         by=.(facility, district, region, sex, age, year)]

# ------------------------------
# linear model

linear = glm(successes~sex, data=dt)
summary(linear)

stargazer(linear, type='text', align=TRUE,
          dep.var.labels = 'Virally suppressed',
          covariate.labels = 'Male (reference: female)',
          out='sex_only_linear.txt')

linear2 = glm(successes~sex+age, data=dt)
summary(linear2)

stargazer(linear2, type='text', align=TRUE,
          dep.var.labels = 'Virally suppressed',
          covariate.labels = 'Male (reference: female)',
          out='sex_age_linear.txt', no.space=TRUE)


#---------------------------------------------------


mod = glm(formula = cbind(successes, trials - successes) ~ sex+region+year+age,
          family = binomial(logit),
          data = dt)

mod2 = glm(formula = cbind(successes, trials - successes) ~ sex+region+year,
          family = binomial(logit),
          data = dt)

mod3 = glm(formula = cbind(successes, trials - successes) ~ sex+region,
           family = binomial(logit),
           data = dt)


stargazer(mod, mod2, mod3, type='text', align=TRUE,
          dep.var.labels = 'Virally suppressed',
          covariate.labels = 'Male (reference: female)',
          out='sex_age_region_logistic.txt', no.space=TRUE)


stargazer(mod2, mod3, type='text', align=TRUE,
          dep.var.labels = 'Virally suppressed',
          covariate.labels = 'Male (reference: female)',
          out='sex_region_logistic.txt', no.space=TRUE)



# ------------------------------
# multiple logistic regression 







smithsonTransform = function(x) { 
  N=length( x[!is.na(x)] )
  prop_lsqueeze = (((x*(N-1))+0.5)/N)
}


dt[ ,ratio:=smithsonTransform(ratio)]


glm(ratio~sex, data=dt, family='binomial')



# ------------------------------

model = glm(successes~sex, data=dt, family='binomial')

summary(model)
