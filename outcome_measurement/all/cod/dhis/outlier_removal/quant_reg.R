# Run a test quantile regression
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/20/2018
#


# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(stringr)
library(quantreg)
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#-----------------------------------
# to run on the cluster
# 
# # install the quant reg package
# install.packages("quantreg", lib=paste0(dir, '/quant_reg'))
# 
# # load 
# library(quantreg, lib.loc=paste0(dir, '/quant_reg/'))
#------------------------------------
# save an interim output for the single variable to test

# pmtct <- pnls[element_id=='gHBcPOF5y3z']
# saveRDS(pmtct, paste0(dir, 'prepped_data/test_pmtct.rds'))

#------------------------------------

pmtct <- readRDS(paste0(dir, 'prepped_data/test_pmtct.rds'))

# lactating women tested Hiv+

hiv <- pmtct[ ,.(count=sum(value)), by=.(element_eng, date, category)]

ggplot(hiv, 
       aes(x=date, y=count, color=category, group=category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng, scales='free_y') +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)


#-----
pmtct2 <- pmtct[  ,.(value=sum(value)), by=.(date, org_unit, element, element_eng, level, dps)]

quantFit <- rq(value~date+factor(org_unit), data=pmtct2, tau=0.5)
r <- resid(quantFit)

pmtct2[ ,resid:=r]

hist(r)

pmtct2[resid >200]

pmtct2[r>(median(r)+(3*sd(r)))]


ggplot(pmtct2[org_unit=="hk Chalanshi Centre de Santé"], 
       aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

#------
subset = data[data_element=='art']

quantFit = rq(value~date+factor(facility_name), data=subset, tau=.5)

r = resid(quantFit)

subset[, resid:=r]
subset[r>(median(r)+(3*sd(r)))]