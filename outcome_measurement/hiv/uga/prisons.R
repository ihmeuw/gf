# ----------------------------------------------
# Caitlin O'Brien-Carelli
# Prisons
# ----------------------------------------------
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
library(Amelia)
library(MASS)
library(gtools)

# set input/output directory
# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uvl1 <- readRDS(paste0(dir, "/sex_data.rds"))
prisons <- uvl1[prison==1 & sex!='Unknown']


# start from scale up and continue through april 2018
table1 <- prisons[ , .(ratio=100*(sum(suppressed)/sum(valid_results))), by=.(facility_id, facility_name, sex, date)]

ggplot(table1, aes(x=date, y=ratio, color=factor(facility_id), group=facility_id)) + 
  facet_wrap(~sex) +
  geom_point() + geom_line() +
  theme_bw()




