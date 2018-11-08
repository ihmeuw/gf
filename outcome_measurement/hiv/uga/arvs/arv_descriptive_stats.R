# Descriptive statistics - ARV stock outs

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
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# working directory to aggregate
dt = readRDS(paste0(dir, 'arv_stockouts_2017_2018.rds'))

# subset dates to before september 30, 2018
dt = dt[date < '2018-10-01']

#------------------------------
# merge in regions

regions = fread(paste0(root, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
regions = regions[ ,.(region = region10_name, district = dist112_name)]
regions = regions[!duplicated(district)]
dt = merge(dt, regions, by='district', all.x=T)



# descrtipve statiss

dt[art_site==T & !all(is.na(arvs)), length(unique(facility)), by=year]

dt[all(is.na(arvs)), unique(facility)]