# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/25/2018
# Descriptive statistics for the Uganda Viral Load Dashboard
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)

# --------------------
# detect if on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# ----------------------------------------------
# Files and directories

# set input/output directory
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')

# upload the data with month, year, sex
dt = readRDS(paste0(dir, "/prepped_data/sex_data.rds"))

#------------------------------------
# merge in the regions
# missing districts will be missing regions

regions = fread(paste0(root, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
regions = regions[ ,.(region = region10_name, district = dist112_name)]
regions = regions[!duplicated(district)]
dt = merge(dt, regions, by='district', all.x=T)

#-----------------------------------------------
# 2018 table

# subset to 2018
tab = dt[year(date)==2018]

# facilities reporting
tab[ ,.(length(unique(facility))), by=region][order(region)]
tab[ ,.(length(unique(facility)))]

# viral load tests performed
tab[sex=='Male',.(sum(samples_tested)), by=region][order(region)]
tab[sex=='Female',.(sum(samples_tested)), by=region][order(region)]
tab[ ,.(sum(samples_tested)), by=region][order(region)]







