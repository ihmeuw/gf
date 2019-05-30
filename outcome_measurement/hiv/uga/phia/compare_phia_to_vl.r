# ---------------------------------------------------------
# David Phillips
# Caitlin O'Brien-Carelli
# 10/22/18
# Compares Population-Based HIV Impact Assessment (PHIA) to VL Dashboard
# The working directory should be the root of this repo

# ---------------------------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(readstata13)
library(tools)
library(boot)
library(lme4)
library(reshape2)
library(stringr)
library(rgeos)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(Hmisc)
library(survey)
library(maptools)
library(rgeos)
library(raster)
library(rgdal)
library(tibble)
library(dplyr)
library(plyr)

# --------------------

# -----------------------------------------------------------
# Files and directories

# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/')

# prep function - sourced from local repo (change directory on cluster)
source('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/phia/prep_phia_vl.r')
 
# graph functions
source('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/phia/graph_phia_vl_overlap.r')

#source('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/phia/graph_phia_vl_new_data.R')

# ouptut data
outFileReg = paste0(dir, 'output/region_vls_full.rds')
outFileDist = paste0(dir, 'output/district_vls_full.rds')

# -----------------------------------------------------------
# export a facility level data set for the same time period as phia

# upload the data with month, year, sex
uvl = readRDS(paste0(j, "/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/prepped_data/sex_data.rds"))

# import the ten regions that are included in phia
regions = fread(paste0(j, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
regions = unique(regions[ ,.(region=region10_alt, district_name=dist112_name)])

 # merge the names of regions and their associated districts with the vl data
uvl = merge(uvl, regions, by='district_name')

# subset to the phia overlap and save
overlap = uvl[date>= '2016-08-01' & date <='2017-03-01']

# export a facility level data set
overlap = overlap[ ,.(suppressed=sum(suppressed), valid_results=sum(valid_results)), by=.(facility=facility_name, district=district_name, region)]
saveRDS(overlap, paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/prepped/vl_data_overlap.rds'))

# subset to 2017 to present and save
prosp = uvl[date >= '2017-01-01']
prosp = prosp[ ,.(suppressed=sum(suppressed), valid_results=sum(valid_results)), by=.(facility=facility_name, district=district_name, region)]
saveRDS(prosp, paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/prepped/vl_data_prospect.rds'))

# annual
dist_ann= uvl[date >= '2017-01-01']
dist_ann = dist_ann[ ,.(suppressed=sum(suppressed), valid_results=sum(valid_results)),
                     by=.(facility=facility_name, district=district_name, region, year)]
saveRDS(dist_ann, paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/prepped/vl_data_annual.rds'))

#------------------------------------------------

# -------------------------
# Prep data at different levels using the prepVL function
# change overlap to true to prep 
regData = prepVL(dir, level='region')
distData = prepVL(dir, level='district')
facData = prepVL(dir, level='facility')
# -------------------------

# ---------------------------------------------------------------------------
# Analysis - use the merged, prep data to run analyses 

# linear fit - initial model 
lmFit = lm(logit(vld_suppression_adj/100)~logit(phia_vls/100), regData)

# store prediction in appropriate range for graphs
s = min(regData$vld_suppression_adj)
e = max(regData$phia_vls)
predData = data.table(phia_vls=seq(s, e, .1))
preds = inv.logit(predict(lmFit, interval='confidence', newdata=predData))*100
predData = cbind(predData, preds)

# linear fit on correction factors (linear mixed effects)
# preferred model
lmFit2 = lmer(vld_suppression_adj/phia_vls~(1|region), distData)

# predict the vl ratio at the district level
distData[, ratio:=predict(lmFit2)]
distData[, vld_suppression_hat:=vld_suppression_adj/ratio]

# predict the vl ratio at the region level
regData[, ratio:=predict(lmFit2, newdata=regData)]
regData[, vld_suppression_hat:=vld_suppression_adj/ratio]

# ---------------------------------------------------------------------------

# ---------------------------------------------------------
# Save estimates
saveRDS(regData, outFileReg)
saveRDS(distData, outFileDist)
# ---------------------------------------------------------

# ----------------------
# Run graphing code on the overlap period for PHIA/Vl or prospective data 
 
graphVL_time1(dir) 

#-----------------------





