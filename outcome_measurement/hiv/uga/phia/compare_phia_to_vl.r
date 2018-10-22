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
library(ggrepel)


# --------------------

# -----------------------------------------------------------
# Files and directories

# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/')

# prep function - sourced from local repo (change directory on cluster)
source('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/phia/prep_phia_vl.r')
 
# graph function
# source('./outcome_measurement/hiv/uga/phia/graph_phia_vl_dashboard.r')

# ouptut data
outFileReg = paste0(dir, 'output/region_vls_full.rds')
outFileDist = paste0(dir, 'output/district_vls_full.rds')

# -----------------------------------------------------------
# export a facility level data set for the same time period as phia

# # upload the data with month, year, sex
# uvl <- readRDS(paste0(j, "/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/prepped_data/sex_data.rds"))
# 
# # import the ten regions that are included in phia
# regions = fread(paste0(j, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
# regions = unique(regions[ ,.(region=region10_alt, district_name=dist112_name)])
# 
# # subset to the relevant dates
# uvl = uvl[date>= '2016-08-01' & date <='2017-03-01']
# 
# # merge the names of regions and their associated districts with the vl data 
# uvl = merge(uvl, regions, by='district_name')
# 
# # export a facility level data set 
# uvl = uvl[ ,.(suppressed=sum(suppressed), valid_results=sum(valid_results)), by=.(facility=facility_name, district=district_name, region)]
# 
# # export the file to analyze - use the names in the shape file 
# saveRDS(uvl, paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/prepped/vl_data.rds'))

#------------------------------------------------

# -------------------------
# Prep data at different levels using the prepVL function
regData = prepVL(dir, level='region')
distData = prepVL(dir, level='district')
facData = prepVL(dir, level='facility')

# -------------------------

# ---------------------------------------------------------------------------
# Analysis - use the merged, prep data to run analyses 

# linear fit
lmFit = lm(logit(vld_suppression_adj/100)~logit(phia_vls/100), regData)

# store prediction in appropriate range for graphs
s = min(regData$vld_suppression_adj)
e = max(regData$phia_vls)
predData = data.table(phia_vls=seq(s, e, .1))
preds = inv.logit(predict(lmFit, interval='confidence', newdata=predData))*100
predData = cbind(predData, preds)

# linear fit on correction factors (linear mixed effects)
lmFit2 = lmer(vld_suppression_adj/phia_vls~(1|region), distData)

# alternate modelS
# lmFit3 = lmer(phia_vls~vld_suppression+art_coverage+vld_suppression_adj+(1|region), distData)
# summary(lm(phia_vls~vld_suppression*art_coverage+factor(region), distData))
# lmFit4 = lmer(phia_vls~vld_suppression*art_coverage+(1|region), distData) # warning: region perfectly predicts phia

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

# ------------------
# Run graphing code
graphVL(dir)
# ------------------
