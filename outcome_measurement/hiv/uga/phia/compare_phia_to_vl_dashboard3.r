# ---------------------------------------------------------
# David Phillips
#
# 10/16/18
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

# --------------------

# -----------------------------------------------------------
# Files and directories

# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  'Project/Evaluation/GF/outcome_measurement/uga/phia_2016/')

# prep function - sourced from local repo (change directory on cluster)
source('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/phia/prep_phia_vl_dashboard_new2.r')
 
# graph function
# source('./outcome_measurement/hiv/uga/phia/graph_phia_vl_dashboard.r')

# ouptut data
outFileDistYear = paste0(dir, 'output/district_year_vls.csv')
# -----------------------------------------------------------


# ---------------------------------------------------------
# Prep data at different levels using the prepVL function

regData = prepVL(dir, level='region')
distData = prepVL(dir, level='district')
facData = prepVL(dir, level='facility')

# if annual data is available
regDataAnnual = prepVL(dir, level='region', annual=TRUE)
distDataAnnual = prepVL(dir, level='district', annual=TRUE)
# ---------------------------------------------------------

# ---------------------------------------------------------------------------
# Analysis

# linear fit
lmFit = lm(logit(vld_suppression_adj/100)~logit(phia_vls/100), regData)

# store prediction in appropriate range for graphs
s = min(regData$vld_suppression_adj)
e = max(regData$phia_vls)
predData = data.table(phia_vls=seq(s, e, .1))
preds = inv.logit(predict(lmFit, interval='confidence', newdata=predData))*100
predData = cbind(predData, preds)

# linear fit on correction factors
lmFit2 = lmer(vld_suppression_adj/phia_vls~(1|region10_name), distData)
# lmFit2 = lmer(phia_vls~vld_suppression_adj+(1|region10_name), distData)

# region-specific correction of district-level data
distData[, ratio:=predict(lmFit2)]
distData[, vld_suppression_hat:=vld_suppression_adj/ratio]

# region-specific correction of region-year-level data
regDataAnnual[, ratio:=predict(lmFit2, newdata=regDataAnnual)]
regDataAnnual[, vld_suppression_hat:=vld_suppression_adj/ratio]

# region-specific correction of district-year-level data
distDataAnnual[, ratio:=predict(lmFit2, newdata=distDataAnnual)]
distDataAnnual[, vld_suppression_hat:=vld_suppression_adj/ratio]
# ---------------------------------------------------------------------------


# ---------------------------------------------------------
# Save estimates
write.csv(distDataAnnual, outFileDistYear, row.names=FALSE)
# ---------------------------------------------------------


# ------------------
# Run graphing code
graphVL(dir)
# ------------------
