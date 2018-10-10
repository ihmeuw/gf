# ---------------------------------------------------------
# David Phillips
#
# 10/31/2017
# Various comparisons between aggregate PHIA VL suppression 
# estimates and numbers from the Uganda VL dashboard
# The working directory should be the root of this repo
# ---------------------------------------------------------

# TO DO
# - test alternate models instead of model 2
# - add covariates?
# - test sensitivity to different ART estimates


# --------------------
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
# --------------------


# -----------------------------------------------------------
# Files and directories

# prep function
source('./outcome_measurement/hiv/uga/phia/prep_phia_vl_dashboard.r')

# graph function
source('./outcome_measurement/hiv/uga/phia/graph_phia_vl_dashboard.r')

# data directory
dir = 'J:/Project/Evaluation/GF/outcome_measurement/uga/'

# ouptut data
outFileDistYear = paste0(dir, 'output/district_year_vls.csv')
# -----------------------------------------------------------


# ---------------------------------------------------------
# Prep data at different levels
regData = prepVL(dir, level='region')
distData = prepVL(dir, level='district')
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
