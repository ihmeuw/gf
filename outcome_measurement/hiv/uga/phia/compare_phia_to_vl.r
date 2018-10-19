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
outFileDist = paste0(dir, 'output/district_vls_full.rds')
# -----------------------------------------------------------

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

lmFit3 = lmer(phia_vls~vld_suppression+art_coverage+vld_suppression_adj+(1|region), distData)
predict(lmFit3)

# look at the output and random effects of the models
summary(lmFit2)
summary(lmFit3)

x = ranef(lmFit2)
y = ranef(lmFit3)

x = data.table(x$region)
setnames(x, '(Intercept)', 'lmFit2')

y = data.table(y$region)

setnames(y, '(Intercept)', 'lmFit3')
z = cbind(x, y)

phia = distData[ ,.(phia_vls = mean(phia_vls)), by=region]

phia = cbind(z, phia)


distData[, ratio2:=predict(lmFit2)]
distData[, ratio3:=predict(lmFit3)/100]

ggplot(distData, aes(x=ratio2, y=ratio3)) + geom_point()

x = distData[ ,.(ratio2=mean(ratio2), ratio3=mean(ratio3)), by=region]


# lmFit4 = lmer(phia_vls~vld_suppression*art_coverage+(1|region), distData)


# region-specific correction of district-level data
distData[, ratio:=predict(lmFit2)]
distData[, vld_suppression_hat:=vld_suppression_adj/ratio]

# predict the vl ratio at the region level
regData[, ratio:=predict(lmFit2, newdata=regData)]
regData[, vld_suppression_hat:=vld_suppression_adj/ratio]

# predict the vl ratio at the district level
distData[, ratio:=predict(lmFit2, newdata=distData)]
distData[, vld_suppression_hat:=vld_suppression_adj/ratio]
# ---------------------------------------------------------------------------

# ---------------------------------------------------------
# Save estimates

saveRDS(distData, outFileDist)
# ---------------------------------------------------------

# ------------------
# Run graphing code
graphVL(dir)
# ------------------
