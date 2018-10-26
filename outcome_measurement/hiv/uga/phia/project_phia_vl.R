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

# graphing function for new data - total and annual
#source('C:/Users/ccarelli/local/gf/outcome_measurement/hiv/uga/phia/graph_phia_vl_new_data.R')

# -------------------------
# Prep data at different levels using the prepVL function
# change overlap to true to prep 

# prospective data - total and annual
distDataAll = prepVL(dir, level='district', overlap=FALSE, annual=FALSE)
distDataAnn = prepVL(dir, level='district', overlap=FALSE, annual=TRUE)

# ---------------------------------------------------------------------------
# import the ratios

# merge in the ratios to apply
distData = readRDS(paste0(dir, 'output/district_vls_full.rds'))
distData = distData[ ,.(district, ratio)]

distDataAll = merge(distDataAll, distData, by='district')
distDataAnn = merge(distDataAnn, distData, by='district')

#---------------------------------------------
# run graph on prospective data

graphVL_time2(dir)

# ------------------
# monthly time trends
distDataAnn = prepVL(dir, level='district', overlap=FALSE, monthly=TRUE)


