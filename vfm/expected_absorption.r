# ------------------------------------------------------
# David Phillips
#
# 10/5/2018
# Predict expected absorption based on past trends
# Loads a file created by ./vfm/absorption_correlates.r
# ------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(boot)
library(data.table)
library(ggplot2)
# ------------------


# -----------------------------------------------------------------
# Files and directories

# root directory for input/output
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/'

# input data
inFile = paste0(dir, 'mapping/cleaned_total_data.csv')

# place to store the regression output
regInFile = paste0(dir, '../../vfm/outputs/absorption_correlates_model_fit.rdata')

# output graphs
outFile = paste0(dir, '../../vfm/visualizations/expected_absorption.pdf')
# -----------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# subset to budgets
data = data[data_source=='fpm']

# subset to upcoming budgets
data[, maxyear:=max(year), by=grant_number]
data = data[maxyear>2016]

# collapse to module level
byVars = c('disease','country','grant_number','year','gf_module')
data=data[, list('budget'=sum(budget,na.rm=TRUE)), by=byVars]

# compute cumulative budget/expenditure by grant-module
byVars = c('grant_number','gf_module')
data[, cumulative_budget:=cumsum(budget), by=byVars]
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Generate extra predictor variables

# year within grant and years from end of grant
data[, grant_year:=as.numeric(as.factor(year)), by='grant_number']
data[, years_from_end:=max(grant_year)-grant_year+1, by='grant_number']

# number of modules within grant
data[, num_modules:=length(unique(gf_module)), by='grant_number']

# grant window 
# (shouldn't matter; already captured in complexity/size/SDA composition)
data[year<2008, window:=1]
data[year>=2008 & year<2011, window:=2]
data[year>=2011 & year<2014, window:=3]
data[year>=2014 & year<2017, window:=4]
data[year>=2017 & year<2020, window:=5]
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep regression output

# load regression output
load(regInFile)

# identify modules that weren't in the model data (case management is the intercept)
origModules = names(coef(lmFit2))
origModules = gsub('gf_module', '', origModules)
missing = unique(data$gf_module)
missing = missing[!missing %in% origModules]

# use substitutes for missing modules
data[, true_module:=gf_module]
data[gf_module=='Comprehensive programs for people in prisons and other closed settings', 
	gf_module:='Comprehensive prevention programs for sex workers and their clients']
data[gf_module=='Comprehensive prevention programs for people who inject drugs and their partners', 
	gf_module:='Comprehensive prevention programs for sex workers and their clients']
data[gf_module=='Performance Based Financing', 
	gf_module:='National health strategies']
# ----------------------------------------------------------------------


# ----------------------------------------------------------
# Predict from regression

# predict absorption
data[, expected_absorption:=predict(lmFit2, newdata=data)]
data[, expected_absorption:=inv.logit(expected_absorption)]

# compute expenditure
data[, expected_expenditure:=budget*expected_absorption]

# set substitutes back to their true module
data[, gf_module:=true_module]
# ----------------------------------------------------------


# -------------------------------------------------------------------------
# Aggregate to grant level

# aggregate
byVars = c('country','grant_number','year')
agg = data[, .(budget=sum(budget),na.rm=TRUE, 
				expected_expenditure=sum(expected_expenditure,na.rm=TRUE)), 
				by=byVars]

# recompute expected absorption
agg[, expected_absorption:=expected_expenditure/budget]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Set up to graph

# colors
cols = c('#008080','#70a494','#b4c8a8','#f6edbd','#edbb8a','#de8a5a','#ca562c')

# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Graphs

# loop over countries

# -------------------------------------------------------------------------------------------------


# -----------------------------
# Save graphs
pdf(outFile, height=6, width=10.5)
p1
p2
p3
dev.off()
# -----------------------------


# -----------------------------
# Save model output
save('lmFit2', file=regOutFile)
# -----------------------------
