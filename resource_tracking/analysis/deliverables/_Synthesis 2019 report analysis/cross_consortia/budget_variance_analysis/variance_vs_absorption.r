# -------------------------------------------------------------------
# David Phillips
# 
# 1/2/2019
# Check if PRs gradually shift money away from low-absorbing modules
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(viridis)
library(ggplot2)
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Files and directories

# input file
dir = 'J:/Project/Evaluation/GF/resource_tracking'
inFile =  paste0(dir, '/_gf_files_gos/combined_prepped_data/budget_exp_wide.rds')

# output file
outDir = paste0(dir, '/visualizations/budget_variance_analysis/')
outFile = paste0(outDir, 'variance_vs_absorption.pdf')
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(inFile)

# generate new variables
data[, variance_S3:=budget_S3-original_budget_S3]
data[, variance_S2:=budget_S2-original_budget_S2]
data[, variance_S1:=budget_S1-original_budget_S1]

data[, absorption_S3:=expenditure_S3/budget_S3]
data[, absorption_S2:=expenditure_S2/budget_S2]
data[, absorption_S1:=expenditure_S1/budget_S1]

# transform data
vars = names(data)[grepl('absorption|variance', names(data))]
for(v in vars) data[, (paste0(v, '_transformed')):=get(v)]

data[absorption_S1>10, absorption_S1_transformed:=NA]
data[absorption_S2>10, absorption_S2_transformed:=NA]
data[absorption_S3>10, absorption_S3_transformed:=NA]

data[variance_S1>1e7 | variance_S1< -2e7, variance_S1_transformed:=NA]
data[variance_S2>1e7 | variance_S2< -2e7, variance_S2_transformed:=NA]
data[variance_S3>1e7 | variance_S3< -2e7, variance_S3_transformed:=NA]

# melt semesters long
long = melt(data, id.vars=c('loc_name','grant','grant_period','disease','gf_module'), 
	measure.vars=c('budget_S', 'original_budget_S', 'expenditure_S', 'variance_S', 'absorption_S'))
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Analysis

# correlate S3 variance with S2 absorption
lm(variance_S3_transformed ~ absorption_S2_transformed)
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Graph
ggplot(data, aes(y=variance_S3_transformed, x=absorption_S2_transformed)) + 
	geom_point() +
	geom_smooth(method='lm') + 
	theme_bw()

# -------------------------------------------------------------------

