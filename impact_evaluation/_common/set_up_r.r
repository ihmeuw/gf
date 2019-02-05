# --------------------------------------------------
# David Phillips
# 
# 1/26/2019
# Script that loads packages and file names
# Intended to be called by 1_master_file.r
# This exists just for code organizational purposes
# --------------------------------------------------


# ------------------
# Load packages
library(lavaanPlot)
# library(semPlot)
library(data.table)
library(lubridate)
library(readxl)
library(stringr)
library(ggplot2)
library(stats)
library(Rcpp)
library(grid)
library(gridExtra)
library(ggrepel)
library(lubridate)
library(lavaan)
# ------------------


# ---------------------------------------------------------------------------------
# Directories

# switch J for portability to cluster
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# directories
dir = paste0(j, '/Project/Evaluation/GF/')
ieDir = paste0(dir, 'impact_evaluation/cod/prepped_data/')
rtDir = paste0(dir, 'resource_tracking/multi_country/mapping/')
mapDir = paste0(dir, '/mapping/multi_country/intervention_categories')
pnlpDir = paste0(dir, 'outcome_measurement/cod/prepped_data/PNLP/post_imputation/')
dhisDir = paste0(dir, 'outcome_measurement/cod/dhis_data/prepped/')
# ---------------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Supporting Files

# code-friendly version of indicator map file
indicatorMapFile = paste0(ieDir, 'DRC Indicator map - to code from.xlsx')

# list of interventions and codes
mfFile = paste0(mapDir, '/intervention_and_indicator_list.xlsx')
# ------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Inputs files

# resource tracking files with prepped budgets, expenditures, disbursements
budgetFile = paste0(rtDir, 'final_budgets.rds')
expendituresFile = paste0(rtDir, 'final_expenditures.rds')
fghFile = paste0(rtDir, 'prepped_current_fgh.csv')

# activities/outputs files
pnlpFile = paste0(pnlpDir, 'imputedData_run2_agg_country.rds') # pnlp
pnlpHZFile = paste0(pnlpDir, 'imputedData_run2_agg_hz.rds')
snisBaseFile <- paste0(dhisDir, 'archive/base_services_drc_01_2017_09_2018_prepped.rds') # snis base services
snisSiglFile <- paste0(dhisDir, 'archive/sigl_drc_01_2015_07_2018_prepped.rds') # snis sigl (supply chain)
# ---------------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Output Files

# output file from 2a_prep_resource_tracking.r
outputFile2a = paste0(ieDir, 'prepped_resource_tracking.RDS')

# output file from 2b_prep_activities_outputs.R
outputFile2b = paste0(ieDir, 'outputs_activites_for_pilot.RDS')
outputFile2b_wide = paste0(ieDir, 'outputs_activities_for_pilot_wide.RDS')

# output file from 3_merge_data.R
outputFile3 = paste0(ieDir, 'pilot_data.RDS')

# output file from 4_explore_data.r (graphs)
outputFile4 = paste0(ieDir, '../visualizations/pilot_data_exploratory_graphs.pdf')

# output file from 5a_set_up_for_analysis.r
outputFile5a = paste0(ieDir, 'pilot_data_pre_model.rds')

# output file from 5b_run_analysis.R
outputFile5b = paste0(ieDir, 'pilot_model_results.rdata')
# -----------------------------------------------------------------------------
