# --------------------------------------------------
# David Phillips
# 
# 1/26/2019
# Script that loads packages and file names
# Intended to be called by 1_master_file.r
# This exists just for code organizational purposes
# (use singularity exec /share/singularity-images/health_fin/forecasting/best.img R on IHME's new cluster)
# --------------------------------------------------

# --------------------------------------------
# Output file labels (set to '' for default) 
# in case we're running some secondary analysis
# this only affects files from step 5 onward
fileLabel = ''
# --------------------------------------------


# ------------------
# Load packages
set.seed(1)
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
library(boot)
library(lavaan)
library(blavaan)
library(viridis)
# library(Hmisc)
# library(lavaanPlot)
# library(semPlot)
library(raster)
library(parallel)
# library(dplyr)
library(splitstackshape)
# ------------------


# ---------------------------------------------------------------------------------
# Directories

# switch J for portability to cluster
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# directories
dir = paste0(j, '/Project/Evaluation/GF/')
ieDir = paste0(dir, 'impact_evaluation/cod/prepped_data/')
rtDir = paste0(dir, 'resource_tracking/_gf_files_gos/combined_prepped_data/')
fghDir = paste0(dir, 'resource_tracking/_fgh/prepped_data/')
whoDir = paste0(dir, 'resource_tracking/_ghe/who/prepped_data/')
mapDir = paste0(dir, '/mapping/multi_country/intervention_categories')
pnlpDir = paste0(dir, 'outcome_measurement/cod/prepped_data/PNLP/post_imputation/')
dhisDir = paste0(dir, 'outcome_measurement/cod/dhis_data/prepped/')
lbdDir = paste0(j, '/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/')
# ---------------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Supporting Files

# code-friendly version of indicator map file
indicatorMapFile = paste0(ieDir, 'DRC Indicator map - to code from.xlsx')

# list of interventions and codes
mfFile = paste0(mapDir, '/intervention_and_indicator_list.xlsx')

# archive function
source('./impact_evaluation/_common/archive_function.R')

# function that runs a SEM as unrelated regressions
source('./impact_evaluation/_common/run_lavaan_as_glm.r')
# ------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Inputs files

# resource tracking files with prepped budgets, expenditures, disbursements
budgetFile = paste0(rtDir, 'final_budgets.rds')
expendituresFile = paste0(rtDir, 'final_expenditures.rds')
fghFile = paste0(fghDir, 'prepped_current_fgh.rds')
gheMalFile = paste0(fghDir, 'ghe_actuals_malaria.rds')
whoFile = paste0(whoDir, 'who_prepped.rds')

# activities/outputs files
# pnlpFile = paste0(pnlpDir, 'imputedData_run2_agg_country.rds') # pnlp
pnlpHZFile = paste0(pnlpDir, 'archive/imputedData_run2_agg_hz.rds')
# snisBaseFile <- paste0(dhisDir, 'archive/base_services_drc_01_2017_09_2018_prepped.rds') # snis base services
# snisSiglFile <- paste0(dhisDir, 'archive/sigl_drc_01_2015_07_2018_prepped.rds') # snis sigl (supply chain)
combinedFile <- paste0(ieDir, 'base_pnlp_sigl_combined_data_hz_level.rds')
comp_base_file <- paste0(dhisDir, "base_reporting_completeness_hz_prepped_quarterly.rds")
comp_sigl_file <-paste0(dhisDir, "sigl/sigl1_reporting_completeness_hz_prepped_quarterly.rds")
ssc_file <- paste0(dhisDir, "SSC_data_2017_2018_prepped.rds")

# outcomes/impact files
mapITNFiles = list.files(paste0(lbdDir, 'mapitncov/mean/1y/'), '*.tif', 
	full.names=TRUE)
mapITNFiles = mapITNFiles[!grepl('tif.',mapITNFiles)]
mapACTFiles = list.files(paste0(lbdDir, 'map_antimalarial/mean/1y/'), '*.tif', 
	full.names=TRUE)
mapACTFiles = mapACTFiles[!grepl('tif.',mapACTFiles)]
mapIncidenceFiles = list.files(paste0(lbdDir, 'map_pf_incidence/mean/1y/'), 
	'*.tif', full.names=TRUE)
mapIncidenceFiles = mapIncidenceFiles[!grepl('tif.',mapIncidenceFiles)]
mapPrevalenceFiles = list.files(paste0(lbdDir, 'map_pf_prevalence/mean/1y/'), 
	'*.tif', full.names=TRUE)
mapPrevalenceFiles = mapPrevalenceFiles[!grepl('tif.',mapPrevalenceFiles)]
mapMortalityFiles = list.files(paste0(lbdDir, '../18_Malaria_GBD/raw/'), 
	'*.tif', full.names=TRUE)
mapMortalityFiles = mapMortalityFiles[!grepl('tif.',mapMortalityFiles)]
popFiles = list.files(paste0(lbdDir, 'worldpop_raked/total/1y/'), '*.tif', 
	full.names=TRUE)

# shapefiles
admin2ShapeFile = paste0(dir, '/mapping/cod/health_zones_who/health2.shp')

# "nodetables" aka "nodetables" 
# listing names of variables in each model, their labels and coordinates for the SEM graph
nodeTableFile1 = './impact_evaluation/drc/visualizations/nodetable_first_half.csv'
nodeTableFile2 = './impact_evaluation/drc/visualizations/nodetable_second_half.csv'
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Intermediate file locations
if (Sys.info()[1]!='Windows') {
username = Sys.info()[['user']]
clustertmpDir1 = paste0('/ihme/scratch/users/', username, '/impact_evaluation/combined_files/')
clustertmpDir2 = paste0('/ihme/scratch/users/', username, '/impact_evaluation/parallel_files/')
clustertmpDireo = paste0('/ihme/scratch/users/', username, '/impact_evaluation/errors_output/')
if (file.exists(clustertmpDir1)!=TRUE) dir.create(clustertmpDir1) 
if (file.exists(clustertmpDir2)!=TRUE) dir.create(clustertmpDir2) 
if (file.exists(clustertmpDireo)!=TRUE) dir.create(clustertmpDireo) 
}
# ---------------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Output Files

# output file from 2a_prep_resource_tracking.r
outputFile2a = paste0(ieDir, 'prepped_resource_tracking.RDS')

# output file from 2b_prep_activities_outputs.R
outputFile2b = paste0(ieDir, 'outputs_activites_for_pilot.RDS')
outputFile2b_wide = paste0(ieDir, 'outputs_activities_for_pilot_wide.RDS')

# output file from 2c_prep_outcomes_impact.r
outputFile2c_estimates = paste0(ieDir, 'aggregated_rasters.rds')
outputFile2c = paste0(ieDir, 'outcomes_impact.rds')

# output file from 3_merge_data.R
outputFile3 = paste0(ieDir, 'inputs_outputs.RDS')

# output files from 3b_correct_to_models.r
outputFile3b = paste0(ieDir, 'outcomes_impact_corrected.RDS')
outputFile3bGraphs = paste0(ieDir, '../visualizations/outcomes_impact_correction_results.pdf')

# output file from 4a_set_up_for_analysis.r
outputFile4a = paste0(ieDir, 'first_half_pre_model.rdata')
if (Sys.info()[1]!='Windows') { 
	outputFile4a_scratch = paste0(clustertmpDir1, 'first_half_data_pre_model.rdata')
}

# output file from 4b_set_up_for_second_half_analysis.r
outputFile4b = paste0(ieDir, 'second_half_data_pre_model.rdata')
if (Sys.info()[1]!='Windows') { 
	outputFile4b_scratch = paste0(clustertmpDir1, 'second_half_data_pre_model.rdata')
}

# output file from 4c and 4d_explore_data.r (graphs)
outputFile4c = paste0(ieDir, '../visualizations/first_half_exploratory_graphs.pdf')
outputFile4d = paste0(ieDir, '../visualizations/second_half_exploratory_graphs.pdf')

# output file from 5a_run_first_half_analysis.R
outputFile5a = paste0(ieDir, 'first_half_model_results', fileLabel, '.rdata')

# output file from 5b_run_second_half_analysis.r
outputFile5b = paste0(ieDir, 'second_half_model_results', fileLabel, '.rdata')

# output file from 6_display_results.r
outputFile6a = paste0(ieDir, '../visualizations/sem_diagrams', fileLabel, '.pdf')
outputFile6b = paste0(ieDir, '../visualizations/efficiency_effectiveness_graphs', fileLabel, '.pdf')
outputFile6c = paste0(ieDir, '../visualizations/impact_analysis', fileLabel, '.pdf')
outputFile6d = paste0(ieDir, '../visualizations/health_zone_effects', fileLabel, '.pdf')
# -----------------------------------------------------------------------------
