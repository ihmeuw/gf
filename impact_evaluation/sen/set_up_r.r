# --------------------------------------------------
# Francisco Rios, adapted from code by David Phillips, Emily Linebarger
# 
# 8/20/2019
# Script that loads packages and file names
# Intended to be called by 1_master_file.r
# This exists just for code organizational purposes
# (use singularity exec /share/singularity-images/health_fin/forecasting/best.img R on IHME's new cluster)
# --------------------------------------------------

# --------------------------------------------
# Output file labels (set to '' for default) 
# in case we're running some secondary analysis
# this only affects files from step 4c onward
#fileLabel = ''
# --------------------------------------------


# ------------------
# Load packages
set.seed(1)
library(Amelia)
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

#----------------------------------
#Set global variables - pulling to top so easier to read EL 8/15/19
# Current model versions

modelVersion1 = 'sen_tb_model1'

START_YEAR = 2014 #If available, what's the earliest year you should model data from? 


# ---------------------------------------------------------------------------------
# Directories

# switch J for portability to cluster
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# directories
dir = paste0(j, '/Project/Evaluation/GF/')
ieDir = paste0(dir, 'impact_evaluation/sen/')
rawIeDir = paste0(ieDir, 'raw_data/')
preppedIeDir =  paste0(ieDir, 'prepped_data/')
visIeDir = paste0(ieDir, 'visualizations/')
rtDir = paste0(dir, 'resource_tracking/_gf_files_gos/combined_prepped_data/')
fghDir = paste0(dir, 'resource_tracking/_odah/prepped_data/')
whoDir = paste0(dir, 'resource_tracking/_ghe/who/prepped_data/')
#mapDir = paste0(dir, '/mapping/multi_country/intervention_categories')
#pnlpDir = paste0(dir, 'outcome_measurement/sen/prepped_data/PNLP/post_imputation/')
#dhisDir = paste0(dir, 'outcome_measurement/sen/dhis_data/prepped/')
#lbdDir = paste0(j, '/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/')
# ---------------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Supporting Files

# code-friendly version of indicator map file
#indicatorMapFile = paste0(ieDir, 'DRC Indicator map - to code from.xlsx')

# list of interventions and codes
#mfFile = paste0(mapDir, '/intervention_and_indicator_list.xlsx')

# archive function
source('./impact_evaluation/_common/archive_function.R')

# function that runs a SEM as unrelated regressions
source('./impact_evaluation/_common/run_lavaan_as_glm.r')
# ------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Inputs files

# resource tracking files with prepped budgets, expenditures, disbursements
#budgetFile = paste0(rtDir, 'final_budgets.rds')
expendituresFile = paste0(rtDir, 'final_expenditures_sen.RDS')
fghFile = paste0(fghDir, 'other_dah_actuals_all_sen.RDS')

#gheMalFile = paste0(fghDir, 'ghe_actuals_malaria.rds')
whoFile = paste0(whoDir, 'who_prepped.rds')

# activities/outputs/outcomes data (raw data are stored in one ) files
raw_data_file = paste0(rawIeDir,'PANEL PNT TB-MR final_26072019.csv')

# data codebook
data_codebook = paste0(rawIeDir, 'codebook2.csv')

# mdrtb data
mdr2014data = paste0(rawIeDir, 'tb_mdr_2014.csv')
mdr2015data = paste0(rawIeDir, 'tb_mdr_2015.csv')
mdr2016data = paste0(rawIeDir, 'tb_mdr_2016.csv')
mdr2017data = paste0(rawIeDir, 'tb_mdr_2017.csv')
mdr2018data = paste0(rawIeDir, 'tb_mdr_2018.csv')

# shapefiles
admin2ShapeFile = paste0(dir, '/mapping/sen/shapefiles/gadm36_SEN_2.shp')

# "nodetables" aka "nodetables" 
# listing names of variables in each model, their labels and coordinates for the SEM graph
nodeTableFile1 = './impact_evaluation/sen/visualizations/nodetable_full.csv'

# reading in raw mdrtb data


# ---------------------------------------------------------------------------------


# # ---------------------------------------------------------------------------------
# # Intermediate file locations
username = Sys.info()[['user']]
clustertmpDir1 = paste0('/ihme/scratch/users/', username, '/impact_evaluation/combined_files/')
clustertmpDir2 = paste0('/ihme/scratch/users/', username, '/impact_evaluation/parallel_files/')
clustertmpDireo = paste0('/ihme/scratch/users/', username, '/impact_evaluation/errors_output/')
if (Sys.info()[1]!='Windows') {
        if (file.exists(clustertmpDir1)!=TRUE) dir.create(clustertmpDir1) 
        if (file.exists(clustertmpDir2)!=TRUE) dir.create(clustertmpDir2) 
        if (file.exists(clustertmpDireo)!=TRUE) dir.create(clustertmpDireo) 
}

#Add a temporary IE dir for when running on a local computer - this should match clustertmpDir2 when running on cluster!
tempIeDir = paste0('C:/Users/frc2/Documents/scratch/impact_evaluation/parallel_files/')
# # ---------------------------------------------------------------------------------
# 
# 
# # -----------------------------------------------------------------------------
# # Output Files
# 
# output file from 2a_prep_outputs_ou.r
outputFile2a = paste0(preppedIeDir, 'prepped_outcomes_data.RDS')

# output file from 2b_prepped_resource_tracking.R
outputFile2b = paste0(preppedIeDir, 'prepped_resource_tracking.RDS')

# output file from 2c_prep_mdrtb_indicators.r
outputFile2c = paste0(preppedIeDir, 'prepped_tb_mdr_data.RDS')

# output file from 2d_impute_outcomes.r
outputFile2d = paste0(preppedIeDir, 'imputed_outcomes.RDS')

# output file from 2e_imputations_mdrtb.r
outputFile2e = paste0(preppedIeDir, 'imputed_mdrtb.RDS')

# output file from 2f_final_data_prep.r
outputFile2f = paste0(preppedIeDir, 'prepped_outputs.RDS')
 
# output file from 3_merge_data.R
outputFile3 = paste0(preppedIeDir, 'inputs_outputs.RDS')
 
# output file from 4a_set_up_for_analysis.r
 outputFile4a = paste0(preppedIeDir, 'first_half_pre_model.rdata')
 if (Sys.info()[1]!='Windows') { 
 outputFile4a_scratch = paste0(clustertmpDir1, 'first_half_data_pre_model.rdata')
}

# output file from 4c and 4d_explore_data.r (graphs)
outputFile4c = paste0(visIeDir, 'first_half_exploratory_graphs.pdf')
outputFile4d = paste0(visIeDir, 'second_half_exploratory_graphs.pdf')
 
# output file from 5a_run_first_half_analysis.R
outputFile5a = paste0(preppedIeDir, 'first_half_model_results.rdata')


# output file from 6_display_results.r
outputFile6a = paste0(visIeDir, 'sem_diagrams.pdf')
outputFile6b = paste0(visIeDir, 'bottleneck_analysis.pdf')
outputFile6c = paste0(visIeDir, 'health_zone_effects.pdf')
# # -----------------------------------------------------------------------------
