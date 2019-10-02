# --------------------------------------------------
# David Phillips
# 
# 1/26/2019
# Script that loads packages and file names
# Intended to be called by 1_master_file.r
# This exists just for code organizational purposes
# (use singularity exec /share/singularity-images/health_fin/forecasting/best.img R on IHME's new cluster)
# --------------------------------------------------

# to do
# make this work on the cluster (it fails to load lubridate and other packages)

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

#----------------------------------
#Set global variables - pulling to top so easier to read EL 8/15/19
# Current model versions

modelVersion1 = 'gtm_tb_first_half11_rssh_interaction'
modelVersion2 = 'gtm_tb_sec_half4'

START_YEAR = 2009 #If available, what's the earliest year you should model data from? 

#-----------------------------------
#Global variable transformations 

#These variables had to be imputed in step 2b, because they're used to create first-line and second-line drugs. EL 9/6/2019 
drugComboVars = c("Total_First_Line_Drugs_inusIsonizide__Distributed_value_act", "Isoniazid_Distributed_value_act", 
                  "Second_Line_Drugs_Distributed_value_act", "Total_MDR_Drugs_Distributed_value_act")

#These variables will be imputed using GLM in step 4a. 
backCastVars = c("Number_of_Cases_Screened_for_MDR_act", "PLHIV_Screened_for_TB_act", 
                           "TB_Patients_Tested_for_HIV_act", 
                           "MDR_Cases_Notified_out", "MDR_Cases_Started_Treatment_out", "Cases_Notified_in_Prisons_out", "Children_in_Contact_with_TB_Started_IPT_out", 
                           "Cases_Started_on_Treatment_out", "Cases_Notified_out", "PLHIV_started_on_IPT_out", "Cases_Started_on_Treatment_in_Prisons_out", "HIV_TB_Cases_Notified_out")

#These variables will be log-transformed in step 4a. 
logVars = c("Case_Notification_Rate_imp", "Proportion_of_HIV_TB_Cases_Treated_out", "Proportion_of_MDR_Cases_Treated_out", "Proportion_of_Cases_in_Prisons_Treated_out", 
            "Proportion_of_Patients_Receiving_DST_out")

# ---------------------------------------------------------------------------------
# Directories

# switch J for portability to cluster
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# directories
dir = paste0(j, '/Project/Evaluation/GF/')
box = "C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/" #Adding new file path to pull latest files from Box account EL 10/2/2019
ieDir = paste0(dir, 'impact_evaluation/gtm/')
rawIeDir = paste0(ieDir, 'raw_data/')
preppedIeDir =  paste0(ieDir, 'prepped_data/')
visIeDir = paste0(ieDir, 'visualizations/')
rtDir = paste0(dir, 'resource_tracking/_gf_files_gos/combined_prepped_data/')
fghDir = paste0(dir, 'resource_tracking/_odah/prepped_data/')
whoDir = paste0(dir, 'resource_tracking/_ghe/who/prepped_data/')
mapDir = paste0(dir, '/mapping/multi_country/intervention_categories')
sicoinDir = paste0(dir, 'resource_tracking/_ghe/sicoin_gtm/prepped_data/')
lbdDir = paste0(j, '/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/')
# ---------------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Supporting Files

# code-friendly version of indicator map file
indicatorMapFile = paste0(ieDir, 'GTM Indicator map.xlsx')

# list of interventions and codes
mfFile = "J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.csv"

# archive function
source('./impact_evaluation/_common/archive_function.R')

# function that runs a SEM as unrelated regressions
source('./impact_evaluation/_common/run_lavaan_as_glm.r')
# ------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Inputs files

# resource tracking files with prepped budgets, expenditures, disbursements
budgetFile = paste0(box, 'final_budgets_gtm.rds')
expendituresFile = paste0(box, 'final_expenditures_gtm.rds')
fghFile = paste0(fghDir, 'other_dah_actuals_all_gtm.rds')
gheMalFile = paste0(fghDir, 'ghe_actuals_malaria.rds')
whoFile = paste0(whoDir, 'who_prepped.rds')
sicoinFile = paste0(sicoinDir, 'prepped_sicoin_data.rds')

# activities/outputs files
actFile = paste0(rawIeDir, "activities_8.21.19.csv")
outputsFile = paste0(rawIeDir, "outputs_8.9.19.csv")

# outcomes/impact files
outcomeFile = paste0(rawIeDir, "outcomes_7.15.19.csv")
impactFile = paste0(rawIeDir, "impact_7.15.19.csv")

# shapefiles

# "nodetables" aka "nodetables" 
# listing names of variables in each model, their labels and coordinates for the SEM graph
nodeTableFile1 = './impact_evaluation/gtm/visualizations/nodetable_first_half.csv'
nodeTableFile2 = './impact_evaluation/gtm/visualizations/nodetable_second_half.csv'
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Intermediate file locations
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
tempIeDir = paste0('C:/Users/elineb/Desktop/tmp_cluster/')
# ---------------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Output Files

# output file from 2a_prep_resource_tracking.r
outputFile2a = paste0(preppedIeDir, 'prepped_resource_tracking.RDS')

# output file from 2b_prep_activities_outputs.R
outputFile2b = paste0(preppedIeDir, 'outputs_activites_for_pilot.RDS')
outputFile2b_wide = paste0(preppedIeDir, 'outputs_activities_for_pilot_wide.RDS')

# output file from 2c_prep_outcomes_impact.r
outputFile2c_estimates = paste0(preppedIeDir, 'aggregated_rasters.rds')
outputFile2c = paste0(preppedIeDir, 'outcomes_impact.rds')

# output file from 3_merge_data.R
outputFile3 = paste0(preppedIeDir, 'inputs_outputs.RDS')

# output file from 3b_merge_model_halves.R
outputFile3b = paste0(preppedIeDir, 'combined_halves.RDS')

# output file from 4a_set_up_for_analysis.r
outputFile4a = paste0(preppedIeDir, 'first_half_pre_model.rdata')
if (Sys.info()[1]!='Windows') { 
	outputFile4a_scratch = paste0(clustertmpDir1, 'first_half_data_pre_model.rdata')
}

# output file from 4b_set_up_for_second_half_analysis.r
outputFile4b = paste0(preppedIeDir, 'second_half_data_pre_model.rdata')
if (Sys.info()[1]!='Windows') { 
	outputFile4b_scratch = paste0(clustertmpDir1, 'second_half_data_pre_model.rdata')
}

# output file from 4c and 4d_explore_data.r (graphs)
outputFile4c = paste0(visIeDir, 'first_half_exploratory_graphs.pdf')
outputFile4d = paste0(visIeDir, 'second_half_exploratory_graphs.pdf')

# output file from 5a_run_first_half_analysis.R
outputFile5a = paste0(preppedIeDir, 'first_half_model_results.rdata')

# output file from 5b_run_second_half_analysis.r
outputFile5b = paste0(preppedIeDir, 'second_half_model_results.rdata')

# output file from 6_display_results.r
outputFile6a = paste0(visIeDir, 'sem_diagrams.pdf')
outputFile6b = paste0(visIeDir, 'bottleneck_analysis.pdf')
outputFile6c = paste0(visIeDir, 'impact_analysis.pdf')
outputFile6d = paste0(visIeDir, 'health_zone_effects.pdf')
# -----------------------------------------------------------------------------
