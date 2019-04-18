# ----------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Master file for preparing impact evaluation dataset. 
# DATE: Last updated January 2019. 
# 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------

#-----------------------------------------
# TO-DO: 


#-----------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------
setwd("C:/local/gf") # set to the root of the repo

# clear memory
rm(list=ls())

# run setup code (load file paths and packages)
source('./impact_evaluation/drc/set_up_r.r')

# ---------------------------------------
# Set boolean switches
# ---------------------------------------
rerun_inputs <- FALSE 
rerun_outputs <- FALSE
rerun_outcomes <- FALSE
rerun_merge <- FALSE
rerun_adjust <- FALSE
rerun_explore <- FALSE
rerun_models <- FALSE
rerun_analysis <- TRUE
rerun_post <- FALSE

# ---------------------------------------
# Read in common files 
# ---------------------------------------

drc_mal_map <- read_excel(indicatorMapFile)
setDT(drc_mal_map)


# ---------------------------------------
# Prep resource tracking data  
# ---------------------------------------
if(rerun_inputs == TRUE){
  source('./impact_evaluation/drc/2a_prep_resource_tracking.r')
}

# ---------------------------------------
# Prep activities and outputs data 
# ---------------------------------------
if(rerun_outputs == TRUE){
  source('./impact_evaluation/drc/2b_prep_activities_outputs.r')
}

# ---------------------------------------
# Prep outputs, outcomes and impact data 
# ---------------------------------------
if(rerun_outcomes == TRUE){
  source('./impact_evaluation/drc/2c_prep_outcomes_impact.r')
}

# ---------------------------------------
# Merge datasets together 
# ---------------------------------------
if (rerun_merge==TRUE) { 
	source('./impact_evaluation/drc/3_merge_data.R')
}

# ---------------------------------------
# Correct rates to model estimates
# ---------------------------------------
if (rerun_adjust==TRUE) { 
	source('./impact_evaluation/drc/3b_correct_to_models.R')
}

# ---------------------------------------
# Validate data (EKL need to add this step)
# ---------------------------------------


# ---------------------------------------
# Exploratory graphs etc 
# these scripts have gotten a little convoluted over time
# 4b actually relies on the output from 5d...
# ---------------------------------------
if (rerun_explore==TRUE) { 
	source('./impact_evaluation/drc/4a_explore_first_half_data.r')
	source('./impact_evaluation/drc/4b_explore_second_half_data.r')
}

# ---------------------------------------
# Run model 
# usually a good idea to run 5a and 5d one at a time and check output
# 5b and 5e will be very slow if not on IHME's cluster
# ---------------------------------------
if (rerun_models==TRUE) { 
	source('./impact_evaluation/drc/5a_set_up_for_analysis.r')
	source('./impact_evaluation/drc/5b_run_analysis.r')
	source('./impact_evaluation/drc/5d_set_up_for_second_half_analysis.r')
	source('./impact_evaluation/drc/5e_run_second_half_analysis.r')
}


# ---------------------------------------
# Run analyses based on model
# ---------------------------------------
if (rerun_analysis==TRUE) { 
	source('./impact_evaluation/drc/6a_display_sem_results.r')
	source('./impact_evaluation/drc/6b_efficiency_effectiveness.r')
	source('./impact_evaluation/drc/6c_impact_analysis.r')
	source('./impact_evaluation/drc/6d_effect_sizes_by_hz.r')
}

print(paste('Master script completed. Outputs saved here:', outputFile3))
