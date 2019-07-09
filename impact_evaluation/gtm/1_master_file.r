# ----------------------------------------------
# AUTHOR: Audrey Batzel, Emily Linebarger, 
# David Phillips, and Jen Ross
# PURPOSE: Master file for preparing impact evaluation dataset. 
# DATE: Last updated May 2019. 
# 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------

#-----------------------------------------
# TO-DO: 


#-----------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------

# clear memory
rm(list=ls())

# run setup code (load file paths and packages)
source('./impact_evaluation/gtm/set_up_r.r')

# ---------------------------------------
# Set boolean switches
# ---------------------------------------
rerun_inputs <- TRUE
rerun_outputs <- TRUE
rerun_outcomes <- TRUE
rerun_merge <- TRUE
rerun_adjust <- FALSE
rerun_explore <- FALSE
rerun_models <- FALSE
rerun_analysis <- FALSE
rerun_post <- FALSE

# ---------------------------------------
# Read in common files 
# ---------------------------------------

indicatorMap <- read_excel(indicatorMapFile)
setDT(indicatorMap)

# ---------------------------------------
# Prep resource tracking data  
# ---------------------------------------
if(rerun_inputs == TRUE){
  source('./impact_evaluation/gtm/2a_prep_resource_tracking.r')
}

# ---------------------------------------
# Prep activities and outputs data 
# ---------------------------------------
if(rerun_outputs == TRUE){
  source('./impact_evaluation/gtm/2b_prep_activities_outputs.R')
}

# ---------------------------------------
# Prep outputs, outcomes and impact data 
# ---------------------------------------
if(rerun_outcomes == TRUE){
  source('./impact_evaluation/gtm/2c_prep_outcomes_impact.r')
}

# ---------------------------------------
# Merge datasets together 
# ---------------------------------------
if (rerun_merge==TRUE) { 
	source('./impact_evaluation/gtm/3_merge_data.R')
}

# ---------------------------------------
# Correct rates to model estimates
# ---------------------------------------
if (rerun_adjust==TRUE) { 
	source('./impact_evaluation/gtm/3b_correct_to_models.R')
}

# ---------------------------------------
# Validate data (EKL need to add this step)
# ---------------------------------------


# ---------------------------------------
# Exploratory graphs etc 
# usually a good idea to run 4a and 4b one at a time and check output
# 4a and 4b will be very slow if not on IHME's cluster
# ---------------------------------------
if (rerun_explore==TRUE) { 
	source('./impact_evaluation/gtm/4a_set_up_for_first_half_analysis.r')
	source('./impact_evaluation/gtm/4b_set_up_for_second_half_analysis.r')
	source('./impact_evaluation/gtm/4c_explore_first_half_data.r')
	source('./impact_evaluation/gtm/4d_explore_second_half_data.r')
}

# ---------------------------------------
# Run models 
# ---------------------------------------
if (rerun_models==TRUE) { 
	source('./impact_evaluation/gtm/5a_run_first_half_analysis.r')
	source('./impact_evaluation/gtm/5b_run_second_half_analysis.r')
}

# ---------------------------------------
# Run analyses based on model
# ---------------------------------------
if (rerun_analysis==TRUE) { 
	source('./impact_evaluation/gtm/6a_display_sem_results.r')
	source('./impact_evaluation/gtm/6b_efficiency_effectiveness.r')
	source('./impact_evaluation/gtm/6c_impact_analysis.r')
	source('./impact_evaluation/gtm/6d_effect_sizes_by_hz.r')
}

print(paste('Master script completed. Outputs saved here:', ieDir))
