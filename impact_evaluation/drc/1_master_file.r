# ----------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Master file for preparing impact evaluation dataset. 
# DATE: Last updated January 2019. 
# 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# This will execute the whole thing on the cluster: 
# qsub -cwd -N master_run -l fthread=12 -l m_mem_free=12G -q all.q -P proj_pce -e /ihme/scratch/users/davidp6/impact_evaluation/master/ -o /ihme/scratch/users/davidp6/impact_evaluation/master/ ./core/r_shell_blavaan.sh ./impact_evaluation/drc/1_master_file.r
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
source('./impact_evaluation/drc/set_up_r.r')

# ---------------------------------------
# Set boolean switches
# ---------------------------------------
rerun_inputs <- FALSE 
rerun_outputs <- FALSE
rerun_outcomes <- FALSE
rerun_merge <- FALSE
rerun_adjust <- TRUE
rerun_explore <- TRUE
rerun_models <- TRUE
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
  source('./impact_evaluation/drc/2a_prep_drc_expenditures.r')
  source('./impact_evaluation/drc/2a_prep_resource_tracking.r')
}

# ---------------------------------------
# Prep activities and outputs data 
# ---------------------------------------
if(rerun_outputs == TRUE){
  source('./impact_evaluation/drc/2b_prep_activities_outputs.R')
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
	source('./impact_evaluation/drc/3b_crosswalk_to_models.R')
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
	source('./impact_evaluation/drc/4a_set_up_for_first_half_analysis.r')
	source('./impact_evaluation/drc/4b_set_up_for_second_half_analysis.r')
	source('./impact_evaluation/drc/4c_explore_first_half_data.r')
	source('./impact_evaluation/drc/4d_explore_second_half_data.r')
}

# ---------------------------------------
# Run models 
# ---------------------------------------
if (rerun_models==TRUE) { 
	source('./impact_evaluation/drc/5a_run_first_half_analysis.r')
	# source('./impact_evaluation/drc/5b_run_second_half_analysis.r')
}

# ---------------------------------------
# Run analyses based on model
# ---------------------------------------
if (rerun_analysis==TRUE) { 
	source('./impact_evaluation/drc/6a_display_sem_results.r')
	source('./impact_evaluation/drc/6b_efficiency_effectiveness.r')
	source('./impact_evaluation/drc/6c_impact_analysis.r')
	# source('./impact_evaluation/drc/6d_effect_sizes_by_hz.r')
}

print(paste('Master script completed. Outputs saved here:', ieDir))
