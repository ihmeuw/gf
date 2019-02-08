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

# clear memory
rm(list=ls())

# run setup code (load file paths and packages)
source('./impact_evaluation/_common/set_up_r.r')


# ---------------------------------------
# Set boolean switches
# ---------------------------------------
rerun_inputs <- TRUE 
rerun_outputs <- FALSE
rerun_merge <- TRUE
rerun_explore <- FALSE
rerun_analysis <- FALSE
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
  source('./impact_evaluation/2a_prep_resource_tracking.r')
}

# ---------------------------------------
# Prep activities and outputs data 
# ---------------------------------------
if(rerun_outputs == TRUE){
  source('./impact_evaluation/2b_prep_activities_outputs.r')
}

# ---------------------------------------
# Merge datasets together 
# ---------------------------------------
if (rerun_merge==TRUE) { 
	source('./impact_evaluation/3_merge_data.r')
}

# ---------------------------------------
# Validate data (EKL need to add this step)
# ---------------------------------------


# ---------------------------------------
# Exploratory graphs etc 
# ---------------------------------------
if (rerun_explore==TRUE) { 
	source('./impact_evaluation/4_explore_data.r')
}

# ---------------------------------------
# Run analysis 
# ---------------------------------------
if (rerun_analysis==TRUE) { 
	source('./impact_evaluation/5a_set_up_for_analysis.r')
	source('./impact_evaluation/5b_run_analysis.r')
	source('./impact_evaluation/6_display_results.r')
}

print(paste('Master script completed. Outputs saved here:', outputFile3))

