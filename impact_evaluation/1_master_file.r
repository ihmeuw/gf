# ----------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Master file for preparing impact evaluation dataset. 
# DATE: Last updated January 2019. 
# ----------------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------
rm(list=ls())
library(data.table)
library(readxl)

user <- "elineb" #Change to reflect your own username 
repo <- paste0("C:/Users/", user, "/Documents/gf/") #Change to your own repository 
code_dir <- paste0(repo, "impact_evaluation/")

# ---------------------------------------
# Read in common files 
# ---------------------------------------

drc_mal_map <- read_excel("J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/DRC Indicator map - to code from.xlsx")
setDT(drc_mal_map)

# ---------------------------------------
# Prep resource tracking data  
# ---------------------------------------

source(paste0(code_dir, "2a_prep_resource_tracking.r"))

# ---------------------------------------
# Prep activities and outputs data 
# ---------------------------------------

source(paste0(code_dir, "2b_prep_activities_outputs.r"))

# ---------------------------------------
# Merge datasets together 
# ---------------------------------------

source(paste0(code_dir, "3_merge_data.r"))

# ---------------------------------------
# Validate data (EKL need to add this step)
# ---------------------------------------

# ---------------------------------------
# Run analysis 
# ---------------------------------------

source(paste0(code_dir, "4_analysis.r"))


print("Master script completed. Outputs saved here: [Emily add file location]")

