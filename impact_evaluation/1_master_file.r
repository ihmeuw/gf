# ----------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Master file for preparing impact evaluation dataset. 
# DATE: Last updated January 2019. 
# ----------------------------------------------

#-----------------------------------------
# TO-DO: 
# - Make sure resource tracking inputs are uniquely identified by year, quarter, module, intervention, and indicator. 
# - in q1 2010, for IPTp/SP, there appears to be two different budgets for resource tracking. Look into this. 
# - sort columns loc_name, disease, year, quarter, code, module, intervention, indicator, indicator_type, data_source, budget, value, completeness
# - order rows by date. 
#one more for you emily - could we get an extra column that's `other_dah`? it would be disbursement 
#from all other donors (`fghData[data_source=='fgh' & fin_data_type=='actual', other_dah=sum(disbursement),
#by=c('sda_activity', 'year')]`) Divide FGH by 4 to make it quarterly and append as a new column. 



#-----------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------
rm(list=ls())
library(data.table)
library(lubridate)
library(readxl)

user <- "elineb" #Change to reflect your own username 
repo <- paste0("C:/Users/", user, "/Documents/gf/") #Change to your own repository 
code_dir <- paste0(repo, "impact_evaluation/")


repo <- 'C:/local/gf/'
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

# source(paste0(code_dir, "2b_prep_activities_outputs.r"))

# ---------------------------------------
# Merge datasets together 
# ---------------------------------------
# file produced by 2b_prep_activities_outputs 
source(paste0(code_dir, "3_merge_data.r"))

# ---------------------------------------
# Validate data (EKL need to add this step)
# ---------------------------------------

# ---------------------------------------
# Run analysis 
# ---------------------------------------

source(paste0(code_dir, "4_analysis.r"))


print("Master script completed. Outputs saved here: [Emily add file location]")

