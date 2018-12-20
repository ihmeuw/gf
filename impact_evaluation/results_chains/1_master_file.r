# ----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Master file for mapping results chains. 
#			Sets up global variables, filepaths, and preps key datasets. 
#       
# DATE: Last updated November 2018. 
#-----------------------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------

rm(list=ls())
library(data.table)
library(ggplot2)
library(googlesheets)
library(RColorBrewer)

user <- "elineb" #Replace with your username
repo <- paste0("C:/Users/", user, "/Documents/gf/impact_evaluation/results_chains/") #Modify to fit your repo location

source(paste0(repo, "mapping_functions.r")) 

# ---------------------------------------
# Set filepaths 
# ---------------------------------------

gtm_save <- "J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations"
cod_save <- "J:/Project/Evaluation/GF/impact_evaluation/cod/visualizations"
uga_save <- "J:/Project/Evaluation/GF/impact_evaluation/uga/visualizations"

# ---------------------------------------
# Prep key datasets.   
# ---------------------------------------

fgh = fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_current_fgh.csv")
fgh_actual = fgh[fin_data_type == "actual"]#Split FGH between actual numbers and model estimates. 
fgh_estimates = fgh[fin_data_type != "actual"] 

gf_budgets <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/final_budgets.csv")
