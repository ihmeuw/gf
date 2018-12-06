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

user <- "elineb" #Replace with your username
repo <- paste0("C:/Users/", user, "/Documents/gf/results_chains/") #Modify to fit your repo location

source(paste0(repo, "mapping_functions.r")) 

# ---------------------------------------
# Set filepaths 
# ---------------------------------------

gtm_save <- "I:/RTs_and_Projects/Global Fund PCE/Reports/Annual Country Reports 2018-2019/Guatemala/Results Chains Visualizations"
cod_save <- "I:/RTs_and_Projects/Global Fund PCE/Reports/Annual Country Reports 2018-2019/DRC/Results Chains Visualizations"
uga_save <- "I:/RTs_and_Projects/Global Fund PCE/Reports/Annual Country Reports 2018-2019/Uganda/Results Chains Visualizations"

# ---------------------------------------
# Prep key datasets.   
# ---------------------------------------

kpis <-readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/cleaned_indicator_performance_data.rds") #Want to eventually pull this from google sheets using Audrey's code so we can know what's going on. 
setDT(kpis)

allRT <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv")

data_source <- unique(allRT[, .(data_source, financing_source)]) #David to review this check- why do we have all of these categories? Why do 
print(data_source)
#Subset by financing source 
fgh = allRT[data_source == "fgh"]
gf = allRT[data_source == "pudr" | data_source == "fpm" | data_source == "gos"] #Is this how we want to subset global fund? 
dah = allRT[financing_source == "dah" | financing_source == "other_dah"]
bil_usa = allRT[financing_source == "bil_usa"]
oop = allRT[financing_source == "oop"]
ppp = allRT[financing_source == "ppp"]
ghe = allRT[financing_source == "ghe" | data_source == "sicoin"]

#Do these all sum to 1? 

#Further subset by data_source 
gf_pudrs = gf[data_source == "pudr"]
gf_budgets = gf[data_source == "fpm"]
gos = gf[data_source == "gos"]