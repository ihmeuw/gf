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

#kpis <-readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/cleaned_indicator_performance_data.rds") #Want to eventually pull this from google sheets using Audrey's code so we can know what's going on. 
#setDT(kpis)

allRT <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv")

#Format data; this should be done at end of RT DB saving process. 
allRT$disbursement <- as.numeric(allRT$disbursement)
allRT$expenditure <- as.numeric(allRT$expenditure)
allRT$budget <- as.numeric(allRT$budget)

data_source <- unique(allRT[, .(data_source, financing_source)]) #David to review this check- why do we have all of these categories? Why do 
print(data_source)
#Subset by financing source 
fgh = fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_current_fgh.csv")
gf = allRT[data_source == "pudr" | data_source == "fpm" | data_source == "gos"] #Is this how we want to subset global fund? 

#Do these all sum to 1? 

#Further subset by data_source 
gf_pudrs = gf[data_source == "pudr" | data_source == "gos"]
gf_budgets = gf[data_source == "fpm" | data_source == "gos"]
fgh_actual = fgh[fin_data_type == "actual"]#Split FGH between actual numbers and model estimates. 
fgh_estimates = fgh[fin_data_type != "actual"] 

test_fgh_actual = allRT[data_source == "fgh" & fin_data_type == "actual"]

# subset to GHE from SICOIN for TB (SICOIN only exists in GTM)
sicoin = allRT[data_source=='sicoin' & financing_source=='ghe']
# drop first/last year of the series because they appear to be incomplete
sicoin = sicoin[year!=min(year) & year!=max(year)]


print(nrow(fgh_actual))
print(nrow(test_fgh_actual))

dis_new <- fgh_actual[, sum(disbursement)]
dis_old <- test_fgh_actual[, sum(disbursement)]

print(dis_new) 
print(dis_old)
print(dis_old/dis_new)
