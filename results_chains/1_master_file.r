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
options(scipen=100)
library(data.table)
library(ggplot2)
library(googlesheets)
library(RColorBrewer)

user <- "elineb" #Replace with your username
repo <- paste0("C:/Users/", user, "/Documents/gf/results_chains/") #Modify to fit your repo location

source(paste0(repo, "mapping_functions.r")) 

# ---------------------------------------
# Set filepaths 
# ---------------------------------------
main = "J:/Project/Evaluation/GF/results_chains/"

gtm_save <- paste0(main, "gtm/")
cod_save <- paste0(main, "cod/")
uga_save <- paste0(main, "uga/")
sen_save <- paste0(main, "sen/")

# ---------------------------------------
# Prep key datasets.   
# ---------------------------------------
#allRT <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv")

other_dah = readRDS("J:/Project/Evaluation/GF/resource_tracking/_fgh/prepped_data/other_dah_actuals_all.rds")
other_dah[loc_name=='COD', country:='Congo (Democratic Republic)']
other_dah[loc_name=='GTM', country:='Guatemala']
other_dah[loc_name=='SEN', country:='Senegal']
other_dah[loc_name=='UGA', country:='Uganda']

gf_budgets <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/final_budgets.csv")
gf_budgets$budget <- as.numeric(gf_budgets$budget)
#Change country factors so they look better
gf_budgets$country <- factor(gf_budgets$country, c('Congo (Democratic Republic)', 'Guatemala', 'Uganda'), c('DRC', 'Guatemala', 'Uganda')) 

#Grab SICOIN for Guatemala funding graph 
sicoin <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                   ,fileEncoding="latin1"))
sicoin = sicoin[financing_source == 'ghe'] #Only want GHE numbers from SICOIN. 
# drop first/last year of the series because they appear to be incomplete
sicoin = sicoin[year!=min(year) & year!=max(year)]

#Convert SICOIN financial data into numeric 
sicoin[, budget:=as.numeric(budget)]
sicoin[, expenditure:=as.numeric(expenditure)]
sicoin[, disbursement:=as.numeric(disbursement)]

#Convert SICOIN currency to FGH units so they are directly comparable. 
# test <- data.table(iso3 = c('UGA','UGA','GTM','GTM','COD','COD'), 
#                    year = c(2000, 2001, 2003, 2004, 2003,2004),
#                    currency_year = c(2000, 2001, 2003, 2004, 2003,2004),
#                    val1 = runif(n = 6,min = 4000, max = 8000),
#                    val2 = runif(n = 6, min = 300, max = 1000))

# 
# `col.loc` is location column(must be iso3 code) in your data,
# `col.value` is value column that you want to be converted (can take multiple value columns)
# `currency` is the raw currency of the value you want to be converted
# `col.currency.year` is the column for year of the currency
# `base.year` is 2018 in your case
# `base.unit` is usd in your case

# sicoin_prepped <- currency_conversion(data = sicoin,
#                                   col.loc = 'loc_name',
#                                   col.currency.year = 'year',
#                                   currency = 'LCU',
#                                   col.value = c('budget', 'expenditure', 'disbursement'),
#                                   base.year = 2018, 
#                                   base.unit = 'USD', 
#                                   simplify = T,
#                                   converter.version = 3)
