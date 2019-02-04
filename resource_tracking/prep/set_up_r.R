# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Set up R for running resource tracking
# DATE: Last updated November 2018. 
# ----------------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------

rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(tidyr)
library(tools)
library(rlang)
library(zoo)
library(dplyr)

options(scipen=100)

# ---------------------------------------
# Set global variables and filepaths.  
# ---------------------------------------

#Replace global variables to match what code you want to run. 
user = "elineb" #Change to your username 

#Filepaths
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')
code_loc = ifelse(Sys.info()[1]=='Windows', paste0('C:/Users/', user, '/Documents/gf/'), paste0('/homes', user, '/gf/'))
code_dir = paste0(code_loc, "resource_tracking/prep/")
combined_output_dir = paste0(j, "resource_tracking/multi_country/mapping")

gf_prep_code = paste0(code_dir, "global_fund_prep/")

#Source shared functions
source(paste0(code_dir, "shared_prep_functions.R"))
