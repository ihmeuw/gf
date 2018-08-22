# ----------------------------------------------
# Audrey Batzel
#
# 8/21/18
# 
# Compare GF provinces to non-GF provinces at the health zone level;
# and using the donor listed in the pnlp data
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
rm(list=ls())

library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(stats)
library(Rcpp)
# --------------------  


# ----------------------------------------------
# Overview - Files and Directories

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_data = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
dir_pop = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/worldpop_data/')

# input files
hz_data <- "imputedData_run2_condensed_hz.rds"
funders <- "fullData_dps_standardized.csv"

# output files
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_analysis/')
# ----------------------------------------------


# -----------------------------
# Load data

# load the imputed data at the hz level
dt <- readRDS(paste0(dir_data, hz_data))

# load the world pop estimates at the hz level?
# -----------------------------


# ----------------------------------------------
# We want to identify which hzs were funded by which partner/funder

# load in full data where dps/hz was standardized (might have to play around with this to find it?)
funder_data <- read.csv(paste0(dir_data, funders))
funder_data <- as.data.table(funder_data)
# subset to just the relevant vars
funder_data <- funder_data[, .(dps, health_zone, donor, year, month, date)]


# ----------------------------------------------


# ----------------------------------------------
# # Clean dps names 
dt$dps <- gsub(" ", "-", dt$dps)
dt[dps=="bas-congo", dps:= "kongo-central"]
# ----------------------------------------------

