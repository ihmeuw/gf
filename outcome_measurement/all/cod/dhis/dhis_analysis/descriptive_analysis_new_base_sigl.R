# ----------------------------------------------
# Audrey Batzel
#
# 1/29/19
# Quick comparison of new prepped BASE/SIGL data with older versions of the data
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')

# input files
base_file <- paste0(dir, "base_services_prepped.rds")
sigl_file <- paste0(dir, "sigl_prepped.rds")

# ----------------------------------------------

# ----------------------------------------------
# Read in data
# ----------------------------------------------
base <- readRDS(base_file)
sigl <- readRDS(sigl_file)
# ----------------------------------------------

# ----------------------------------------------
# 
# ----------------------------------------------

# ----------------------------------------------




