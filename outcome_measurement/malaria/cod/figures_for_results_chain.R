# Audrey Batzel 
# 12-10-18
#
# Figures for results chain for DRC report 
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(readxl)
library(stats)
library(Rcpp)
library(Amelia)
# --------------------  


# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# source in variable names
# variable_names <-"./outcome_measurement/malaria/cod/variable_names.R"
# source(variable_names)

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_data = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/')

# input file:

# output file:
# ----------------------------------------------