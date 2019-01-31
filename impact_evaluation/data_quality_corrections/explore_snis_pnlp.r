# -------------------------------------------
# David Phillips and Audrey Batzel
# 
# 1/31/2019
# Exploratory graphs comparing SNIS and PNLP
# -------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# -------------------


# -------------------------------------------
# Files and directories

# root directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod')

# input file
inFile = paste0(dir, '/prepped_data/snis_pnlp_malaria_hz_level.rds')

# output file
outFile = paste0(dir, 'visualizations/snis_pnlp_comparisons.pdf')
# -------------------------------------------


# -------------------------------------------
# Load/prep data

# load
data = readRDS(inFile)
# -------------------------------------------


# -------------------------------------------
# Set up to graph
# -------------------------------------------


# -------------------------------------------
# Graph
# -------------------------------------------


# -------------------------------------------
# Save
# -------------------------------------------
