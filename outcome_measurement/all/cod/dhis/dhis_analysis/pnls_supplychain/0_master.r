#------------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Run PNLS cleaning and visualizations
#   for supply chain data 
# DATE: Updated May 2019
#-------------------------------------------------

rm(list=ls())
# -----------------------------------------------
# Set up R
#------------------------------------------------
library(data.table)
library(raster)
library(ggplot2)
library(ggrepel)
library(knitr)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(rmarkdown)
library(maptools)
#Source your prep code
repo_root = "C:/Users/elineb/Documents/gf/" #Set to the root of your repository 
setwd(repo_root)
source('./core/standardizeDPSNames.R')

#Set up directories 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/') #Home directory
saveDir = paste0(j, 'Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pnls/')
codeDir = paste0(repo_root, "outcome_measurement/all/cod/dhis/dhis_analysis/pnls_supplychain/")

#-------------------------------------
# CLEAN DATA 
#------------------------------------
source(paste0(codeDir, "1_pnls_supplychain_clean.r"))

#-------------------------------------
# VISUALIZE  
#------------------------------------
# source(paste0(codeDir, "2_pnls_supplychain_gen_variables.r"))
# source(paste0(codeDir, "3_pnls_supplychain_visualize.r"))

#-------------------------------------
# MAKE SLIDES   
#------------------------------------

#Render testing R-markdowns
rmarkdown::render(paste0(codeDir, "4_determine_analysis.rmd"))
rmarkdown::render(paste0(codeDir, "4_unigold_analysis.rmd"))
rmarkdown::render(paste0(codeDir, "4_doublecheck_analysis.rmd"))

#Render treatment R-markdowns
rmarkdown::render(paste0(codeDir, "4_treatment_firstline.rmd"))
rmarkdown::render(paste0(codeDir, "4_treatment_secondline.rmd"))
