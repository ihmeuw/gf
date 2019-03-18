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

# functions
source("./core/standardizeDPSNames.r")
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

# archived files
base_archive_file <- paste0(dir, 'archive/base_services_drc_01_2017_09_2018_prepped.rds')
sigl_archive_file <- paste0(dir, 'archive/sigl_drc_01_2015_07_2018_prepped.rds')
# ----------------------------------------------

# ----------------------------------------------
# Read in data
# ----------------------------------------------
base <- readRDS(base_file)
sigl <- readRDS(sigl_file)
base_archive <- readRDS(base_archive_file)
sigl_archive <- readRDS(sigl_archive_file)
# ----------------------------------------------

# ----------------------------------------------
# 
# ----------------------------------------------
compare_new_old_data <- function(new_data, old_data){
  
  dt1 <- new_data[, unique(org_unit_id), by = c("date", "dps")]
  dt1 <- dt1[, .N, by= c("date", "dps")]
  setnames(dt1, "N", "new_data")
  
  dt2 <- old_data[, unique(org_unit_id), by = c("date", "dps")]
  dt2 <- dt2[, .N, by= c("date", "dps")]
  setnames(dt2, "N", "old_data")
  dt2$dps <- standardizeDPSNames(dt2$dps)
  
  dt <- merge(dt1, dt2, by = c("date", "dps"), all = TRUE)
  dt[, num_new_fac:= new_data - old_data]
  return(dt)
}

dt_base <- compare_new_old_data(base, base_archive)
dt_sigl <- compare_new_old_data(sigl, sigl_archive)
# ----------------------------------------------




