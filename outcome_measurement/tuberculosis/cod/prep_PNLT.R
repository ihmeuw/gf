# ----------------------------------------------
# Audrey Batzel
#
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(readxl)
library(stats)
library(rlang)
library(zoo)
library(tidyr)
library(dplyr)
# --------------------


# ----------------------------------------------
# Overview - Files and Directories

# data directory
# file path where the files are stored
dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/"
dir_prepped <-"J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/"

# input file
file2018 <- "Synthèse Nationale T1 2018.xlsx"

# output file


# ----------------------------------------------

# load data from excel to visualize initial data

dt <- data.table(read_excel(paste0(dir, file2018), sheet= 'DEPISTAGE T1 018'))









