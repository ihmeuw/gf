# ----------------------------------------------
# Audrey Batzel
#
# 3/6/18
# Prepping DRC PNLP data for analaysis.
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
# --------------------


# ----------------------------------------------
# Files and directories

# data directory


# input file


# output files

# ----------------------------------------------


# ----------------------------------------------
# Load/prep data

# load
##read the data: 
gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))



# subset variables


# subset observations


# format variables


# reshape data


# make new variables

# ----------------------------------------------