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
# Overview - Files and Directories

# data directory
# J:\Project\Evaluation\GF\outcome_measurement\cod\National_Malaria_Program\Malaria_Data_2014_to_2016

# input file
  # file path where the files are stored
  dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/National_Malaria_Program/Malaria_Data_2014_to_2016"
  
  PNLP_files <- read.csv(paste0(dir,"/","PNLP_file_names.csv"), fileEncoding = "latin1")
  
# output files 
  #(note: output to prepped_data folder within cod folder)
  # cod_malaria_dataset_(year) - prepped data.table object, one per year (?)
  # cod_malaria_dataset_master - appended version
# ----------------------------------------------


# ----------------------------------------------
# Load data

cod_mdata_KIN16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "KIN"))
cod_mdata_BDD16 <- data.table(read_excel(paste0(dir, "/", PNLP_files$File.Names.[1], '.xls'), sheet= "BDD"))
cod_mdata_OR16 <- data.table(read_excel(paste0(dir,"/", PNLP_files$File.Names.[1], '.xls'), sheet= "OR"))

    #should I combine first and then clean? or clean data and then combine into one dataset?

# ----------------------------------------------


# ----------------------------------------------
# Set names of columns


# ----------------------------------------------


# ----------------------------------------------
# Get rid of rows you don't need (and columns) "subset"
# Clean values any missing, or format numbers such as 1,000 as 1000 
    # (make it a number not a string)
# Reshape data
# Make this all into a function to replicate it for each sheet within
    # excel doc
# Append provinces within years, and then all years together

# ----------------------------------------------
