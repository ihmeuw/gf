# ----------------------------------------------
# Audrey Batzel
#
# 5/21/2019
# post-processing of duplicates

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
library(parallel)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ---------------------------------------------- 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/")

# input files
dt_for_dup_removal = "ameliaDT_with_index_for_dup_removal.rds"
dup_matrix_initial =  "pnlp_matrix_of_duplicate_rows (initial results).rds"
dups_zeroes = "number_of_zeroes_for_dup_matrix.rds"

# output files
dup_matrix_output = "pnlp_matrix_of_duplicate_rows.rds"
dup_matrix_final = "final_pnlp_duplicates_matrix.rds"
# ----------------------------------------------   

# ----------------------------------------------     
# read in data
# ---------------------------------------------- 
dt = readRDS( paste0(dir, dt_for_dup_removal) ) 
dups = readRDS( paste0(dir, dup_matrix_initial) )
# ----------------------------------------------   

# ----------------------------------------------     
# remove 0s from dups
# ----------------------------------------------
dups = dups[ num_identical != 0, ]
# ----------------------------------------------   

# ----------------------------------------------     
# get the number of NAs in each row in dt
# ----------------------------------------------
id_vars <- c("dps", "health_zone", "date", "donor", "operational_support_partner", "population", "id")
inds = names(dt)[!names(dt) %in% id_vars]
dt[, number_not_na := rowSums(!is.na(dt[, inds, with = FALSE]))]

dups = merge(dups, dt[, .(id, number_not_na)], by.x = "i", by.y = "id")
setnames(dups, "number_not_na", "number_not_na_i")
dups = merge(dups, dt[, .(id, number_not_na)], by.x = "j", by.y = "id" )
setnames(dups, "number_not_na", "number_not_na_j")

dups[, i_j_same_number_not_na := (number_not_na_i==number_not_na_j)]
dups[, proportion_identical_i := num_identical / number_not_na_i ]
dups[, proportion_identical_j := num_identical / number_not_na_j ]
# ----------------------------------------------  

# ----------------------------------------------     
# if RowSum is 0, then remove any matches with that row from dups
# ----------------------------------------------
ids_to_remove = dt[ rowSums(dt[, inds, with = FALSE], na.rm = TRUE) == 0, id]
dups = dups[! i %in% ids_to_remove]
dups = dups[! j %in% ids_to_remove]
# ----------------------------------------------

# # ----------------------------------------------
# # save here
# # ----------------------------------------------
# saveRDS(dups, paste0(dir, dup_matrix_output) )
# # ----------------------------------------------

# ----------------------------------------------     
# loop through remaining ids and get the number that are the same that are 0
# ----------------------------------------------
## RUN ON THE CLUSTER ( update_duplicates.R )

# load result from this: 
zeroes = readRDS(paste0(dir, dups_zeroes))

# merge with dups
dups = merge(dups, zeroes, by = c("i", "j"), all = TRUE)
# calculate the proportion of identical columns that are 0s
dups[, prop_identical_0s := num_identical_0s/num_identical]

# remove rows that aren't actually duplicates...
dups = dups[num_identical != num_identical_0s]  # where the identical columns are only ones that are 0s
dups = dups[ proportion_identical_j > .2, ] # where the identical columns are a small portion of non-na columns in the corresponding row
dups =  dups[ num_identical > 15, ] # where the number of identical columns is less than 15 (vetted most of these by hand)
dups = dups[ prop_identical_0s <= .9 ] # where the identical columns are at least 90% 0s

# # add id column for subsetting
# dups[ , id := seq(1:nrow(dups))]
# def_dups = dups[ ( i_j_same_number_not_na == TRUE | (proportion_identical_i >= 0.8 & proportion_identical_j >= 0.8) ) & num_identical >= 30 & prop_identical_0s <= 0.7, ]
# maybe_dups = dups[ ! id %in% unique(def_dups$id)]
# ----------------------------------------------

# ----------------------------------------------     
# get the ids to remove from the final dups dt
# ----------------------------------------------
remove_ids = unique(c(dups$i, dups$j))
# ----------------------------------------------

