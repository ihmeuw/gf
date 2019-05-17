# ----------------------------------------------
# Audrey Batzel
#
# 5/16/2019
# screen PNLP data for duplicate rows

# qsub to run this script on the cluster:
# qsub -e /ihme/scratch/users/abatzel/duplicate_removal_output/ -o /ihme/scratch/users/abatzel/duplicate_removal_output/ -cwd -l fthread=50 -l m_mem_free=50G -l h_rt=108:00:00 -P proj_pce -q long.q -l archive=TRUE ./core/r_shell.sh ./outcome_measurement/malaria/cod/screen_duplicates.R

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

# output files
outFile =  "pnlp_matrix_of_duplicate_rows.rds"
# ----------------------------------------------   

# ----------------------------------------------     
# read in data
# ---------------------------------------------- 
dt <- readRDS( paste0(dir, dt_for_dup_removal) ) 
# ----------------------------------------------     

# ----------------------------------------------     
# remove duplicates
# ---------------------------------------------- 
# id variables
id_vars <- c("dps", "health_zone", "date", "donor", "operational_support_partner", "population", "id")

# # create a subset for unit testing:
# subset = dt[ id %in% c(27280:27288, 27654, 27666, 26124:26147, 6920, 6921, 27653:27655, 20473, 20474)]
#   # new idea for subset rows
#   subset$id = seq(nrow(subset))
  
# full run:
dup_matrix = data.table()
current_dup_matrix = data.table()
inds = names(dt)[!names(dt) %in% id_vars]
tot_cols = length(dt[, inds, with = FALSE])


dup_matrix = mclapply(seq(nrow(dt)), function(i) {
  # sink(file = paste0(dir, "output_from_duplicates_loop/dups_", i, ".txt"))
  
  print(paste('Checking row', i, 'for duplicates...'))
  
  for(j in seq(from=i+1, to=nrow(dt))) {
    
    print(paste("Checking", i, "against", j, "..."))
    
    if (i==j) next
    n = sum( dt[i, inds, with=FALSE] == dt[j, inds, with=FALSE], na.rm=T)
    print(n)
    print(paste0("comparing n to ( tot_cols - sum(is.na(dt[i, inds, with=FALSE])) ) * 0.80 ) which =", ( tot_cols - sum(is.na(dt[i, inds, with=FALSE])) ) * 0.80 ))
    if ( n < ( ( tot_cols - sum(is.na(dt[i, inds, with=FALSE])) ) * 0.80 ) ) {
      print(paste(i, "and", j, "not duplicates"))
      next
    }
    
    print(paste(i, "and", j, "are duplicates, so add to data table"))
    tmp = data.table(i=i, j=j, num_identical=n)
    print("printing tmp data table...")
    print(tmp)
    current_dup_matrix = rbind(current_dup_matrix, tmp)
    print("printing dup matrix up to this point...")
    print(current_dup_matrix)
    
  }
  if (nrow(current_dup_matrix)>0) return(current_dup_matrix)
  # sink()
}, mc.cores=ifelse(Sys.info()[1]=='Windows', 1, 55 ))


dup_matrix_final = rbindlist(dup_matrix)
saveRDS(dup_matrix_final, paste0(dir, outFile))

remove_ids = unique(c(dup_matrix_final$i, dup_matrix_final$j))




# # noticed problem with duplicate values:
# inds = names(ameliaDT)[!names(ameliaDT) %in% id_vars]
# inds = inds[58:75]
# 
# # get all duplicates over inds
# dups = ameliaDT[ (duplicated( ameliaDT[, inds, with = FALSE] )) | (duplicated( ameliaDT[, inds, with = FALSE], fromLast =  TRUE )) , ]
# # exclude "duplicates" where all are na and/or 0  
# # dups = dups[ rowSums( dups[, ..inds], na.rm = TRUE ) != 0 , ]
# 
# inds = names(ameliaDT)[!names(ameliaDT) %in% id_vars]
# inds = inds[1:27]
# dups2 = ameliaDT[ (duplicated( ameliaDT[, inds, with = FALSE] )) | (duplicated( ameliaDT[, inds, with = FALSE], fromLast =  TRUE )) , ]
# # dups2 = dups2[ rowSums( dups2[, ..inds], na.rm = TRUE ) != 0 , ]
# 
# check1 = as.data.table(anti_join(dups, dups2, by = id_vars))
# # # this way is more memory-efficient: (both get same results)
# # dt[, dups := as.numeric(duplicated(dt[, inds, with=FALSE]))]
# # dt[, dups := max(dups), by=inds ]
# # dt[, row_sum := rowSums(.SD, na.rm = TRUE), .SDcols = inds]
# # dt[row_sum == 0, dups := 0]
# 
# # use the id variable in dups to set those entire rows to na
# set_to_na = unique(dups$id)
# check = copy(ameliaDT)
# check = check[set_to_na, (names(ameliaDT)[!names(ameliaDT) %in% id_vars]) := .SD[NA] ]


# different way to detect duplicates:

# # test runs:
# # loop over rows and compare them, keeping track of the number of values that are the same
# small_subset = ameliaDT[1901:2200, 1:25]
# dup_matrix = data.table()
# current_dup_matrix = data.table()
# inds = names(small_subset)[!names(small_subset) %in% id_vars]
# tot_cols = length(small_subset[, inds, with = FALSE])
# 
# #by number identical
# dup_matrix = mclapply(seq(nrow(small_subset)), function(i) { 
#     print(paste('Checking against row', i, 'for duplicates...'))
#   for(j in seq(from=i+1, to=nrow(small_subset))) { 
#     if (i==j) next 
#     n = sum( small_subset[i, inds, with=FALSE] == small_subset[j, inds, with=FALSE], na.rm=T)
#     if (n < ((tot_cols) - 5)) next
#     tmp = data.table(i=i, j=j, num_identical=n)
#     current_dup_matrix = rbind(current_dup_matrix, tmp)
#   }
#   if (nrow(current_dup_matrix)>0) return(current_dup_matrix)
# }, mc.cores=ifelse(Sys.info()[1]=='Windows',1,30))
# 
# dup_matrix = rbindlist(dup_matrix)
# saveRDS(dup_matrix, paste0(dir, "pnlp_matrix_of_duplicate_rows.rds"))


# dup_matrix = readRDS("J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/pnlp_matrix_of_duplicate_rows.rds")

  