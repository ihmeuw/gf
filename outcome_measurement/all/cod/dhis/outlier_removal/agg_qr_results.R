# Aggregate and save quant reg results (separate script so I can submit this as a qsub)
# ----------------------------------------------
# Audrey Batzel
#
# 3/14/19
# The current working directory should be the root of the repo
# This code must be run on the cluster. 
# ----------------------------------------------

# --------------------
# Set up R
# --------------------
library(data.table)
library(quantreg)
library(fst) # to save data tables as .fst for faster read/write and full random access

user_name = 'abatzel'
# --------------------

#------------------------------------
# set directories, switchs, arguments  <---- CHANGE THESE FOR YOUR OWN DATA
#------------------------------------
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')

# files:
outFile = paste0(dir, 'sigl_quantreg_imputation_results.rds') # at the very end, once all of the files are aggregated from /ihme/scratch/
#------------------------------------

#------------------------------------
# once all files are done, collect all output into one data table
#------------------------------------
fullData = data.table()
numFiles = length(list.files(paste0('/ihme/scratch/users/', user_name, '/qr_results')))
for (j in seq(numFiles)) {
  tmp = read.fst(paste0('/ihme/scratch/users/', user_name, '/qr_results/quantreg_output', j, '.fst'), as.data.table = TRUE)
  if(j==1) fullData = tmp
  if(j>1) fullData = rbind(fullData, tmp)
  cat(paste0('\r', j))
  flush.console() 
}

# save full data
write.fst(fullData, paste0('/ihme/scratch/users/', user_name, '/agg_quantreg_output.fst'))
saveRDS(fullData, outFile)
#------------------------------------