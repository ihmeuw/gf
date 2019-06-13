# ----------------------------------------------
# Audrey Batzel
#
# 6/12/2019
# 
# qsub script to run amelia using 50 qsubs of 1 run rather than 1 qsub for 50 runs
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(stats)
library(Rcpp)
library(Amelia)
library(parallel)
library(boot)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# command args 
tol = commandArgs()[4]
tol = as.numeric(gsub('\r', '', tol))
print(tol)

aggregate = commandArgs()[5]
aggregate = gsub('\r', '', aggregate)
print(aggregate)

lags_leads = commandArgs()[6]
lags_leads = gsub('\r', '', lags_leads)
print(lags_leads)

run_name = commandArgs()[7]
run_name = gsub('\r', '', run_name)
print(run_name)

# detect the user operating on the cluster
user = Sys.info()[['user']]

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/raw_files/')

scratchDir = paste0('/ihme/scratch/users/', user, '/')
resultsDir = paste0(scratchDir, 'mi_results/')
oeDir = paste0(scratchDir, 'mi_errors_output/')

# input file:
if (aggregate == "agg"){ 
  inFile = "data_for_amelia_aggVars.rds" } else {
  inFile = "data_for_amelia_noAggVars.rds" }

# get task id for naming output file
i = as.integer(Sys.getenv("SGE_TASK_ID"))

# output files
rawFile = paste0("imputedRawData_", run_name, "_", i, ".rds")
# ----------------------------------------------

# ----------------------------------------------
# read in data table prepped by prep_for_MI.R and initial set up
# ---------------------------------------------- 
dt = readRDS(paste0(scratchDir, inFile))

# #test subset
# dt = dt[ dps == unique(dt$dps)[1], ]

dt[ , combine := paste(dps, health_zone, sep = "_")] # make a combined variable for dps-health_zone to use for the cs var in amelia

id_vars = c("id", "dps", "health_zone", "date", "donor", "operational_support_partner", "population")
id_vars_for_amelia = id_vars[!id_vars %in% c("date")]  # needs to exclude date and health_zone

measured_vars <- colnames(dt)
measured_vars <- measured_vars[!measured_vars %in% c(id_vars_for_amelia, "combine", "date", with=FALSE)]
# ----------------------------------------------

# ----------------------------------------------
# run one iteration of amelia
# ---------------------------------------------- 
# set up for parallel
parallelMethod = ifelse(as.logical(Sys.info()['sysname']=='Windows'), 'snow', 'multicore')

# run imputation - no polytime
# ts variable: date
# cs variable: health zone and dps
# MI will ignore ID vars and include them as is in the output
# lags/leads: all indicators
# intercs = FALSE by default

# quick check 
lags_leads == "lags_and_leads"

if (lags_leads == "lags_and_leads"){ 
  amelia.results <- amelia(dt, m=1, cs= "combine", ts="date", idvars= id_vars_for_amelia, tolerance= tol, # the passed in tolerance
                           lags = measured_vars, leads= measured_vars,
                           parallel= parallelMethod, ncpus= 2 ) # ncpus should correspond to m
  } else {
  amelia.results <- amelia(dt, m=1, cs= "combine", ts="date", idvars= id_vars_for_amelia, tolerance= tol, # the passed in tolerance
                           # lags = measured_vars, leads= measured_vars,
                           parallel= parallelMethod, ncpus= 2 ) # ncpus should correspond to m 
  }
# ----------------------------------------------

# ----------------------------------------------
# save the data produced from the one run to /ihme/scratch
# ---------------------------------------------- 
amelia.results$imputations[[1]]$imputation_number <- i
amelia_data <- data.table(amelia.results$imputations[[1]])

saveRDS(amelia_data, paste0(output_dir, run_name, "/", rawFile))
# ----------------------------------------------



