# ----------------------------------------------
# Audrey Batzel
#
# 3/16/18
# COD PNLP data for 2014-2016
# 6/15/18
# Modified for COD PNLP data 2010-2017

# 3/2019 this version of run_amelia log transforms the data, 
# lemon squeezes healthFacilitiesProportion, then runs amelia
# then rbinds all "m" together, transforms the data back, and saves.
# no priors

# 6/2019 run for final model with data improvements (end of month deliverable)
# ----------------------------------------------
# THE FOLLOWING R SCRIPT WAS RUN ON THE CLUSTER AT IHME
# qsub to run this script on the cluster:
# qsub -e /ihme/scratch/users/abatzel/mi_errors_output/ -o /ihme/scratch/users/abatzel/mi_errors_output/ -cwd -l fthread=50 -l m_mem_free=50G -l h_rt=120:00:00 -P proj_pce -q long.q -l archive=TRUE ./core/r_shell.sh ./outcome_measurement/malaria/cod/run_amelia.R 0.01 run01_aggVars_nolagsLeads agg

# runs:
# 0.1 test_run no_agg
# 0.1 test_run_aggVars agg
# 0.001 run001_wlagsleads no_agg
# 0.001 run001_aggVars_wlagsleads agg
# 0.01 run01_wlagsleads no_agg
# 0.01 run01_aggVars_wlagsleads agg

# test subset qsub
# qsub -e /ihme/scratch/users/abatzel/mi_errors_output/ -o /ihme/scratch/users/abatzel/mi_errors_output/ -cwd -l fthread=10 -l m_mem_free=20G -l h_rt=2:00:00 -P proj_pce -q all.q -l archive=TRUE ./core/r_shell.sh ./outcome_measurement/malaria/cod/test_run_amelia.R 0.1 test_run_aggVars agg
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
tol = commandArgs()[4]
tol = as.numeric(gsub('\r', '', tol))
print(tol)

run_name = commandArgs()[5]
run_name = gsub('\r', '', run_name)
print(run_name)

aggregate = commandArgs()[6]
aggregate = gsub('\r', '', aggregate)
print(aggregate)

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
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
  j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
  dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
  output_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')
  
# detect the user operating on the cluster
  user = Sys.info()[['user']]
  
# set the directory for cluster output and error messages
  oeDir = paste0('/ihme/scratch/users/', user, '/mi_errors_output/')
  if (!file.exists(oeDir)) dir.create(oeDir)
  
# input file:
  inFile = 'PNLP_dt_forMI_updated_6_10_19.rds'

# output files:
  rawFile = paste0("PNLP_imputed_rawData_", run_name, ".rds")
  cleanedFile = paste0("PNLP_imputedData_", run_name, ".rds")
  
# set switches
  cleanup_start = FALSE
# ----------------------------------------------

# ----------------------------------------------
# ----------------------------------------------
if (cleanup_start == TRUE){
  # before starting the process, delete the existing files on the cluster
  # this allows us to avoid duplication or aggregating old files 
  system(paste0('rm -r /ihme/scratch/users/', user, '/mi_errors_output/*')) # removes all files and folders within the directory
}  
# ----------------------------------------------

# ----------------------------------------------
# read in data table prepped by prep_for_MI.R and initial set up
# ---------------------------------------------- 
dt = readRDS(paste0(dir, inFile))
dt[, thinSmearTest := NULL]
# test subset
dt = dt[dps == unique(dt$dps)[1]]

if (aggregate == "agg"){
  # combine age groups for variables where these are combined in different years of data- check with David, is this okay? best way to do this?
  dt[, RDT_completed := ifelse( year <= 2014, RDT_completed, (RDT_completedUnder5 + RDT_completed5andOlder))]
  dt[, RDT_positive := ifelse( year <= 2014, RDT_positive, (RDT_positiveUnder5 + RDT_positive5andOlder))]
  dt[, smearTest_completed := ifelse( year <= 2014, smearTest_completed,(smearTest_completedUnder5 + smearTest_completed5andOlder))]
  dt[, smearTest_positive := ifelse( year <= 2014, smearTest_positive, (smearTest_positiveUnder5 + smearTest_positive5andOlder))]
  
  dt = dt[, -c("smearTest_completedUnder5", "smearTest_completed5andOlder", "smearTest_positiveUnder5", "smearTest_positive5andOlder",
                "RDT_positive5andOlder", "RDT_positiveUnder5", "RDT_completedUnder5", "RDT_completed5andOlder")]
  
  # Standardize SSC variables:
  dt[, SSCACT := ifelse( year <= 2016, SSCACT, (SSCACT_5andOlder + SSCACT_under5)) ]
  dt[, SSCRDT_completed := ifelse( year <= 2016, SSCRDT_completed, (SSCRDT_completedUnder5 + SSCRDT_completed5andOlder)) ]
  dt[, SSCRDT_positive := ifelse( year <= 2016, SSCRDT_positive, (SSCRDT_positiveUnder5 + SSCRDT_positive5andOlder)) ]
  dt[, SSCcasesCrossReferred := ifelse( year <= 2016, SSCcasesCrossReferred, (SSCcasesCrossReferred_5andOlder + SSCcasesCrossReferred_under5)) ]
  dt[, SSCcasesReferred := ifelse( year <= 2016, SSCcasesReferred, (SSCcasesReferred_5andOlder + SSCcasesReferred_under5)) ]
  dt[, SSCfevers := ifelse( year <= 2016, SSCfevers, (SSCfevers_5andOlder + SSCfevers_under5)) ]
  
  dt = dt[, -c("year", "SSCACT_5andOlder", "SSCACT_under5", "SSCRDT_completedUnder5", "SSCRDT_completed5andOlder",
               "SSCRDT_positiveUnder5", "SSCRDT_positive5andOlder", "SSCcasesCrossReferred_5andOlder", "SSCcasesCrossReferred_under5",
               "SSCcasesReferred_5andOlder", "SSCcasesReferred_under5", "SSCfevers_5andOlder", "SSCfevers_under5" )]
}

all_vars = names(dt)
id_vars = c("id", "dps", "health_zone", "date", "donor", "operational_support_partner", "population")
inds = all_vars[!all_vars %in% id_vars] 
# ----------------------------------------------

# ---------------------------------------------- 
# changes to variables to run - change 0s so we can log transform,
# but "lemon-squeeze" healthFacilitiesProportion
# ---------------------------------------------- 
# save original data
dtOrig <- copy(dt)
    
# store which observations had zero so we can switch them back to zero at the end, after imputation
for (ind in inds) dt[ get(ind) < 0, (ind) := NA]
zeroes <- dt[, lapply(.SD, function(x) {x==0}), .SDcols=inds, by= c(id_vars)] 
  
# log transform all of the data except for healthFacilitiesProportion to run amelia on it
log_inds <- inds[!inds %in% c("healthFacilitiesProportion")]

# logit transform the healthFacilities proportion data
N <- length( dt$healthFacilitiesProportion[!is.na(dt$healthFacilitiesProportion)])

prop_lsqueeze <- dt[, .(health_zone, dps, date, healthFacilitiesProportion =((healthFacilitiesProportion*(N-1)+0.5)/N))]
prop_lsqueeze <- prop_lsqueeze[, healthFacilitiesProportion:= logit(healthFacilitiesProportion)]

# replace all 0s with really low values so log works 
for(var in log_inds) {
  # taking the 5th percentile for each column to replace the 0s with 
  pctle <- quantile(dt[get(var)!=0][[var]], .01, na.rm=TRUE)  
  # change/store these back in dt so that we can use that to run amelia() on
  dt[get(var)==0, (var):=pctle]
}

# log transform
dtLog <- dt[, lapply(.SD, function(x) log(x)), .SDcols=log_inds, by= c(id_vars)]

# make a uniform random variable to convince amelia to extrapolate
dtLog[, random:=runif(nrow(dtLog))]

# merge the logit transformation of health facilities prop with dtLog
dt <- merge(dtLog, prop_lsqueeze, by=c("health_zone", "dps", "date"), all=TRUE)
# ---------------------------------------------- 
    
# ---------------------------------------------- 
# set up parameters and run amelia
# ---------------------------------------------- 
# set up for parallel
parallelMethod = ifelse(as.logical(Sys.info()['sysname']=='Windows'), 'snow', 'multicore')
ncores = detectCores()

# run imputation - no polytime
# ts variable: date
# cs variable: health zone and dps
# MI will ignore ID vars and include them as is in the output
# lags/leads: all indicators
# intercs = FALSE by default
dt[ , combine := paste(dps, health_zone, sep = "_")] # make a combined variable for dps-health_zone to use for the cs var in amelia

id_vars_for_amelia = id_vars[!id_vars %in% c("date")]  # needs to exclude date and health_zone

measured_vars <- colnames(dt)
measured_vars <- measured_vars[!measured_vars %in% c(id_vars_for_amelia, "combine", "date", with=FALSE)]

num_of_runs = 5

amelia.results <- amelia(dt, m=num_of_runs, cs= "combine", ts="date", idvars= id_vars_for_amelia, tolerance= tol, # the passed in tolerance
                         # lags = measured_vars, leads= measured_vars,
                         parallel= parallelMethod, ncpus= num_of_runs ) # ncpus should correspond to m
# ---------------------------------------------- 
  
# ----------------------------------------------

# ----------------------------------------------         
# bind amelia results into one data.table, and save
# ----------------------------------------------         
for( i in 1:num_of_runs ) {
    # include a column with the imputation number in order to keep track of diff iterations
    amelia.results$imputations[[i]]$imputation_number <- i
    if (i==1) amelia_data <- data.table(amelia.results$imputations[[i]])
    if (i>1) amelia_data <- rbind(amelia_data, amelia.results$imputations[[i]])}
  
saveRDS(amelia_data, paste0(output_dir, rawFile))
# ----------------------------------------------  
  
# ----------------------------------------------
# inv.logit() and exp() the data produced by amelia() to re-transform it back to how it was before imputation
# set original zeroes back to zero
# ----------------------------------------------S
# include imputation number in the id_vars used to exponentiate the data set so exp() happens for each of the 50 imputations
imputed_id_vars <- c(id_vars, "imputation_number")

# make sure healthFaciliesProportion is excluded from indicators bc we will inv.logit() it rather than exp()
inds <- inds[!inds %in% c("healthFacilitiesProportion")]

# inv.logit of healthFacilitiesProportion
amelia_data[, healthFacilitiesProportion:= inv.logit(healthFacilitiesProportion)]
amelia_data[, healthFacilitiesProportion:=((healthFacilitiesProportion * N)-0.5) / (N-1)]

# exponentiate the rest of the data set
dtExp <- amelia_data[, lapply(.SD, function(x) exp(x)), .SDcols=inds]

# convert values back to 0s that were originally 0s
for (var in inds){
  dtExp <- dtExp[zeroes[get(var)== TRUE, id], (var):= 0]
}
# ----------------------------------------------  
  
# ---------------------------------------------- 
# export imputed data to have a saved full version 
# (do this as an RDS so it is faster/smaller file)
# ---------------------------------------------- 
saveRDS(dtExp, paste0(output_dir, cleanedFile))
# ---------------------------------------------- 
    
    

  
  
  
  
    