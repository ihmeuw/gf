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

# ----------------------------------------------
# THE FOLLOWING R SCRIPT WAS RUN ON THE CLUSTER AT IHME
# steps to run this script:
# 1) set the working directory in the qlogin by navigating to it

# 2) once you have navigated to the directory, 
# git pull (make sure you have pushed from github desktop)

# 3) to open an interactive r session on the cluser:
# singularity exec /share/singularity-images/health_fin/forecasting/best_new.img R

# 4)
# set switches
aggregate = "agg" # noAgg for FALSE
lags_leads = "no_lags_leads" # "lags_and_leads" for TRUE
tolerance = 0.1
# set run_name manually:
run_name = 'run_0_1_aggVars_noLagsLeads'

cleanup_start = FALSE
# runs:
# for testing-
# 1.0 run1_0_aggVars_noLagsLeads
# 1.0 run1_0_noAgg_noLagsLeads
# other - 
# 0.1 run0_1_aggVars_noLagsLeads
# 0.1 run0_1_noAgg_noLagsLeads

# 0.001 run0_001_wlagsleads no_agg
# 0.001 run0_001_aggVars_wlagsleads agg
# 0.01 run0_01_wlagsleads no_agg
# 0.01 run0_01_aggVars_wlagsleads agg

# 5) run this script up to the qsub line
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
# detect the user operating on the cluster
user = Sys.info()[['user']]

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
  j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
  dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
  
  scratchDir = paste0('/ihme/scratch/users/', user, '/')
  if (!file.exists(scratchDir)) dir.create(scratchDir)
  resultsDir = paste0(scratchDir, 'mi_results/')
  if (!file.exists(resultsDir)) dir.create(resultsDir)
  oeDir = paste0(scratchDir, 'mi_errors_output/')
  if (!file.exists(oeDir)) dir.create(oeDir)

# input file:
  inFile = 'PNLP_dt_forMI_updated_6_10_19.rds'
  
# output files:
  if (aggregate == "agg"){ 
    outFile = "data_for_amelia_aggVars.rds" } else {
    outFile = "data_for_amelia_noAggVars.rds" }
  if (aggregate == "agg"){ 
    origData = "initial_data_for_amelia_aggVars.rds" } else {
    origData = "initial_data_for_amelia_noAggVars.rds" }
  if (aggregate == "agg"){ 
    zeroesData = "zeroes_data_for_amelia_aggVars.rds" } else {
    zeroesData = "zeroes_data_for_amelia_noAggVars.rds" }
# ----------------------------------------------
  
# ----------------------------------------------
if (cleanup_start == TRUE){
  # before starting the process, delete the existing files on the cluster
  # this allows us to avoid duplication or aggregating old files 
  system(paste0('rm -r /ihme/scratch/users/', user, '/mi_errors_output/*')) # removes all files and folders within the directory
}  
# ----------------------------------------------
  
# ---------------------------------------------- 
# submit qsubs to run amelia 50 times
# ----------------------------------------------
N = 50
system(paste0('qsub -e ', oeDir, ' -o ', oeDir,' -q all.q -P proj_pce -N ', run_name, ' -l m_mem_free=10G -l fthread=1 -l h_rt=50:00:00 -l archive=TRUE -cwd -t 1:', N, ' ./core/r_shell.sh ./outcome_measurement/malaria/cod/run_amelia_qsub.R ', tolerance, ' ', aggregate, ' ', lags_leads, ' ', run_name)) 
# ----------------------------------------------
  


# ----------------------------------------------
# BELOW IS PREP CODE PREVIOUSLY RUN ON THE CLUSTER, don't have to do it again
# ----------------------------------------------

# ----------------------------------------------
# read in data table prepped by prep_for_MI.R and initial set up
# ---------------------------------------------- 
dt = readRDS(paste0(dir, inFile))
dt[, thinSmearTest := NULL]

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
               "SSCcasesReferred_5andOlder", "SSCcasesReferred_under5", "SSCfevers_5andOlder", "SSCfevers_under5" )]}

all_vars = names(dt)
id_vars = c("id", "dps", "health_zone", "date", "donor", "operational_support_partner", "population")
inds = all_vars[!all_vars %in% id_vars] 
# ----------------------------------------------

# ---------------------------------------------- 
# changes to variables to run - change 0s so we can log transform,
# but "lemon-squeeze" healthFacilitiesProportion
# ---------------------------------------------- 
# save original data
saveRDS(dt, paste0(scratchDir, origData))
    
# store which observations had zero so we can switch them back to zero at the end, after imputation
for (ind in inds) dt[ get(ind) < 0, (ind) := NA]
zeroes <- dt[, lapply(.SD, function(x) {x==0}), .SDcols=inds, by= c(id_vars)] 
# save zeroes
saveRDS(zeroes, paste0(scratchDir, zeroesData))

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
# save data sets to /ihme/scratch/ on the cluster
# ---------------------------------------------- 
saveRDS(dt, paste0(scratchDir, outFile))
# ---------------------------------------------- 


    
    

  
  
  
  
    