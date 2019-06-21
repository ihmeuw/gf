# ----------------------------------------------
# Audrey Batzel
# 6/2019 
# bind all the data together and exponentiate and inv.logit it
# and then convert 0s back

aggregate = "agg"
run_name = 'run_0_001_aggVars_lagsLeads'
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
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')

# directories on the cluster
scratchDir = paste0('/ihme/scratch/users/', user, '/')
if (!file.exists(scratchDir)) dir.create(scratchDir)
resultsDir = paste0(scratchDir, 'mi_results/')
if (!file.exists(resultsDir)) dir.create(resultsDir)
oeDir = paste0(scratchDir, 'mi_errors_output/')
if (!file.exists(oeDir)) dir.create(oeDir)

# input files
if (aggregate == "agg"){ 
  origData = "initial_data_for_amelia_aggVars.rds" } else {
    origData = "initial_data_for_amelia_noAggVars.rds" }
if (aggregate == "agg"){ 
  zeroesData = "zeroes_data_for_amelia_aggVars.rds" } else {
    zeroesData = "zeroes_data_for_amelia_noAggVars.rds" }

# output file
cleanedFile = paste0("PNLP_imputedData_", run_name, ".rds")
# ----------------------------------------------

# ----------------------------------------------
# read in and bind all the imputations together
# ----------------------------------------------
dtOrig = readRDS(paste0(scratchDir, origData))
zeroes = readRDS(paste0(scratchDir, zeroesData))

files = list.files(paste0(dir, 'raw_files/', run_name, '/'), recursive=TRUE)
if (length(files)!= 50) stop("Missing imputation results; there are not 50 files.")

dt = data.table()

# merge all the data together:
for(f in files) {
  # load the RDS file
  file_name = f
  current_data = data.table(readRDS(paste0(dir, 'raw_files/', run_name, '/', f)))
  # rbind data together
  dt = rbind(dt, current_data)
}
# ----------------------------------------------

# ----------------------------------------------
# inv.logit() and exp() the data produced by amelia() to re-transform it back to how it was before imputation
# set original zeroes back to zero
# ----------------------------------------------
all_vars = names(dt)
id_vars = c("id", "dps", "health_zone", "date", "donor", "operational_support_partner", "population")

dt[, random:=NULL]

if (dim(dt)[1] != 2477400 ) stop("check data - dimensions aren't right")

# include imputation number in the id_vars used to exponentiate the data set so exp() happens for each of the 50 imputations
imputed_id_vars <- c(id_vars, "imputation_number", "combine")
inds = all_vars[!all_vars %in% imputed_id_vars] 

# make sure healthFaciliesProportion is excluded from indicators bc we will inv.logit() it rather than exp()
inds <- inds[!inds %in% c("healthFacilitiesProportion")]

# inv.logit of healthFacilitiesProportion
N = length( dtOrig$healthFacilitiesProportion[!is.na(dtOrig$healthFacilitiesProportion)])
dt[, healthFacilitiesProportion:= inv.logit(healthFacilitiesProportion)]
dt[, healthFacilitiesProportion:=((healthFacilitiesProportion * N)-0.5) / (N-1)]

# exponentiate the rest of the data set
dtExp <- dt[, lapply(.SD, function(x) exp(x)), .SDcols=inds, by = c(imputed_id_vars)]

dtFac = dt[, c(imputed_id_vars, "healthFacilitiesProportion"), with = FALSE]
dt = merge(dtExp, dtFac, by = imputed_id_vars )

# reps = max(dt$imputation_number)
# zeroes = do.call("rbind", replicate(reps, zeroes, simplify = FALSE))
if (dim(dt)[1] != 2477400) stop("check data - dimensions aren't right")

# convert values back to 0s that were originally 0s
for (var in inds){
  set_ids = zeroes[get(var) == TRUE, id]
  dt = dt[id %in% set_ids, (var):= 0]
}
# ----------------------------------------------

# ----------------------------------------------
# export imputed data to have a saved full version
# (do this as an RDS so it is faster/smaller file)
# ----------------------------------------------
saveRDS(dt, paste0(dir, cleanedFile))
# ----------------------------------------------