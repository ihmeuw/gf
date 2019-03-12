# ----------------------------------------------
  # Audrey Batzel
  #
  # 3/16/18
  # COD PNLP data for 2014-2016
  # 6/15/18
  # Modified for COD PNLP data 2010-2017
  
  # THE FOLLOWING R SCRIPT WAS RUN ON THE CLUSTER AT IHME
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

# input file:
  input <- "PNLP_dt_forMI_updated_3_11_19.rds"

# output directory:
  output_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')

# output file:
  initial_imputed_data_file = paste0("PNLP_imputedData_", run_name, "_rawData.rds")
  cleaned_imputed_data_file = paste0("PNLP_imputedData_", run_name, ".rds")
# ----------------------------------------------

# ----------------------------------------------
# read in data table prepped by prep_for_MI.R and initial set up
# ---------------------------------------------- 
dt <- readRDS(paste0( dir, input ))

dt$dps <- gsub(" ", "-", dt$dps)

all_vars <- names(dt)
id_vars <- c("id", "dps", "health_zone", "date", "donor", "operational_support_partner", "population")
indicators <- all_vars[!all_vars %in% id_vars] 

dt = dt[, lapply(.SD, as.numeric), .SDcols=indicators, by = c(id_vars) ]
# ----------------------------------------------

# ---------------------------------------------- 
# changes to variables to run - change 0s so we can log transform,
# but "lemon-squeeze" healthFacilitiesProportion
# ---------------------------------------------- 
# save original data
dtOrig <- copy(dt)
    
# store which observations had zero so we can switch them back to zero at the end, after imputation
for (ind in indicators) dt[get(ind)<0, (ind):=NA]
zeroes <- dt[, lapply(.SD, function(x) {x==0}), .SDcols=indicators, by= c(id_vars)] 
  
# log transform all of the data except for healthFacilitiesProportion to run amelia on it
indicators <- indicators[!indicators %in% c("healthFacilitiesProportion")]

# logit transform the healthFacilities proportion data
N <- length( dt$healthFacilitiesProportion[!is.na(dt$healthFacilitiesProportion)])

prop_lsqueeze <- dt[, .(health_zone, dps, date, healthFacilitiesProportion =((healthFacilitiesProportion*(N-1)+0.5)/N))]
prop_lsqueeze <- prop_lsqueeze[, healthFacilitiesProportion:= logit(healthFacilitiesProportion)]

# replace all 0s with really low values so log works 
for(var in indicators) {
  # taking the 5th percentile for each column to replace the 0s with 
  pctle <- quantile(dt[get(var)!=0][[var]], .01, na.rm=TRUE)  
  # change/store these back in dt so that we can use that to run amelia() on
  dt[get(var)==0, (var):=pctle]
}

# log transform
dtLog <- dt[, lapply(.SD, function(x) log(x)), .SDcols=indicators, by= c(id_vars)]

# make a constant to convince amelia to extrapolate
dtLog[, random:=runif(nrow(dtLog))]

# merge the logit transformation of health facilities prop with dtLog
dtLog <- merge(dtLog, prop_lsqueeze, by=c("health_zone", "dps", "date"), all=T)
# ---------------------------------------------- 
    
# ---------------------------------------------- 
# set up parameters and run amelia
# ---------------------------------------------- 
# set up for parallel
parallelMethod = ifelse(as.logical(Sys.info()['sysname']=='Windows'), 'snow', 'multicore')
ncores = detectCores()

# run imputation - no polytime
# ts variable: date
# cs variable: health zone
# MI will ignore ID vars and include them as is in the output
# lags/leads: indicators
# intercs = FALSE by default

id_vars_for_amelia = id_vars[!id_vars %in% c("date", "health_zone")]  # needs to exclude date and health_zone

dtLog$date <- as.Date(dtLog$date)

measured_vars <- colnames(dtLog)
measured_vars <- measured_vars[!measured_vars %in% c(id_vars_for_amelia, "health_zone", "date", with=F)]

amelia.results <- amelia(dtLog, m=1, cs= "health_zone", ts="date", idvars= id_vars_for_amelia, tolerance= tol, # the passed in tolerance
                         lags = measured_vars, leads= measured_vars,
                         parallel= parallelMethod, ncpus= 10) # ncpus should correspond to m
# ---------------------------------------------- 
  
# ----------------------------------------------

# ----------------------------------------------         
# bind amelia results into one data.table, include a column with the imputation number in order to keep track of diff iterations
  for( i in 1:50 ) {
    amelia.results$imputations[[i]]$imputation_number <- i
    if (i==1) amelia_data <- data.table(amelia.results$imputations[[i]])
    if (i>1) amelia_data <- rbind(amelia_data, amelia.results$imputations[[i]])
  }
  
saveRDS(amelia_data, paste0(output_dir, initial_imputed_data_file))
# ----------------------------------------------  
  
# ----------------------------------------------
# Inv.logit() and exp() the data produced by amelia() to re-transform it back to how it was before imputation
# ----------------------------------------------S
# include imputation number in the id_vars used to exponentiate the data set so exp() happens for each of the 50 imputations
imputed_id_vars <- c(id_vars, "imputation_number")

# make sure healthFaciliesProportion is excluded from indicators bc we will inv.logit() it rather than exp()
indicators <- indicators[!indicators %in% c("healthFacilitiesProportion")]

# inv.logit of healthFacilitiesProportion
amelia_data[, healthFacilitiesProportion:= inv.logit(healthFacilitiesProportion)]
amelia_data[, healthFacilitiesProportion:=((healthFacilitiesProportion * N)-0.5) / (N-1)]

# exponentiate the rest of the data set
dtExp <- amelia_data[, lapply(.SD, function(x) exp(x)), .SDcols=indicators]

# convert values back to 0s that were originally 0s
for (var in indicators){
  dtExp <- dtExp[zeroes[get(var)== TRUE, id], (var):= 0]
}
# ----------------------------------------------  
  
# ---------------------------------------------- 
# export imputed data to have a saved full version (do this as an RDS so it is faster/smaller file)
saveRDS(dtExp, paste0(output_dir, cleaned_imputed_data_file))
# ---------------------------------------------- 
    
    

  
  
  
  
    