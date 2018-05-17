# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/16/2018
# Multiple imputation for the Uganda Viral Load Dashboard
# Run MI (linear and polytime models) on the cluster
# ----------------------------------------------
# Set up R
runAmelia = function(p=NULL) { 
  rm(list=ls())
  library(data.table)
  library(rgeos)
  library(raster)
  library(ggplot2)
  library(rgdal)
  library(dplyr)
  library(RColorBrewer)
  library(Amelia)
  library(MASS)
  library(gtools)
  library(parallel)
  
  # set input/output directory
  # ----------------------------------------------
  j <- ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
  dir <- paste0(j,'/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/imputed')
  
  # upload the data with month, year, sex
  uvl <- readRDS(paste0(dir, "/impute_ready_offset.rds"))
  
  #------------------------------------
  
  #------------------------------------
  # run imputation using amelia
  # offset model - the counts are offset by +1 and the ratios are lemon squeeze
  
  #---------------
  # make cs variable
  uvl[, cs_variable:=paste0(facility_id, sex)]
  
  # set idvars
  idVars <- c("facility_id", "sex", "facility_name", "dhis2name", "dist_name", "imputed", 
              'missing_males', 'missing_females', 'ratio1',
              'patients_received0', 'samples_received0', 'rejected_samples0', 'dbs_samples0', 'samples_tested0', 'valid_results0', 'suppressed0')
  
  # list the variables to lag
  lagVars <- c( "patients_received", "samples_received", "rejected_samples", "dbs_samples", "samples_tested", "valid_results", "ratio")
  
  # set up for parallel
  ncores = detectCores()
  
  # run the imputation - no polytime
  imputed_data <- amelia(uvl, m=50, cs='cs_variable', ts='date', 
                          lags=lagVars, leads=lagVars, idvars=idVars, 
                         noms='district_id', ords='level', parallel='snow', 
                         ncpus=ncores, polytime=p)
  
  #--------------------------------------------------------------
  # bind the imputations together to create a single data set called amelia_data
  
  for( i in 1:50 ) {
    imputed_data$imputations[[i]]$imputation_number <- i
    if (i==1)  amelia_data <- data.table(imputed_data$imputations[[i]])
    if (i>1) amelia_data <- rbind(amelia_data, imputed_data$imputations[[i]])
  }
  
  # save the imputed data as an RDS
  saveRDS(amelia_data, file= paste0(dir, "/imputed_offset_time", p, ".rds"))  
  #-------------------------------

}