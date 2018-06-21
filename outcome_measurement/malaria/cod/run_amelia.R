# ----------------------------------------------
  # Audrey Batzel
  #
  # 3/16/18
  # COD PNLP data for 2014-2016
  # 6/15/18
  # Modified for COD PNLP data 2010-2017
  
    setwd('C:/local/gf/')

  # THE FOLLOWING R SCRIPT WAS RUN ON THE CLUSTER AT IHME
# ----------------------------------------------


# --------------------
  # Set up R / install packages
    rm(list=ls())
    library(data.table)
    library(stringr)
    library(reshape2)
    library(ggplot2)
    library(lubridate)
    library(readxl)
    library(stats)
    library(Rcpp)
    library(Amelia)
    library(parallel)
# --------------------  


# ----------------------------------------------
  # Overview - Files and Directories
  
    # data directory
    # when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
      j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
      dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
    
    # input file:
      input <- "PNLP_2010to2017_preppedForMI.csv"
    
    # output files:
      output <- "PNLP_2010to2017_imputedData.csv"
# ----------------------------------------------


# ----------------------------------------------
  # read in data table prepped by prep_for_MI.R
    dt <- fread(paste0(dir, input)) 
# ----------------------------------------------
           
      
# ----------------------------------------------
  # convert column types to proper types
    dt[, date := as.Date(date)]
    
    # remove V1 column
    dt <- dt[, V1:=NULL]
    
    # change name of province column
    setnames(dt, "province11_name", "province")
    
    # vector of id variables and indicator variables
    all_vars <- c(colnames(dt))
    id_vars <- c("province", "dps", "health_zone", "date", "id")
    indicators <- all_vars[!all_vars %in% id_vars] 
    indicators <- indicators[!indicators %in% c("healthFacilities_numReported")]
    
    # remove healthFacilities_numReported from the data.table
    dt <- dt[,c(id_vars, indicators), with=F] 
    
    # when a health zone is entirely na for an indicator over the entire time series,
    # set those NAs to 0 to avoid overimputing
    dtMelt <- melt(dt, id.vars= id_vars)
    dtMelt[, value := as.numeric(value)]
    
    dtMeltNAs <- dtMelt[, .(totNAObs = sum(is.na(value))), by= c("dps", "health_zone", "variable" )]
    dtMeltNAs <- dtMeltNAs[totNAObs==96, noData:= T]
    
    dtMerge <- merge(dtMelt, dtMeltNAs, by= c("dps", "health_zone", "variable"))
    
    dtMerge <- dtMerge[noData==T, value:=0]
    
    dtMerge <- dtMerge[, totNAObs:= NULL]
    dtMerge <- dtMerge[, noData:= NULL]
    
    dtMerge[ value < 0, value:= NA]
    
    dt <- dcast(dtMerge, province + dps + health_zone + date + id ~ variable, value.var = "value")
    dt <- as.data.table(dt)
# ---------------------------------------------- 
    

# ---------------------------------------------- 
  # log transform the data to run amelia on it
    # save original data
      dtOrig <- copy(dt)
  
  # store which observations had zero so we can switch them back to zero at the end, after imputation
      for (ind in indicators) dt[get(ind)<0, (ind):=NA]
      zeroes <- dt[, lapply(.SD, function(x) {x==0}), .SDcols=indicators, by= c(id_vars)] 
  
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
# ---------------------------------------------- 
    
    
# ---------------------------------------------- 
  # set up for parallel
    parallelMethod = ifelse(as.logical(Sys.info()['sysname']=='Windows'), 'snow', 'multicore')
    ncores = detectCores()
  
  # run the imputation - no polytime
    # ts variable: the date
    # cs variable: health zone - try with and without this and see what results are like
    # MI will ignore ID vars and include them as is in the output
    # lags/leads: indicators no la
    # intercs = FALSE by default, try with = TRUE
    
    id_vars_for_amelia <- id_vars[!id_vars %in% c("health_zone", "date")]
    
    measured_vars <- all_vars[!all_vars %in% id_vars]
    measured_vars <- measured_vars[!measured_vars %in% c("healthFacilities_numReported")]
    
    amelia.results <- amelia(dtLog, m=50, cs= "health_zone", ts="date", idvars= id_vars_for_amelia, tolerance= 0.001, lags = measured_vars,
                             leads = measured_vars, parallel= parallelMethod, ncpus= 40)

# ---------------------------------------------- 
  
  
# ----------------------------------------------         
  # bind amelia results into one data.table, include a column with the imputation number in order to 
  # keep track of this information
  
    for( i in 1:50 ) {
      amelia.results$imputations[[i]]$imputation_number <- i
      if (i==1)  amelia_data <- data.table(amelia.results$imputations[[i]])
      if (i>1) amelia_data <- rbind(amelia_data, amelia.results$imputations[[i]])
    }
# ----------------------------------------------  
  
  
# ----------------------------------------------
  imputed_id_vars <- c(id_vars, "imputation_number")
      
  # exponentiate the data set
  dtExp <- amelia_data[, lapply(.SD, function(x) exp(x)), .SDcols=indicators, by= c(imputed_id_vars)]
  
  # convert values back to 0s that were originally 0s
  for (var in indicators){
    dtExp <- dtExp[zeroes[get(var)== TRUE, id], (var):= 0]
  }
# ----------------------------------------------  
  
  
# ---------------------------------------------- 
  # export imputed data to have a saved version (do this as an RDS so it is faster/smaller file)
  # write.csv(dtExp, paste0(dir, output))
  saveRDS(dtExp, paste0(dir, "imputedData.rds"))
# ---------------------------------------------- 
  
# ----------------------------------------------     
  # Set up a data table for graphing:
  
  #-------IF STARTING HERE
    # run up to dtLog (don't need to run this or imputation again)
    # read in rds file of imputations
    # dtExp <- readRDS(paste0(dir, "imputedData.rds"))
    # imputed_id_vars <- c(id_vars, "imputation_number")
  #---------------------
    
  # reshape imputed data long
    imputedDataLong <- melt(dtExp, id.vars=c(imputed_id_vars))
  
  # compute upper middle and lower for the imputed points for the error bars in the graphs
    graphData <- imputedDataLong[, .(mean=mean(value), 
                                   lower=quantile(value, .05), 
                                   upper=quantile(value, .95)), by=c(id_vars,"variable")]
  
  # split subpopulations out from indicators
    graphData <- graphData[, c("indicator", "subpopulation") := tstrsplit(variable, "_", fixed=TRUE)]
    
    missMatrixMelt <- melt(dtOrig, id.vars=id_vars)
    missMatrixMelt[, isMissing:=is.na(value)]
    
    graphDataComplete <- merge(graphData, missMatrixMelt, by= c(id_vars, "variable"))
    
  # get rid of lower and upper values for values that were NOT missing, so these don't show up on the graph
    graphDataComplete <- graphDataComplete[isMissing==F, lower:= NA ]
    graphDataComplete <- graphDataComplete[isMissing==F, upper:= NA ]
    
  # export graphDataComplete
    # write.csv(graphDataComplete, "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/Imputed Data.csv")
    saveRDS(graphDataComplete, paste0(dir, "condensed_imputedData_forGraphing.rds"))
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    