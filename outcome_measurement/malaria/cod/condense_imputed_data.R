# read in all of the imputed data files by dps created by run_amelia and 
# calculate mean and upper/lower quantiles to condense the rounds of imputation
# do this across the 50 imputations, by hz, dps, and at the country level
# ----------------------------------------------     


# --------------------  
# Set up R / install packages
library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(stats)
library(Rcpp)
library(parallel)
library(boot)

run_name = commandArgs()[5]
run_name = gsub('\r', '', run_name)
# --------------------  


# ----------------------------------------------
# Overview - Files and Directories

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/DPSLevelPriors_imputations/')

# input file:


# directory 

# output directory:
output_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/DPSLevelPriors_imputations/')

# output files
condensed_imputed_data_hz <- paste0("condensed_imputed_data", run_name, ".rds")
condensed_imputed_data_dps <- paste0("condensed_imputed_data_dps", run_name, ".rds")
condensed_imputed_data_country <- paste0("condensed_imputed_data_country", run_name,".rds")
# ----------------------------------------------


# Wait until all other jobs are done
nJobs = 26
nFinished = length(list.files(dir))
i = 1
while(nJobs>nFinished) { 
  print(paste(nFinished, 'of', nJobs, 'complete, waiting...(', i, ')'))
  Sys.sleep(60)
  nFinished = length(list.files(dir))
  i=i+1
}

#-------IF STARTING HERE
# run up to dtLog (don't need to run this or imputation again)
# read in rds file of imputations
# dtExp <- readRDS(paste0(dir, "imputedData.rds"))
# imputed_id_vars <- c(id_vars, "imputation_number")
#---------------------


# ----------------------------------------------    
# loop through all of the files

# rbind them all together
# ----------------------------------------------    


# ----------------------------------------------    
# Set up a data table for graphing:

# reshape imputed data long
imputedDataLong <- melt(dtExp, id.vars=c(imputed_id_vars))
imputedDataLong <- imputedDataLong[, c("indicator", "subpopulation") := tstrsplit(variable, "_", fixed=TRUE)]
# ----------------------------------------------


# ----------------------------------------------
# Calculate by health_zone 
# compute upper middle and lower for the imputed points for the error bars in the graphs
graphData <- imputedDataLong[, .(mean=mean(value), 
                                 lower=quantile(value, .05), 
                                 upper=quantile(value, .95)), by=c(id_vars,"indicator", "subpopulation")]

# split subpopulations out from indicators
missMatrixMelt <- melt(dtOrig, id.vars=id_vars)
missMatrixMelt[, isMissing:=is.na(value)]

graphDataComplete <- merge(graphData, missMatrixMelt, by= c(id_vars, "variable"))

# get rid of lower and upper values for values that were NOT missing, so these don't show up on the graph
graphDataComplete <- graphDataComplete[isMissing==F, lower:= NA ]
graphDataComplete <- graphDataComplete[isMissing==F, upper:= NA ]

# export graphDataComplete
# write.csv(graphDataComplete, "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/Imputed Data.csv")
saveRDS(graphDataComplete, paste0(output_dir, condensed_imputed_data_hz))

# ----------------------------------------------


# ----------------------------------------------
# Aggregate and calculate mean by DPS to graph by DPS

# aggregate all indicator/intervention data by dps, within each imputation
aggData  <- imputedDataLong[, .(aggValue = sum(value)), by=c( "date", "province", "dps", "indicator", "subpopulation", "imputation_number" )]

# then compute the mean, upper and lower across all imputations for each unique dps/date
aggData <- aggData[, .(mean=mean(aggValue), 
                       lower=quantile(aggValue, .05), 
                       upper=quantile(aggValue, .95)), by=c("date", "province", "dps", "indicator", "subpopulation")]

# set upper and lower values to NA where the value was not imputed (where mean==lower and mean==upper)
aggData <- aggData[mean==lower, lower := NA]
aggData <- aggData[mean==upper, upper := NA]

# export data
# write.csv(graphDataComplete, "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/Imputed Data.csv")
saveRDS(aggData, paste0(output_dir, condensed_imputed_data_dps))
# ----------------------------------------------


# ----------------------------------------------    
# Aggregate and calculate at the country level to graph national values

# aggregate all indicator/intervention data by dps, within each imputation
fullCountryData  <- imputedDataLong[, .(aggValue = sum(value)), by=c( "date", "indicator", "subpopulation", "imputation_number" )]

# then compute the mean, upper and lower across all imputations for each unique dps/date
fullCountryData <- fullCountryData[, .(mean=mean(aggValue), 
                                       lower=quantile(aggValue, .05), 
                                       upper=quantile(aggValue, .95)), by=c("date", "indicator", "subpopulation")]

# set upper and lower values to NA where the value was not imputed (where mean==lower and mean==upper)
fullCountryData <- fullCountryData[mean==lower, lower := NA]
fullCountryData <- fullCountryData[mean==upper, upper := NA]

# export data
# write.csv(graphDataComplete, "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/Imputed Data.csv")
saveRDS(fullCountryData, paste0(output_dir, condensed_imputed_data_country))
# ----------------------------------------------          
