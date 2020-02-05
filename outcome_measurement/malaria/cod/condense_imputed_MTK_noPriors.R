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
# --------------------  


# ----------------------------------------------
# Overview - Files and Directories

# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
input_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')

# original data 
# input file:
input <- "final_data_for_impuation.csv"

# directory for full Data file with no priors
fullData_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
# input file:
fullData_noPriors <- "imputedData_amelia_run_fullData_noPriors.rds"
# read in this data:
dtExp <- readRDS(paste0(fullData_dir, fullData_noPriors))

dtExp <- dtExp[dps %in% c("maniema", "tshopo", "kinshasa"),]

# output directory:
output_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')

# output files
imputed_data_long <- "imputed_MTK_noPriors_long.rds"
condensed_imputed_data_hz <- "condensed_imputed_MTK_noPriors_hz.rds"
condensed_imputed_data_dps <- "condensed_imputed_MTK_noPriors_dps.rds"
condensed_imputed_data_country <- "condensed_imputed_MTK_noPriors_country.rds"

# read in data table prepped by prep_for_MI.R 
dt <- fread(paste0(input_dir, input)) 
dt <- dt[, V1:=NULL]

dt$dps <- gsub(" ", "-", dt$dps)

all_vars <- c(colnames(dt))
id_vars <- c("province", "dps", "health_zone", "date")
indicators <- all_vars[!all_vars %in% id_vars] 
indicators <- indicators[!indicators %in% c("V1")]

dtOrig <- copy(dt)
# ----------------------------------------------


# ----------------------------------------------    
# Set up a data table for graphing:
imputed_id_vars <- c('province', 'dps', 'health_zone', 'date', 'imputation_number')

# reshape imputed data long
imputedDataLong <- melt(dtExp, id.vars=c(imputed_id_vars))
imputedDataLong <- imputedDataLong[, c("indicator", "subpopulation") := tstrsplit(variable, "_", fixed=TRUE)]

# save data in this format (since it takes a while to run), so it can be used later on if the code breaks somewhere before all versions of the dt are produced
saveRDS(imputedDataLong, paste0(output_dir, imputed_data_long))
# ----------------------------------------------



# ----------------------------------------------
# Calculate by health_zone 
# compute upper middle and lower for the imputed points for the error bars in the graphs
graphData <- imputedDataLong[, .(mean=mean(value), 
                                 lower=quantile(value, .05), 
                                 upper=quantile(value, .95)), by=c(id_vars, "variable", "indicator", "subpopulation")]

# identify where data was missing before
missMatrixMelt <- melt(dtOrig, id.vars=id_vars)
missMatrixMelt[, isMissing:=is.na(value)]

graphData$date <- as.Date(graphData$date)
missMatrixMelt$date <- as.Date(missMatrixMelt$date)

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
