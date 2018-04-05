# ----------------------------------------------
  # Audrey Batzel
  #
  # 3/16/18
  # COD PNLP data for 2014-2016; descriptive analysis
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
# --------------------  


# ----------------------------------------------
  # Overview - Files and Directories
  
  # data directory
    dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data"
  
  # input file:
  # J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/COD_PNLP_Data_Indicators_Long
  # csv files were produced by prep_COD_Malaria_data_function.R
    dt <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators",".csv")) 
  
  # output files:
    # exports all exports all health zone graphs to to a pdf document here:
    # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs all Indicators.pdf
    
    # exports aggregate graphs to a pdf document here: 
    # J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Time Series Graphs Aggregate Indicators Data.pdf
# ----------------------------------------------
    
    
# ----------------------------------------------    
  # Set up for amelia():
    # convert column types to proper types (figure out why they're changing in the first place?)
    dt[, date := as.Date(date)]
    dt[, population := as.numeric(population)]
    
    # subset the variables we want to include in the amelia() mulitple imputation
      dt <- dt[, c("province", "dps", "health_zone", "date", "newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen", 
             "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen", 
             "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen", 
             "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen" )]
      
    # vector of id variables and indicator variables
      id_vars <- c("province", "dps", "health_zone", "population", "date")
    
      indicators <- c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen", 
        "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen", 
        "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen", 
        "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen" )
    
    # convert column types to proper types (figure out why they're changing in the first place?)
      # either of the following ways works:
        # lapply( indicators, function(x) {
        #   dt[, (x) := as.numeric(get(x))]
        # } )
        
        for (i in indicators){
          dt[, (i) := as.numeric(get(i))]
        }
      
# two different ways to try to get rid of negative values in amelia output:
  # 1:
    # bounds matrix to pass as a parameter
      # sets a lower bound for column 5
        bds <- matrix(c(5, 0, Inf ), nrow = 1, ncol = 3)
      # better way to do this?  to set a lower bound for every column that needs it...
        bound <- rbind(c(5, 0, Inf), c(6, 0, Inf ), c(7, 0, Inf ), c(8, 0, Inf ),
                     c(9, 0, Inf ), c(10, 0, Inf ), c(11, 0, Inf ), c(12, 0, Inf ), c(13, 0, Inf ),
                     c(14, 0, Inf ), c(15, 0, Inf ), c(16, 0, Inf ), c(17, 0, Inf ), c(18, 0, Inf ), c(19, 0, Inf ))
      
    # run amelia() to impute missing values- will change this to m=50 after learning how to work with it
      amelia.results <- amelia(dt, m=5, ts="date", cs= "health_zone", idvars= c( "province", "dps"), bounds = bound) 
      
  # 2: 
    # log transform the data to run 
      cols <- c(5:19) 
      dt[, names(dt)[5:19] := lapply(.SD, function(x) log(x)), .SDcols=5:19]
      
      amelia.resultsLog <- amelia(dt, m=5, ts="date", cs= "health_zone", idvars= c( "province", "dps")) 

      
    # bind amelia results into one data.table, include a column with the imputation number in order to 
      # keep track of this information
      
      for( i in 1:5 ) {
        amelia.results$imputations[[i]]$imputation_number <- i
        if (i==1)  amelia_data <- data.table(amelia.results$imputations[[i]])
        if (i>1) amelia_data <- rbind(amelia_data, amelia.results$imputations[[i]])
      }
    
      
      