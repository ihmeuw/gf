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
    # J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/fullData_forMI_outliers_removed.csv
    # csv files were produced by prep_COD_Malaria_data_function.R
      dt <- fread(paste0(dir,"/","fullData_forMI_outliers_removed",".csv")) 
  
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
  
  # remove V1 column
    dt <- dt[, -c("V1", "outlier" )]
    
  # make values numeric
    dt[, value := as.numeric(value)]
  
  # dcast so it is wide for imputation
    dt <- dcast.data.table(dt, date+province+dps+health_zone ~ indicator, value.var='value')
# ----------------------------------------------   
    
  # # vector of id variables and indicator variables
id_vars <- c("province", "dps", "health_zone", "population", "date")
  # 
    indicators <- c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
      "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
      "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
      "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen", "ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
      "ITN_distAtPreschool", "VAR", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ArtLum_received", "ArtLum_used",
      "smearTest_completed", "smearTest_positive", "RDT_completed", "RDT_positive", "healthFacilities_total", "healthFacilities_numReported", "healthFacilitiesProduct")
  #   
  # 
  # # convert column types to proper types (figure out why they're changing in the first place?)
  #   # either of the following ways works:
  #     # lapply( indicators, function(x) {
  #     #   dt[, (x) := as.numeric(get(x))]
  #     # } )
  #     
  #     for (i in indicators){
  #       dt[, (i) := as.numeric(get(i))]
  #     }
# ----------------------------------------------   
      
      
# ---------------------------------------------- 
# two different ways to try to get rid of negative values in amelia output:
  # # 1:
  #   # bounds matrix to pass as a parameter
  #     # sets a lower bound for column 5
  #       bds <- matrix(c(5, 0, Inf ), nrow = 1, ncol = 3)
  #     # better way to do this?  to set a lower bound for every column that needs it...
  #       bound <- rbind(c(5, 0, Inf), c(6, 0, Inf ), c(7, 0, Inf ), c(8, 0, Inf ),
  #                    c(9, 0, Inf ), c(10, 0, Inf ), c(11, 0, Inf ), c(12, 0, Inf ), c(13, 0, Inf ),
  #                    c(14, 0, Inf ), c(15, 0, Inf ), c(16, 0, Inf ), c(17, 0, Inf ), c(18, 0, Inf ), c(19, 0, Inf ))
  #     
  #   # run amelia() to impute missing values- will change this to m=50 after learning how to work with it
  #     amelia.results <- amelia(dt, m=5, ts="date", cs= "health_zone", idvars= c( "province", "dps"), bounds = bound) 
  # 

      
      
# ---------------------------------------------- 
# 2: 
  # log transform the data to run amelia on it
    
    # store which observations had zero so we can switch them back to zero at the end
    zeroes <- dt[, indicators[!indicators %in% c("healthFacilitiesProduct", "healthFacilities_numReported", "healthFacilities_total")], with=FALSE]
    zeroes[, lapply(.SD, function(x) {x==0})] # double check this@!
    
    # replace all 0s with really low values so log works
      for(var in indicators[!indicators %in% c("healthFacilitiesProduct", "healthFacilities_numReported", "healthFacilities_total")]) {
        pctle <- quantile(dt[get(var)!=0][[var]], .05, na.rm=TRUE)  
        dt[get(var)==0, (var):=pctle]
      }
      
      dt <- dt[,-c("healthFacilitiesProduct", "healthFacilities_numReported", "healthFacilities_total")]
    
    # log transform
      cols <- c(5:43) 
      dt2 <- dt[, names(dt)[5:40] := lapply(.SD, function(x) log(x)), .SDcols=5:40]

     
    # amelia:
      # ts variable: the date
      # cs variable: health zone - try with and without this and see what results are like
      # MI will ignore ID vars and include them as is in the output
      # lags/leads:
      # intercs = FALSE by default, try with = TRUE
        amelia.results <- amelia(dt2, m=5, ts="date", cs= "health_zone", idvars= c( "province", "dps"))
  
        
# ---------------------------------------------- 
          
          
# ----------------------------------------------         
  # bind amelia results into one data.table, include a column with the imputation number in order to 
    # keep track of this information
    
    for( i in 1:5 ) {
      amelia.results$imputations[[i]]$imputation_number <- i
      if (i==1)  amelia_data <- data.table(amelia.results$imputations[[i]])
      if (i>1) amelia_data <- rbind(amelia_data, amelia.results$imputations[[i]])
    }
# ----------------------------------------------  
        
        
# ----------------------------------------------  
   # exponentiate the data set
        dtExp <- amelia_data[, names(dt)[5:40] := lapply(.SD, function(x) exp(x)), .SDcols=5:40]

   # convert 0.01 back to 0
        dt[dt==0.01] <- 0
# ----------------------------------------------  
        
        
# ----------------------------------------------
  # re-run graphs with imputed data, using the different imputations to create a spread? variance? for each imputed point         
        
        # subset original data to current hz
        tmporigdata <- dt[health_zone=='BAGATA']
        
        # subset imputed data to current hz
        imputeddata <- amelia_data[health_zone=='BAGATA']
          
        # subset imputed data to only observations that actually got imputed
          tmpmissmatrix <- amelia.results$missMatrix[dt$health_zone=='BAGATA',]
          imputeddata <- imputeddata[rep(tmpmissmatrix[,'severeMalariaTreated_pregnantWomen'],5)] # 5=number of imputations
        
        # compute upper middle and lower for the error bars
          imputeddata[, .(mean=mean(severeMalariaTreated_pregnantWomen), 
                          lower=quantile(severeMalariaTreated_pregnantWomen, .05), 
                          upper=quantile(severeMalariaTreated_pregnantWomen, .95)), by='date']
          
        # plot the points that didn't need imputation
        p <- ggplot(tmporigdata, aes(y=severeMalariaTreated_pregnantWomen, x=date)) + 
            geom_point() 
        
        
        # plots the imputations if any
        if (nrow(imputeddata)>0) p = p + geom_point(data=imputeddata, aes(y=mean)) + geom_errorbar(aes(ymin=lower, ymax=upper))
        