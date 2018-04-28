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
      output_dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/"
      MI_results_indicators <- "Imputed Time Series for Indicators by Health Zone.pdf"
# ----------------------------------------------
    
    
# ----------------------------------------------    
# Set up for amelia():
  # convert column types to proper types (figure out why they're changing in the first place?)
  dt[, date := as.Date(date)]
  
  # remove V1 column
    dt <- dt[, -c("V1", "outlier" )]
    
  # fix the messed up values 
    dt <- dt[(health_zone=="Isangi" & date=="2016-11-01" & indicator=="ANC_1st"), value := 339]

    dt <- dt[(health_zone=="Maluku1" & date=="2016-06-01" & indicator=="SP_1st"), value := 529]

    dt <- dt[(health_zone=="Masimanimba" & date=="2015-10-01" & indicator=="ArtLum_used"), value := 0]
    
    #---> add this to prepped data, or save a new updated version of the full data. 
    
  # make values numeric
    dt[, value := as.numeric(value)]
  
  # dcast so it is wide for imputation
    dt <- dcast.data.table(dt, date+province+dps+health_zone ~ indicator, value.var='value')
    
  # vector of id variables and indicator variables
    id_vars <- c("province", "dps", "health_zone", "id", "date")
  
    indicators <- c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
      "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
      "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
      "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen", "ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
      "ITN_distAtPreschool", "VAR", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ArtLum_received", "ArtLum_used",
      "smearTest_completed", "smearTest_positive", "RDT_completed", "RDT_positive", "healthFacilities_total", "healthFacilities_numReported", "healthFacilitiesProduct")
 
    indicators <- indicators[!indicators %in% c("healthFacilities_numReported")]
                             
  # remove healthFacilities_numReported from the data.table
    dt <- dt[,-c("healthFacilities_numReported")]  
    
  # add an id column
    dt[, id:= .I]
    
# ----------------------------------------------   
      
      
# ---------------------------------------------- 
  # log transform the data to run amelia on it
    # save original data
      dtOrig <- copy(dt)
    
    # store which observations had zero so we can switch them back to zero at the end, after imputation
      zeroes <- dt[, lapply(.SD, function(x) {x==0}), .SDcols=indicators, by= c("province", "dps", "health_zone", "date", "id")] 
    
    # replace all 0s with really low values so log works 
      for(var in indicators) {
        # taking the 5th percentile for each column to replace the 0s with 
        pctle <- quantile(dt[get(var)!=0][[var]], .05, na.rm=TRUE)  
        # change/store these back in dt so that we can use that to run amelia() on
        dt[get(var)==0, (var):=pctle]
      }
    
    # log transform
      dtLog <- dt[, lapply(.SD, function(x) log(x)), .SDcols=indicators, by= c("province", "dps", "health_zone", "date", "id")]

     # make a constant to convince amelia to extrapolate
      dtLog[, random:=runif(nrow(dtLog))]
      
    # amelia:
      # ts variable: the date
      # cs variable: health zone - try with and without this and see what results are like
      # MI will ignore ID vars and include them as is in the output
      # lags/leads: indicators
      # intercs = FALSE by default, try with = TRUE
        amelia.results <- amelia(dtLog, m=5, cs= "health_zone", ts="date", lags= indicators, idvars= c("id", "province", "dps"))
   
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
        dtExp <- amelia_data[, lapply(.SD, function(x) exp(x)), .SDcols=indicators, by= c("province", "dps", "health_zone", "date", "id", "imputation_number")]

   # convert values back to 0s that were originally 0s
        for (var in indicators){
          dtExp <- dtExp[zeroes[get(var)== TRUE, id], (var):= 0]
        }
# ----------------------------------------------  
        
        
# ----------------------------------------------
  # Set up for graphing:
    # vectors of indicator names and intervention names to subset
      indicators <- c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                      "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
                      "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
                      "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen")
          
      indicator_names <- c(
        `newCasesMalariaMild` = "Incidence of Mild Malaria",
        `newCasesMalariaSevere` = "Incidence of Severe Malaria",
        `mildMalariaTreated` = "Cases of Mild Malaria Treated",
        `severeMalariaTreated` = "Cases of Severe Malaria Treated",
        `malariaDeaths` = "Number of Deaths from Malaria"
      )
          
      interventions <- c("ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
                      "ITN_distAtPreschool", "VAR", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ArtLum_received", "ArtLum_used",
                      "smearTest_completed", "smearTest_positive", "RDT_completed", "RDT_positive", "healthFacilities_total", "healthFacilitiesProduct")
  
      # make a vector of all health zones in dt to loop through 
        hz_vector <- dt[["health_zone"]]
        # remove duplicates to have just unique values:
          hz_vector <- unique(hz_vector)
  
  # Set up a data table for graphing:
      # reshape imputed data long
        imputedDataLong <- melt(dtExp, id.vars=c(id_vars, "imputation_number"))
      # split subpopulations out from indicators
        imputedDataLong <- imputedDataLong[, c("indicator", "subpopulation") := tstrsplit(variable, "_", fixed=TRUE)]
      
      # compute upper middle and lower for the imputed points for the error bars in the graphs
        graphData <- imputedDataLong[, .(mean=mean(value), 
                                    lower=quantile(value, .05), 
                                    upper=quantile(value, .95)), by=c(id_vars,"variable", 'indicator','subpopulation')]

      
      missMatrixMelt <- melt(dtOrig, id.vars=id_vars)
      missMatrixMelt[, isMissing:=is.na(value)]
      
      graphDataComplete <- merge(graphData, missMatrixMelt, by= c(id_vars, "variable"))
      graphDataComplete <- graphDataComplete[isMissing==F, lower:= NA ]
      graphDataComplete <- graphDataComplete[isMissing==F, upper:= NA ]

          
    pdf((paste0(output_dir, MI_results_indicators)), height=6, width=10)   
      graphData <- graphDataComplete[ variable %in% indicators ]    
      for (hz in hz_vector){
      
        g <- ggplot(graphData[health_zone==hz], aes(x=date, y=mean, color = subpopulation)) +
            
             geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper, y=NULL)) + geom_line() + theme_bw() + ggtitle(hz) + 
              
             facet_wrap(~indicator, scales="free_y", labeller = as_labeller(indicator_names))
      
        print (g)
    }
    dev.off()    
# ----------------------------------------------  


# ----------------------------------------------    
    # Aggregate Indicators Data

      aggData  <- imputedDataLong[, .(aggValue = sum(value)), by=c( "date", "dps", "indicator", "subpopulation", "imputation_number" )]

      aggGraphData <- aggData[, .(mean=mean(aggValue), 
                                     lower=quantile(aggValue, .05), 
                                     upper=quantile(aggValue, .95)), by=c("date", "dps", "indicator", "subpopulation")]
    
      aggGraphData <- aggGraphData[mean==lower, lower := NA]
      aggGraphData <- aggGraphData[mean==upper, upper := NA]
      
      indicatorInput <- aggGraphData[["indicator"]]
      indicatorInput <- unique(indicatorInput)
    
      pdf(paste0(output_dir, "Imputed Time Series for Indicators by DPS.pdf"), height=6, width=9)   

        for (i in indicatorInput){
          aggGraphTitle <- indicator_names[i]
          
          g <- ggplot(aggGraphData[indicator==i], aes(x=date, y=mean, color = subpopulation)) +
            
            geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper, y=NULL)) + geom_line() + theme_bw() + ggtitle(paste0("Aggregate Data by Provincial Health Division (DPS): ", aggGraphTitle)) + 
            
            labs(x= "Date", y="Value", color= "Subpopulation") + facet_wrap(~ dps, scales="free_y") + scale_color_manual(labels = c("Ages 5 and Older", "Pregnant Women", "Ages 4 and Under"), values = c("steelblue4", "palegreen4", "steelblue1"))
          
          print(g)
        }
      
      dev.off()
    
    # interventions data
      pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Imputed Time Series for Interventions by Health Zone.pdf", height=6, width=9)   

      
      
        