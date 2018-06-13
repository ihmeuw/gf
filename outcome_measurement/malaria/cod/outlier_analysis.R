# ----------------------------------------------
# Audrey Batzel
#
# 6/4/18
# PNLP data for 2010-2017 all provinces
# make full set of histograms and scatterplots by dps to assess outliers
  setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
  # Set up R / install packages
  rm(list=ls())
  library(data.table)
  library(reshape2)
  library(stringr)
  library(RColorBrewer)
  library(ggplot2)
  library(GGally)
  library(lubridate)
  library(readxl)
  library(stats)
  library(rlang)
  library(zoo)
  library(tidyr)
  library(gridExtra)
# --------------------


# ----------------------------------------------
# Overview - Files and Directories

  # data directory
    dir_prepped <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/"
    dir_vis <- "J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_data/"
    fullData <- "fullData_dps_standardized.csv"
  
  # input files:
    dt <- fread(paste0(dir_prepped, fullData))

  # output files:
    histograms_output <- "histograms_byIndicatorDPS.pdf"
    scatterplots_output <- "scatterplots_correlatedVars_byDPS.pdf"
  
  # Set up:
  dt[, date := as.Date(date)]
# ----------------------------------------------
  
  
# ----------------------------------------------
# variable vectors to use later
  all_vars <- c(colnames(dt))
  
  id_vars <- c("V1", "province", "dps", "dps_in_original_data", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month",
               "year", "stringdate", "date", "natl", "natl_name", "province11", "province11_name", "province26", "province26_name",                                
               "dps_name_2015", "dps_name_2014", "dps_name_2013", "dps_name_2012", "dps_name_2010to2011" ) 
  
  
  num_vars <- all_vars[!all_vars %in% id_vars]

# melt dt so indicators are long
  dt_long <- melt(dt, id.vars= id_vars, measure_vars=num_vars, variable.name="indicator", value.name="value")
  dt_long[, value := as.numeric(value)]
    # split out subpopulations
      dt_long[,c("indicator", "subpopulation") := tstrsplit(indicator, "_", fixed=TRUE)]
      dt_long[is.na(subpopulation), subpopulation:= "none"]
# ----------------------------------------------


# ----------------------------------------------
# Make histograms by dps to detect outliers:
# indicators to loop through and make a histogram for
  indicators <- unique(dt_long$indicator)
  
  # indicator_names <- c(
  #   `newCasesMalariaMild` = "Incidence of Mild Malaria",
  #   `newCasesMalariaSevere` = "Incidence of Severe Malaria",
  #   `mildMalariaTreated` = "Cases of Mild Malaria Treated",
  #   `severeMalariaTreated` = "Cases of Severe Malaria Treated",
  #   `malariaDeaths` = "Number of Deaths from Malaria"
  # )
  
# Make a histogram for each indicator by supopulation and dps
  makeGraph <- function(dt, i){
    # for geom_vline() - calculate the diff percentiles for the data
    p95 <- dt[, quantile(value, .95, na.rm=T), by=c('dps', 'subpopulation')]
    p99 <- dt[, quantile(value, .99, na.rm=T), by=c('dps', 'subpopulation')]
    p995 <- dt[, quantile(value, .995, na.rm=T), by=c('dps', 'subpopulation')]
    p999 <- dt[, quantile(value, .999, na.rm=T), by=c('dps', 'subpopulation')]
    # p95 = dt[, quantile(value, .95, na.rm=T),by=c('dps')]
    
    g <- ggplot(dt, aes(value)) + 
      geom_histogram(bins = 50, fill="cornflowerblue") + 
      ggtitle(paste0("Outlier Analysis for ", i)) + xlab("Value") + ylab("Count") +
      geom_vline(data=p95, aes(xintercept=V1, color="green"), linetype="dashed") + 
      geom_vline(data=p99, aes(xintercept=V1, color="yellow"), linetype="dashed") + 
      geom_vline(data=p995, aes(xintercept=V1, color="orange"), linetype="dashed") + 
      geom_vline(data=p999, aes(xintercept=V1, color="red"), linetype="dashed") + 
      scale_color_manual(name = "Percentile", labels = c("95%", "99%", "99.5%", "99.9%"), values = c("green", "yellow", "orange", "red")) +
      facet_grid(subpopulation~dps, scales='free') + 
      xlab("Value") + ylab("Count")
    return(g)
  }
  
  dps_names <- unique(dt_long$dps)
  dps_names <- dps_names[!dps_names %in% c("0")]
  dps1 <- dps_names[1:4]
  dps2 <- dps_names[5:8]
  dps3 <- dps_names[9:12]
  dps4 <- dps_names[13:16]
  dps5 <- dps_names[17:20]
  dps6 <- dps_names[21:24]
  dps7 <- dps_names[25:26]
  
  pdf(paste0(dir_vis, histograms_output), height=6, width=9)   
  for (i in indicators){
  # Subset of data
    dt_byIndicator <- dt_long[indicator == i,
                         indicator,
                         by= c('date', 'dps', 'health_zone', 'subpopulation', 'value')]
    
    
    dt1 <- dt_byIndicator[dps %in% dps1,]
    print(makeGraph(dt1, i))
    dt2 <- dt_byIndicator[dps %in% dps2,]
    print(makeGraph(dt2, i))
    dt3 <- dt_byIndicator[dps %in% dps3,]
    print(makeGraph(dt3, i))
    dt4 <- dt_byIndicator[dps %in% dps4,]
    print(makeGraph(dt4, i))
    dt5 <- dt_byIndicator[dps %in% dps5,]
    print(makeGraph(dt5, i))
    dt6 <- dt_byIndicator[dps %in% dps6,]
    print(makeGraph(dt6, i))
    dt7 <- dt_byIndicator[dps %in% dps7,]
    print(makeGraph(dt7, i))
  }
  dev.off()  
# ----------------------------------------------
  
  
# ----------------------------------------------
# Scatterplots to detect outliers:
  # use a correlation matrix to identify good pairs of variables to scatterplot 
  # loop through each pair of variables, calculate the correlation
  i=1
  for(v1 in numVars) { 
    for(v2 in numVars ) {
      # skip cases where v1 and v2 are the same variable
      if (v1==v2) next 
      # to track progess:
      # print(v1)
      # print(v2)
      
      # store the correlation information in a data.table, extracting the second value from the correlation matrix, which is 
      # the correlation between the two variables
      tmpData <- data.table(variable1=v1, 
                            variable2=v2,
                            correlation=(cor(dt[!is.na(get(v1)) & !is.na(get(v2)), c(v1, v2), with=FALSE]))[2])
      
      # rbind() each iteration to the previous to create one data.table called fullCorrMatrix
      if (i==1) fullCorrMatrix = tmpData
      if (i>1) fullCorrMatrix = rbind(fullCorrMatrix, tmpData)
      
      i=i+1
    }
  }
  # find the maximum correlation for each variable and store that information in a data.table called maxCorr
  maxCorr <- fullCorrMatrix[,
                            .(variable2, correlation, maxCorr = (max(correlation, na.rm=T))),
                            by = variable1 ]
  
  # make it so maxCorr contains only the pairs of variables with the maximum correlation for each variable 
  maxCorr <- maxCorr[maxCorr == correlation]
  
  # remove duplicates of pairs of variables
  i = 1
  
  for(i in (1:nrow(maxCorr))) { 
    v1 = maxCorr[i]$variable1
    v2 = maxCorr[i]$variable2
    if (nrow(maxCorr[variable1==v2 & variable2==v1])>0) maxCorr <- maxCorr[-i]
    i <- i + 1
  }
  
  #add a column to see see if maxCorr > 0.75
  maxCorr[maxCorr>0.55, isHigher :=T]  
  
  maxCorr <- maxCorr[isHigher==T,]
  
  
  
  # loop through pairs of variables in maxCorr for graphing
    # graphs by DPS:


  pdf(paste0(dir_vis, scatterplots_output), height=6, width=9) 
  for (j in dps_names){
    dtDPS <- dt[dps==j, ]
  
    i = 1
    for (v in maxCorr$variable1){
      
      v2 = maxCorr$variable2[i]
      
      maxAxis <- (max(cbind(dtDPS[[v]], dtDPS[[v2]]), na.rm=T))
  
      g <- ggplot(dtDPS, aes_string(v, v2)) + geom_point() + xlim(0, maxAxis) + ylim(0, maxAxis)
      g <- g + ggtitle(paste0("Variable Comparisons for ", dtDPS$dps))
      
      print(g)
      
      i = i + 1
    }
  }
  dev.off()
