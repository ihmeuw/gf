# ----------------------------------------------
  # Audrey Batzel
  #
  # 3/16/18
  # COD PNLP data for 2014-2016; descriptive analysis of missing data
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
      dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data"
    
    # input file:
    # J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/COD_PNLP_Data_Indicators_Long
    # csv files were produced by prep_COD_Malaria_data_function.R
      dt <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators_Long.csv")) 
      dt_wide <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators_Wide.csv")) 
      dt2 <- fread(paste0(dir, "/", "COD_PNLP_Data_Interventions_Long.csv"))
      fullData <- fread(paste0(dir, "/", "Full Data for MI.csv"))
        
    # output files:

    # Set up:
      dt[, date := as.Date(date)]
      dt2[, date := as.Date(date)]
      dt_wide[, date := as.Date(date)]
      fullData[, date := as.Date(date)]
      dt[, value := as.numeric(value)]
      dt2[, value := as.numeric(value)]
# ----------------------------------------------

      
# ----------------------------------------------
# create a vector of each set of column headings, one for id variables, one for the numeric variables
  	idVars = names(fullData)[1:5]
    numVars = names(fullData)[6:44]
    
  # make sure each of the numeric variables is actually numeric
  	for (i in numVars){
  	  fullData[, (i) := as.numeric(get(i))]
  	}
    
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
                              correlation=(cor(fullData[!is.na(get(v1)) & !is.na(get(v2)), c(v1, v2), with=FALSE]))[2])
        
        # rbind() each iteration to the previous to create one data.table called fullCorrMatrix
        if (i==1) fullCorrMatrix = tmpData
        if (i>1) fullCorrMatrix = rbind(fullCorrMatrix, tmpData)
        
        i=i+1
      }
    }
  # find the maximum correlation for each variable and store that information in a data.table called maxCorr
    maxCorr <- fullCorrMatrix[,
                            maxCorr := .(max(correlation)),
                            by = c("variable1")]
    
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
     
# scatterplot the pairs of variables identified by correlation matrix analysis

     # variable names for labelling axes 
     # ----------------------------------------------   
     variable_names <- c(
       `ArtLum_received` = "Artéméther - Lumefatrine Recieved",
       `ArtLum_used` = "Artéméther - Lumefatrine Used",
       `SP_1st` = "SP administered during 1st ANC Visit",
       `SP_2nd` = "SP administered during 2nd ANC Visit",
       `SP_3rd` = "SP administered during 3rd ANC Visit",
       `ASAQ_2to11mos` = "Artesunate Amodiaquine (ACT): 2 to 11 mos",
       `ASAQ_1to5yrs` = "Artesunate Amodiaquine (ACT): 1 to 5 yrs",
       `ASAQ_6to13yrs` = "Artesunate Amodiaquine (ACT): 6 to 13 yrs",
       `ASAQ_14yrsAndOlder` = "Artesunate Amodiaquine (ACT): 14 yrs and Older",
       `ASAQ_total` = "Artesunate Amodiaquine (ACT)",
       `ITN_received` = "ITNs recieved",
       `ITN_distAtANC` = "ITNs distributed at ANC Visit",
       `ITN_distAtPreschool` = "ITNs distributed at Preschool",
       `ANC_1st` = "1st Antenatal Care Visit",
       `ANC_2nd` = "2nd Antenatal Care Visit",
       `ANC_3rd` = "3rd Antenatal Care Visit",
       `ANC_4th` = "4th Antenatal Care Visit",
       `RDT_completed` = "Rapid Diagnostic Tests: Completed",
       `RDT_positive` = "Rapid Diagnostic Tests: Positive",
       `smearTest_completed` = "Smear Tests: Completed",
       `smearTest_positive` = "Smear Tests: Positive",
       `VAR` = "Measles Vaccine",
       `newCasesMalariaMild_under5` = "Incidence of Mild Malaria: Under 5",
       `newCasesMalariaMild_5andOlder` = "Incidence of Mild Malaria: 5 and Older",
       `newCasesMalariaMild_pregnantWomen`= "Incidence of Mild Malaria: Pregnant Women",
       `newCasesMalariaSevere_under5` = "Incidence of Severe Malaria: Under 5",
       `newCasesMalariaSevere_5andOlder` = "Incidence of Severe Malaria: 5 and Older",
       `newCasesMalariaSevere_pregnantWomen` = "Incidence of Severe Malaria: Pregnant Women",
       `mildMalariaTreated_under5` = "Cases of Mild Malaria Treated: Under 5",
       `mildMalariaTreated_5andOlder` = "Cases of Mild Malaria Treated: 5 and Older",
       `mildMalariaTreated_pregnantWomen` = "Cases of Mild Malaria Treated: Pregnant Women",
       `severeMalariaTreated_under5` = "Cases of Severe Malaria Treated: Under 5",
       `severeMalariaTreated_5andOlder` = "Cases of Severe Malaria Treated: 5 and Older",
       `severeMalariaTreated_pregnantWomen` = "Cases of Severe Malaria Treated: Pregnant Women",
       `malariaDeaths_under5` = "Number of Deaths from Malaria: Under 5",
       `malariaDeaths_5andOlder` = "Number of Deaths from Malaria: 5 and Older",
       `malariaDeaths_pregnantWomen` = "Number of Deaths from Malaria: Pregnant Women"
     )
     # ---------------------------------------------- 

     
  # loop through pairs of variables in maxCorr for graphing 
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Scatterplots for Variable Comparisons.pdf", height=6, width=9)   
    
    i = 1
    for (v in maxCorr$variable1){
      
      v2 = maxCorr$variable2[i]
      
      #minmax <- range(c(maxCorr$variable1, maxCorr$variable2))
      
      maxAxis <- max(c(fullData[[v]], fullData[[v2]]), na.rm=T)
      # if (max(fullData[,get(v)])> max(fullData[,get(v2[i])])) maxAxis <- max(fullData[,get(v)])
      # if (max(fullData[,get(v)])< max(fullData[,get(v2[i])])) maxAxis <- max(fullData[,get(v2[i])])

      g <- ggplot(fullData, aes_string(v, v2)) 
      g <- g + geom_point() + geom_smooth(method='lm')
      g <- g + xlab(variable_names[v]) + ylab(variable_names[v2]) + xlim(0, maxAxis) + ylim(0, maxAxis)
      
      print(g)
      
      i = i + 1
    }
    dev.off()
      
  # identify and color code suspected outliers
      # upload excel doc of outliers as a data table
        outliers <- as.data.table(read_excel(paste0(dir, "/Outliers.xlsx")))
        outliers[, date := as.Date(date)]
        
        # melt data in order to merge
          fullDataMelt <- melt(fullData, id.vars= c("V1", "date", "province", "dps", "health_zone"),
                                         measure.vars = c(),
                                         variable.name = "indicator", 
                                         value.name="value")
        
          dtOutliers <- merge(fullDataMelt, outliers, by= c("health_zone", "date", "indicator"), all=T)
          
          dtOutliersWide1 <- dcast.data.table(dtOutliers, V1+date+province+dps+health_zone ~ indicator, value.var='value')
          dtOutliersWide2 <- dcast.data.table(dtOutliers, V1+date+province+dps+health_zone ~ indicator, value.var='outlier')
          oldNames = names(dtOutliersWide2)[!names(dtOutliersWide2) %in% c("V1", "date", "province", "dps", "health_zone")]
          newNames = paste0(oldNames, '_outlier')
          setnames(dtOutliersWide2, oldNames, newNames)
          dtOutliersWide <- merge(dtOutliersWide1, dtOutliersWide2, by=c("V1", "date", "province", "dps", "health_zone"))
          
  # remake scatterplots with outliers color-coded
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Scatterplots with Outliers.pdf", height=6, width=9)   
    
    i = 1
    for (x in maxCorr$variable1){
      
      y = maxCorr$variable2[i]

      maxAxis <- max(c(dtOutliersWide[[x]], dtOutliersWide[[y]]), na.rm=T)
      
      dtOutliersWide[get(paste0(x,"_outlier"))==1, tmp:='outlier x']
      dtOutliersWide[get(paste0(y,"_outlier"))==1, tmp:='outlier y']
      g <- ggplot(dtOutliersWide, aes_string(x=x, y=y, color="tmp")) + 
           geom_point() + 
           xlab(variable_names[x]) + ylab(variable_names[y]) + xlim(0, maxAxis) + ylim(0, maxAxis)
      
      print(g)
      
      i = i + 1
    }
    
    dev.off()
# ---------------------------------------------- 
  j <- 1
  
  pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Scatterplots for Variable Comparisons.pdf", height=6, width=9)   
  for ( i in (2:39) ) {
    for (j in (1:(i - 1)) ){
      g <- getPlot(ggpairs, i, j) 
      g <- g + geom_smooth()
      print(g)
    }
    i <- i + 1
  }
  dev.off()
  
  ggpairs <- ggpairs(fullData[,names(fullData)[!names(fullData) %in% idVars], with=FALSE])
  pairsIndicators <- ggpairs(fullData, columns= 6:20)
  
  # Indicators data 
  ggplot(fullData, aes(x=newCasesMalariaMild_under5, y=mildMalariaTreated_under5))+ geom_point() + geom_smooth() + coord_fixed()
  ggplot(fullData, aes(x=newCasesMalariaMild_5andOlder, y=mildMalariaTreated_5andOlder)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  ggplot(fullData, aes(x=newCasesMalariaMild_pregnantWomen, y=mildMalariaTreated_pregnantWomen)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  ggplot(fullData, aes(x=newCasesMalariaSevere_under5, y=severeMalariaTreated_under5)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  ggplot(fullData, aes(x=newCasesMalariaSevere_5andOlder, y=severeMalariaTreated_5andOlder)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  ggplot(fullData, aes(x=newCasesMalariaSevere_pregnantWomen, y=severeMalariaTreated_pregnantWomen)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)

  # ITN data
  ggplot(fullData, aes(x=ITN_received, y=ITN_distAtANC)) + geom_point() + geom_smooth() + geom_abline(intercept = 0)
  ggplot(fullData, aes(x=ITN_received, y=ITN_distAtPreschool)) + geom_point() + geom_smooth() + geom_abline(intercept = 0)
  
  dataITN <- fullData[,
                      .(ITN_dist = sum(ITN_distAtANC, ITN_distAtPreschool)),
                      by = c('date', 'health_zone', 'ITN_received')]
  
  ggplot(dataITN, aes(x=ITN_received, y=ITN_dist)) + geom_point() + geom_smooth() + geom_abline(intercept = 0)
  
  # smearTest data
  ggplot(fullData, aes(x=smearTest_completed, y=smearTest_positive)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  
  # RDT data
  ggplot(fullData, aes(x=RDT_completed, y=RDT_positive)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  
  # ArtLum data
  ggplot(fullData, aes(x=ArtLum_received, y=ArtLum_used)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  
  # ANC data
  ggplot(fullData, aes(x=ANC_1st, y= ANC_2nd)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  ggplot(fullData, aes(x=ANC_2nd, y=ANC_3rd)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  ggplot(fullData, aes(x=ANC_3rd, y=ANC_4th)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  ggplot(fullData, aes(x=ANC_1st, y=SP_1st)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  ggplot(fullData, aes(x=ANC_2nd, y=SP_2nd)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
  ggplot(fullData, aes(x=ANC_3rd, y=SP_3rd)) + geom_point() + geom_smooth() + coord_fixed() + geom_abline(intercept = 0)
# ---------------------------------------------- 

  
# ----------------------------------------------
  # indicators to loop through and make a histogram for
    indicators <- unique(dt$indicator)
    
    indicator_names <- c(
      `newCasesMalariaMild` = "Incidence of Mild Malaria",
      `newCasesMalariaSevere` = "Incidence of Severe Malaria",
      `mildMalariaTreated` = "Cases of Mild Malaria Treated",
      `severeMalariaTreated` = "Cases of Severe Malaria Treated",
      `malariaDeaths` = "Number of Deaths from Malaria"
    )
    
  # Make a histogram for each indicator by supopulation and dps
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Outlier Analysis Indicators.pdf", height=6, width=9)   
    for (i in indicators){
      # Subset of data
          dt_byIndicator <- dt[indicator == i,
                               indicator,
                               by= c('date', 'dps', 'health_zone', 'subpopulation', 'value')]
     
      # for geom_vline() - calculate the diff percentiles for the data
          p95 <- dt_byIndicator[, quantile(value, .95, na.rm=T), by=c('dps', 'subpopulation')]
          p99 <- dt_byIndicator[, quantile(value, .99, na.rm=T), by=c('dps', 'subpopulation')]
          p995 <- dt_byIndicator[, quantile(value, .995, na.rm=T), by=c('dps', 'subpopulation')]
          p999 <- dt_byIndicator[, quantile(value, .999, na.rm=T), by=c('dps', 'subpopulation')]
          # p95 = dt_byIndicator[, quantile(value, .95, na.rm=T),by=c('dps')]
  
      g <- ggplot(dt_byIndicator, aes(value)) + 
          geom_histogram(bins = 50, fill="cornflowerblue") + 
          ggtitle(paste0("Outlier Analysis for ", indicator_names[i])) + xlab("Value") + ylab("Count") +
          geom_vline(data=p95, aes(xintercept=V1, color="green"), linetype="dashed") + 
          geom_vline(data=p99, aes(xintercept=V1, color="yellow"), linetype="dashed") + 
          geom_vline(data=p995, aes(xintercept=V1, color="orange"), linetype="dashed") + 
          geom_vline(data=p999, aes(xintercept=V1, color="red"), linetype="dashed") + 
          scale_color_manual(name = "Percentile", labels = c("95%", "99%", "99.5%", "99.9%"), values = c("green", "yellow", "orange", "red")) +
          facet_grid(subpopulation~dps, scales='free') + 
          xlab("Value") + ylab("Count")
      print(g)
    }
    dev.off()  
# ----------------------------------------------
    
    
# ----------------------------------------------
  # Outlier Analysis - Interventions
  # indicators to loop through and make a histogram for
    interventions <- unique(dt2$intervention)
    
    intervention_names <- c(
      `ArtLum` = "Artéméther - Lumefatrine",
      `SP` = "SP administered during ANC",
      `ASAQ` = "Artesunate Amodiaquine (ACT)",
      `ITN` = "ITNs",
      `ANC` = "Antenatal Care Visits",
      `RDT` = "Rapid Diagnostic Tests",
      `smearTest` = "Smear Tests",
      `VAR` = "Measles Vaccine",
      `healthFacilities` = "Health Facilities Reporting",
      `reports` = "Number of Reports"
    )
  # Make a histogram for each intervention by subpop and dps
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Outlier Analysis Interventions.pdf", height=6, width=9)   
    for (i in interventions){
      # Subset of data
          dt_byIntervention <- dt2[intervention == i,
                               intervention,
                               by= c('date', 'dps', 'health_zone', 'intervention_spec', 'value')]
          
      # for geom_vline() - calculate the diff percentiles for the data
          p95 <- dt_byIntervention[, quantile(value, .95, na.rm=T), by=c('dps', 'intervention_spec')]
          p99 <- dt_byIntervention[, quantile(value, .99, na.rm=T), by=c('dps', 'intervention_spec')]
          p995 <- dt_byIntervention[, quantile(value, .995, na.rm=T), by=c('dps', 'intervention_spec')]
          p999 <- dt_byIntervention[, quantile(value, .999, na.rm=T), by=c('dps', 'intervention_spec')]
      
      g <- ggplot(dt_byIntervention, aes(value)) + 
        geom_histogram(bins = 75, fill="cornflowerblue") + 
        ggtitle(paste0("Outlier Analysis for ", intervention_names[i])) + xlab("Value") + ylab("Count") +
        geom_vline(data=p95, aes(xintercept=V1, color="blue"), linetype="dashed") + 
        geom_vline(data=p99, aes(xintercept=V1, color="yellow"), linetype="dashed") + 
        geom_vline(data=p995, aes(xintercept=V1, color="orange"), linetype="dashed") + 
        geom_vline(data=p999, aes(xintercept=V1, color="red"), linetype="dashed") + 
        scale_color_manual(name = "Percentile", labels = c("95%", "99%", "99.5%", "99.9%"), values = c("green", "yellow", "orange", "red")) +
        facet_grid(intervention_spec~dps, scales='free') + 
        xlab("Value") + ylab("Count")
      print(g)
    }
    dev.off()  
# ---------------------------------------------- 
    
      
# ----------------------------------------------
  # Subset of health zone and data and indicators
    
    dt_missing <- dt[, 
                     .(missing_value = ifelse(is.na(value), 1, 0)), 
                      by=c('date', 'dps', 'health_zone', 'indicator', 'subpopulation', 'value')]
    
# ----------------------------------------------
      + facet_wrap(~ dps, scales="free_y")

# ----------------------------------------------
  # Make a histogram for counts of missing data by each month (total)
    
    dt_missing_byDate <- dt_missing[,
                                 .(sumMissing = sum(missing_value)),
                                 by=c('date', 'indicator', 'subpopulation')]
    
    g <- ggplot(data=dt_missing_byDate, aes(x = date, y = sumMissing)) + geom_bar(stat="identity")
    g <- g + ggtitle("Missing Data by Date") + xlab("Date (Month and Year)") + ylab("Count of Missing Values")
    print(g)
    
    g_reorder <- ggplot(dt_missing_byDate, aes(x = reorder(date, sumMissing), y = sumMissing)) + geom_bar(stat="identity")
    g_reorder <- g_reorder + ggtitle("Missing Data by Date") + xlab("Date (Month and Year)") + ylab("Count of Missing Values")
    print(g_reorder)
    
  # Make a histogram for counts of missing data by health zone (total)
    
    dt_missing_byHZ <- dt_missing[,
                                 .(sumMissing = sum(missing_value)),
                                 by=c('health_zone')]
    
    
    g <- ggplot(data=dt_missing_byHZ, aes(x = health_zone, y = sumMissing)) + geom_bar(stat="identity")
    g <- g + ggtitle("Missing Data by Health Zone") + xlab("Health Zone)") + ylab("Count of Missing Values")
    print(g)
    
    g_reorder <- ggplot(dt_missing_byHZ, aes(x = reorder(health_zone, sumMissing), y = sumMissing)) + geom_bar(stat="identity")
    g_reorder <- g_reorder + ggtitle("Missing Data by Health Zone") + xlab("Health Zone") + ylab("Count of Missing Values")
    print(g_reorder)
    
# ----------------------------------------------


# ----------------------------------------------
  # Make a histogram for counts of missing data by each month by indicator
    dt_percentMissing <- dt_missing[, 
                                    .(percent_missing = (mean(missing_value)*100)), 
                                    by=c('date', 'indicator', 'subpopulation')]

    g <- ggplot(data=dt_percentMissing, aes(x = date, y = percent_missing, color = subpopulation)) + geom_bar(stat="identity") + facet_grid(indicator ~ subpopulation)
    g <- g + ggtitle("Missing Data by Indicator and Subpopulation") + ylim(0, 100) + labs(x= "Date", y= "Percent Missing Data") 
    print(g)
# ----------------------------------------------
  # Make a histogram for counts of missing data by health zone by indicator  

    dt2 <- dt_missing[indicator == "newCasesMalariaSevere",
                                    .(percent_missing = (mean(missing_value)*100)), 
                                    by=c('date')]
    
    g <- ggplot(data=dt2, aes(x = date, y = percent_missing)) + geom_bar(stat="identity")
    g <- g + ggtitle("Missing Data for New Cases of Severe Malaria") + ylim(0, 100) + labs(x= "Date", y= "Percent Missing Data") 
    print(g)
    
    
    
    