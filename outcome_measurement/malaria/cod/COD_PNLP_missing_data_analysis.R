# ----------------------------------------------
  # Audrey Batzel
  #
  # 3/16/18
    # COD PNLP data for 2014-2016; data quality analysis
    # outlier analysis
    # internal consistency checks
    # descriptive analysis of missing data
  # 5/22/18 and on -
    # PNLP data for 2010-2017 all provinces
    # build on analysis for earlier set of data/rerun for new data
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
      dt_long <- "PNLP_2010to2017_long.csv"
      fullData <- "PNLP_2010to2017_fullPrepped.csv"
      outliersRemoved <- "fullData_outliers_removed.csv"
      
    # input file:
      # J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/COD_PNLP_Data_Indicators_Long
      # csv files were produced by prep_COD_Malaria_data_function.R
      dt <- fread(paste0(dir_prepped, dt_long))
      
      dt_wide <- fread(paste0(dir_prepped, outliersRemoved))
      
      dtMI <- fread(paste0(dir, "/", "Full Data for MI.csv"))
      
      # upload excel doc of outliers as a data table to merge with full data set
        outliers <- as.data.table(read_excel(paste0(dir_prepped, "outliers to graph (temp).xlsx")))
        outliers[, date := as.Date(date)]
        
    # output files:
      # outputs to J:\Project\Evaluation\GF\outcome_measurement\cod\visualizations\PNLP_Data
      # Scatterplots for Variable comparisons
      # Scatterplots with Outliers 

    # Set up:
      dt[, date := as.Date(date)]
      dt[, value := as.numeric(value)]
      
      dt_wide[, date := as.Date(date)]

# ----------------------------------------------

      
# ----------------------------------------------
# create a vector of each set of column headings, one for id variables, one for the numeric variables
    dt_wide <- dt_wide[,-c("V1")]
      
    all_vars <- c(colnames(dt_wide))
    
    id_vars <- c("id", "province", "dps", "health_zone", "donor", "operational_support_partner", "population", "quarter", "month",
                 "year", "stringdate", "date") 
    
    
    numVars <- all_vars[!all_vars %in% id_vars]
    
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
                              correlation=(cor(dt_wide[!is.na(get(v1)) & !is.na(get(v2)), c(v1, v2), with=FALSE]))[2])
        
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

 # variable names for labelling axes 
     # ----------------------------------------------   
     variable_names <- c(
       `ArtLum_received` = "Artéméther - Lumefatrine Recieved",
       `ArtLum_used` = "Artéméther - Lumefatrine Used",
       `SP_1st` = "SP administered during 1st ANC Visit",
       `SP_2nd` = "SP administered during 2nd ANC Visit",
       `SP_3rd` = "SP administered during 3rd ANC Visit",
       `ASAQ_2to11mos` = "Artesunate Amodiaquine: 2 to 11 mos",
       `ASAQ_1to5yrs` = "Artesunate Amodiaquine: 1 to 5 yrs",
       `ASAQ_6to13yrs` = "Artesunate Amodiaquine: 6 to 13 yrs",
       `ASAQ_14yrsAndOlder` = "Artesunate Amodiaquine: 14 yrs and Older",
       `ASAQ_total` = "Artesunate Amodiaquine",
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
       `newCasesMalariaMild_under5` = "Confirmed Cases of Mild Malaria: Under 5",
       `newCasesMalariaMild_5andOlder` = "Confirmed Cases of Mild Malaria: 5 and Older",
       `newCasesMalariaMild_pregnantWomen`= "Confirmed Cases of Mild Malaria: Pregnant Women",
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
       `malariaDeaths_5andOlder` =     "Number of Deaths from Malaria: 5 and Older",
       `malariaDeaths_pregnantWomen` = "Number of Deaths from Malaria: Pregnant Women",
       `healthFacilities_total` = "Total Health Facilities",
       `healthFacilities_numReported` = "Number of Health Facilities Reporting"
       )
     # ---------------------------------------------- 

# ----------------------------------------------      
  # loop through pairs of variables in maxCorr for graphing
    #first round of graphs:
    #pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Scatterplots for Variable Comparisons.pdf", height=6, width=9)
    
  
    maxCorr <- maxCorr[isHigher==T,]
    #second round of graphs with some outliers removed, only graph where correlation is 0.55 or better:
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Scatterplots for Variable Comparisons - some outliers removed.pdf", height=6, width=9)
    i = 1
    for (v in maxCorr$variable1){

      v2 = maxCorr$variable2[i]

      #minmax <- range(c(maxCorr$variable1, maxCorr$variable2))
      maxAxis <- (max(cbind(dt_wide[[v]], dt_wide[[v2]]), na.rm=T))
      # maxAxis <- max(c(fullData[[v]], fullData[[v2]]), na.rm=T)
      # if (max(fullData[,get(v)])> max(fullData[,get(v2[i])])) maxAxis <- max(fullData[,get(v)])
      # if (max(fullData[,get(v)])< max(fullData[,get(v2[i])])) maxAxis <- max(fullData[,get(v2[i])])

      g <- ggplot(dt_wide, aes_string(v, v2)) + geom_point() + xlim(0, maxAxis) + ylim(0, maxAxis)
      #g <- g + xlab(variable_names[v]) + ylab(variable_names[v2]) 

      print(g)

      i = i + 1
    }

    dev.off()
# ----------------------------------------------   
    
    
# ---------------------------------------------- 
  # identify and color code suspected outliers
        # melt data in order to merge
          fullDataMelt <- melt(fullData, id.vars= id_vars,
                                         measure.vars = numVars,
                                         variable.name = "indicator", 
                                         value.name="value")
        
          dtOutliers <- merge(fullDataMelt, outliers, by= c("health_zone", "date", "indicator", "value"), all=T)
          
          # subset dtOutliers to necessary columns
          dtOutliers <- dtOutliers[, c("id", "health_zone", "date", "indicator", "value", "outlier", "province.x")]
          setnames(dtOutliers, "province.x", "province")
          
          dtOutliersWide1 <- dcast.data.table(dtOutliers, id+date+province+health_zone ~ indicator, value.var='value')
          dtOutliersWide2 <- dcast.data.table(dtOutliers, id+date+province+health_zone ~ indicator, value.var='outlier')
          oldNames = names(dtOutliersWide2)[!names(dtOutliersWide2) %in% c("id", "date", "province", "dps", "health_zone")]
          newNames = paste0(oldNames, '_outlier')
          setnames(dtOutliersWide2, oldNames, newNames)
          dtOutliersWide <- merge(dtOutliersWide1, dtOutliersWide2, by=c("id", "date", "province", "health_zone"))
          
  # remake scatterplots with outliers color-coded
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Scatterplots with Outliers.pdf", height=6, width=9)   
    
    
    # ASAQreceived_6to13yrs
    # severeMalariaTreated_under5
    # newCasesMalariaSevere_5andOlder
    
    i = 1
    
    for (x in maxCorr$variable1){
      x = maxCorr$variable1[i]
      y = maxCorr$variable2[i]
      
      maxAxis <- max(na.omit(cbind(dtOutliersWide[[x]], dtOutliersWide[[y]])))
      
      #maxAxis <- max(c(dtOutliersWide[[x]], dtOutliersWide[[y]]), na.rm=T)
      
      dtOutliersWide[get(paste0(x,"_outlier"))==1, tmp:='Outlier x']
      dtOutliersWide[get(paste0(y,"_outlier"))==1, tmp:='Outlier y']
      dtOutliersWide[get(paste0(y,"_outlier"))==1 & get(paste0(x,"_outlier"))==1, tmp:='Outlier x and y']
      dtOutliersWide[is.na(get(paste0(y,"_outlier"))) & is.na(get(paste0(x,"_outlier"))), tmp:='Not suspected outlier']
      
      g <- ggplot(dtOutliersWide, aes_string(x=x, y=y, color="tmp")) + 
           geom_point(size=3) + geom_abline(alpha=1/4) + theme_bw() +
           scale_color_manual(values=c("Outlier x"="blue","Outlier y"="red", "Outlier x and y"="purple", "Not suspected outlier"="gray")) +
           labs(color = NULL)  + xlim(0, maxAxis) + ylim(0, maxAxis) #+ xlab(variable_names[x]) + ylab(variable_names[y])
      
      print(g)
      dtOutliersWide$tmp <- NULL
      i = i + 1
    }
    
    dev.off()
# ---------------------------------------------- 
    

# internal consistency check - sum treatments vs sum cases treated   
# ----------------------------------------------

    # dtTreated <- fullData[,
    #                       .(casesTreated = sum(mildMalariaTreated_under5, mildMalariaTreated_5andOlder, mildMalariaTreated_pregnantWomen, 
    #                                             severeMalariaTreated_under5, severeMalariaTreated_5andOlder, severeMalariaTreated_pregnantWomen, na.rm=T)),
    #                       by = c('date', 'dps', 'health_zone')]
    # dtTreatments <- fullData[,
    #                         .(treatment = sum(ArtLum_used, SP_1st, ASAQ_2to11mos, ASAQ_1to5yrs, 
    #                                              ASAQ_6to13yrs, ASAQ_14yrsAndOlder, na.rm=T)),
    #                         by = c('date', 'dps', 'health_zone')]
    # 
    # dtTreatments2 <- fullData[,
    #                          .(treatmentSPall = sum(ArtLum_used, SP_1st, SP_2nd, SP_3rd, ASAQ_2to11mos, ASAQ_1to5yrs, 
    #                                               ASAQ_6to13yrs, ASAQ_14yrsAndOlder, na.rm=T)),
    #                          by = c('date', 'dps', 'health_zone')] 
    # 
    # 
    # dtTreated <- merge(dtTreated, dtTreatments, by= c('date', 'dps', 'health_zone'), all=T)
    # dtTreated <- merge(dtTreated, dtTreatments2, by= c('date', 'dps', 'health_zone'), all=T)
    # 
    # maxAxis <- max(na.omit(cbind(dtTreated$casesTreated, dtTreated$treatmentSP1)))
    # 
    # g <- ggplot(dtTreated, aes(x=casesTreated, y=treatmentSPall)) + 
    #   geom_point(size=3) + geom_abline(alpha=1/4) + theme_bw() + xlim(0, 30000) + ylim(0, 30000)
    # print(g)
    
# ----------------------------------------------  
    
    
# ----------------------------------------------  
# internal consistency checks
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Internal Consistency Checks.pdf", height=6, width=9)   
    
  # Indicators data 
  ggplot(dt_wide, aes(x=newCasesMalariaMild_under5, y=mildMalariaTreated_under5))+ geom_point()  + geom_abline(intercept = 0) + coord_fixed()
  ggplot(dt_wide, aes(x=newCasesMalariaMild_5andOlder, y=mildMalariaTreated_5andOlder)) + geom_point() + coord_fixed() + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(y=newCasesMalariaMild_pregnantWomen, x=mildMalariaTreated_pregnantWomen)) + geom_point() + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=newCasesMalariaSevere_under5, y=severeMalariaTreated_under5)) + geom_point()  + coord_fixed() + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=newCasesMalariaSevere_5andOlder, y=severeMalariaTreated_5andOlder)) + geom_point()  + coord_fixed() + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(y=newCasesMalariaSevere_pregnantWomen, x=severeMalariaTreated_pregnantWomen)) + geom_point() + geom_abline(intercept = 0)

  ggplot(dt_wide, aes(x=newCasesMalariaMild_under5, y=totalCasesAllDiseases_under5))+ geom_point()  + geom_abline(intercept = 0) + coord_fixed()
  ggplot(dt_wide, aes(x=newCasesMalariaMild_5andOlder, y=totalCasesAllDiseases_5andOlder))+ geom_point()  + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=newCasesMalariaMild_pregnantWomen, y=totalCasesAllDiseases_pregnantWomen))+ geom_point()  + geom_abline(intercept = 0)
  
  ggplot(dt_wide, aes(x=newCasesMalariaSevere_under5, y=totalHospAllDiseases_under5))+ geom_point()  + geom_abline(intercept = 0) 
  ggplot(dt_wide, aes(x=newCasesMalariaSevere_5andOlder, y=totalHospAllDiseases_5andOlder))+ geom_point()  + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=newCasesMalariaSevere_pregnantWomen, y=totalHospAllDiseases_pregnantWomen))+ geom_point()  + geom_abline(intercept = 0) 
  
  ggplot(dt_wide, aes(x=malariaDeaths_under5, y=totalDeathsAllDiseases_under5))+ geom_point()  + geom_abline(intercept = 0) + coord_fixed()
  ggplot(dt_wide, aes(x=malariaDeaths_5andOlder, y=totalDeathsAllDiseases_5andOlder))+ geom_point()  + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=malariaDeaths_pregnantWomen, y=totalDeathsAllDiseases_pregnantWomen))+ geom_point()  + geom_abline(intercept = 0)
  
  # ITN data
  ggplot(dt_wide, aes(x=ITN_distAtANC, y=ITN_received)) + geom_point()  + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=ITN_distAtPreschool, y=ITN_received)) + geom_point()  + geom_abline(intercept = 0)
  
  dataITN <- dt_wide[,
                      .(ITN_dist = sum(ITN_distAtANC, ITN_distAtPreschool)),
                      by = c('date', 'health_zone', 'ITN_received')]
  
  ggplot(dataITN, aes(x=ITN_received, y=ITN_dist)) + geom_point()  + geom_abline(intercept = 0)
  
  # smearTest data
  ggplot(dt_wide, aes(x=smearTest_positive, y=smearTest_completed)) + geom_point()  +  geom_abline(intercept = 0)
  
  # RDT data
  ggplot(dt_wide, aes(x=RDT_positive, y=RDT_completed)) + geom_point()  +  geom_abline(intercept = 0)
  
  # ArtLum data
  ggplot(dt_wide, aes(x=ArtLum_received, y=ArtLum_used)) + geom_point()  + coord_fixed() + geom_abline(intercept = 0)
  
  # ANC data
  ggplot(dt_wide, aes(x=ANC_1st, y= ANC_2nd)) + geom_point()  + coord_fixed() + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=ANC_2nd, y=ANC_3rd)) + geom_point()  + coord_fixed() + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=ANC_3rd, y=ANC_4th)) + geom_point()  + coord_fixed() + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=ANC_1st, y=SP_1st)) + geom_point()  + coord_fixed() + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=ANC_2nd, y=SP_2nd)) + geom_point()  + coord_fixed() + geom_abline(intercept = 0)
  ggplot(dt_wide, aes(x=ANC_3rd, y=SP_3rd)) + geom_point()  + coord_fixed() + geom_abline(intercept = 0)
  
  # health facilities data
  ggplot(dt_wide, aes(x=healthFacilities_numReported, y=healthFacilities_total)) + geom_point()  + coord_fixed() + geom_abline(intercept = 0)
  
  
  dev.off()
# ---------------------------------------------- 

  
# ----------------------------------------------
# histograms to detect outliers
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
                      by=c('date', 'province', 'dps', 'health_zone', 'indicator', 'subpopulation', 'value')]
    
# ----------------------------------------------


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
    
  # Make a histogram for counts of missing data by health zone (total) for each date
    
    dt_missing_byHZ <- dt_missing[health_zone=="lubunga",
                                 .(percentMissing = (mean(missing_value)*100)),
                                 by=c('date', 'health_zone')]
    
    
    g <- ggplot(data=dt_missing_byHZ, aes(x = date, y = percentMissing)) + geom_bar(stat="identity")
    g <- g + ggtitle("Missing Data for Health Zone Lubunga") + xlab("Health Zone") + ylab("Count of Missing Values")
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

    dt2 <- dt_missing[indicator == "ANC" & subpopulation == "1st",
                                    .(percent_missing = (mean(missing_value)*100)), 
                                    by=c('date')]
    
    g <- ggplot(data=dt2, aes(x = date, y = percent_missing)) + geom_bar(stat="identity")
    g <- g + ggtitle("Missing Data for 1st ANC visit") + ylim(0, 100) + labs(x= "Date (Month and Year)", y= "Percent Missing Data") 
    print(g)
    
    
    
    