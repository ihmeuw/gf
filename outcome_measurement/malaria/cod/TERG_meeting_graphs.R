# ----------------------------------------------
  # Audrey Batzel
  #
  # 05/01/18
  # DRC Outcome Measurement / Supply Chain Data
  
  # Make a graph for Kinshasa and Tshopo DPS for May 2018 TERG 
  # with aggregate confirmed cases of severe malaria across all subpopulations
  # and aggregated ASAQ across all ages, and aggregate RDT completed tests (to show an output)
  
  # facet wrap by dps, with three lines on each graph, one for each indicator
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
    dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/"
  
  # import graphDataComplete
    dt <- as.data.table(read.csv(paste0(dir, "Imputed Data.csv")))
    # NOTE - this data is the average of 5 imputations,
    # this will be changed to 50 when all of the data is
    # included and for analysis of current data
    
  # output files:
    output_dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/"
    terg_graphs <- "Graphs for TERG slides.pdf"
# ----------------------------------------------


# ----------------------------------------------
  # subset to just Tshopo and Kinshasa
    dt <- dt[dps=="TSHOPO"|dps=="KINSHASA",]
  # aggregate indicators to dps level
    dt <- dt[, .(dpsTotal= sum(mean)), by= c("province", "dps", "date", "indicator", "subpopulation")]


    dt2 <- dt[indicator=="ASAQ", .(value= sum(dpsTotal)), by= c("province", "dps", "date", "indicator")]
    
    dt3 <- dt[indicator=="RDT" & subpopulation=="completed", .(value= sum(dpsTotal)), by = c("province", "dps", "date", "indicator")]

    dt4 <- dt[indicator=="ITN" & (subpopulation=="distAtANC"|subpopulation=="distAtPreschool"), .(value= sum(dpsTotal)), by = c("province", "dps", "date", "indicator")]
    
    dt5 <- dt[indicator=="SP" & subpopulation=="1st", .(value= sum(dpsTotal)), by = c("province", "dps", "date", "indicator")]
    
    dt6 <- dt[indicator=="smearTest" & subpopulation=="completed", .(value= sum(dpsTotal)), by = c("province", "dps", "date", "indicator")]
    
    
    dataToGraph <- rbind(dt2, dt3)

    dataToGraph <- rbind(dataToGraph, dt4)
    dataToGraph <- rbind(dataToGraph, dt5)
    dataToGraph <- rbind(dataToGraph, dt6)
    
    dataToGraph[, date := as.Date(date)] 
    
    
    pdf(paste0(output_dir,terg_graphs), height=6, width=9)
      
      g <- ggplot(dataToGraph[dps=="TSHOPO"|dps=="KINSHASA",], aes(x=date, y=value, color = indicator)) +
        
        geom_point() + geom_line() + theme_bw() + facet_wrap(~ dps, scales="free_y") + theme(axis.title.x=element_blank(), legend.position="bottom", legend.direction="vertical") +
      
        ggtitle("Selected Indicators by Provincial Health Division (DPS)") +
        
        labs(y="Count", color= "Indicator") + expand_limits(y=0) +
      
        scale_color_manual(labels = c("ASAQ (Artesunate Amodiaquine)", "ITNs Distributed", "RDTs Completed", "Smear Tests Completed","SP Distributed at 1st Antenatal Care Visit"), values = c("steelblue4", "orangered2", "palegreen2", "darkolivegreen", "skyblue" ))
      
      print(g)
      
    dev.off()
      
    #   g <- ggplot(dataToGraph[dps=="KWILU",], aes(x=date, y=value, color = indicator)) +
    #     
    #     geom_point() + geom_line() + theme_bw() + facet_wrap(~ dps, scales="free_y") + theme(axis.title.x=element_blank(), legend.position="bottom", legend.direction="vertical") +
    #     
    #     ggtitle("Selected Indicators by Provincial Health Division (DPS)") +
    #     
    #     labs(y="Count", color= "Indicator") + expand_limits(y=0) +
    #     
    #     scale_color_manual(labels = c("ASAQ", "Confirmed Cases of Uncomplicated Malaria", "RDTs completed"), values = c("steelblue4", "orangered2", "limegreen"))
    #   
    #   print(g)
    #   
    #   g <- ggplot(dataToGraph, aes(x=date, y=value, color = indicator)) +
    #     
    #     geom_point() + geom_line() + theme_bw() + facet_wrap(~ dps, scales="free_y") + theme(axis.title.x=element_blank(), legend.position="bottom", legend.direction="vertical") +
    #     
    #     ggtitle("Selected Indicators by Provincial Health Division (DPS)") +
    #     
    #     labs(y="Count", color= "Indicator") + expand_limits(y=0) +
    #     
    #     scale_color_manual(labels = c("ASAQ", "Confirmed Cases of Uncomplicated Malaria", "RDTs completed"), values = c("steelblue4", "orangered2", "limegreen"))
    #   
    #   print(g)
    #   
    # dev.off()
    
    
  # # graphs with ITN data
  #   g <- ggplot(anotherGraph, aes(x=date, y=value, color = indicator)) +
  #     
  #     geom_point() + geom_line() + theme_bw() + facet_wrap(~ dps, scales="free_y") + theme(axis.title.x=element_blank(), legend.position="bottom", legend.direction="vertical") +
  #     
  #     ggtitle("Selected Indicators by Provincial Health Division (DPS)") +
  #     
  #     labs(y="Count", color= "Indicator") + expand_limits(y=0) +
  #     
  #     scale_color_manual(labels = c("ASAQ", "ITNs received", "Confirmed Cases of Uncomplicated Malaria", "RDTs completed"), values = c("steelblue4", "lightblue", "orangered2", "limegreen"))
  #   
  #   print(g)
      
    
  # specific health zone graphs - Kalamu 1 
    dtHZ <- dt[health_zone=="Kalamu 1",]
    dtHZ <- dtHZ[indicator=="newCasesMalariaMild" | indicator=="newCasesMalariaSevere" | indicator=="ASAQ" | indicator=="smearTest"| indicator=="RDT",]
    dtHZ <- dtHZ[isMissing==F,]   
    dtHZ[, date := as.Date(date)] 
    
    dtHZ$year <- year(dtHZ$date)
    
    dt1 <- dtHZ[indicator=="ASAQ", .(value= sum(value)), by= c("health_zone", "province", "dps", "date", "year", "indicator")]
    
    dt2 <- dtHZ[indicator=="newCasesMalariaMild" & (subpopulation== "under5" | subpopulation== "5andOlder"), .(value= sum(value)), by= c("health_zone", "province", "dps", "date", "year", "indicator")]
    
    dt3 <- dtHZ[indicator=="newCasesMalariaSevere" & (subpopulation== "under5" | subpopulation== "5andOlder"), .(value= sum(value)), by= c("health_zone", "province", "dps", "date", "year", "indicator")]
    
    dt4 <- dtHZ[indicator=="smearTest" & subpopulation== "completed", .(value= sum(value)), by= c("health_zone", "province", "dps", "date", "year", "indicator")]
    
    dt5 <- dtHZ[indicator=="RDT" & subpopulation== "completed", .(value= sum(value)), by= c("health_zone", "province", "dps", "date", "year", "indicator")]
    
    
    dataToGraphHZ <- rbind(dt1, dt2)
    
    dataToGraphHZ <- rbind(dataToGraphHZ, dt3)
    dataToGraphHZ <- rbind(dataToGraphHZ, dt4)
    dataToGraphHZ <- rbind(dataToGraphHZ, dt5)
    
    dataToGraphHZ[, date := as.Date(date)] 
    
    
    pdf(paste0(output_dir,"healthZone_terg_graphs.pdf"), height=6, width=9)
    g <- ggplot(dataToGraphHZ, aes(x=date, y=value, color = indicator)) +
      
      geom_point() + geom_line() + theme_bw() +
      
      ggtitle("Selected Indicators for Kalamu 1") +
      
      labs(y="Count", color= "Indicator") + expand_limits(y=0) +
      
      scale_color_manual(labels = c("ASAQ all ages", "Confirmed Cases of Uncomplicated Malaria", "Cases of Severe Malaria", "RDTs completed", "Smear Tests completed"), values = c("palegreen2", "orangered1", "orangered4", "steelblue4", "steelblue1"))
    
    print(g)
    
    dev.off()
    
    
    # specific health zone graphs - Yahuma
    dtHZ <- dt[health_zone=="Yahuma",]
    #dtHZ <- dtHZ[indicator=="newCasesMalariaMild" | indicator=="newCasesMalariaSevere" | indicator=="ASAQ" | indicator=="smearTest"| indicator=="RDT",]
    dtHZ <- dtHZ[isMissing==F,]   
    dtHZ[, date := as.Date(date)] 
    dtHZ$year <- year(dtHZ$date)
    
    #dt1 <- dtHZ[indicator=="ASAQ", .(variable="totalASAQ", value= sum(value)), by= c("health_zone", "province", "dps", "date", "year", "indicator")]
    
    dt2 <- dtHZ[indicator=="malariaDeaths" & (subpopulation== "under5" | subpopulation== "5andOlder"), .(variable="totalMalariaDeaths", value= sum(value)), by= c("health_zone", "province", "dps", "date", "year", "indicator")]
    
    dt3 <- dtHZ[indicator=="ITN" & (subpopulation== "distAtANC" | subpopulation== "distAtPreschool"), .(variable="ITN_distributed", value= sum(value)), by= c("health_zone", "province", "dps", "date", "year", "indicator")]
    
    dt4 <- dtHZ[indicator=="ITN" & (subpopulation== "received"), .(variable="ITN_recieved", value= sum(value)), by= c("health_zone", "province", "dps", "date", "year", "indicator")]
    
    
    dataToGraphHZ <- rbind(dt2, dt3)
    
    #dataToGraphHZ <- rbind(dataToGraphHZ, dt3)
    dataToGraphHZ <- rbind(dataToGraphHZ, dt4)

    
    dataToGraphHZ[, date := as.Date(date)] 
    
    
    pdf(paste0(output_dir,"Yahuma.pdf"), height=6, width=9)
    g <- ggplot(dataToGraphHZ[year>2014], aes(x=date, y=value, color = variable)) +
      
      geom_point() + geom_line() + theme_bw() +
      
      ggtitle("Selected Indicators for Yahuma") +
      
      labs(y="Count", color= "Indicator") + expand_limits(y=0) +
      
      scale_color_manual(labels = c("ITNs distributed", "ITNs received", "Deaths due to Malaria"), values = c("steelblue4", "steelblue1", "orangered1"))
    
    print(g)
    
    dev.off()
    