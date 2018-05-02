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
    dt <- dt[dps=="TSHOPO"|dps=="KINSHASA"|dps=="KWILU",]
  # aggregate indicators to dps level
    dt <- dt[, .(dpsTotal= sum(mean)), by= c("province", "dps", "date", "indicator", "subpopulation")]


    dt2 <- dt[indicator=="ASAQ" | indicator=="newCasesMalariaMild", .(value= sum(dpsTotal)), by= c("province", "dps", "date", "indicator")]
    
    dt3 <- dt[indicator=="RDT" & subpopulation=="completed", .(value= dpsTotal), by = c("province", "dps", "date", "indicator")]

    dt4 <- dt[indicator=="ITN" & subpopulation=="received", .(value= dpsTotal), by = c("province", "dps", "date", "indicator")]
    
    dataToGraph <- rbind(dt2, dt3)
    dataToGraph[, date := as.Date(date)] 
    
    anotherGraph <- rbind(dataToGraph, dt4)
    anotherGraph[, date := as.Date(date)] 
    
    
    pdf(paste0(output_dir,terg_graphs), height=6, width=9)
      
      g <- ggplot(dataToGraph[dps=="TSHOPO"|dps=="KINSHASA",], aes(x=date, y=value, color = indicator)) +
        
        geom_point() + geom_line() + theme_bw() + facet_wrap(~ dps, scales="free_y") + theme(axis.title.x=element_blank(), legend.position="bottom", legend.direction="vertical") +
      
        ggtitle("Selected Indicators by Provincial Health Division (DPS)") +
        
        labs(y="Count", color= "Indicator") + expand_limits(y=0) +
      
        scale_color_manual(labels = c("ASAQ", "Confirmed Cases of Uncomplicated Malaria", "RDTs completed"), values = c("steelblue4", "orangered2", "limegreen"))
      
      print(g)
      
      g <- ggplot(dataToGraph[dps=="KWILU",], aes(x=date, y=value, color = indicator)) +
        
        geom_point() + geom_line() + theme_bw() + facet_wrap(~ dps, scales="free_y") + theme(axis.title.x=element_blank(), legend.position="bottom", legend.direction="vertical") +
        
        ggtitle("Selected Indicators by Provincial Health Division (DPS)") +
        
        labs(y="Count", color= "Indicator") + expand_limits(y=0) +
        
        scale_color_manual(labels = c("ASAQ", "Confirmed Cases of Uncomplicated Malaria", "RDTs completed"), values = c("steelblue4", "orangered2", "limegreen"))
      
      print(g)
      
      g <- ggplot(dataToGraph, aes(x=date, y=value, color = indicator)) +
        
        geom_point() + geom_line() + theme_bw() + facet_wrap(~ dps, scales="free_y") + theme(axis.title.x=element_blank(), legend.position="bottom", legend.direction="vertical") +
        
        ggtitle("Selected Indicators by Provincial Health Division (DPS)") +
        
        labs(y="Count", color= "Indicator") + expand_limits(y=0) +
        
        scale_color_manual(labels = c("ASAQ", "Confirmed Cases of Uncomplicated Malaria", "RDTs completed"), values = c("steelblue4", "orangered2", "limegreen"))
      
      print(g)
      
    dev.off()
    
    
  # graphs with ITN data
    g <- ggplot(anotherGraph, aes(x=date, y=value, color = indicator)) +
      
      geom_point() + geom_line() + theme_bw() + facet_wrap(~ dps, scales="free_y") + theme(axis.title.x=element_blank(), legend.position="bottom", legend.direction="vertical") +
      
      ggtitle("Selected Indicators by Provincial Health Division (DPS)") +
      
      labs(y="Count", color= "Indicator") + expand_limits(y=0) +
      
      scale_color_manual(labels = c("ASAQ", "ITNs received", "Confirmed Cases of Uncomplicated Malaria", "RDTs completed"), values = c("steelblue4", "lightblue", "orangered2", "limegreen"))
    
    print(g)
        
    