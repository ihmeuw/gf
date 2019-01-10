# ----------------------------------------------
  # Audrey Batzel
  #
  # 5/4/18
  # COD PNLP data for 2014-2016; examining health zones using percent change for each indicator over time
# ----------------------------------------------


# --------------------
  # Set up R / install packages
  rm(list=ls())
  library(tidyverse)
  library(data.table)
  library(stringr)
  library(reshape2)
  library(ggplot2)
  library(lubridate)
  library(readxl)
  library(stats)
  library(Rcpp)
  library(Amelia)
  library(dplyr)
# --------------------  


# ----------------------------------------------
  # Overview - Files and Directories
  
  # data directory
  dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/"
  
  # import data.table with calculated percent change
  percent_change <- "Percent Change for each Indicator by Health Zone.csv"
  dt <- as.data.table(read.csv(paste0(dir, percent_change)))
  fullData <- as.data.table(read.csv(paste0(dir, "Imputed Data.csv")))
  
  # output files:
  output_graph <- "J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/"
  output_data <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/"
  hzGraphs <- "Selected Indicators by Health Zone over time.pdf"
  hzGraphs2 <- "Health zone-level graphs for TERG slides.pdf"
  
  # variable names
  interventions <- c("ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
                     "ITN_distAtPreschool", "VAR", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ArtLum_received", "ArtLum_used",
                     "smearTest_completed", "smearTest_positive", "RDT_completed", "RDT_positive", "healthFacilities_total")
  indicators <- c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                  "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
                  "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
                  "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen")
  measured_vars <- c(indicators, interventions)
# ----------------------------------------------
  
  
# ----------------------------------------------
# Set up for graphing 
  # use percent change by year and health zone data
  # calculate the top 5th percentile health zones for each year to year change for each indicator

  # for each indicator and year-year calculate the 95th percentile value to compare back to the data table with all values
    dt2 <- dt[, lapply(.SD, function(x) quantile(x, .95, na.rm=T)), .SDcols= "percentChange", by= c("indicator", "year_end")] 
    
  # merge quantile table with dt
    dt <- merge(dt, dt2, by= c("indicator" , "year_end"))
    
    # percent change column from x (dt) represents the percent change for that health zone/indicator/year
    # percent change column from y (dt2) represents the percent change value that represents the 95th percentile value for that indicator/year
    setnames(dt, "percentChange.x", "percentChange")
    setnames(dt, "percentChange.y", "p95")
    
  # indicate where percent change > the 95th percentile (we are going to graph these instances
    # but also want to graph other data from that health zone)
    # binary column "isHigher" is 1 if the value for that health zone/indicator/year is higher than the 95th percentile value
    dt$isHigher <- 0
    dt[percentChange>p95, isHigher:=1]
    
  # split indicator and subpop so we can aggregate by indicator for graphing later (even though subpop is already a column)
    dt[, c("indicator", "subpop") := tstrsplit(indicator, "_", fixed=TRUE)]
# ----------------------------------------------

    
# ----------------------------------------------   
    fullData$year <- year(fullData$date)
    
    outputs <- c("ASAQ", "smearTest", "RDT", "ITN", "SP", "ANC", "ArtLum")
    graph_vars <- c("date", "indicator")
    
  # health zones to loop through
    hz <- dt[isHigher==1 & indicator %in% outputs, c("health_zone", "indicator", "subpopulation")]
    hz <- hz[!(subpopulation=="2nd" | subpopulation=="3rd" | subpopulation=="4th"| subpopulation=="positive" | subpopulation=="used")]
    
    unique_hz <- hz[["health_zone"]]
    unique_hz <- as.character(unique(unique_hz))

  # function to produce graphs
    makeGraph <- function(h){
      dtHZ <- fullData[health_zone==h & indicator %in% outputs, ]
      
      dt1 <- dtHZ[indicator=="ASAQ", .(value= sum(mean)), by= c(graph_vars)] #aggregate ASAQ all ages
      
      dt2 <- dtHZ[(indicator=="smearTest" | indicator=="RDT") & subpopulation== "completed", .(value= sum(mean)), by= c(graph_vars)] #only RDTs and Smear Tests completed
      
      dt3 <- dtHZ[indicator=="ArtLum" & subpopulation== "received", .(value= sum(mean)), by= c(graph_vars)] #only ArtLum received
      
      dt4 <- dtHZ[indicator=="ITN" & subpopulation== "received", .(indicator="ITN_recieved", value= sum(mean)), by= c(graph_vars)] # only ITNs recieved
      dt4 <- dt4[,-c(2)]
      
      dt5 <- dtHZ[indicator=="ITN" & (subpopulation== "distAtANC" | subpopulation== "distAtPreschool"), .(indicator="ITN_distributed", value= sum(mean)), by= c(graph_vars)] #aggregate ITNs dist.
      dt5 <- dt5[,-c(2)] 
      
      dt6 <- dtHZ[(indicator== "SP" | indicator== "ANC") & (subpopulation== "1st"), .(value= sum(mean)), by= c(graph_vars)] #only 1st ANC visit and SP from 1st ANC
      
      dataToGraphHZ <- rbind(dt1, dt2)
      dataToGraphHZ <- rbind(dataToGraphHZ, dt3)
      dataToGraphHZ <- rbind(dataToGraphHZ, dt4)
      dataToGraphHZ <- rbind(dataToGraphHZ, dt5)
      dataToGraphHZ <- rbind(dataToGraphHZ, dt6)
      dataToGraphHZ[, date := as.Date(date)] 
      
      # change hz data table to have subpopulations included in ITN indicator to match the dataToGraphHZ data table
      hz <- hz[indicator=="ITN" & subpopulation== "received", indicator:= "ITN_received"]
      hz <- hz[indicator=="ITN" & subpopulation== "distAtANC", indicator:= "ITN_distributed"]
      hz <- hz[indicator=="ITN" & subpopulation== "distAtPreschool", indicator:= "ITN_distributed"]
      
      indicators_of_relevance <- hz[health_zone==h]$indicator
      dataToGraphHZ[, relevant:=indicator %in% indicators_of_relevance]
      
      g <- ggplot(dataToGraphHZ, aes(x=date, y=value, color = indicator, alpha=relevant)) +
        geom_point() + geom_line() + theme_bw() + scale_alpha_discrete(range=c(0.2, 1)) +
        ggtitle(paste0("Selected Indicators for ", h)) +
        labs(y="Count", color= "Indicator") + expand_limits(y=0) +
        scale_color_manual(labels = c("1st ANC Visit", "Artemether/Lumefantrine received", "ASAQ (Artesunate Amodiaquine) all ages", "RDTs Completed", "Smear Tests Completed", "SP Distributed at 1st ANC Visit", "ITNs Received", "ITNs Distributed"), values = c("steelblue4", "darkorange", "orangered3", "green3", "darkgreen", "steelblue1", "goldenrod4", "goldenrod1")) +
        guides(alpha=F)
      
      return(g)
    }

  # loop through HZs with a high %change for at least one of the output indicators      
   pdf(paste0(output_graph, hzGraphs), height=6, width=9)
     for (h in unique_hz){
        print(makeGraph(h))
     }
   dev.off()

  
   
   
   
  # Make graphs for TERG Slides
     dtHZ <- fullData[(health_zone=="Bengamisa" | health_zone=="Ubundu") & indicator %in% outputs, ]
     
     dt1 <- dtHZ[indicator=="ASAQ", .(value= sum(mean)), by= c(graph_vars, "health_zone")] #aggregate ASAQ all ages
     
     dt2 <- dtHZ[(indicator=="smearTest" | indicator=="RDT") & subpopulation== "completed", .(value= sum(mean)), by= c(graph_vars, "health_zone")] #only RDTs and Smear Tests completed
     
     #dt3 <- dtHZ[indicator=="ArtLum" & subpopulation== "received", .(value= sum(mean)), by= c(graph_vars, "health_zone")] #only ArtLum received
     
     #dt4 <- dtHZ[indicator=="ITN" & subpopulation== "received", .(indicator="ITN_recieved", value= sum(mean)), by= c(graph_vars, "health_zone")] # only ITNs recieved
     #dt4 <- dt4[,-c(2)]
     
     dt5 <- dtHZ[indicator=="ITN" & (subpopulation== "distAtANC" | subpopulation== "distAtPreschool"), .(indicator="ITN_distributed", value= sum(mean)), by= c(graph_vars, "health_zone")] #aggregate ITNs dist.
     dt5 <- dt5[,-c(2)] 
     
     dt6 <- dtHZ[(indicator== "SP") & (subpopulation== "1st"), .(value= sum(mean)), by= c(graph_vars, "health_zone")] #only 1st ANC visit and SP from 1st ANC
     
     dataToGraphHZ <- rbind(dt1, dt2)
     #dataToGraphHZ <- rbind(dataToGraphHZ, dt3)
     #dataToGraphHZ <- rbind(dataToGraphHZ, dt4)
     dataToGraphHZ <- rbind(dataToGraphHZ, dt5)
     dataToGraphHZ <- rbind(dataToGraphHZ, dt6)
     dataToGraphHZ[, date := as.Date(date)] 
     
     # change hz data table to have subpopulations included in ITN indicator to match the dataToGraphHZ data table
     hz <- hz[indicator=="ITN" & subpopulation== "received", indicator:= "ITN_received"]
     hz <- hz[indicator=="ITN" & subpopulation== "distAtANC", indicator:= "ITN_distributed"]
     hz <- hz[indicator=="ITN" & subpopulation== "distAtPreschool", indicator:= "ITN_distributed"]
     
     # relevant indicators for TERG presentation : RDTs and ASAQ
     graph_indicators <- c("RDT", "ASAQ")
     dataToGraphHZ[, relevant:=indicator %in% graph_indicators]
   
     
  pdf(paste0(output_graph, hzGraphs2), height=6, width=9) 
     g <- ggplot(dataToGraphHZ, aes(x=date, y=value, color = indicator, alpha=relevant)) +
       geom_point() + geom_line() + theme_bw() + scale_alpha_discrete(range=c(0.23, 1)) +
       ggtitle(paste0("Examples of Health Zone-Level Results")) +
       labs(y="Number of Outputs", color= "Indicator") + expand_limits(y=0) +
       guides(alpha=F) + facet_wrap("health_zone", scales="free_y") +
       theme(axis.title.x=element_blank(), legend.position="bottom", legend.direction="vertical") +
       scale_color_manual(labels = c("ASAQ (Artesunate Amodiaquine)", "RDTs Completed", "Smear Tests Completed", "SP Distributed at 1st ANC Visit", "ITNs Distributed"), values = c("steelblue4", "palegreen2", "darkolivegreen", "skyblue", "orangered2")) 
     
     print(g)
  dev.off()
  