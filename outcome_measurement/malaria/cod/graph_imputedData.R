# ----------------------------------------------
  # Audrey Batzel
  #
  # 6/18/18
  # Working with the imputed data and creating visualizations

  setwd('C:/local/gf/')
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
    # when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
      j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
      dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
      
    # input file:
      input <- "PNLP_2010to2017_preppedForMI.csv"
      imputedData <- "condensed_imputedData_forGraphing.rds"
    
    # source in variable names
      variable_names <-"./outcome_measurement/malaria/cod/variable_names.R"
      source(variable_names)
# ----------------------------------------------
      
      
# ----------------------------------------------
  # read in data table prepped by prep_for_MI.R
    dtOrig <- fread(paste0(dir, input)) 
# ----------------------------------------------
      
      
# ----------------------------------------------
  # make a vector of all health zones in dt to loop through 
    hz_vector <- dtOrig[["health_zone"]]
  # remove duplicates to have just unique values:
    hz_vector <- unique(hz_vector) 
      
# ----------------------------------------------
    

# ----------------------------------------------
  # read in data table prepped by prep_for_MI.R
    dt <- readRDS(paste0(dir, imputedData))
# ----------------------------------------------
      
    
    # separate indicators and interventions data for graphing
    graphDataIndicators <- graphDataComplete[ variable %in% indicators ] 
    graphDataInterventions <- graphDataComplete[ variable %in% interventions ]  
    # ----------------------------------------------  
    
    
    # ----------------------------------------------   
    # Examples of one-facet health zone MI graphs:
    
    graphDataComplete <- as.data.table(graphDataComplete) 
    graphDataComplete[, date := as.Date(date)]
    
    # tmp = graphDataComplete[health_zone==hz & indicator == i & subpopulation == "pregnantWomen"]
    # for()
    
    makeGraph <- function(hz, i, subpop){ 
      g <- ggplot(graphDataComplete[health_zone==hz & indicator == i & subpopulation == subpop,], aes(x=date, y=mean, color=isMissing)) + theme_bw()+
        
        geom_point(aes(shape=isMissing), size=3) + scale_shape_manual(values=c(19, 1)) + geom_errorbar(aes(ymin=lower, ymax=upper, y=NULL, width=75), alpha=0.5) + #geom_line(alpha=0.9, size=0.60) +
        
        ggtitle(paste0("Multiple Imputation for ", variable_names[i], " Completed in ", hz)) + 
        
        labs(color="Imputed Value") + ylab("Count") + theme(axis.title.x=element_blank()) +
        
        guides(shape=FALSE) + scale_color_manual(values=c("steelblue1", "tomato1"))
      
      return (g)
    }  
    
    pdf((paste0(output_dir, "Example of MI Results - Confirmed Cases.pdf")), height=6, width=10) 
    
    print(makeGraph( "Mosango", "newCasesMalariaMild", "under5" ))
    print(makeGraph( "Bandal", "newCasesMalariaMild", "under5" ))
    
    dev.off()
    
    
    pdf((paste0(output_dir, "Example of MI Results - RDTs.pdf")), height=6, width=10) 
    
    for (healthzone in c("Pay kongila", "Vanga", "Selembao", "lemba")){
      print(makeGraph( healthzone, "RDT", "completed" ))
    }
    
    dev.off()   
    
    
    # ----------------------------------------------          
    
    # Graph indicators data by health zone         
    pdf((paste0(output_dir, MI_results_indicators)), height=6, width=10)   
    
    for (hz in hzProvinceDPS$health_zone){
      
      g <- ggplot(graphDataIndicators[health_zone==hz], aes(x=date, y=mean, color = subpopulation)) + theme_bw()+
        
        geom_point(aes(shape=isMissing), size=2) + scale_shape_manual(values=c(19, 1)) + geom_errorbar(aes(ymin=lower, ymax=upper, y=NULL, width=75), alpha=0.5) + geom_line(alpha=0.9, size=0.60) +
        
        facet_wrap(~indicator, scales="free_y", labeller = as_labeller(indicator_names))+ ggtitle(paste0("Time Series for ", hz, " (", hzProvinceDPS[health_zone==hz, dps], ")", " with Imputed Data")) +
        
        labs(shape="Imputed Value", color="Subpopulation")
      
      print (g)
      
    }
    
    dev.off()    
    # ----------------------------------------------  
    #version without error bars or missing values in geom_point for TERG presentation graphs 
    # ----------------------------------------------  
    pdf((paste0(output_dir,"Indicators by Health Zone for TERG Presentation.pdf")), height=6, width=10)   
    
    
    for (hz in hzProvinceDPS$health_zone){
      
      g <- ggplot(graphDataIndicators[health_zone==hz], aes(x=date, y=mean, color = subpopulation)) + theme_bw()+
        
        geom_point() + scale_shape_manual(values=c(19, 1)) + geom_line(alpha=0.9, size=0.60) +
        
        facet_wrap(~indicator, scales="free_y", labeller = as_labeller(indicator_names))+ ggtitle(paste0("Time Series for ", hz, " (", hzProvinceDPS[health_zone==hz, dps], ")", " with Imputed Data")) +
        
        labs(shape="Imputed Value", color="Subpopulation")
      
      print (g)
      
    }
    
    dev.off() 
    # ----------------------------------------------  
    
    
    # ----------------------------------------------   
    # Graph interventions data by health zone
    pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Imputed Time Series for Interventions by Health Zone.pdf", height=6, width=9)   
    
    # ----------------------------------------------  
    
    
    # ----------------------------------------------    
    # Set up for agg graphs
    # aggregate all indicator/intervention data by dps, within each imputation
    aggData  <- imputedDataLong[, .(aggValue = sum(value)), by=c( "date", "dps", "indicator", "subpopulation", "imputation_number" )]
    
    # then compute the mean, upper and lower across all imputations for each unique dps/date
    aggData <- aggData[, .(mean=mean(aggValue), 
                           lower=quantile(aggValue, .05), 
                           upper=quantile(aggValue, .95)), by=c("date", "dps", "indicator", "subpopulation")]
    
    # set upper and lower values to NA where the value was not imputed (where mean==lower and mean==upper)
    aggData <- aggData[mean==lower, lower := NA]
    aggData <- aggData[mean==upper, upper := NA]
    
    indicatorInput <- aggData[["indicator"]]
    indicatorInput <- unique(indicatorInput)
    
    # add a subpopulation value to VAR so it will work in the graph loop
    aggData <- aggData[indicator=="VAR", subpopulation := 0]
    # ----------------------------------------------  
    
    
    # ----------------------------------------------
    # Graph Aggregate Indicators Data by dps  
    pdf(paste0(output_dir, MI_agg_indicators), height=6, width=9)   
    
    for (i in 1:5){
      
      aggGraphTitle <- indicator_names[i]
      
      g <- ggplot(aggData[indicator==indicatorInput[i]], aes(x=date, y=mean, color = subpopulation)) +
        
        geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper, y=NULL), alpha=0.5) + geom_line() + theme_bw() + ggtitle(paste0("Aggregate Data by Provincial Health Division (DPS): ", aggGraphTitle)) + 
        
        labs(x= "Date", y="Value", color= "Subpopulation") + facet_wrap(~ dps, scales="free_y") + scale_color_manual(labels = c("Ages 5 and Older", "Pregnant Women", "Ages 4 and Under"), values = c("steelblue4", "palegreen4", "steelblue1"))
      
      print(g)
    }
    
    dev.off()
    
    # ----------------------------------------------  
    #version without error bars for TERG presentation graphs 
    # ----------------------------------------------  
    pdf(paste0(output_dir,"Indicators by DPS for TERG presentation.pdf"), height=6, width=9)   
    
    for (i in 1:5){
      
      aggGraphTitle <- indicator_names[i]
      
      g <- ggplot(aggData[indicator==indicatorInput[i]], aes(x=date, y=mean, color = subpopulation)) +
        
        geom_point() +  geom_line() + theme_bw() + ggtitle(paste0("Aggregate Data by Provincial Health Division (DPS): ", aggGraphTitle)) + 
        
        labs(x= "Date", y="Value", color= "Subpopulation") + facet_wrap(~ dps, scales="free_y") + scale_color_manual(labels = c("Ages 5 and Older", "Pregnant Women", "Ages 4 and Under"), values = c("steelblue4", "palegreen4", "steelblue1"))
      
      print(g)
    }
    
    dev.off()
    # ----------------------------------------------     
    
    
    # ----------------------------------------------     
    # Graph Aggregate interventions data by dps
    intervention_names <- c(
      `ArtLum` = "Artéméther - Lumefatrine",
      `SP` = "SP administered during ANC",
      `ASAQ` = "Artesunate Amodiaquine (ASAQ)",
      `ITN` = "ITNs",
      `ANC` = "Antenatal Care Visits",
      `RDT` = "Rapid Diagnostic Tests",
      `smearTest` = "Smear Tests",
      `VAR` = "Measles Vaccine",
      `healthFacilities` = "Health Facilities Reporting",
      `reports` = "Number of Reports"
    )    
    
    
    pdf(paste0(output_dir, MI_agg_interventions), height=6, width=9)   
    
    for (i in 6:15){
      if (i==15) next() # for some reason healthFacilitiesProduct won't work
      
      aggGraphTitle <- indicator_names[i]
      
      g <- ggplot(aggData[indicator==indicatorInput[i]], aes(x=date, y=mean, color = subpopulation)) +
        
        geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper, y=NULL), alpha=0.5) + geom_line() + theme_bw() + ggtitle(intervention_names[[indicatorInput[i]]]) + 
        
        labs(x= "Date", y="Value", color= " ") + facet_wrap(~ dps, scales="free_y")
      
      print(g)
    }
    
    dev.off()
      
      
      