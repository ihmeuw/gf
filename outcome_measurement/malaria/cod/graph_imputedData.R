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
    
    # output
      output_dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/"
# ----------------------------------------------
      
      
# ----------------------------------------------
  # read in data table prepped by prep_for_MI.R
    dtOrig <- fread(paste0(dir, input)) 
# ----------------------------------------------
      
      
# ----------------------------------------------
  # make a dt of all health zone/dps pairs in dt to loop through (some hz names are used in more than one dps)
    hzDPS <- dtOrig[, .(health_zone, dps)]
  # remove duplicates to have just unique values:
    hzDPS <- unique(hzDPS) 
    hz_vector <- hzDPS[["health_zone"]]
# ----------------------------------------------
    

# ----------------------------------------------
  # read in data table prepped by prep_for_MI.R
    dt <- readRDS(paste0(dir, imputedData))
# ----------------------------------------------
      

# ----------------------------------------------     
  # separate indicators and interventions data for graphing
    graphIndicators <- dt[ variable %in% indicators ] 
    graphOutputs <- dt[ variable %in% outputs ]  
    graphStockouts <- dt[ variable %in% stockouts ]
    graphSSC <- dt[ variable %in% SSC ]
    graphHealthSystem <- dt[ variable %in% health_system ]
# ----------------------------------------------  
    
   
# ---------------------------------------------- 
  # function to capitalize hz and dps names
    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
    }
    
  # function to produce graphs
    makeGraph <- function(dt, label) {
      g <- ggplot(dt[ health_zone==(hzDPS[row, health_zone]) ], aes(x=date, y=mean, color = subpopulation)) + theme_bw()+
        
        geom_point(aes(shape=isMissing), size=2) + scale_shape_manual(values=c(19, 1)) + geom_errorbar(aes(ymin=lower, ymax=upper, y=NULL, width=75), alpha=0.2) + geom_line(alpha=0.9, size=0.60) +
        
        facet_wrap(~ indicator, scales="free_y", labeller = as_labeller(label)) + ggtitle(paste0("Time Series for ", simpleCap(hzDPS[row, health_zone]), " (", simpleCap(hzDPS[row, dps]), ")", " with Imputed Data")) +
        
        labs(shape="Imputed Value", color="Subpopulation") + ylab("value (count)")
    }
# ----------------------------------------------  
    

# ----------------------------------------------   
  # Graph indicators data by health zone   
    # "cheat" way to get it to facet wrap what I want per page 
    graphIndicatorsFirstSet <- graphIndicators[ indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere", "malariaDeaths") ] 
    graphIndicatorsSecondSet <- graphIndicators[ indicator %in% c("mildMalariaTreated", "severeMalariaTreated", "presumedMalaria", "suspectedMalaria") ] 
    
    
    pdf((paste0(output_dir, "Imputed 2010-2017 Indicators by Health Zone.pdf")), height=6, width=10)   
    
      for (row in 1:nrow(hzDPS)){
        print(makeGraph(graphIndicatorsFirstSet, indicator_names))
        print(makeGraph(graphIndicatorsSecondSet, indicator_names))
      }
    dev.off()   
# ----------------------------------------------  

    
# ----------------------------------------------   
  # Graph outputs data by health zone
    graphOutputsFirst <- graphOutputs[ indicator %in% c("ANC") ] 
    graphOutputsSecond <- graphOutputs[ indicator %in% c("ASAQreceived", "ASAQused") ] 
    graphOutputsThird <- graphOutputs[ indicator %in% c("ArtLum", "ITN") ] 
    graphOutputsFourth <- graphOutputs[ indicator %in% c("smearTest", "RDT", "thinSmearTest") ] 
    
    # "VAR_0to11mos" ? 
    
    pdf(paste0(output_dir, "Imputed 2010-2017 Outputs by Health Zone.pdf"), height=6, width=9)
    
      for (row in 1:nrow(hzDPS)){
        
        print(makeGraph(graphOutputsFirst, output_names))
        print(makeGraph(graphOutputsSecond, output_names))
        print(makeGraph(graphOutputsThird, output_names))
        print(makeGraph(graphOutputsFourth, output_names))
        
        print(row) # to track progress
      }
    dev.off() 
# ---------------------------------------------- 
    
    
# ----------------------------------------------   
  # Graph stockouts data by health zone
    graphStockoutsFirst <- graphStockouts[ indicator %in% c("stockOutSP", "stockOutASAQ") ] 
    graphStockoutsSecond <- graphStockouts[ indicator %in% c( "stockOutRDT", "stockOutartLum", "stockOutqui") ] 
  
    pdf(paste0(output_dir, "Imputed 2010-2017 Stockouts by Health Zone.pdf"), height=6, width=9)
    
      for (row in 1:nrow(hzDPS)){
        
        print(makeGraph(graphStockoutsFirst, stockout_names))
        print(makeGraph(graphStockoutsSecond, stockout_names))
        
        print(row) # to track progress
      }
    dev.off() 
# ----------------------------------------------
    
    
# ----------------------------------------------   
  # Graph SSC data by health zone
    graphSSCFirst <- graphSSC[ indicator %in% c("SSCACT") ] 
    graphSSCSecond <- graphSSC[ indicator %in% c( "SSCRDT") ] 
    graphSSCThird <- graphSSC[ indicator %in% c( "SSCcasesReferred", "SSCcasesCrossReferred" ) ]
    graphSSCFourth <- graphSSC[ indicator %in% c( "SSCfevers") ] 
    
    pdf(paste0(output_dir, "Imputed 2010-2017 SSC by Health Zone.pdf"), height=6, width=9)
    
      for (row in 1:nrow(hzDPS)){
        
        print(makeGraph(graphSSCFirst, SSC_names))
        print(makeGraph(graphSSCSecond, SSC_names))
        print(makeGraph(graphSSCThird, SSC_names))
        print(makeGraph(graphSSCFourth, SSC_names))
        
        print(row) # to track progress
      }
    dev.off() 
# ----------------------------------------------
    
    
# ----------------------------------------------   
  # Graph Health System data by health zone
    graphHealthSystemFirst <- graphHealthSystem[ indicator %in% c("healthFacilities") ] 
    graphHealthSystemSecond <- graphHealthSystem[ indicator %in% c( "supervisors", "employees") ] 
    graphHealthSystemThird <- graphHealthSystem[ indicator %in% c( "awarenessTrainings" ) ]
    
    pdf(paste0(output_dir, "Imputed 2010-2017 HealthSystem by Health Zone.pdf"), height=6, width=9)
    
      for (row in 1:nrow(hzDPS)){
        
        print(makeGraph(graphHealthSystemFirst, output_names))
        print(makeGraph(graphHealthSystemSecond, health_system_names))
        print(makeGraph(graphHealthSystemThird, health_system_names))
        
        print(row) # to track progress
      }
    dev.off() 
# ----------------------------------------------
    
    
# ----------------------------------------------
  # Graph by DPS (aggregate graphs)
    
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

      
# ----------------------------------------------     
    # Graph Aggregate interventions data by dps

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
      
      
      