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
      imputedData1 <- "imputedData_forGraphing_run1.rds"
      imputedData2 <- "imputedData_forGraphing_run2.rds"
    
    # source in variable names
      variable_names <-"./outcome_measurement/malaria/cod/variable_names.R"
      source(variable_names)
    
    # output
      output_dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/Imputed_2010to2017_Graphs/"
# ----------------------------------------------
      
      
# ----------------------------------------------
  # read in data table prepped by prep_for_MI.R
    dtOrig <- fread(paste0(dir, input)) 
    dps_names <- unique(dtOrig$dps)
    dps_names <- dps_names[!dps_names %in% c("0")]
# ----------------------------------------------
    

# ----------------------------------------------
  # read in data table prepped by prep_for_MI.R
    dt1 <- readRDS(paste0(dir, imputedData1))
    dt1[is.na(subpopulation), subpopulation:="none"]
    dt2 <- readRDS(paste0(dir, imputedData2))
    dt2[is.na(subpopulation), subpopulation:="none"]
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
        
        facet_wrap(~ indicator, scales="free_y", labeller = as_labeller(label)) + ggtitle(paste0("Time Series for ", simpleCap(hzDPS[row, health_zone]), " (", simpleCap(dps_name), ")", " with Imputed Data")) +
        
        labs(shape="Imputed Value", color="Subpopulation") + ylab("value (count)")
    }
# ----------------------------------------------  
    dtASAQStockouts <- dt2[indicator=="stockOutASAQ" & subpopulation != "inj" & dps !=0, ]
    dtASAQStockouts[, totStockouts := sum(mean), by=c("dps", "date")]
    dtASAQStockouts[, totStockoutsbyType := sum(mean), by=c("dps", "date", "subpopulation")]
    
    dtASAQRecieved <- dt2[indicator=="ASAQreceived" & dps !=0, ]
    dtASAQRecieved[, totASAQRecieved := sum(mean), by=c("dps", "date")]
    dtASAQRecieved[, totASAQRecievedbyType := sum(mean), by=c("dps", "date", "subpopulation")]
    
    dtASAQUsed <- dt2[indicator=="ASAQused" & dps !=0, ]
    dtASAQUsed[, totASAQUsed := sum(mean), by=c("dps", "date")]
    dtASAQUsed[, totASAQUsedbyType := sum(mean), by=c("dps", "date", "subpopulation")]
    
    
  pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/GraphsForTERG.pdf", height=6, width=9)
      g <- ggplot(dtASAQStockouts[dps %in% c("maniema", "tshopo", "kinshasa"), ], aes(x=date, y=totStockouts, color = dps)) + theme_bw()+
        geom_point() + geom_line() +
        ggtitle(paste0("Time Series for Stock-outs")) +
        labs(color="DPS") + ylab("Days of Stock-outs") + xlab("Date") +
        theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=13))
      print(g)
      
      g <- ggplot(dtASAQRecieved[dps %in% c("maniema", "tshopo", "kinshasa"), ], aes(x=date, y=totASAQRecieved, color = dps)) + theme_bw()+
        geom_point() + geom_line() +
        ggtitle(paste0("Time Series for ASAQ Received by Health Zones")) +
        labs(color="DPS") + ylab("Doses of ASAQ") + xlab("Date") + scale_y_continuous(labels = scales::comma) +
        theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=13))
      print(g)

      g <- ggplot(dtASAQUsed[dps %in% c("maniema", "tshopo", "kinshasa"), ], aes(x=date, y=totASAQUsed, color = dps)) + theme_bw()+
        geom_point() + geom_line() +
        ggtitle(paste0("Time Series for ASAQ Distributed to Patients")) +
        labs(color="DPS") + ylab("Doses of ASAQ") + xlab("Date") + scale_y_continuous(labels = scales::comma) +
        theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=13))
      print(g)
  #----------------- 
    graphDPS <- function(d){
      g <- ggplot(dtASAQStockouts[dps==d,], aes(x=date, y=totStockoutsbyType, color = subpopulation)) + theme_bw()+
        geom_point() + geom_line() +
        ggtitle(paste0("Time Series for Stock-outs (", simpleCap(d), ")")) +
        labs(color="Age Group") + ylab("Days of Stock-outs") + xlab("Date") +
        scale_color_discrete(name="Age Group",
                            breaks=c("14yrsAndOlder", "6to13yrs", "1to5yrs", "2to11mos" ),
                            labels=c("14 yrs+", "6 to 13 yrs", "1 to 5 yrs", "2 to 11 mos")) +
        theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=13))

      print(g)

      g <- ggplot(dtASAQRecieved[dps==d,], aes(x=date, y=totASAQRecievedbyType, color = subpopulation)) + theme_bw()+
        geom_point() + geom_line() +
        ggtitle(paste0("Time Series for ASAQ Received by Health Zones (", simpleCap(d), ")")) +
        labs(color="Age Group") + ylab("Doses of ASAQ") + xlab("Date") + scale_y_continuous(labels = scales::comma) +
        scale_color_discrete(name="Age Group",
                             breaks=c("14yrsAndOlder", "6to13yrs", "1to5yrs", "2to11mos" ),
                             labels=c("14 yrs+", "6 to 13 yrs", "1 to 5 yrs", "2 to 11 mos")) +
        theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=13))
      print(g)
      
      g <- ggplot(dtASAQUsed[dps==d, ], aes(x=date, y=totASAQUsedbyType, color = subpopulation)) + theme_bw()+
        geom_point() + geom_line() +
        ggtitle(paste0("Time Series for ASAQ Distributed to Patients (", simpleCap(d), ")")) +
        labs(color="Age Group") + ylab("Doses of ASAQ") + xlab("Date") + scale_y_continuous(labels = scales::comma) +
        scale_color_discrete(name="Age Group",
                             breaks=c("14yrsAndOlder", "6to13yrs", "1to5yrs", "2to11mos" ),
                             labels=c("14 yrs+", "6 to 13 yrs", "1 to 5 yrs", "2 to 11 mos")) +
        theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=13))
      print(g)
    }
  #-----------------
  for (i in c("maniema", "tshopo", "kinshasa")){ graphDPS(i) }
      
  dev.off()
# ----------------------------------------------  
# set dt and tol... choose 1 or 2
  # dt <- copy(dt1)
  # tol <- 0.01
    
  dt <- copy(dt2)
  tol <- 0.001
  
  dps_names <- c("tshopo", "kinshasa", "maniema")
  
for (dps_name in dps_names){   
  
  # separate indicators and interventions data for graphing
    graphIndicators <- dt[ dps== dps_name & variable %in% indicators ] 
    graphOutputs <- dt[ dps== dps_name & variable %in% outputs ]  
    graphStockouts <- dt[ dps== dps_name & variable %in% stockouts ]
    graphSSC <- dt[ dps== dps_name & variable %in% SSC ]
    graphHealthSystem <- dt[ dps== dps_name & variable %in% health_system ]

  # make a dt of all health zone/dps pairs in dt to loop through (some hz names are used in more than one dps)
    hzDPS <- dtOrig[dps==dps_name, .(health_zone, dps)]
  # remove duplicates to have just unique values:
    hzDPS <- unique(hzDPS) 
    hz_vector <- hzDPS[["health_zone"]]

# ---------------------------------------------- 
    
  # Graph indicators data by health zone   
    # "cheat" way to get it to facet wrap what I want per page 
    graphIndicatorsFirstSet <- graphIndicators[ indicator %in% c("newCasesMalariaMild", "newCasesMalariaSevere") ]
    graphIndicatorsSecondSet <- graphIndicators[ indicator %in% c("malariaDeaths") ] 
    graphIndicatorsThirdSet <- graphIndicators[ indicator %in% c("mildMalariaTreated", "severeMalariaTreated") ]
    graphIndicatorsFourthSet <- graphIndicators[ indicator %in% c("presumedMalaria", "suspectedMalaria") ] 
    
    pdf((paste0(output_dir, "Indicators by Health Zone (", simpleCap(dps_name), " and tol=", tol, ").pdf")), height=6, width=10)   
  
      for (row in 1:nrow(hzDPS)){
        print(makeGraph(graphIndicatorsFirstSet, indicator_names))
        print(makeGraph(graphIndicatorsSecondSet, indicator_names))
        print(makeGraph(graphIndicatorsThirdSet, indicator_names))
        print(makeGraph(graphIndicatorsFourthSet, indicator_names))
        
        print(row) # to track progress
      }
    dev.off()   
# ----------------------------------------------  
  
  # Graph outputs data by health zone
    graphOutputsFirst <- graphOutputs[ indicator %in% c("ANC") ] 
    graphOutputsSecond <- graphOutputs[ indicator %in% c("ASAQreceived") ]
    graphOutputsThird <- graphOutputs[ indicator %in% c("ASAQused") ]
    graphOutputsFourth <- graphOutputs[ indicator %in% c("ITN") ]
    graphOutputsFifth <- graphOutputs[ indicator %in% c("ArtLum","thinSmearTest") ] 
    graphOutputsSixth <- graphOutputs[ indicator %in% c("smearTest", "RDT") ] 

    
    pdf(paste0(output_dir, "Outputs by Health Zone (", simpleCap(dps_name), " and tol=", tol, ").pdf"), height=6, width=10)
    
      for (row in 1:nrow(hzDPS)){
        
        print(makeGraph(graphOutputsFirst, output_names))
        print(makeGraph(graphOutputsSecond, output_names))
        print(makeGraph(graphOutputsThird, output_names))
        print(makeGraph(graphOutputsFourth, output_names))
        print(makeGraph(graphOutputsFifth, output_names))
        print(makeGraph(graphOutputsSixth, output_names))
        
        print(row) # to track progress
      }
    dev.off() 
    
# ---------------------------------------------- 
  
  # Graph stockouts data by health zone
    graphStockoutsFirst <- graphStockouts[ indicator %in% c("stockOutSP", "stockOutASAQ") ] 
    graphStockoutsSecond <- graphStockouts[ indicator %in% c("stockOutRDT", "stockOutartLum") ] 
    graphStockoutsThird <- graphStockouts[ indicator %in% c("stockOutqui") ] 
    
    pdf(paste0(output_dir, "Stockouts by Health Zone (", simpleCap(dps_name), " and tol=", tol, ").pdf"), height=6, width=10)
    
      for (row in 1:nrow(hzDPS)){
        
        print(makeGraph(graphStockoutsFirst, stockout_names))
        print(makeGraph(graphStockoutsSecond, stockout_names))
        print(makeGraph(graphStockoutsThird, stockout_names))

        print(row) # to track progress
      }
    dev.off() 
    
# ----------------------------------------------

  # Graph SSC data by health zone
    graphSSCFirst <- graphSSC[ indicator %in% c("SSCACT") ] 
    graphSSCSecond <- graphSSC[ indicator %in% c( "SSCRDT") ] 
    graphSSCThird <- graphSSC[ indicator %in% c( "SSCcasesReferred", "SSCcasesCrossReferred" ) ]
    graphSSCFourth <- graphSSC[ indicator %in% c( "SSCfevers") ] 
    
    pdf(paste0(output_dir, "CHW Data by Health Zone (", simpleCap(dps_name), " and tol=", tol, ").pdf"), height=6, width=10)
    
      for (row in 1:nrow(hzDPS)){
        
        print(makeGraph(graphSSCFirst, SSC_names))
        print(makeGraph(graphSSCSecond, SSC_names))
        print(makeGraph(graphSSCThird, SSC_names))
        print(makeGraph(graphSSCFourth, SSC_names))
        
        print(row) # to track progress
      }
    dev.off() 
    
# ----------------------------------------------
  
  # Graph Health System data by health zone
    graphHealthSystemFirst <- graphHealthSystem[ indicator %in% c("healthFacilities") ] 
    graphHealthSystemSecond <- graphHealthSystem[ indicator %in% c( "supervisors", "employees") ] 
    graphHealthSystemThird <- graphHealthSystem[ indicator %in% c( "awarenessTrainings" ) ]
     
    pdf(paste0(output_dir, "Health System Data by Health Zone (", simpleCap(dps_name), " and tol=", tol, ").pdf"), height=6, width=10)
    
      for (row in 1:nrow(hzDPS)){
        
        print(makeGraph(graphHealthSystemFirst, output_names))
        print(makeGraph(graphHealthSystemSecond, health_system_names))
        print(makeGraph(graphHealthSystemThird, health_system_names))
        
        print(row) # to track progress
      }
    dev.off() 
}
# ----------------------------------------------
    
