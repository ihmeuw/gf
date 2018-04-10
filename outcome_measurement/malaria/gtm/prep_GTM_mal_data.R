# ----------------------------------------------
  # Audrey Batzel
  #
  # 4/5/18
  # Prepping Guatemala HIV and Malaria Data for analysis
# ----------------------------------------------


# --------------------
  # Set up R / install packages
    rm(list=ls())
    library(data.table)
    library(reshape2)
    library(stringr)
    library(RColorBrewer)
    library(ggplot2)
    library(lubridate)
    library(readxl)
    library(stats)
    library(rlang)
    library(zoo)
    library(tidyr)
    library(dplyr)
# --------------------


# ----------------------------------------------
  # Overview - Files and Directories
    
    # data directory
      # J:/Project/Evaluation/GF/outcome_measurement/cod/National_Malaria_Program/Malaria_Data_2014_to_2016
    
    # input files
      # file path where the files are stored
        dir <- "J:/Project/Evaluation/GF/outcome_measurement/gtm"
        antimalarials <- "J:/Project/Evaluation/GF/outcome_measurement/gtm/MALARIA/Antimalaricos"
        
    # output files 
      # (output to prepped_data folder within gtm folder)
    
      # file path: 
    
      # file names: 
        
# ----------------------------------------------

        
# ----------------------------------------------
  # files to import/rbind() together (years)
    antimalaricos_files <- c(
      "INFORME ANTIMALARICOS 2012",
      "INFORME ANTIMALARICOS 2013",
      "INFORME ANTIMALARICOS 2014",
      "INFORME ANTIMALARICOS 2015",
      "INFORME ANTIMALARICOS 2016",
      "INFORME ANTIMALARICOS 2017"
    )
  
  # sheets within each file to bind together (months) for each year 
    sheetnames12 <- c("ENE12", "FEB12", "MAR2012", "ABR12", "MAY12", "JUL12", "AGO12", "SEPT12", "OCT12", "NOV12", "DIC12")
    sheetnames13 <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Octubre"," Noviembre")
    sheetnames14 <- c("ABR", "MAY", "JUN","JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
    sheetnames15 <- c("OCT15", "NOV15", "DIC15." )
    sheetnames16 <- c("ENE16", "FEB16", "MAR16", "ABR16", "MAY16", "JUN16")
    sheetnames17 <- c("Enero 2017", "Febrero 2017", "Abril 2017", "Mayo 2017", "Junio 2017") 
    
  # July 2012 - December 2012 column names (some order is reversed, names the same)
    columnNames2 <- c("DAS_code", "DAS", "Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", 
                     "Cloroquina250_notDeliveredToUser", "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", 
                     "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", "Cloroquina250_monthsAvailable", "Primaquina15_previousBalance", 
                     "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser", "Primaquina15_realDemand", 
                     "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                     "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", 
                     "Primaquina5_notDeliveredToUser", "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", 
                     "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", "Primaquina5_monthsAvailable")
  
  # January 2012 - May 2012 column names
    columnNames1 <- c("DAS_code", "DAS", "Primaquina15_previousBalance", "Primaquina15_topLevelEntries", "Primaquina15_deliveredToUser", "Primaquina15_notDeliveredToUser",
                      "Primaquina15_realDemand", "Primaquina15_readjustments", "Primaquina15_nextMonthBalance", "Primaquina15_existenceInBodega", "Primaquina15_avgMonthlyDemand", 
                      "Primaquina15_monthsAvailable", "Primaquina5_previousBalance", "Primaquina5_topLevelEntries", "Primaquina5_deliveredToUser", "Primaquina5_notDeliveredToUser", 
                      "Primaquina5_realDemand", "Primaquina5_readjustments", "Primaquina5_nextMonthBalance", "Primaquina5_existenceInBodega", "Primaquina5_avgMonthlyDemand", 
                      "Primaquina5_monthsAvailable", "Cloroquina250_previousBalance", "Cloroquina250_topLevelEntries", "Cloroquina250_deliveredToUser", "Cloroquina250_notDeliveredToUser",
                      "Cloroquina250_realDemand", "Cloroquina250_readjustments", "Cloroquina250_nextMonthBalance", "Cloroquina250_existenceInBodega", "Cloroquina250_avgMonthlyDemand", 
                      "Cloroquina250_monthsAvailable" )

  # 2012 data processing:
    # this is really slow....
    i = 1
    # loop through each sheet and clean & bind them together into a DT for each year
      for (s in sheetnames12){  # for each of the sheets in 2012 data
        
        # read in current sheet
          currentDT <- data.table(read_excel("J:/Project/Evaluation/GF/outcome_measurement/gtm/MALARIA/Antimalaricos/INFORME ANTIMALARICOS 2012.xlsx",  sheet=s))
        
        # drop extra column that is in the MAY12 data tacked on at the end
          ifelse ((s == "MAY12"), currentDT <- currentDT[,-c(33)], currentDT <- currentDT)
          
        # set column names, different for Jan-May, and Jul-Dec (no Jun)
          ifelse (i < 6, names(currentDT) <- columnNames1, names(currentDT) <- columnNames2)
        
        # add a column to keep track of the sheetname/month within the DT as we loop through each sheet with the doc
          currentDT$month <- s
          
        # and a column for the year - add this to a function later, hard-coded for now
          currentDT$year <- "2012"
        
        # delete first two rows in sheets ENE12-MAY12, first three rows in JUL12-DIC12
          # ifelse ( i < 6, (currentDT <- currentDT[-c(1:3),]), (currentDT <- currentDT[-c(1:4),]))
          currentDT <- currentDT[-c(1:3),]
          
        # delete first two rows in sheets ENE12-MAY12, first three rows in JUL12-DIC12
          currentDT <- currentDT[complete.cases(currentDT[ , 1]),]
          
          if (i==1) dt <- currentDT
          if (i>1)  dt <- rbind(dt, currentDT, fill=TRUE)
          i <- i + 1
          
        # print the sheet that processed - for us to see where it's working/goes wrong for now
          print(s)
      }
# ----------------------------------------------

        
# ----------------------------------------------
  # Test that the output has the right number of rows
    if (nrow(dt)!=228) stop('Output data has wrong number of rows!')
# ----------------------------------------------            
    
    
    
    