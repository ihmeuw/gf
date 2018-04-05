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
  
  # sheets within each file to bind together (months)
    sheetnames12 <- c("ENE12", "FEB12", "MAR2012", "ABR12", "MAY12", "JUL12", "AGO12", "SEPT12", "OCT12", "NOV12", "DIC12")
    sheetnames13 <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Octubre"," Noviembre")
    sheetnames14 <- c("ABR", "MAY", "JUN","JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
    sheetnames15 <- c("OCT15", "NOV15", "DIC15." )
    sheetnames16 <- c("ENE16", "FEB16", "MAR16", "ABR16", "MAY16", "JUN16")
    sheetnames17 <- c("Enero 2017", "Febrero 2017", "Abril 2017", "Mayo 2017", "Junio 2017") 
    
    
    # 2012 data   
    i = 1
    # read in file, looping through each sheet and binding them together   
      for (s in sheetnames12){
        currentDT <- data.table(read_excel("J:/Project/Evaluation/GF/outcome_measurement/gtm/MALARIA/Antimalaricos/INFORME ANTIMALARICOS 2012.xlsx",  sheet=s))
        
        # add a column to keep track of the sheetname/month within the dt
          currentDT$month <- s
        
        # delete first two rows in sheets ENE12-MAY12, first three rows in JUL12-DIC12
          
        
        
        if (i==1) dt <- currentDT
        if (i>1)  dt <- rbind(dt, currentDT, fill=TRUE)
        i <- i + 1
        
        # print the sheet that processed - for us to see where it's working/goes wrong
          print(s)
      }
        
# ----------------------------------------------
# ---------------------------------------------- 
# ----------------------------------------------
        
        
        