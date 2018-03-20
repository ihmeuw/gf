# ----------------------------------------------
  # Audrey Batzel
  #
  # 3/16/18
  # COD PNLP data for 2014-2016; descriptive analysis
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
    dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data"
 
  # input file:
  # J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/COD_PNLP_Data_Indicators
    # csv files were produced by prep_COD_Malaria_data_function.R
    df <- fread(paste0(dir,"/","COD_PNLP_Data_Indicators",".csv")) 
    
  # output files:
# ----------------------------------------------


# ----------------------------------------------        
  # create a big set of graphs where there's one plot for each health zone, showing each indicator over time
  
    # plot for one health zone, with each indicator
     # time on x axis, number of new cases of malaria (indicator) on y axis
      # time needs to encapsulate month & year
      # select by health zone and then replicate for each health zone

    
    qplot(month, newCasesMalaria, data=df, facets = . ~ year, geom = c("point", "smooth"))

    g <- ggplot(df[health_zone=='BAGATA' & subpopulation != "pregnantWomen"], aes(month, newCasesMalaria, color = subpopulation))
    
    g + geom_point() + facet_grid(.~year) + theme_bw()
    
# ----------------------------------------------       

    
# ----------------------------------------------       
  # convert months to standard, usable date... translate?
1
    # translate french to numeric version of month Janvier=1
    df[month=='Janvier', month:="01"]
    df[grepl("Janvier", month), month:="01"]
    #df[df$month=='Janvier',]$month <- "1"
    # make sure it worked for all instances of month
    
    # make string version of the date
    df[, stringdate:=paste('01', month, year, sep='/')]
    
    # combine year and month into one variable
    df[, date:=as.Date(stringdate, "%d/%m/%Y")]
        # will make a date format variable
    
    # add to prep code: check for warnings / characters
    df[, newCasesMalaria := as.numeric(newCasesMalaria)]
  
  