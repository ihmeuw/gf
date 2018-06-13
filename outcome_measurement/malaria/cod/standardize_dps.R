# ----------------------------------------------
# Audrey Batzel
#
# 6/4/18
# PNLP data for 2010-2017 all provinces
# merge standardized dps names onto prepped data to work with
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
  # input files
    dt_input <- "fullData_outliers_removed.csv" 
    dtDPSnames_input <- "drc_geographies_map.xlsx"
      
  # read in data
    dt <- fread(paste0(dir_prepped, dt_input))
    dtDPSnames <- read_excel(paste0(dir_prepped, dtDPSnames_input))
    dtDPSnames <- as.data.table(dtDPSnames)
    
  # output files
    dt_output <- "fullData_dps_standardized.csv"
# ----------------------------------------------
  
# further prep on dps names to be able to merge  
  dt$dps <- tolower(dt$dps)
  
  dt[province=="SK", province := "SUD KIVU"]
  dt[province=="38", province := "BDD"]
  dt[province=="4481", province := "BDD"]
  dt[province=="NordKivu", province := "Nord Kivu"]
  
  dt[dps=="mai - ndo", dps := "mai-ndombe"]
  dt[dps=="sud uban", dps := "sud ubangi"]
  dt[dps=="nord uban", dps := "nord ubangi"]
  dt[dps=="mbuji-may", dps := "mbuji-mayi"]
  dt[dps=="mbuji may", dps := "mbuji-mayi"]
  dt[dps=="nordkivu", dps := "nord kivu"]
  dt[dps=="haut lomami", dps := "haut-lomami"]
  dt[dps=="tanganika", dps := "tanganyika"]
  dt[dps=="kkt", dps := "kikwit"]
  dt[dps=="haut katanga", dps := "haut-katanga"]
  
  dt[province=="Kinshasa" & is.na(dps), dps := "nsele"]
  

# because health zone names are duplicated in different provinces, we need to merge on province so make
  # an equivalent province column i nthe dps names dt
  dtDPSnames <- dtDPSnames[, province:=province11_name]
  
    # -----to test the merge-----
    geographies <- unique(dt[, c("province", "dps", "health_zone", "year")])
    orderBy <- c("province", "health_zone", "dps", "year")
    setorderv(geographies, orderBy)
    
    dtDPS <- merge(geographies, dtDPSnames, by=c("province", "health_zone"), all=T)

    hz <- unique(dtDPSnames$health_zone)
    
    for (h in hz){
      if ( nrow(dtDPS[health_zone==h, ]) != 8){
        print( h )
        print( nrow(dtDPS[health_zone==h, ]) )
      }
    }
    # -----------------------------
    
# merge dps names dt with full data dt
    fullData <- merge(dt, dtDPSnames, by=c("province", "health_zone"), all=T)
    
  # remove dps.y column and rename dps.x to dps
    fullData<- fullData[, -c("dps.y", "V1", "id")]
    setnames(fullData, "dps.x", "dps_in_original_data")
    setnames(fullData, "current_dps_name_2016to2017", "dps")
    
# export new data table
    write.csv(fullData, paste0(dir_prepped, dt_output))
# ----------------------------------------------