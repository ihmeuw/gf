
# ----------------------------------------------
# Audrey Batzel
#
# 7/2/18

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

# source in variable names
  variable_names <-"./outcome_measurement/malaria/cod/variable_names.R"
  source(variable_names)

# output
  output_dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/"
  output_dataStats <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/Stats/"
  meanAndSD <- "PNLP_meanAndSD.csv" #of indicators/outputs/health facilities data by dps
  medianAndMAD <- "PNLP_healthFacilities_medianAndMAD.csv" #of health facilities by health zone
# ----------------------------------------------


# ----------------------------------------------
# read in data table prepped by prep_for_MI.R
  dtOrig <- fread(paste0(dir, input)) 
  dps_names <- unique(dtOrig$dps)
  dps_names <- dps_names[!dps_names %in% c("0")]
  
  id_vars <- c("dps", "date", "province11_name", "health_zone")
# ----------------------------------------------


# ----------------------------------------------
# Create a table of the mean and standard deviation of each indicator by DPS (across all HZ-month-subpops)
  # Include in this the indicators, outputs, and health system variables 
  
  dt <- dtOrig[, c(id_vars, indicators, outputs, "healthFacilities_total", "healthFacilities_totalOrig", "healthFacilities_max", "healthFacilities_numReported"), with=F]
  
  dtMelt <- melt(dt, id.vars= id_vars, variable.name="indicator")
  dtMelt$value<- as.numeric(dtMelt$value)
  
  dtByDPS <- dtMelt[, .(dpsMean = mean(value, na.rm=T), dpsSD = sd(value, na.rm=T)), by= c("dps", "indicator")]
  dtByDPSMelt <- melt(dtByDPS, id.vars=c("dps", "indicator"), variable.name= "measurement")
  
  dt <- dcast(dtByDPSMelt, dps + measurement ~ indicator, value.var="value")
  dt <- as.data.table(dt)
  dt <- dt[ dps!="0",]
  
  write.csv(dt, file=paste0(output_dataStats, meanAndSD))
# ----------------------------------------------


# ---------------------------------------------- 
# Apply a simple heuristic to detect outliers in health facilities data
  # Step 1: compute the median and MAD of total facilities by health zone (across months)
  dtFacilities <- dtOrig[, c(id_vars, "healthFacilities_total", "healthFacilities_totalOrig", "healthFacilities_max", "healthFacilities_numReported"), with=F]
  dtFacilities$date <- as.Date(dtFacilities$date)
  dtFacilities[, year := year(date)]

  
  dtFacilities$healthFacilities_adjusted <- NA
  dtFacilities$healthFacilities_adjusted <- as.numeric(dtFacilities$healthFacilities_adjusted)
  dtFacilities$healthFacilities_total <- as.numeric(dtFacilities$healthFacilities_total)
  
  dtFacilities <- dtFacilities[year %in% c("2010", "2011", "2012"), healthFacilities_adjusted := max(healthFacilities_total, na.rm=T), by=c("health_zone", "year")]
  dtFacilities <- dtFacilities[healthFacilities_adjusted == "-Inf", healthFacilities_adjusted:=NA]
  
  # keep track of where values were changed to be filled in/deterministically imputed
  dtFacilities  <- dtFacilities[is.na(healthFacilities_total) & !is.na(healthFacilities_adjusted), valueChanged := T]
  dtFacilities[is.na(healthFacilities_adjusted) & !is.na(healthFacilities_total), healthFacilities_adjusted := healthFacilities_total]
  
  dtFacilitiesByHealthZone <- dtFacilities[ ,.(hzMedianTotFacilities = median(healthFacilities_adjusted, na.rm=T), 
                                              hzMADTotFacilities = mad(healthFacilities_adjusted, na.rm=T)), 
                                              by= c("dps", "health_zone", "year")]
  
  dtFacilitiesByHealthZone <- dtFacilitiesByHealthZone[,ratio:=hzMADTotFacilities/hzMedianTotFacilities]
  
  write.csv(dtFacilitiesByHealthZone, file=paste0(output_dataStats, medianAndMAD))
  

# ----------------------------------------------


# ---------------------------------------------- 



healthFac <- colnames(dtOrig)[grep("healthFacilities", colnames(dtOrig))]
dt<- dtOrig[, c("date", "province11_name", "health_zone", "dps", healthFac), with=F]

dt$date <- as.Date(dt$date)
dt$year <- year(dt$date)

dtMelt <- melt(dt, id.vars= c("date", "province11_name", "health_zone", "dps", "year"), variable.name="indicator")


# make a dt of all health zone/dps pairs in dt to loop through (some hz names are used in more than one dps)
hzDPS <- dtOrig[, .(health_zone, dps)]
# remove duplicates to have just unique values:
hzDPS <- unique(hzDPS) 
hz_vector <- hzDPS[["health_zone"]]


pdf((paste0(output_dir, "Time Series for Number of Health Facilities (HZ level).pdf")), height=6, width=9)   

for (row in 1:nrow(hzDPS)){
  g <- ggplot(dtMelt[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]) & indicator %in% c("healthFacilities_total", "healthFacilities_numReported")], aes(x=date, y=value, color=indicator)) + 
    
    theme_bw()+ geom_point() + geom_line(alpha=0.9, size=0.60) +
    
    ggtitle(paste0("Time Series for ", simpleCap(hzDPS[row, health_zone]), " (", simpleCap(hzDPS[row, dps]), ")")) +
    
    ylab("Total Health Facilities") + xlab("Date") + ylim(0, NA)
  
  print(g)
}

dev.off()  
#--------------------------------------------


#--------------------------------------------
dt$healthFacilities_adjusted <- NA
dt$healthFacilities_adjusted <- as.numeric(dt$healthFacilities_adjusted)
dt$healthFacilities_total <- as.numeric(dt$healthFacilities_total)

dt2 <- dt[year %in% c("2010", "2011", "2012"), healthFacilities_adjusted := max(healthFacilities_total, na.rm=T), by=c("health_zone", "year")]
dt2 <- dt2[healthFacilities_adjusted == "-Inf", healthFacilities_adjusted:=NA]
#dt2 <- merge(dt2, dt, all.x=T, by=c("date", "province11_name", "health_zone", "dps", "year", "healthFacilities_total", "healthFacilities_numReported"))

dt2$healthFacilities_adjusted <- as.numeric(dt2$healthFacilities_adjusted)
dt2$healthFacilities_total <- as.numeric(dt2$healthFacilities_total)

dt2[is.na(healthFacilities_adjusted) & !is.na(healthFacilities_total), healthFacilities_adjusted := healthFacilities_total]

dt2$date <- as.Date(dt2$date)

dtMelt2 <- melt(dt2, id.vars= c("date", "province11_name", "health_zone", "dps", "year"), variable.name="indicator")


pdf((paste0(output_dir, "Time Series for Number of Health Facilities (HZ level), Adjusted Values.pdf")), height=6, width=9)   

for (row in 1:nrow(hzDPS)){
  g <- ggplot(dtMelt2[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps])& indicator %in% c("healthFacilities_adjusted", "healthFacilities_numReported"),], aes(x=date, y=value, color=indicator)) + 
    
    theme_bw()+ geom_point() + geom_line(alpha=0.9, size=0.60) +
    
    ggtitle(paste0("Time Series for ", simpleCap(hzDPS[row, health_zone]), " (", simpleCap(hzDPS[row, dps]), ")")) +
    
    ylab("Total Health Facilities") + xlab("Date") + ylim(0, NA)
  
  print(g)
}

dev.off() 

#--------------------------------------------