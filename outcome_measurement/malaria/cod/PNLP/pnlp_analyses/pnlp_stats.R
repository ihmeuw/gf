
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
  library(DescTools)
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
  hzSplits <- "health_zone_splits.xlsx"
# ----------------------------------------------


# ----------------------------------------------
# read in data table prepped by prep_for_MI.R
  dtOrig <- fread(paste0(dir, input)) 
  
  # create a vector of dps names
    dps_names <- unique(dtOrig$dps)
    dps_names <- dps_names[!dps_names %in% c("0")]
    
  # make a dt of all health zone/dps pairs in dt to loop through (some hz names are used in more than one dps)
    hzDPS <- dtOrig[, .(health_zone, dps)]
  # remove duplicates to have just unique values:
    hzDPS <- unique(hzDPS) 
    hz_vector <- hzDPS[["health_zone"]]
  
  id_vars <- c("dps", "date", "province11_name", "health_zone")
  
# read in health zone splits data to merge with original data (for calculating mean/sd/etc by continuous segments)
  dtSplits <- read_excel(paste0(output_dir, hzSplits))
  dtSplits <- dtSplits[, c(1:3)]
  dtSplits$date_of_change <- as.Date(dtSplits$date_of_change)
# ----------------------------------------------


# ----------------------------------------------
# # Create a table of the mean and standard deviation of each indicator by DPS (across all HZ-month-subpops)
# ----------------------------------------------
#   # Include in this the indicators, outputs, and health system variables 
#   
#   dt <- dtOrig[, c(id_vars, indicators, outputs, "healthFacilities_total", "healthFacilities_totalOrig", "healthFacilities_max", "healthFacilities_numReported"), with=F]
#   
#   dtMelt <- melt(dt, id.vars= id_vars, variable.name="indicator")
#   dtMelt$value<- as.numeric(dtMelt$value)
#   
#   dtByDPS <- dtMelt[, .(dpsMean = mean(value, na.rm=T), dpsSD = sd(value, na.rm=T)), by= c("dps", "indicator")]
#   dtByDPSMelt <- melt(dtByDPS, id.vars=c("dps", "indicator"), variable.name= "measurement")
#   
  
  
#   dt <- dcast(dtByDPSMelt, dps + measurement ~ indicator, value.var="value")
#   dt <- as.data.table(dt)
#   dt <- dt[ dps!="0",]
#   
#   write.csv(dt, file=paste0(output_dataStats, meanAndSD))
# # ----------------------------------------------


# ---------------------------------------------- 
# Apply a simple heuristic to detect outliers in health facilities data for total facilities

  dtFacilities <- dtOrig[, c(id_vars, "healthFacilities_total", "healthFacilities_totalOrig", "healthFacilities_max", "healthFacilities_numReported"), with=F]
  dtFacilities$date <- as.Date(dtFacilities$date)
  dtFacilities[, year := year(date)]
  
  #merge dtSplits with dtFacilities
  
  dtFacilities <- merge(dtSplits, dtFacilities, all=T, by= c("dps", "health_zone"))
  dtFacilities <- as.data.table(dtFacilities)
  
  dtFacilities <- dtFacilities[!is.na(date_of_change) & date >= date_of_change, segment:=2 ]
  dtFacilities <- dtFacilities[!is.na(date_of_change) & date < date_of_change, segment:=1 ]
  dtFacilities <- dtFacilities[is.na(date_of_change), segment:=1 ]
  
  dtFacilities$healthFacilities_adjusted <- NA
  dtFacilities$healthFacilities_adjusted <- as.numeric(dtFacilities$healthFacilities_adjusted)
  dtFacilities$healthFacilities_totalOrig <- as.numeric(dtFacilities$healthFacilities_totalOrig)
  
  dtFacilities <- dtFacilities[year %in% c("2010", "2011", "2012"), healthFacilities_adjusted := max(healthFacilities_totalOrig, na.rm=T), by=c("dps", "health_zone", "year")]
  
  # when there are no non-missing values for max() it sets the result to "-Inf" - change these to NA
  dtFacilities <- dtFacilities[healthFacilities_adjusted == "-Inf", healthFacilities_adjusted:=NA]

  dtFacilities[is.na(healthFacilities_adjusted) & !is.na(healthFacilities_totalOrig), healthFacilities_adjusted := healthFacilities_totalOrig]
#--------------------------------------------
  
  
#--------------------------------------------
  # Original graphs to visualize data
#--------------------------------------------
  # healthFac <- colnames(dtOrig)[grep("healthFacilities", colnames(dtOrig))]
  # dt<- dtOrig[, c("date", "province11_name", "health_zone", "dps", healthFac), with=F]
  # 
  # dt$date <- as.Date(dt$date)
  # dt$year <- year(dt$date)
  # 
  # dtMelt <- melt(dt, id.vars= c("date", "province11_name", "health_zone", "dps", "year"), variable.name="indicator")
  # 
  # 
  # 
  # 
  # pdf((paste0(output_dir, "Time Series for Number of Health Facilities (HZ level).pdf")), height=6, width=9)   
  # 
  # for (row in 1:nrow(hzDPS)){
  #   g <- ggplot(dtMelt[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]) & indicator %in% c("healthFacilities_total", "healthFacilities_numReported")], aes(x=date, y=value, color=indicator)) + 
  #     
  #     theme_bw()+ geom_point() + geom_line(alpha=0.9, size=0.60) +
  #     
  #     ggtitle(paste0("Time Series for ", simpleCap(hzDPS[row, health_zone]), " (", simpleCap(hzDPS[row, dps]), ")")) +
  #     
  #     ylab("Total Health Facilities") + xlab("Date") + ylim(0, NA)
  #   
  #   print(g)
  # }
  # 
  # dev.off()
#--------------------------------------------
  
  
    #--------------------------------------------
    # Graph health facilities adjusted and health facilities reporting over time to visualize
    #--------------------------------------------
      # dtFacilitiesMelt <- melt(dtFacilities, id.vars= c(id_vars, "year", "valueChanged"), with=F, variable.name="indicator")
      # 
      # pdf((paste0(output_dir, "Time Series for Number of Health Facilities (HZ level), Adjusted Values from Original Totals.pdf")), height=6, width=9)   
      # 
      # for (row in 1:nrow(hzDPS)){
      #   g <- ggplot(dtFacilitiesMelt[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps])& indicator %in% c("healthFacilities_totalOrig", "healthFacilities_adjusted", "healthFacilities_numReported"),], aes(x=date, y=value, color=indicator)) + 
      #     
      #     theme_bw()+ geom_point() + geom_line(alpha=0.9, size=0.60) + 
      #     
      #     geom_point(data=dtFacilitiesMelt[health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps])& indicator=="healthFacilities_totalOrig", ]) + 
      #     
      #     geom_line(data=dtFacilitiesMelt[health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps])& indicator=="healthFacilities_totalOrig", ]) +
      #     
      #     ggtitle(paste0("Time Series for ", hzDPS[row, health_zone], " (", hzDPS[row, dps], ")")) +
      #     
      #     ylab("Total Health Facilities") + xlab("Date") + ylim(0, NA) +
      #     
      #     scale_color_manual(values=c("navy", "tomato", "lightskyblue"))
      #   
      #   print(g)
      # }
      # 
      # dev.off() 
    #--------------------------------------------
  
  
#--------------------------------------------
# Compute the median and MAD of total facilities by health zone (across months)
#--------------------------------------------
  # dtFacilitiesByHealthZone <- dtFacilities[ ,.(medianFacilities = median(healthFacilities_totalOrig, na.rm=T),
  #                                             MADFacilities = mad(healthFacilities_totalOrig, na.rm=T)),
  #                                             by= c("dps", "health_zone")]
  # 
  # dtFacilitiesByHealthZone <- dtFacilitiesByHealthZone[,ratio:=MADFacilities/medianFacilities]
  # 
  # dtFacilitiesByHealthZone <- dtFacilitiesByHealthZone[,threeMADs:= 3 * MADFacilities,
  #                                                      by= c("dps", "health_zone")]
  # dtFacilitiesByHealthZone <- dtFacilitiesByHealthZone[,fourMADs:= 4 * MADFacilities,
  #                                                      by= c("dps", "health_zone")]
  # dtFacilitiesByHealthZone <- dtFacilitiesByHealthZone[,fiveMADs:= 5 * MADFacilities,
  #                                                      by= c("dps", "health_zone")]
  # 
  # dt <- merge(dtFacilitiesByHealthZone, dtFacilities, all=T, by=c("dps", "health_zone"))
  
  # write.csv(dtFacilitiesByHealthZone, file=paste0(output_dataStats, medianAndMAD))
  
#--------------------------------------------
  
  
#--------------------------------------------
# Compute the median and SD of total facilities by health zone and segment 
#--------------------------------------------
      dtFacilitiesByHealthZone <- dtFacilities[ , medianFacilities := median(healthFacilities_totalOrig, na.rm=T),
                                                   by= c("dps", "health_zone", "segment")]
      
      dtFacilitiesByHealthZone <- dtFacilitiesByHealthZone[ , SDFacilities := sd(Trim(healthFacilities_totalOrig, trim=0.05, na.rm=T), na.rm=T),
                                                by= c("dps", "health_zone", "segment")]
      
      dt <- copy(dtFacilitiesByHealthZone)
      
      dt[, healthFacilities_max:=NULL]
      dt[, healthFacilities_total:=NULL]
      dt[, healthFacilities_adjusted:=NULL]
# ----------------------------------------------
      
      
# ----------------------------------------------
  id_vars <- colnames(dt)
  id_vars <- id_vars[! id_vars %in% c("healthFacilities_totalOrig", "healthFacilities_numReported")]
  
  dtMelt <- melt(dt, id.vars= c(id_vars), with=F, variable.name="indicator")
  dtMelt$indicator <- as.character(dtMelt$indicator)
  
    # medianMADsByHZ <- dt[, .(dps, health_zone, medianFacilities, MADFacilities, threeMADs, fourMADs, fiveMADs)]
    # medianMADsByHZ <- unique(medianMADsByHZ)
    # 
    # medianMADsByHZ <- medianMADsByHZ[MADFacilities==0, threeMADs:=NA]
    # medianMADsByHZ <- medianMADsByHZ[MADFacilities==0, fourMADs:=NA]
    # medianMADsByHZ <- medianMADsByHZ[MADFacilities==0, fiveMADs:=NA]
    
  medianSDs <- dt[, .(dps, health_zone, medianFacilities, SDFacilities, segment)]
  medianSDs <- unique(medianSDs)
  
  pdf((paste0(output_dir, "Time Series for Number of Health Facilities (HZ level), Visualize Outliers.pdf")), height=6, width=9)   
  
  for (row in 1:nrow(hzDPS)){
    g <- ggplot( dtMelt[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]),], aes(x=date, y=value, color=indicator)) + 
      
      theme_bw() + geom_point() + geom_line(alpha=0.9, size=0.60) + 
      
      # ifelse(medianSDs[health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]), MADFacilities]==0, line= "solid", line="dashed")+

      # geom_hline( yintercept= medianSDs[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]), medianFacilities], color="grey24", size=2, alpha=0.15, linetype="solid") +

      # geom_hline( yintercept= medianMADsByHZ[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]), medianFacilities+threeMADs], color="gold", size=2, alpha=0.25) +
      # geom_hline( yintercept= medianMADsByHZ[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]), medianFacilities+fourMADs], color="orangered", size=2, alpha=0.25) +
      # geom_hline( yintercept= medianMADsByHZ[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]), medianFacilities+fiveMADs], color="red4", size=2, alpha=0.25) +
      # 
      # geom_hline( yintercept= medianMADsByHZ[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]), medianFacilities-threeMADs], color="gold", size=2, alpha=0.25) +
      # geom_hline( yintercept= medianMADsByHZ[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]), medianFacilities-fourMADs], color="orangered", size=2, alpha=0.25) +
      # geom_hline( yintercept= medianMADsByHZ[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]), medianFacilities-fiveMADs], color="red4", size=2, alpha=0.25) +

      ggtitle(paste0("Time Series for ", hzDPS[row, health_zone], " (", hzDPS[row, dps], ")")) +
      
      ylab("Total Health Facilities") + xlab("Date") + ylim(0, NA) +
      
      geom_line( aes(x=date, y=medianFacilities), alpha=0.5, color="gold", size=1) +
      
      geom_line( aes(x=date, y=medianFacilities+(3*SDFacilities)), alpha=0.5, color="red", size=1, linetype="dashed") +
      
      geom_line( aes(x=date, y=medianFacilities-(3*SDFacilities)), alpha=0.5, color="red", size=1, linetype="dashed") +
    
      scale_color_manual(values=c("palegreen3", "navy"))
    
    print(g)
  }
  
  dev.off() 
# -------------------------------------------

    
#--------------------------------------------
# REMOVE OUTLIERS AND MERGE BACK TO dtOrig FOR MI

# calculate 3 * trimmed SDs
dt <- dt[, threeSDs := SDFacilities*3]

# if health facilities total is more than 3 trimmed SDs from the median, remove it
dt[, healthFacilities_total := healthFacilities_totalOrig]
dt <- dt[healthFacilities_totalOrig > (medianFacilities+threeSDs), healthFacilities_total := NA] #this removes 349 points
dt <- dt[healthFacilities_totalOrig < (medianFacilities-threeSDs), healthFacilities_total := NA] #this removes 330 points; now 20,752 missing points

# if SD is = 0 set all the points for total health facilities to be = to the median
dt[SDFacilities==0, healthFacilities_total := medianFacilities] #this makes it so there are now 16,707 points NA

# regraph to visualize changes...
  # set up for graphing
  id_vars <- colnames(dt)
  id_vars <- id_vars[! id_vars %in% c("healthFacilities_total", "healthFacilities_totalOrig")]
  
  dtMelt <- melt(dt, id.vars= c(id_vars), with=F, variable.name="indicator")
  dtMelt$indicator <- as.character(dtMelt$indicator)

    # pdf((paste0(output_dir, "Time Series for Number of Health Facilities (HZ level), Outliers Removed.pdf")), height=6, width=9)   
    # 
    # for (row in 1:nrow(hzDPS)){
    #   g <- ggplot( dtMelt[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]),], aes(x=date, y=value, color=indicator)) + 
    #     
    #     theme_bw() + geom_point() + geom_line(alpha=0.9, size=0.60) + 
    #     
    #     ggtitle(paste0("Time Series for ", hzDPS[row, health_zone], " (", hzDPS[row, dps], ")")) +
    #     
    #     ylab("Total Health Facilities") + xlab("Date") + ylim(0, NA) +
    #     
    #     geom_line( aes(x=date, y=medianFacilities), alpha=0.5, color="gold", size=1) +
    #     
    #     geom_line( aes(x=date, y=medianFacilities+(3*SDFacilities)), alpha=0.5, color="red", size=1, linetype="dashed") +
    #     
    #     geom_line( aes(x=date, y=medianFacilities-(3*SDFacilities)), alpha=0.5, color="red", size=1, linetype="dashed") +
    #     
    #     scale_color_manual(values=c("navy", "tomato"))
    #   
    #   print(g)
    # }
    # 
    # dev.off() 
#--------------------------------------------
  
  
#--------------------------------------------
# fill in/deterministically impute the rest of the total health facilities data
  
  # when there is just one data point for the year, fill it in
  # test <- dt[year %in% c("2010", "2011", "2012") & is.na(healthFacilities_total & !is.na()), healthFacilities_total:=max(healthFacilities_totalOrig, na.rm=T), by=c("dps", "health_zone", "year")]
  # 
  # test <- dt[order("health_zone", "date")]
  # 
  # df <- df[, change_val := (shift(val, 1, 'lag') - val) / shift(val, 1, 'lag')]
  # df <- df[, year_start := shift(year, 1, 'lag')]

# why does something like this not work?? 
# test <- dt[with( dt, order(dps, health_zone, date )),]
# test <- test[, healthFacilities_total := shift(.SD, 1, 'lag'), .SDcols="healthFacilities_total" ]

for (row in 1:nrow(hzDPS)){
  test <- dt[ health_zone==(hzDPS[row, health_zone]) & dps==(hzDPS[row, dps]), ]
  test <- test[with( test, order(date )),]
  for (r in 2:nrow(test)){
    if (is.na(test[r, healthFacilities_total])){
      number = test[(r-1), healthFacilities_total]
      test[r, healthFacilities_total:=number]
    }
  } 
  
  test <- test[with( test, order(date, decreasing=T)),]
  for (r in 2:nrow(test)){
    if (is.na(test[r, healthFacilities_total])){
      number = test[(r-1), healthFacilities_total]
      test[r, healthFacilities_total:=number]
    }
  } 
  # if ( all(is.na(test[year=="2012", healthFacilities_total])) ){
  #   test[year==2012, healthFacilities_total:= test[date=="2013-01-01", healthFacilities_total]]
  # }
  # if ( all(is.na(test[year=="2011", healthFacilities_total])) ){
  #   test[year==2011, healthFacilities_total:= test[date=="2012-01-01", healthFacilities_total]]
  # }
  # if ( all(is.na(test[year=="2010", healthFacilities_total])) ){
  #   test[year==2010, healthFacilities_total:= test[date=="2011-01-01", healthFacilities_total]]
  # }
  if (row==1){
    current <- copy(test)
  } else {
    current <- rbind(current, test)
  }
}

  # write.csv(current, file="J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/detImp_facData.csv")
  
  # merge back with dtOrig 
    # drop columns not needed from current
      current <- current[, .(dps, health_zone, province11_name, date, healthFacilities_total)]
      current$date <- as.Date(current$date)
      dtOrig$date <- as.Date(dtOrig$date)
    # merge:
      finalDT <- merge(dtOrig, current, by=c("dps", "health_zone", "province11_name", "date"), all=T)
      finalDT[, healthFacilities_total.x := NULL]
      setnames(finalDT, "healthFacilities_total.y", "healthFacilities_total")
    # recalculate the proportion to be used in Amelia()
      finalDT$healthFacilities_numReported <- as.numeric(finalDT$healthFacilities_numReported)
      
      finalDT[healthFacilities_numReported> healthFacilities_total, healthFacilities_numReported := NA]
      finalDT[, healthFacilitiesProportion:= healthFacilities_numReported/healthFacilities_total]
    
    # finish prep for Amelia()/MI:
      # convert column types to proper types
      finalDT[, date := as.Date(date)]
      
      # remove V1 column
      finalDT <- finalDT[, V1:=NULL]
      
      # change name of province column
      setnames(finalDT, "province11_name", "province")
      
      # vector of id variables and indicator variables
      all_vars <- c(colnames(finalDT))
      id_vars <- c("province", "dps", "health_zone", "date", "id")
      indicators <- all_vars[!all_vars %in% id_vars] 
      indicators <- indicators[!indicators %in% c("healthFacilities_max", "healthFacilities_totalOrig", "healthFacilities_numReported", "healthFacilities_numReportedWithinDeadline")]
      
      # remove healthFacilities_numReported from the data.table
      finalDT <- finalDT[, c(id_vars, indicators), with=F] 
      
      # when a health zone is entirely na for an indicator over the entire time series,
      # set those NAs to 0 to avoid overimputing
      dtMelt <- melt(finalDT, id.vars= id_vars)
      dtMelt[, value := as.numeric(value)]
      
      dtMeltNAs <- dtMelt[, .(totNAObs = sum(is.na(value))), by= c("dps", "health_zone", "variable" )]
      dtMeltNAs <- dtMeltNAs[totNAObs==96, noData:= T]
      
      dtMerge <- merge(dtMelt, dtMeltNAs, by= c("dps", "health_zone", "variable"))
      
      dtMerge <- dtMerge[noData==T, value:=0]
      
      dtMerge <- dtMerge[, totNAObs:= NULL]
      dtMerge <- dtMerge[, noData:= NULL]
      
      dtMerge[ value < 0, value:= NA]
      
      finalDT <- dcast(dtMerge, province + dps + health_zone + date + id ~ variable, value.var = "value")
      finalDT <- as.data.table(finalDT)
    # write to a file to be used for Amelia
      # write.csv(finalDT, file="J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/final_data_for_impuation.csv")
#--------------------------------------------
  
  
#--------------------------------------------
# re-run graphs
# set up for graphing

  id_vars <- colnames(current)
  id_vars <- id_vars[! id_vars %in% c("healthFacilities_total", "healthFacilities_totalOrig")]
  
  dtMelt <- melt(current, id.vars= c(id_vars), with=F, variable.name="indicator")
  dtMelt$indicator <- as.character(dtMelt$indicator) 
  
pdf((paste0(output_dir, "Time Series for Number of Health Facilities (HZ level), Values Filled-in.pdf")), height=6, width=9)   

for (row in 1:nrow(hzDPS)){
  g <- ggplot( dtMelt[ health_zone==(hzDPS[row, health_zone]) & dps== (hzDPS[row, dps]),], aes(x=date, y=value, color=indicator)) + 
    
    theme_bw() + geom_point() + geom_line(alpha=0.9, size=0.60) + 
    
    ggtitle(paste0("Time Series for ", hzDPS[row, health_zone], " (", hzDPS[row, dps], ")")) +
    
    ylab("Total Health Facilities") + xlab("Date") + ylim(0, NA) +
    
    geom_line( aes(x=date, y=medianFacilities), alpha=0.5, color="gold", size=1) +
    
    geom_line( aes(x=date, y=medianFacilities+(3*SDFacilities)), alpha=0.5, color="red", size=1, linetype="dashed") +
    
    geom_line( aes(x=date, y=medianFacilities-(3*SDFacilities)), alpha=0.5, color="red", size=1, linetype="dashed") +
    
    scale_color_manual(values=c("navy", "tomato"))
  
  print(g)
}

dev.off() 

