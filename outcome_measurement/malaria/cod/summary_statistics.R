# ----------------------------------------------
  # Audrey Batzel
  #
  # 4/30/18
  # COD PNLP data for 2014-2016; descriptive statistics, percent change from year to year by health zone
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
  library (dplyr)
# --------------------  


# ----------------------------------------------
  # Overview - Files and Directories
  
  # data directory
    dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/"
  
  # import graphDataComplete
    dt <- as.data.table(read.csv(paste0(dir, "Imputed Data.csv")))

  # output files:
    output_dir <- "J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_Data/"
    output_data <- "J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/"
# ----------------------------------------------
    
    
# ----------------------------------------------
  # aggregate every HZ-indicator to the year level and see what the percent change from 2015 to 2016 
  # and 2016 to 2017 is and whether there are any that stand out as very different.
    
    dt$year <- year(dt$date)
    dt$month <- month(dt$date)

    aggByYear <- dt[, .(yearValue = sum(mean)), by=c( "year", "health_zone", "dps", "province", "indicator", "subpopulation")]
    
    aggByYear$indicator = paste(aggByYear$indicator, aggByYear$subpopulation, sep="_")
    
    indicators <- c("newCasesMalariaMild_under5", "newCasesMalariaMild_5andOlder", "newCasesMalariaMild_pregnantWomen", "newCasesMalariaSevere_under5", "newCasesMalariaSevere_5andOlder", "newCasesMalariaSevere_pregnantWomen",
                    "mildMalariaTreated_under5", "mildMalariaTreated_5andOlder", "mildMalariaTreated_pregnantWomen",
                    "severeMalariaTreated_under5", "severeMalariaTreated_5andOlder", "severeMalariaTreated_pregnantWomen",
                    "malariaDeaths_under5", "malariaDeaths_5andOlder", "malariaDeaths_pregnantWomen")

    interventions <- c("ANC_1st", "ANC_2nd", "ANC_3rd", "ANC_4th", "SP_1st", "SP_2nd","SP_3rd", "ITN_received", "ITN_distAtANC",
                       "ITN_distAtPreschool", "VAR_NA", "ASAQ_2to11mos", "ASAQ_1to5yrs", "ASAQ_6to13yrs", "ASAQ_14yrsAndOlder", "ArtLum_received", "ArtLum_used",
                       "smearTest_completed", "smearTest_positive", "RDT_completed", "RDT_positive", "healthFacilities_total")
    
    measuredVars <- c(indicators, interventions)
    
    dtWide <- dcast(aggByYear, year + province + dps + health_zone ~ indicator, value.var="yearValue")
# ----------------------------------------------


# ----------------------------------------------
 # one way to calculate percent change:       
  rateOfChange <- aggByYear %>%
        group_by(indicator) %>%
        arrange(health_zone, year, indicator) %>%
        mutate(rate = 100 * (yearValue - lag(yearValue))/lag(yearValue)) %>%
        ungroup()
      
  rateOfChangeWide <- dcast(rateOfChange, year + province + dps + health_zone ~ indicator, value.var="rate")
  
  
  highValues <- rateOfChange[rate>95, c("health_zone", "year", "rate")]
  
# ----------------------------------------------


# ----------------------------------------------
  # another way to calculate percent change from year to year
  # by health zone
  id_vars <- colnames(aggByYear)[!colnames(aggByYear) %in% c("year", "yearValue")]
  
  # data table sort, with the last variable the one you want to measure rate of change across
  # sort data table so that it is grouped by hz, indicator, subpopulation from one year to the next
  rateOfChange <- aggByYear[
                      with( aggByYear, order(health_zone, dps, province, indicator, subpopulation, year )),
                  ]
  # percent change = ((new value - old value) / old value) * 100
  # use shift function to shift one row up on the sorted data.table to calculate percent change
  rateOfChange <- rateOfChange[, percentChange := ((yearValue - shift(yearValue, 1, 'lag')) / (shift(yearValue, 1, 'lag'))) * 100 ]
  
  # delete instances where percent change is calculated from one indicator to the next isntead of across the same indicator
    rateOfChange <- rateOfChange[, year_start := shift(year, 1, 'lag')]
    setnames(rateOfChange, "year", "year_end")
  # this would take into account if the min year was different for different dps or indicators
  # won't actually be because we rectangularized the data before imputation
    rateOfChange <- rateOfChange[, min_year:=min(year_end), by=c("health_zone", "dps", "province", "indicator", "subpopulation")]
    rateOfChange <- rateOfChange[year_end==min_year, percentChange := NA]
    
    rateOfChange <- rateOfChange[, c(id_vars, "year_start", "year_end", "yearValue", "percentChange"), with=F]
    rateOfChange <- rateOfChange[year_start>year_end, year_start:=NA]  
    setnames(rateOfChange, "yearValue", "year_endValue")
    
    
  # export as csv
    write.csv(rateOfChange, paste0(output_data, "Percent Change for each Indicator by Health Zone.csv"))
    
# ----------------------------------------------


# ----------------------------------------------
  # calculate percent change for each DPS
  
  # first aggByYear -> aggByDPS
    aggByDPS <- aggByYear[, .(dpsYearValue = sum(yearValue)), by= c("year", "dps", "province", "indicator", "subpopulation")]
  
    rateOfChangeDPS <- aggByDPS[
                        with( aggByDPS, order(dps, province, indicator, subpopulation, year )),
                      ]
  
    rateOfChangeDPS <- rateOfChangeDPS[, percentChange := ((dpsYearValue - shift(dpsYearValue, 1, 'lag')) / (shift(dpsYearValue, 1, 'lag'))) * 100 ]
    
    # delete instances where percent change is calculated from one indicator to the next isntead of across the same indicator
      rateOfChangeDPS <- rateOfChangeDPS[, year_start := shift(year, 1, 'lag')]
      setnames(rateOfChangeDPS, "year", "year_end")
    # this would take into account if the min year was different for different dps or indicators
    # won't actually be because we rectangularized the data before imputation
      rateOfChangeDPS <- rateOfChangeDPS[, min_year:=min(year_end), by=c( "dps", "province", "indicator", "subpopulation")]
      rateOfChangeDPS <- rateOfChangeDPS[year_end==min_year, percentChange := NA]
      
      rateOfChangeDPS <- rateOfChangeDPS[, c("dps", "province", "indicator", "subpopulation", "year_start", "year_end", "dpsYearValue", "percentChange"), with=F]
      rateOfChangeDPS <- rateOfChangeDPS[year_start>year_end, year_start:=NA]  
      setnames(rateOfChangeDPS, "dpsYearValue", "year_endValue")
      
# ----------------------------------------------
      
      
# ----------------------------------------------
  # calculate percent change for country level
     # first aggByYear -> agg
      agg <- aggByYear[, .(yearValue = sum(yearValue)), by= c("year", "indicator", "subpopulation")]
      
      rateOfChangeCountry <- agg[
        with( agg, order( indicator, subpopulation, year )),
        ]
      
      rateOfChangeCountry <- rateOfChangeCountry[, percentChange := ((yearValue - shift(yearValue, 1, 'lag')) / (shift(yearValue, 1, 'lag'))) * 100 ]
      
      # delete instances where percent change is calculated from one indicator to the next isntead of across the same indicator
      rateOfChangeCountry <- rateOfChangeCountry[, year_start := shift(year, 1, 'lag')]
      setnames(rateOfChangeCountry, "year", "year_end")
      # this would take into account if the min year was different for different dps or indicators
      # won't actually be because we rectangularized the data before imputation
      rateOfChangeCountry <- rateOfChangeCountry[, min_year:=min(year_end), by=c( "indicator", "subpopulation")]
      rateOfChangeCountry <- rateOfChangeCountry[year_end==min_year, percentChange := NA]
      
      rateOfChangeCountry <- rateOfChangeCountry[, c("indicator", "subpopulation", "year_start", "year_end", "yearValue", "percentChange"), with=F]
      rateOfChangeCountry <- rateOfChangeCountry[year_start>year_end, year_start:=NA]  
      setnames(rateOfChangeCountry, "yearValue", "year_endValue")
  
      
     rateOfChange <- rateOfChange[!is.na(rateOfChange$year_start)]
# ----------------------------------------------
     
     
# ---------------------------------------------- 
    # make histograms of percent change values
    
    pdf((paste0(output_dir,"Histograms for Percent Change by HZ.pdf")), height=6, width=10)  
    for (i in measuredVars){
      g <- ggplot(rateOfChange[indicator==i,], aes(percentChange)) + 
        geom_histogram(bins = 50, fill="cornflowerblue") + 
        ggtitle(paste0("Percent Change Values for ", i," at the health zone level" )) + xlab("Percent Change") + ylab("Count") +
        geom_vline(xintercept=rateOfChangeCountry[indicator==i & year_end==2015, percentChange], color="green", linetype="dashed") + 
        geom_vline(xintercept=rateOfChangeCountry[indicator==i & year_end==2016, percentChange], color="red", linetype="dashed") + 
        facet_wrap (~year_end, scales='free')
      print(g)   
    }
    dev.off()
      

    