# ----------------------------------------------
# Irena Chen
#
# 1/5/2018

### This code is to AGGREGATE all data sources from all countries: 

# ----------------------------------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(readxl)

# ----------------------------------------------

##function to turn columns into date type variables: 


###DRC: 
totalCod <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/cod/prepped/all_fpm_cod_budgets.csv",
                                fileEncoding="latin1"))

#create some variables: 
totalCod$country <- "Congo (Democratic Republic)"
totalCod$source <- "gf"
# --------------------------------------------
##load UGA: 

totalUga <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/uga/prepped/all_fpm_mapped_budgets.csv",
                                 fileEncoding = "latin1"))

#create some variables: 
totalUga$country <- "Uganda"
totalUga$year <- year(totalUga$start_date)

# --------------------------------------------
##load GTM 
totalGtm <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/fpm_mapped_budgets_1818.csv", 
                                 fileEncoding = "latin1"))


# and also create a "year" variable: 
totalGtm$year <- year(totalGtm$start_date)
totalGtm$country <- "Guatemala"


# --------------------------------------------

##rbind with UGA data: 

totalData <- rbind(totalGtm, totalUga, totalCod)
##change the start_date column to be of type "Date"
totalData$start_date <- as.Date(totalData$start_date,"%Y-%m-%d")
totalData[, end_date:=start_date + period-1]
# --------------------------------------------
##read the already mapped gos data: 
gos_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapped_gos_data.csv", 
                              fileEncoding = "latin1"))

##change the dates into date format: 
gos_data$start_date <- as.Date(gos_data$start_date, "%Y-%m-%d")
gos_data$end_date <- as.Date(gos_data$end_date, "%Y-%m-%d")
gos_data[, period:=end_date - start_date]
gos_data$period <- as.integer(gos_data$period)

##since we don't have subnational data for GOS, just make it a copy of the country variable: 
gos_data$loc_id <- gos_data$country
gos_data$disbursement <- 0
gos_data$recipient <- gos_data$grant_number
gos_data$data_source <- "gos"
gos_data$source <- "gf"
gos_data$X <- NULL

##aggregate with gos data: 

totalData <- rbind(totalData, gos_data)

##export to correct folder: 

write.csv(totalData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_mapped_data.csv", row.names = FALSE)


# --------------------------------------------
###DROP FPM DATA IF IT OVERLAPS WITH GOS DATA: 

## some of the GOS data is missing so we'll fill it in w/ the FPM data (and drop the FPM data that overlaps): 

fpmCod<- totalCod[!((year < 2016 &disease=="malaria") | (year < 2015 & disease=="hiv") | (year < 2017 & disease=="tb"))]
fpmUga <- totalUga[!((year < 2016 &disease%in%c("hss", "tb", "hiv")) | (year < 2017 & disease%in%c("malaria")))]
fpmGtm <- totalGtm[!((year < 2016 &disease=="tb") | (year < 2017 & disease%in%c("malaria", "hiv")))]


totalFpm <- rbind(fpmCod, fpmUga, fpmGtm)
totalFpm $start_date <- as.Date(totalFpm $start_date,"%Y-%m-%d")
totalFpm[, end_date:=start_date + period-1]

cleaned_aggregate_data <- rbind(totalFpm, gos_data)

write.csv(cleaned_aggregate_data, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/cleaned_total_data.csv", row.names = FALSE)

