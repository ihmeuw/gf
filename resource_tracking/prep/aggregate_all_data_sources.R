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
library(readxl)

# ----------------------------------------------

##function to turn columns into date type variables: 


###DRC: 
totalCod <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/cod/prepped/all_fpm_budgets.csv",
                                fileEncoding="latin1"))

#create some variables: 
totalCod$country <- "Congo (Democratic Republic)"
totalCod$start_date <- as.Date(totalCod$start_date,"%Y-%m-%d")

# --------------------------------------------
##load UGA: 

totalUga <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/uga/prepped/prepped_uga_data.csv",
                                fileEncoding = "latin1"))

#create some variables: 
totalUga$country <- "Uganda"
totalUga$start_date <- as.Date(totalUga$start_date,"%Y-%m-%d")
totalUga$year <- year(totalUga$start_date)

# --------------------------------------------
##load GTM 
gtmBudgets <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_fpm_pudr.csv", 
                                fileEncoding = "latin1"))

sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                      ,fileEncoding="latin1"))

##change the start dates from factors to dates: 
sicoin_data$start_date <- as.Date(sicoin_data$start_date,"%Y-%m-%d")
gtmBudgets$start_date <- as.Date(gtmBudgets$start_date,"%Y-%m-%d")
gtmBudgets$country <- "Guatemala"
###: technically not the country, but we're keeping the loc ids attached to the sicoin data
##so it will map to a municipality anyway 
setnames(sicoin_data, "loc_name", "country")
sicoin_data$intervention <- "All"
sicoin_data$sda_activity <- "All"
sicoin_data$grant_number <- "none"
sicoin_data$recipient <- sicoin_data$country
##rbind the sicoin and FPM data:
totalGtm <- rbind(sicoin_data, gtmBudgets)
totalGtm$year <- year(totalGtm$start_date)



# --------------------------------------------

##rbind with UGA data: 

totalData <- rbind(totalGtm, totalUga, totalCod)
##change the start_date column to be of type "Date"
totalData$start_date <- as.Date(totalData$start_date,"%Y-%m-%d")
totalData$period <- as.numeric(totalData$period)
totalData[, end_date:=start_date + period-1]
# --------------------------------------------
##read the already mapped gos data: 
gos_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_gos_data.csv", 
                                fileEncoding = "latin1"))

##change the dates into date format: 
gos_data$start_date <- as.Date(gos_data$start_date, "%Y-%m-%d")
gos_data$end_date <- as.Date(gos_data$end_date, "%Y-%m-%d")
gos_data[, period:=end_date - start_date]
gos_data$period <- as.integer(gos_data$period)

##since we don't have subnational data for GOS, just make it a copy of the country variable: 
gos_data$disbursement <- 0

##aggregate with gos data: 

totalData <- rbind(totalData, gos_data)

# --------------------------------------------
#DUPLICATE CHECK: 
d1 <- nrow(totalData )
d2 <- unique(length(totalData))

if(d1 !=d2){
  stop("Your dataset has duplicates!")
}


# --------------------------------------------
##export to correct folder: 

write.csv(totalData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv", row.names = FALSE)


# --------------------------------------------
###DROP FPM DATA IF IT OVERLAPS WITH GOS DATA: 

## some of the GOS data is missing so we'll fill it in w/ the FPM data (and drop the FPM data that overlaps): 

fpmCod<- totalCod[!((year < 2016 &disease=="malaria") | (year < 2015 & disease=="hiv") | (year < 2017 & disease=="tb"))]
fpmUga <- totalUga[!((year < 2016 &disease%in%c("hss", "tb", "hiv"))
                     | (year < 2017 & disease%in%c("malaria")))]
fpmGtm <- totalGtm[!((year < 2016 &disease=="tb") | (year < 2017 & disease%in%c("malaria", "hiv")))]


totalFpm <- rbind(fpmCod, fpmUga, fpmGtm)
totalFpm $start_date <- as.Date(totalFpm $start_date,"%Y-%m-%d")
totalFpm[, end_date:=start_date + period-1]
cleaned_aggregate_data <- rbind(totalFpm, gos_data)

byVars = names(cleaned_aggregate_data)[names(cleaned_aggregate_data)%in%c('program_activity', 'year', 'start_date', 'period', 'country', 
                                                                          'grant_number', 'disease', 'data_source', 'source', 'recipient', 'loc_id')]

cleaned_aggregate_data <- cleaned_aggregate_data[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure)), disbursement=sum(na.omit(disbursement))), by=byVars]


write.csv(cleaned_aggregate_data, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/cleaned_total_data.csv", row.names = FALSE)

