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
totalCod <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/cod/prepped/prepped_fpm_budgets.csv",
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
totalGtm <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_fpm_pudr.csv", 
                                fileEncoding = "latin1"))

sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                      ,fileEncoding="latin1"))

##change the start dates from factors to dates: 
sicoin_data$start_date <- as.Date(sicoin_data$start_date,"%Y-%m-%d")
totalGtm$start_date <- as.Date(totalGtm$start_date,"%Y-%m-%d")

## if you want to aggregate the sicoin and FPM data:
# totalGtm$country <- "Guatemala"
# totalGtm <- rbind(sicoin_data, totalGtm)



# --------------------------------------------

##aggregate all country data into one dataset:  

fpmData <- rbind(totalGtm, totalUga, totalCod)
##change the start_date column to be of type "Date"
fpmData$start_date <- as.Date(fpmData$start_date,"%Y-%m-%d")
fpmData$period <- as.numeric(fpmData$period)
fpmData[, end_date:=start_date + period-1]
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
gos_data$adm1 <- gos_data$loc_name
gos_data$adm2 <- gos_data$loc_name
gos_data$lang <- "eng"
gos_data$cost_category <- "all"
##aggregate with gos data: 

totalData <- rbind(fpmData, gos_data)

# --------------------------------------------
#DUPLICATE CHECK: 
d1 <- nrow(totalData)
d2 <- unique(nrow(totalData))

if(d1 !=d2){
  stop("Your dataset has duplicates!")
}

byVars = names(totalData)[!names(totalData)%in%c('budget', 'disbursement', 'expenditure')]
totalData= totalData[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement)),expenditure=sum(na.omit(expenditure))), by=byVars]




# --------------------------------------------
##export to correct folder: 

write.csv(totalData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv", row.names = FALSE)


# --------------------------------------------
###DROP FPM DATA IF IT OVERLAPS WITH GOS DATA: 
# --------------------------------------------
## some of the FPM data is missing so we'll fill it in w/ the FPM data (and drop the FPM data that overlaps): 

##pudrs overlap with the FPM budgets - drop this so we don't double count 
fpmGtm <- totalGtm[!(data_source=="pudr")] ##all of the PUDRs we have correspond to available FPM 
fpmUga <- totalUga[!(data_source=="pudr"&year>2015)] ##we have a lot of recent PUDRs that overlap with FPM 
fpmCod <-  totalCod[!(data_source=="pudr")] #all of the PUDRs we have correspond to available FPM 

cleanData <- rbind(fpmGtm, fpmUga, fpmCod)
cleanData[,end_date:=start_date+period-1]

gos_cod<- gos_data[country=="Congo (Democratic Republic)"]
gos_uga <- gos_data[country=="Uganda"]
gos_gtm <- gos_data[country=="Guatemala"]

gos_cod <- gos_cod[(disease=="hss")|(disease=="hiv"&year<2012|year%in%c(2013, 2014))|(disease=="malaria"&(year<=2014))|(disease=="tb"&grant_number!="COD-T-MOH")]
gos_uga <- gos_uga[(disease=="hiv"&(year<2011|year==2014))|(disease=="malaria"&(year%in%c(2013,2014)|year<2012))|(disease=="tb"&(year<2012|year%in%c(2013,2014)))]
gos_gtm <- gos_gtm[(disease=="hiv"&year<2011)|(disease=="malaria"&year<2011)|(disease=="tb"&(year<2011|year==2015))]


totalGos <- rbind(gos_uga, gos_cod, gos_gtm)

cleanData <- rbind(cleanData, totalGos)


write.csv(cleanData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/cleaned_total_data.csv", row.names = FALSE)

