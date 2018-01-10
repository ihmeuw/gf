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

fpm_cod_budgets<- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/cod/prepped/mapped_fpm_budgets_1418.csv", 
                                       fileEncoding="latin1"))
mappedRej <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/cod/prepped/rejected_cod_budgets_1518.csv", 
                                 fileEncoding = "latin1"))


##we want to merge the old (current) and new (upcoming) budgets: 
fpm_cod_budgets$start_date <- as.Date(fpm_cod_budgets$start_date,"%Y-%m-%d")
fpm_cod_budgets[, end_date:=start_date + period-1]
fpm_cod_budgets$year <- year(fpm_cod_budgets$start_date)
## some of the GOS data is missing so we'll fill it in w/ the FPM data (and drop the FPM data that overlaps): 
## we need 2015 data for HIV, 2016 data for malaria, and 2017+ for all of them: 
fpmCod<- fpm_cod_budgets[!((year < 2016 &disease=="malaria") | (year < 2015 & disease=="hiv") | (year < 2017 & disease=="tb"))]

##drop some of the variables in order to aggregate the reject + approved: 
names= names(fpmCod)[names(fpmCod)%in%names(mappedRej)]
fpmCod[, names, with=FALSE]

##aggregate the rejected and approved budgets: 
fpmCod <- rbind(mappedRej, fpm_cod_budgets)


##write to csv as a backup: 
write.csv(fpmCod, "J:/Project/Evaluation/GF/resource_tracking/cod/prepped/rej_and_approved_cod_fpm_1518.csv", 
          row.names = FALSE, fileEncoding = "latin1")

# --------------------------------------------
##UGA: 

mappedUga <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/uga/prepped/all_fpm_mapped_budgets.csv",
                                 fileEncoding = "latin1"))


mappedUga $start_date <- as.Date(mappedUga$start_date,"%Y-%m-%d")
#create some variables: 
mappedUga[, end_date:=start_date + period-1]
mappedUga$country <- "Uganda"
mappedUga$year <- year(mappedUga$start_date)

# --------------------------------------------
###DROP FPM DATA IF IT OVERLAPS WITH GOS DATA: 

fpmUga <- mappedUga[!((year < 2016 &disease%in%c("hss", "tb", "hiv")) | (year < 2017 & disease%in%c("malaria")))]

##rbind with DRC data: 

totalData <- rbind(fpmUga, fpmCod)
# --------------------------------------------
##GTM 


fpmGua <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/fpm_mapped_budgets_1818.csv", 
                                 fileEncoding = "latin1"))

##change the start_date column to be of type "Date"
fpmGua $start_date <- as.Date(fpmGua$start_date,"%Y-%m-%d")
fpmGua [, end_date:=start_date + period-1]

# and also create a "year" variable: 
fpmGua$year <- year(fpmGua$start_date)

##we need 2016 data for TB and post 2016 data for HIV/Malaria - we can drop everything else since the GOS data has it: 
totalGua<- fpmGua[!((year < 2016 &disease=="tb") | (year < 2017 & disease%in%c("malaria", "hiv")))]


##rbind with UGA data: 

totalData <- rbind(totalGua, totalData)
# --------------------------------------------
##read the already mapped gos data: 
gos_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/mapped_gos_data_1918.csv", 
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
gos_data$gf_program <- NULL ##don't need this

##aggregate with gos data: 

totalData <- rbind(totalData, gos_data)



##export to correct folder: 

write.csv(totalData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_mapped_data.csv", row.names = FALSE)



