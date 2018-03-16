

# ----------------------------------------------
# Irena Chen
#
# 11/27/2017
# ### GOS DATA graphs 

# TO DO
# fix time series graph so that there are gaps where appropriate (use `group` aesthetic)

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
## prep data 


gos_data  <- data.table(read_excel('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/Expenditures from GMS and GOS for PCE IHME countries.xlsx',
                                   sheet=as.character('GOS Mod-Interv - Extract')))

oldNames <-  c("Country","Grant Number", "Year", "Financial Reporting Period Start Date",
               "Financial Reporting Period End Date","Module", "Intervention", "Total Budget Amount (in budget currency)", 
               "Total Expenditure Amount (in Budget currency)", "Component")
newNames <-  c("country","grant_number", "year","start_date","end_date","module","intervention", 
               "budget", "expenditure", "disease")

setnames(gos_data,oldNames, newNames)

gos_clean <- gos_data[, newNames, with=FALSE]

gos_intervention <- copy(gos_data)

gms_data  <- data.table(read_excel('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/Expenditures from GMS and GOS for PCE IHME countries.xlsx',
                                   sheet=as.character('GMS SDAs - extract')))

gmsOld <- c(oldNames[1:5], "Service Delivery Area", "Total Budget Amount (USD equ)", "Total Expenditure Amount (USD equ)", "Component")
gmsNew <- c(newNames[1:6], newNames[8:10])

setnames(gms_data, gmsOld, gmsNew)
gms_clean <- gms_data[, gmsNew, with=FALSE]
gms_clean$intervention <- "All"

totalGos <- rbind(gms_clean, gos_clean)

map_disease <- unique(totalGos$disease)
names(map_disease) <- c("tb", "malaria", "hiv", "hss", "hiv/tb")
  
kDT = data.table(map_disease = names(map_disease), value = TRUE, disease = unname(map_disease))
totalGos[kDT, on=.(disease), disease := i.map_disease]
totalGos[disease=='hiv/tb', disease:='hiv']

totalGos$sda_activity <- "All"
totalGos$recipient <- totalGos$grant_number
totalGos$data_source <- "gos"
totalGos$source <- "gf"
totalGos$loc_id <- totalGos$country

write.csv(totalGos, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_gos_data.csv",
          row.names = FALSE, fileEncoding = "latin1")



