# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(stats)
library(stringr)
library(rlang)
library(zoo)


# ----------------------------------------------
###### Load in the resource tracking dataset ###### 
# ----------------------------------------------
#### STEP 1: Download the resource tracking file from Basecamp and save it to your local drive ######

rt_dataset <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv"))

fgh_data <- rt_dataset[data_source=="fgh"]
none_fgh <- rt_dataset[data_source!="fgh"]


fgh_data$start_date <- paste0(fgh_data$year, "-01-01")
fgh_data$start_date  <- as.Date(fgh_data$start_date,"%Y-%m-%d")
fgh_data[, end_date:=start_date+period-1]


none_fgh$start_date  <- as.Date(none_fgh$start_date,"%Y-%m-%d")


rt_dataset <- rbind(none_fgh, fgh_data)


# ----------------------------------------------
###### Sum up the numeric variables by variables of interest ###### 
# ----------------------------------------------

## you can pick whichever variables you want to use in your analysis: 
byVars = names(rt_dataset)[names(rt_dataset)%in%c('gf_module', 'gf_intervention',"code", 'year','start_date', 'period',
                                                'source', 'grant_number', 'disease', 'data_source', 'country', 'data_source')]
rt_dataset  = rt_dataset[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement))
                             ,expenditure=sum(na.omit(expenditure))), by=byVars]

# ----------------------------------------------
###### export as CSV file ###### 
# ----------------------------------------------
write.csv(rt_dataset, "rt_dataset.csv", row.names = FALSE)


