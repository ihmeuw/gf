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
rt_dataset <- data.table(read.csv("file path here"))


# ----------------------------------------------
###### Sum up the numeric variables by variables of interest ###### 
# ----------------------------------------------
byVars = names(rt_dataset)[names(rt_dataset)%in%c('gf_module', 'gf_intervention',"code", 'year','start_date', 'period',
                                                'source', 'grant_number', 'disease', 'data_source', 'country')]
rt_dataset  = rt_dataset[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement))
                             ,expenditure=sum(na.omit(expenditure))), by=byVars]

# ----------------------------------------------
###### export as CSV file ###### 
# ----------------------------------------------
write.csv(rt_dataset, "rt_dataset.csv", row.names = FALSE)