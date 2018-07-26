
# ----------------------------------------------
# Irena Chen
#
# 4/20/2018
# ### Create a stratified random sample of activity descriptions, by language and disease: 
# ----------------------------------------------
# Set up R
library(ggplot2)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(readxl)
library(dplyr)
library(caret)



# ----------------------------------------------
totalData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv',
                                 fileEncoding = "latin1"))


fpmData <- totalData[data_source=="fpm"]

sda_data <-fpmData[!sda_activity%in%c("All","all")]
sda_data <- sda_data[!(is.na(sda_activity))]

##drop HSS for now since it creates imbalance (no HSS grant with GTM)
sda_data <- sda_data[disease!="hss"]
##concatenate the disease and language variables to create a multi-level stratification
sda_data$disease_lang_concat <- paste0(sda_data$disease, sda_data$lang)

sda_data <- sda_data [, c("gf_module", "gf_intervention", "module", "intervention",
                          "cost_category","sda_activity", "budget", "disease_lang_concat"), with=FALSE]

##only have 9 values for this, so drop: 
sda_data <- sda_data[!disease_lang_concat=="malariaesp"]

set.seed(2)
train.index  <- createDataPartition(sda_data$disease_lang_concat, p = 0.004, list = FALSE)
training_sample <-sda_data[ train.index,]
test_sample  <- sda_data[-train.index,]

write.csv(training_sample, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/nlp_training_sample.csv",
          row.names=FALSE)

write.csv(test_sample, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/nlp_test_sample.csv",
          row.names=FALSE)














