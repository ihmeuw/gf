
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



# ----------------------------------------------
totalData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv',
                                 fileEncoding = "latin1"))


fpmBudgets <- totalData[data_source=="fpm"]

sda_data <- fpmBudgets[sda_activity!="All"]
sda_data <- sda_data[!(is.na(sda_activity))]

# ----------------------------------------------
##keep HSS for now (technically it's a disease): 

sda_data$disease_lang_concat <- paste0(sda_data$disease, sda_data$lang)


set.seed(1)
sda_sample <- lapply(split(sda_data,sda_data$disease_lang_concat),
                     function(subdf) subdf[sample(1:nrow(subdf),30),]
)


total_sample <- do.call('rbind', sda_sample)

write.csv(total_sample, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/nlp_sample_with_hss.csv",
          row.names=FALSE)

# ----------------------------------------------
##drop HSS for now since it creates imbalance (no HSS grant with GTM)
set.seed(2)
sda_data <- sda_data[!(is.na(sda_activity))]
sda_no_hss <- sda_data[disease!="hss"]

sda_no_hss$disease_lang_concat <- paste0(sda_no_hss$disease, sda_no_hss$lang)

sda_no_hss_sample <- lapply(split(sda_no_hss,sda_no_hss$disease_lang_concat),
                     function(subdf) subdf[sample(1:nrow(subdf),30),]
)


total_no_hss_sample <- do.call('rbind', sda_no_hss_sample)



write.csv(total_no_hss_sample, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/nlp_sample.csv",
          row.names=FALSE)














