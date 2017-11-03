# ----------------------------------------------
# Irena Chen
#
# 11/1/2017
# Master prep code that runs all other functions
# The current working directory should be the same as this code
# ----------------------------------------------

# Set up R
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(rlang)

# ----------------------------------------------
#define variables: 

dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/ghe_s/'
period <-365
cost_category <- "All"
source <- "gf"
disease <- "malaria"
loc_id <- "gtm"

# ----------------------------------------------

# load csv
file_list <- read.csv("C:/Users/irenac2/repos/gf/resource_tracking/prep/file_format_list.csv")

source('./prep_sicoin.r')

## loop over all of the files 
for(i in 1:length(file_list$filename)){
  if(file_list$format[i]=="c_coin_muni"){
  tmpData <- prepSicoin(dir, as.character(file_list$filename[i]), file_list$year[i], disease, period, cost_category, source)
  } else { 
  tmpData <- prep_cost_sicoin(dir, as.character(file_list$filename[i]), file_list$year[i], disease, period, loc_id, source)}
  if(i==1){
    resource_database = tmpData
  }
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE, fill=TRUE)
  }
}


