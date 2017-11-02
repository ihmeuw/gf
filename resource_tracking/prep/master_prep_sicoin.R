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

# ----------------------------------------------

# load csv
file_list <- read.csv("C:/Users/irenac2/repos/gf/resource_tracking/prep/file_format_list.csv")

source('./prep_sicoin.r')

file_names <- as.character(file_list$filename[file_list$format=="c_coin"])
file_years <- file_list$year[file_list$format=="c_coin"]


# inFile <- '2013 MALARIA PRESUPUESTO POR ORGANISMO (departamento municipio)'

resource_database <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), 
                          c("loc_id","budget","disbursement", "source", 
                            "start_date","period", "cost", "expenditures"))

## loop over all of the files 
for(i in 1:length(file_names)) {
  tmpData <- prepSicoin(dir, file_names[i], file_years[i], period, cost_category, source)
  resource_database = rbind(resource_database, tmpData, use.names=TRUE, fill=TRUE)
  ## trouble shoot this area b/c of error issues w/out fill=TRUE
}


# data directory
dir <- 'C:/Users/irenac2/Documents/'

# input file
inFile <- paste0(dir, '2013 MALARIA PRESUPUESTO POR ORGANISMO (departamento municipio).xls')
