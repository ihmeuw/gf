# ----------------------------------------------
# Irena Chen
#
# 10/31/2017
# Template for prepping C-COIN budget data 
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(rlang)
# --------------------


# ----------------------------------------------
# Files and directories

# data directory
dir <- 'C:/Users/irenac2/Documents/'

# input file
inFile <- paste0(dir, '2013 MALARIA PRESUPUESTO POR ORGANISMO (departamento municipio).xls')

# output files
modelOutputFile <- paste0(dir, 'output.rdata')

# Load/prep data

# load excel data 
gf_data <- read_excel(inFile)

# ----------------------------------------------
##define some variables - change these when appropriate


budget_period <- 365 
budget_source <- 'gf'
budget_cost <- "All"

## variable for the year 
budget_year <- paste(c(gf_data$X__8[13],"01","01"), collapse="-")

# ----------------------------------------------
## remove empty columns 
gf_data<- Filter(function(x)!all(is.na(x)), gf_data)


##pull all rows from between columns that have "FONDO MUNDIAL" in them 

gf_data <- gf_data[c(grep("FONDO MUNDIAL", gf_data$X__10):(grep("FONDO MUNDIAL", gf_data$X__6))),]


# remove rows with "TOTAL"  -> should be able to calculate total from summing municipaliies

gf_subset <- data.table(gf_data[ grep("TOTAL", gf_data$X__3, invert = TRUE) , ])

# remove rows where X__10 (municipalities) are missing values
gf_subset <- na.omit(gf_subset, cols="X__10")

# ----------------------------------------------
## Code to aggregate into a dataset 

## now get region + budgeted expenses 
budget_dataset <- gf_subset[, c("X__10", "X__19", "X__26"), with=FALSE]
names(budget_dataset) <- c("loc_id", "vigente", "devengado")

## we only want the municpalities so get rid of GF and Guatemala

toMatch <- c("MUNDIAL", "GUATEMALA")

for (i in 1:length(toMatch)){
  budget_dataset <- budget_dataset[grep(string(toMatch[i]),budget_dataset$loc_id, invert = TRUE),]
}
# ----------------------------------------------

## Create other variables 

## will want to change this part of the code when appending multiple budgets: 
budget_dataset$source <- budget_source
budget_dataset$start_date <- as.Date(budget_year, origin="1960-01-01")
budget_dataset$period <- budget_period
budget_dataset$cost <- budget_cost

# ----------------------------------------------
