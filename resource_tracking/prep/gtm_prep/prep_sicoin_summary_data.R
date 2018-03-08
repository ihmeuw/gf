# ----------------------------------------------
# Irena Chen
#
# 11/2/2017
# Template for prepping SICOIN cost category data 
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object

# ----------------------------------------------
# Set up R

library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(rlang)
library(zoo)
library(lubridate)

# start function
prep_summary_sicoin = function(inFile, start_date, disease, period, source) {
  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <- data.table(read_excel(inFile)) 
  colnames(gf_data)[3] <- "loc_id"
  gf_data$loc_id <- na.locf(gf_data$loc_id, na.rm=FALSE)
  gf_data$X__3 <- na.locf(gf_data$X__3, na.rm=FALSE)
  gf_data$X__11 <- na.locf(gf_data$X__11, na.rm=FALSE)
  gf_data$X__10 <- na.locf(gf_data$X__10, na.rm=FALSE)
  # ----------------------------------------------
  ## remove empty columns 
  if (source=="gf") {
    if(year(start_date)%in%c(2012, 2013)&disease=="malaria"){
      gf_data <- na.omit(gf_data, cols=c("X__23", "X__25"))
      budget_dataset<- gf_data[, c("X__3","X__10", "X__23", "X__25"), with=FALSE]
      names(budget_dataset) <- c("loc_id","loc_name", "budget", "disbursement")
      budget_dataset$sda_orig <- "REGISTRO, CONTROL Y VIGILANCIA DE LA MALARIA"
      
    } else {
      budget_dataset <- gf_data[, c("loc_id", "X__10", "X__11","X__23", "X__25"), with=FALSE]
      names(budget_dataset) <- c("loc_id","sda_orig", "loc_name", "budget","disbursement")
    }
    # remove rows where cost_categories are missing values
    budget_dataset <- na.omit(budget_dataset, cols=c("loc_name", "budget"))
    budget_dataset <- unique(budget_dataset, by=c("loc_id","loc_name", "sda_orig"))
    ##get rid of extra rows where there are NAs or 0s: 
    budget_dataset  <- budget_dataset[, list(budget=sum(na.omit(budget)),
                                             disbursement=sum(na.omit(disbursement))),by=c("loc_id","sda_orig", "loc_name")]
  }
  toMatch <- c("government", "recursos", "resources", "multire", "multimu")
  budget_dataset <- budget_dataset[ !grepl(paste(toMatch, collapse="|"), tolower(budget_dataset$loc_name)),]
  
  # ----------------------------------------------
  
  ## Create other variables 
  budget_dataset$source <- source
  budget_dataset$start_date <- start_date
  budget_dataset$period <- period
  budget_dataset$expenditure <- 0 ## change this once we figure out where exp data is
  budget_dataset$disease <- disease
  # ----------------------------------------------
  
  # Enforce variable classes
  if (!is.numeric(budget_dataset$budget)) budget_dataset[,budget:=as.numeric(budget)]
  if (!is.numeric(budget_dataset$disbursement)) budget_dataset[,disbursement:=as.numeric(disbursement)]
  if (!is.numeric(budget_dataset$expenditure)) budget_dataset[,expenditure:=as.numeric(expenditure)]
  
  # ----------------------------------------------
  
  # return prepped data
  return(budget_dataset)
  
}