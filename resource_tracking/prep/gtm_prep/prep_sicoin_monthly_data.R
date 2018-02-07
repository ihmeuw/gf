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
  
  # ----------------------------------------------
  ## remove empty columns 
 if (source=="gf") {
    if(year(start_date)==2012&disease=="malaria"){
      gf_data <- na.omit(gf_data, cols=c("X__10"))
      gf_data [,disb1:=as.numeric(X__25)]
      gf_data [,disb2:=as.numeric(X__26)]
      gf_data $disbursement <- coalesce(gf_data $disb1, gf_data$disb2)
      gf_data <- na.omit(gf_data, cols=c("X__19", "disbursement"))
      budget_dataset<- gf_data[, c( "X__10", "X__19", "disbursement"), with=FALSE]
      names(budget_dataset) <- c("loc_id", "budget", "disbursement")
      budget_dataset$sda_orig <- "REGISTRO, CONTROL Y VIGILANCIA DE LA MALARIA"
      
    } else if (month(start_date)==12){
        gf_data$X__11 <- na.locf(gf_data$X__11, na.rm=FALSE)
        gf_data$X__10 <- na.locf(gf_data$X__10, na.rm=FALSE)
        budget_dataset <- gf_data[, c("X__10", "X__11","X__19", "X__25"), with=FALSE]
        names(budget_dataset) <- c("sda_orig", "loc_id", "budget","disbursement")
    } else {
        gf_data$X__11 <- na.locf(gf_data$X__11, na.rm=FALSE)
        gf_data$X__10 <- na.locf(gf_data$X__10, na.rm=FALSE)
        budget_dataset <- gf_data[, c("X__10", "X__11", "X__25"), with=FALSE]
        names(budget_dataset) <- c("sda_orig", "loc_id", "disbursement")
        budget_dataset$budget <- 0
    }
    # remove rows where cost_categories are missing values
    budget_dataset <- na.omit(budget_dataset, cols="loc_id")
    setkeyv(budget_dataset,c("sda_orig", "loc_id"))
    budget_dataset <- unique(budget_dataset)
    ##get rid of extra rows where there are NAs or 0s: 
    budget_dataset  <- budget_dataset[, list(budget=sum(na.omit(budget)),
                                             disbursement=sum(na.omit(disbursement))),by=c("sda_orig", "loc_id")]
  }
  toMatch <- c("government", "recursos", "resources", "multire")
  budget_dataset <- budget_dataset[ !grepl(paste(toMatch, collapse="|"), tolower(budget_dataset$loc_id)),]
  
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