# ----------------------------------------------
# Irena Chen
#
# 2/9/2018
# Template for prepping SICOIN donations data 
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object

# --------------------------------------------------------------
# Set up R

library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(rlang)
library(zoo)
# --------------------------------------------------------------

# start function
prep_donacions_sicoin = function(inFile, start_date, disease, period, source, loc_name, loc_id) {
  
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # --------------------------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <- data.table(read_excel(inFile))
  ## remove empty columns 
  gf_data<- Filter(function(x)!all(is.na(x)), gf_data)
    if(disease=="tb"){
      gf_data <- na.omit(gf_data, cols="X__10")
      budget_dataset <- gf_data[, c("X__3","X__10","X__17", "X__24"), with=FALSE]
      names(budget_dataset) <- c("loc_id", "loc_name","budget", "disbursement")
      budget_dataset$sda_orig <- "All"
    } else if (disease=="hiv"){ 
      if(length(unique(na.omit(gf_data$X__12))) >1){ 
      ## grab loc_id: 
      gf_data$X__14 <- na.locf(gf_data$X__14, na.rm=FALSE)
      gf_data$X__4 <- na.locf(gf_data$X__4, na.rm=FALSE)
      # remove rows where cost_categories are missing values
      if(year(start_date)== 2011 & period==30){ 
        gf_data <- gf_data[c(grep("Banco", gf_data$X__12):grep("FONDO", gf_data$X__12)),]
      } else {
        gf_data <- gf_data[c(grep("Gobierno de", gf_data$X__12):.N),]
      }
      gf_data <- na.omit(gf_data, cols="X__15")
      budget_dataset <- gf_data[, c("X__4","X__14","X__15", "X__22", "X__29"), with=FALSE]
      names(budget_dataset) <- c("loc_id", "loc_name","sda_orig", "budget", "disbursement")
      } else {  ## if there are no other external sources other than GF 
        budget_dataset <- setnames(data.table(matrix(nrow = 1, ncol = 10)), 
                                   c("sda_orig","loc_id","loc_name","budget", "disbursement", 
                                     "source", "period",	"start_date", "disease", "expenditure"))
        budget_dataset$loc_name<- as.character(budget_dataset$loc_name)
        budget_dataset$loc_name <- loc_name
        budget_dataset$loc_id <- loc_id
    }
  }
  toMatch <- c("government", "recursos", "resources", "multire")
  budget_dataset <- budget_dataset[ !grepl(paste(toMatch, collapse="|"), tolower(budget_dataset$loc_name)),]
  
  
  ##enforce variable classes 
  if (!is.numeric(budget_dataset$budget)) budget_dataset[,budget:=as.numeric(budget)]
  if (!is.numeric(budget_dataset$disbursement)) budget_dataset[,disbursement:=as.numeric(disbursement)]
  
  ## in the off chance that there are duplicates by loc_id & sda_orig (NAs in the budget for instance)
  ## this gets rid of them:
  budget_dataset <- budget_dataset[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement))),
                                   by=c("loc_id","loc_name", "sda_orig")]

  # --------------------------------------------------------------
  
  ## Create other variables 
  budget_dataset$source <- source
  budget_dataset$start_date <- start_date
  budget_dataset$period <- period
  budget_dataset$expenditure <- 0 ## change this once we figure out where exp data is
  budget_dataset$disease <- disease
  # ----------------------------------------------
  
  # Enforce variable classes again 
  if (!is.numeric(budget_dataset$expenditure)) budget_dataset[,expenditure:=as.numeric(expenditure)]
  
  # ----------------------------------------------
  # return prepped data
  return(budget_dataset)
}

