# ----------------------------------------------
# Irena Chen
#
# 11/2/2017
# Template for prepping C-COIN cost category data 
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------

# start function
prep_cost_sicoin = function(dir, inFile, year, disease, period, loc_id, source) {


  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <- read_excel(paste0(dir,inFile, '.xls'))
  
  # ----------------------------------------------
  ## remove empty columns 
  gf_data<- Filter(function(x)!all(is.na(x)), gf_data)
  
  ##pull just the cost categories 
  gf_data <- gf_data[c(grep("SERVICIOS", gf_data$X__14):(grep("11130009-0283", gf_data$X__4))),]

  ## grab loc_id: 
  gf_subset$X__13 <- na.locf(gf_subset$X__13, fromLast = TRUE)
  
  # remove rows where cost_categories are missing values
  gf_subset <- na.omit(gf_subset, cols="X__15")
  # ----------------------------------------------
  ## Code to aggregate into a dataset 
  
  ## now get region + budgeted expenses 
  budget_dataset <- gf_subset[, c("X__13","X__15", "X__22", "X__29"), with=FALSE]
  names(budget_dataset) <- c("loc_id", "cost_category", "budget", "disbursement")
  # ----------------------------------------------
  
  ## Create other variables 
  budget_dataset$source <- source
  budget_dataset$start_date <- as.Date(paste(c(year,"01","01"), collapse="-"),origin="1960-01-01")
  budget_dataset$period <- period
  budget_dataset$expenditures <- 0 ## change this once we figure out where exp data is
  budget_dataset$disease <- disease
  # ----------------------------------------------
  
  # return prepped data
  return(budget_dataset)
  
}