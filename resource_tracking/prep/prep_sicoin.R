# ----------------------------------------------
# Irena Chen
#
# 10/31/2017
# Template for prepping C-COIN budget data 
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------

# start function
prepSicoin = function(dir, inFile, year, period, cost_category, source) {
  
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
  
  ##pull all rows from between columns that have "FONDO MUNDIAL" in them 
  gf_data <- gf_data[c(grep("FONDO MUNDIAL", gf_data$X__10):(grep("FONDO MUNDIAL", gf_data$X__6))),]
  
  # remove rows with "TOTAL"  -> should be able to calculate total from summing municipaliies
  ## create a check for dropping missing data: 
  gf_subset <- data.table(gf_data[ grep("TOTAL", gf_data$X__3, invert = TRUE) , ])
  
  # remove rows where X__10 (municipalities) are missing values
  gf_subset <- na.omit(gf_subset, cols="X__10")
  
  # ----------------------------------------------
  ## Code to aggregate into a dataset 
  
  ## now get region + budgeted expenses 
  budget_dataset <- gf_subset[, c("X__10", "X__19", "X__26"), with=FALSE]
  names(budget_dataset) <- c("loc_id", "budget", "disbursement")
  
  ## we only want the municpalities so get rid of GF and Guatemala
  
  toMatch <- c("MUNDIAL", "GUATEMALA")
  for (i in 1:length(toMatch)){
    budget_dataset <- budget_dataset[grep(string(toMatch[i]),budget_dataset$loc_id, invert = TRUE),]
  }
  # ----------------------------------------------
  
  ## Create other variables 
  
  budget_dataset$source <- source
  budget_dataset$start_date <- as.Date(paste(c(year,"01","01"), collapse="-"),origin="1960-01-01")
  budget_dataset$period <- period
  budget_dataset$cost <- cost_category
  budget_dataset$expenditures <- 0 ## change this once we figure out where exp data is
  
  # ----------------------------------------------
  
  # return prepped data
  return(budget_dataset)
  
  
}