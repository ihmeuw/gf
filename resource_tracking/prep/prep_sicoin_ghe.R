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
prep_ghe_sicoin = function(dir, inFile, year, loc_id, period, cost_category, source) {
  
  
  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  ghe_data <- data.table(read_excel(paste0(dir, inFile, '.xls')))
  
  # ----------------------------------------------
  ## remove empty columns 
  ghe_data<- Filter(function(x)!all(is.na(x)), ghe_data)
  # ----------------------------------------------
  ## code to get diseases -- add more if necessary
  toMatch <- c("VIH", "SIDA", "TUBER", "MALAR", "VECTOR")
  ghe_data <- ghe_data[grepl(paste(toMatch, collapse="|"), ghe_data$X__10), ]
  
  ## now get region + budgeted expenses 
  budget_dataset <- ghe_data[, c("X__10", "X__17", "X__24"), with=FALSE]
  names(budget_dataset) <- c("disease", "budget", "disbursement")
  # ----------------------------------------------
  
  ## Create other variables 
  budget_dataset$source <- source
  budget_dataset$loc_id <- loc_id
  budget_dataset$cost_category <- cost_category
  budget_dataset$start_date <- as.Date(paste(c(year,"01","01"), collapse="-"),origin="1960-01-01")
  budget_dataset$period <- period
  budget_dataset$expenditures <- 0 ## change this once we figure out where exp data is
  # ----------------------------------------------
  
  # return prepped data
  return(budget_dataset)
}
