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
prep_ghe_muni_sicoin = function(dir, inFile, year, period, disease, source, grant_number) {
  
  
  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  ghe_data <- data.table(read_excel(paste0(dir, inFile, '.xls')))
  
  # ----------------------------------------------
  ghe_data<- Filter(function(x)!all(is.na(x)), ghe_data)
  # ----------------------------------------------
  ## code to get diseases -- add more if necessary
  setnames(ghe_data, c("X__14", "X__15", "X__22", "X__29"), c("loc_id", "cost_category", "budget", "disbursement"))
  ## remove empty columns 
  ghe_data <- ghe_data[c(grep("GUATEM", ghe_data$loc_id):.N),]
  ghe_data$loc_id <- na.locf(ghe_data$loc_id, na.rm=FALSE)
  
  toMatch <- c("vih", "sida", "tuber", "malar", "vector", "violencia sexual")
  ghe_data <- ghe_data[grepl(paste(toMatch, collapse="|"), tolower(ghe_data$cost_category)), ]
  
  
  budget_dataset <- ghe_data[, c("loc_id", "cost_category", "budget", "disbursement"), with=FALSE]
  # ----------------------------------------------
  
  toMatch <- c("mundial", "guate", "government", "recursos", "resources", "multire")
  budget_dataset <- budget_dataset[ !grepl(paste(toMatch, collapse="|"), tolower(budget_dataset$loc_id)),]
  ## Create other variables 
  budget_dataset$source <- source
  budget_dataset$disease <- disease
  budget_dataset$cost_category <- as.factor(budget_dataset$cost_category)
  budget_dataset$start_date <- as.Date(paste(c(year,"01","01"), collapse="-"),origin="1960-01-01")
  budget_dataset$period <- period
  budget_dataset$expenditure <- NA ## change this once we figure out where exp data is
  budget_dataset$grant_number <- grant_number
  
  # ----------------------------------------------
  
  # Enforce variable classes
  if (!is.numeric(budget_dataset$budget)) budget_dataset[,budget:=as.numeric(budget)]
  if (!is.numeric(budget_dataset$disbursement)) budget_dataset[,disbursement:=as.numeric(disbursement)]
  if (!is.numeric(budget_dataset$expenditure)) budget_dataset[,expenditure:=as.numeric(expenditure)]
  
  # ----------------------------------------------
  
  # return prepped data
  return(budget_dataset)
}
