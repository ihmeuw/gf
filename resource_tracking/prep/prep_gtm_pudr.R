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
prep_pudr = function(dir, inFile, year, disease, period, cost_category, source) {
  
  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <- read_excel(paste0(dir,inFile, '.xls'))
  
  ##clean the data depending on if in spanish or english
  if(lang=="eng"){
    colnames(gf_data)[1] <- "cost_category"
    gf_data<- Filter(function(x)!all(is.na(x)), gf_data)
    gf_data <- gf_data[c(grep("cost grouping", tolower(gf_data$cost_category)):(grep("total", tolower(gf_data$cost_category)))),]
    colnames(gf_data)[3] <- "expenditures"

  ## drop 1st 3 rows: 
  gf_subset <- gf_data[-(1:5), ,drop = FALSE]
  gf_subset <- gf_subset[!grepl("grand total", tolower(gf_subset$cost_category)),]
  budget_dataset <- gf_subset[, c("cost_category", "expenditures"), with=FALSE]
  } else {
    colnames(gf_data)[2] <- "cost_category"
    gf_data <- gf_data[c(grep("catego", tolower(gf_data$cost_category)):(grep("otros", tolower(gf_data$cost_category)))),]
    gf_data<- Filter(function(x)!all(is.na(x)), gf_data)
    colnames(gf_data)[3] <- "budget"
    colnames(gf_data)[5] <- "expenditures"
    gf_subset <- gf_data[-1, ,drop = FALSE]
    budget_dataset <- gf_subset[, c("cost_category", "budget" ,"expenditures")]
  }
  # ----------------------------------------------
  ## Code to aggregate into a dataset 
  
  ## now get region + budgeted expenses 
 
  
  ## Create other variables 
  
  budget_dataset$source <- source
  budget_dataset$start_date <- as.Date(paste(c(year,"01","01"), collapse="-"),origin="1960-01-01")
  budget_dataset$period <- period
  budget_dataset$cost_category <- cost_category
  budget_dataset$expenditures <- 0 ## change this once we figure out where exp data is
  budget_dataset$disease <- disease
  # ----------------------------------------------
  
  # Enforce variable classes
  if (!is.numeric(budget_dataset$budget)) budget_dataset[,budget:=as.numeric(budget)]
  if (!is.numeric(budget_dataset$disbursement)) budget_dataset[,disbursement:=as.numeric(disbursement)]
  if (!is.numeric(budget_dataset$expenditures)) budget_dataset[,expenditures:=as.numeric(expenditures)]
  
  # ----------------------------------------------
  
  # return prepped data
  return(budget_dataset)
}
  
  