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
prepSicoin = function(dir, inFile, year, disease, period, cost_category, source, grant_number) {
  
  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories

  # Load/prep data
  gf_data <- read_excel(paste0(dir,inFile, '.xls'))
  
  # ----------------------------------------------
  
  ##avoid using rlangs so manually name the columns w/ "FONDO MUNDIAL" 
  colnames(gf_data)[11] <- "loc_id"
  colnames(gf_data)[7] <- "col2"
  colnames(gf_data)[4] <- "col3"
  colnames(gf_data)[20] <- "budget"
  colnames(gf_data)[27] <- "disbursement"
  ## remove empty columns 
  gf_data<- Filter(function(x)!all(is.na(x)), gf_data)
  
  
  ##pull all rows from between columns that have "FONDO MUNDIAL" in them 
  if (source=="gf"){
    gf_data <- gf_data[c(grep("fondo mundial", tolower(gf_data$loc_id)):(grep("fondo mundial", tolower(gf_data$col2)))),]
  } else {
    gf_data <- gf_data[c(grep("guat", tolower(gf_data$loc_id)):.N),]
  }
  # remove rows with "TOTAL"  -> should be able to calculate total from summing municipaliies
  ## create a check for dropping missing data: 
  gf_subset <- data.table(gf_data[ grep("TOTAL", gf_data$col3, invert = TRUE) , ])
  
  # remove rows where X__10 (municipalities) are missing values
  gf_subset <- na.omit(gf_subset, cols="loc_id")
  
  # ----------------------------------------------
  ## Code to aggregate into a dataset 
  
  ## now get region + budgeted expenses 
    budget_dataset <- gf_subset[, c("loc_id", "budget", "disbursement"), with=FALSE]
  
  ## we only want the municpalities so get rid of GF and Guatemala
  
  toMatch <- c("mundial", "guate", "government", "recursos", "resources", "multire")
  budget_dataset <- budget_dataset[ !grepl(paste(toMatch, collapse="|"), tolower(budget_dataset$loc_id)),]
  # ----------------------------------------------
  
  ## Create other variables 
  
  budget_dataset$source <- source
  budget_dataset$start_date <- as.Date(paste(c(year,"01","01"), collapse="-"),origin="1960-01-01")
  budget_dataset$period <- period
  budget_dataset$cost_category <- cost_category
  budget_dataset$expenditure <- NA ## change this once we figure out where exp data is
  budget_dataset$disease <- disease
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