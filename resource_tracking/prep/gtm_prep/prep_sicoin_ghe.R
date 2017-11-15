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
prep_ghe_sicoin = function(dir, inFile, year, loc_id, period, disease, source) {
  
  
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
  toMatch <- c("vih", "sida", "tuber", "malar", "vector", "violencia sexual")
  ghe_data <- ghe_data[grepl(paste(toMatch, collapse="|"), tolower(ghe_data$X__10)), ]
  
  ## now get region + budgeted expenses 
  budget_dataset <- ghe_data[, c("X__10", "X__17", "X__24"), with=FALSE]
  names(budget_dataset) <- c("cost_category", "budget", "disbursement")
  # ----------------------------------------------
  
  ## Create other variables 
  budget_dataset$source <- source
  budget_dataset$loc_id <- loc_id
  budget_dataset$disease <- disease
  hivMatch <- c("vih", "sida", "violencia")
  tbMatch <- c("tuber")
  for(i in 1:length(budget_dataset$disease)){
    if(grepl(paste(hivMatch, collapse = "|"), tolower(budget_dataset$cost_category[i]))){
    budget_dataset$disease[i] <- "hiv"
    } else if(grepl(tbMatch, tolower(budget_dataset$cost_category[i]))){
    budget_dataset$disease[i] <- "tb"
    } else{
    budget_dataset$disease[i] <- "malaria"
    }
    i=i+1
  }
  
  budget_dataset$start_date <- as.Date(paste(c(year,"01","01"), collapse="-"),origin="1960-01-01")
  budget_dataset$period <- period
  budget_dataset$expenditure <- NA ## change this once we figure out where exp data is
  # ----------------------------------------------
  
  # Enforce variable classes
  if (!is.numeric(budget_dataset$budget)) budget_dataset[,budget:=as.numeric(budget)]
  if (!is.numeric(budget_dataset$disbursement)) budget_dataset[,disbursement:=as.numeric(disbursement)]
  if (!is.numeric(budget_dataset$expenditure)) budget_dataset[,expenditure:=as.numeric(expenditure)]

  # ----------------------------------------------
  
  # return prepped data
  return(budget_dataset)
}
