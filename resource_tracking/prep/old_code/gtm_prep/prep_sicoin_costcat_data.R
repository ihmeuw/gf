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
prep_cost_sicoin = function(dir, inFile, year, disease, period, source, grant_number, category) {
  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <- data.table(read_excel(paste0(dir,inFile, '.xls')))
  
  # ----------------------------------------------
  ## remove empty columns 
  gf_data<- Filter(function(x)!all(is.na(x)), gf_data)
  
  if(source=="ghe"){ 
    if(year > 2014 && disease=="hiv"){
        ##pull just the cost categories 
      gf_data <- gf_data[c(grep("GUATEM", gf_data$X__14):grep("DONACIONES EXTERNAS", gf_data$X__12)),]
      ## grab loc_id: 
      gf_data$X__14 <- na.locf(gf_data$X__14, na.rm=FALSE)
      # remove rows where cost_categories are missing values
      gf_subset <- na.omit(gf_data, cols="X__15")
      # ----------------------------------------------
      ## Code to aggregate into a dataset 
      ## now get region + budgeted expenses 
      budget_dataset <- gf_subset[, c("X__14","X__15", "X__22", "X__29"), with=FALSE]
      names(budget_dataset) <- c("loc_id", "cost_category", "budget", "disbursement")
      
    } else if (year==2014 && disease=="hiv") {
        gf_data <- gf_data[c(grep("GUATEM", gf_data$X__10):grep("DONACIONES EXTERNAS", gf_data$X__10)),]
        gf_data <- na.omit(gf_data, cols="X__10")
        budget_dataset <- gf_data[, c("X__10","X__19", "X__26"), with=FALSE]
        # remove rows where cost_categories are missing values
        names(budget_dataset) <- c("loc_id", "budget", "disbursement")
        budget_dataset <- budget_dataset[-nrow(budget_dataset),]
        budget_dataset$cost_category <- category
      
    } else if (year > 2010 && disease=="hiv"){
        gf_data <- gf_data[c(grep("GUATEM", gf_data$X__13):grep("DONACIONES EXTERNAS", gf_data$X__12)),]
        gf_data$X__13 <- na.locf(gf_data$X__13, na.rm=FALSE)
        # remove rows where cost_categories are missing values
        gf_subset <- na.omit(gf_data, cols="X__15")
      ## now get region + budgeted expenses 
      budget_dataset <- gf_subset[, c("X__13","X__15", "X__22", "X__29"), with=FALSE]
      names(budget_dataset) <- c("loc_id", "cost_category", "budget", "disbursement")
    } else {
        gf_data <- gf_data[c(grep("GUATEM", gf_data$X__13):.N),]
        ## grab loc_id: 
        gf_data$X__13 <- na.locf(gf_data$X__13, na.rm=FALSE)
        # remove rows where cost_categories are missing values
        gf_subset <- na.omit(gf_data, cols="X__15")
        # ----------------------------------------------
        ## Code to aggregate into a dataset 
        ## now get region + budgeted expenses 
        budget_dataset <- gf_subset[, c("X__13","X__15", "X__22", "X__29"), with=FALSE]
        names(budget_dataset) <- c("loc_id", "cost_category", "budget", "disbursement")
    }
  } else{
      gf_data <- gf_data[c(grep("GUATEM", gf_data$X__11):.N),]
      gf_data$X__11 <- na.locf(gf_data$X__11, na.rm=FALSE)
      gf_subset <- na.omit(gf_data, cols="X__10")
      gf_subset<- gf_subset[!grepl('GUAT',gf_subset$X__10),]
      budget_dataset <- gf_subset[, c("X__10","X__11", "X__19", "X__26"), with=FALSE]
      names(budget_dataset) <- c("cost_category", "loc_id", "budget", "disbursement")
  }
    toMatch <- c("government", "recursos", "resources", "multire")
    budget_dataset <- budget_dataset[ !grepl(paste(toMatch, collapse="|"), tolower(budget_dataset$loc_id)),]
  # ----------------------------------------------
  
  ## Create other variables 
  budget_dataset$source <- source
  budget_dataset$start_date <- as.Date(paste(c(year,"01","01"), collapse="-"),origin="1960-01-01")
  budget_dataset$period <- period
  budget_dataset$expenditure <- 0 ## change this once we figure out where exp data is
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