# ----------------------------------------------
# Irena Chen
#
# 11/8/2017
# Template for prepping GF UGA budgets where the data is in "summary" form 
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object

# ----------------------------------------------
##function to clean the data: 

prep_summary_uga_budget = function(dir, inFile, sheet_name, start_date, qtr_num, cashText, grant, disease, period, recipient, source){

  if(!is.na(sheet_name)){
    gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name), col_names = FALSE))
  } else {
    gf_data <- data.table(read_excel(paste0(dir, inFile)))
  }
  
  ##we don't need the first column
  gf_data <- gf_data[, -1]
  colnames(gf_data)[1] <- "module"
  colnames(gf_data)[2] <- "intervention"
  colnames(gf_data)[3] <- "sda_activity"
  ##only keep data that has a value in the "category" column 
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  
  ## this type of budget data should always have 13 cost categories
  gf_data <- gf_data[c((grep("macro-cate",tolower(gf_data$module))):(grep("implementing", tolower(gf_data$sda_activity)))),]
  
  ## drop the last row of the data (we don't need this)
  gf_data <- head(gf_data,-1)
  
  ## Drop the rows that contain certain phrases (usually this is the year summary budget column, which we don't need)
  toMatch <- c("%", "Phase", "Total")
  drop.cols <- grep(paste(toMatch, collapse="|"), ignore.case=TRUE, gf_data)
  gf_data <- gf_data[, (drop.cols) := NULL]
  
  
  ##create a vector that contains the start dates that correspond to the data
  dates <- rep(start_date, qtr_num) 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else if(i==2&grant=="UGD-708-G07-H"){ ##one grant doesn't have a full quarter (only two months)
      dates[i] <- ymd("2011-10-01")
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }
  
  ## grab only the relevant columns of the data (module, intervention, and sda along with the quarters)
  col_num = qtr_num+3
  gf_data <- gf_data[, 1:col_num]
  gf_data <- gf_data[-1,]
  
  ##
  setnames(gf_data, c("module", "intervention", "sda_activity", as.character(dates)))

  setDT(gf_data)
  budget_dataset<- melt(gf_data,id=c("module", "intervention", "sda_activity"), variable.name = "start_date", value.name="budget")

  ##add categories
  budget_dataset$disease <- disease 
  budget_dataset$period <- period
  budget_dataset$expenditure <- 0 ## since we don't have expenditure data yet 
  budget_dataset$grant_number <- grant
  budget_dataset$recipient <- recipient
  budget_dataset$cost_category <- "all"
  budget_dataset$data_source <- source
  budget_dataset$start_date <- as.Date(budget_dataset$start_date)
  budget_dataset$year <- year(budget_dataset$start_date)
  return(budget_dataset)
  
}

