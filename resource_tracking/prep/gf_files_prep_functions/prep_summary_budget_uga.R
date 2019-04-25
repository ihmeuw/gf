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
prep_summary_uga_budget = function(dir, inFile, sheet_name, start_date, period, qtr_num, grant){
  
  ######## TROUBLESHOOTING HELP
  ### fill in variables below with information from line where the code breaks (use file list to find variables)
  ### uncomment by "ctrl + shift + c" and run code line-by-line
  ### look at gf_data and find what is being droped where.
  ########
  
  # dir = file_dir
  # inFile = file_list$file_name[i]
  # sheet_name = file_list$sheet[i]
  # start_date = file_list$start_date[i]
  # qtr_num = file_list$qtr_number[i]
  # period = file_list$period[i]
  # disease = file_list$disease[i]
  # grant = file_list$grant[i]
  # cashText = " Cash Outflow"
  # data_source = file_list$data_source[i]

  if(!is.na(sheet_name)){
    gf_data <- data.table(read.xlsx(paste0(dir, inFile), sheet=as.character(sheet_name), detectDates=TRUE))
  } else {
    gf_data <- data.table(read.xlsx(paste0(dir, inFile), detectDates=TRUE))
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
  
  # the budgets that start in August have a 2 month quarter, followed by just 3 month quarters
  org = start_date
  if (month(start_date) == 8 | (start_date == "2014-05-01" & grant == 'UGD-011-G10-S')){
    start_date = seq(as.Date(start_date), length = 2, by = "-1 months")[2]
  } else if(inFile == 'UGD-011-G11-M_IL1_SumBudget_3Aug12.xls'){
    start_date = as.Date("2012-07-01")
  }
  
  ##create a vector that contains the start dates that correspond to the data
  dates <- rep(start_date, qtr_num) 
  dates = as.Date(dates)
  if(inFile == 'UGD-011-G10-S_IL1_SumBudget_24Aug12.xls'){
    # THIS FILE IS SUPER WEIRD
    for (i in 1:length(dates)){
      if (i==1){
        dates[i] <- start_date
      }else if (i == 2 | i == 3) {
        dates[i] <- dates[i-1]%m+% months(4)
      }else if (i > 3 & i < 8){
        dates[i] <- dates[i-1]%m+% months(3)
      } else{
        dates[i] <- dates[i-1]%m+% months(2)
      }
    }
  }else{
    for (i in 1:length(dates)){
      if (i==1){
        dates[i] <- start_date
      }else {
        dates[i] <- dates[i-1]%m+% months(3)
      }
    }
  }
  
  #make first date equal to first date
  if(dates[1] != org){
    dates[1]  = org
  }
  
  
  #EKL verified to here for file "UGD-011-G12-M_IL4 Summary Budget.xlsx"
  ## grab only the relevant columns of the data (module, intervention, and sda along with the quarters)
  if(inFile == "UGD-011-G12-M_IL4 Summary Budget.xlsx" |
     inFile == "UGD-708-G13-H_Summary Budget_Phase 2.xlsx"){
    col_num = qtr_num+2
  }else{
    col_num = qtr_num+3
  }
  
  gf_data <- gf_data[, 1:col_num]
  gf_data <- gf_data[-1,]
  ##
  if(inFile == "UGD-011-G12-M_IL4 Summary Budget.xlsx" | 
     inFile == "UGD-708-G13-H_Summary Budget_Phase 2.xlsx"){
    setnames(gf_data, c("module", "intervention", as.character(dates)))
    gf_data$sda_activity = gf_data$intervention
  }else{
    setnames(gf_data, c("module", "intervention", "sda_activity", as.character(dates)))
  }
  
  setDT(gf_data)
  budget_dataset<- melt(gf_data,id=c("module", "intervention", "sda_activity"), variable.name = "start_date", value.name="budget")
  
  #Verify the budget numbers were pulled correctly (make sure they're all numeric) 
  stopifnot(class(budget_dataset$budget)=="character")
  budget_dataset[, budget:=as.numeric(budget)]
  
  if(inFile == "UGD-011-G10-S_IL3 Summary Budget_20May14.xlsx" | inFile == 'UGD-011-G09-S_IL1_SumBudget.xls'){
    budget_dataset$sda_activity = gsub("HSS:", "", budget_dataset$sda_activity)
    budget_dataset$intervention = budget_dataset$sda_activity
    budget_dataset$sda_activity = "all"
    
  }
  
  ##add categories
  budget_dataset$start_date = as.Date(budget_dataset$start_date)
  if(inFile == 'UGD-011-G10-S_IL1_SumBudget_24Aug12.xls'){
    budget_dataset$period = ifelse(year(budget_dataset$start_date) == 2012, 120, ifelse(
      year(budget_dataset$start_date) == 2013, 90, 60))
  }else{
  budget_dataset$period = ifelse(month(budget_dataset$start_date) == 8 | (budget_dataset$start_date == "2014-05-01" & grant == 'UGD-011-G10-S'), 60, 
                                 ifelse(month(org) == 8 & (budget_dataset$start_date == max(budget_dataset$start_date)), 120, period))} #make up for the 1st period being 2 months and the final being 4 months
  budget_dataset$grant_number <- grant
  budget_dataset$data_source <- source
  budget_dataset$start_date <- as.Date(budget_dataset$start_date)
  budget_dataset$quarter <- quarter(budget_dataset$start_date)
  budget_dataset$year <- year(budget_dataset$start_date)
  return(budget_dataset)
  
}

