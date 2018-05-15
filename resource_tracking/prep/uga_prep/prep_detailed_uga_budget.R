# ----------------------------------------------
# Irena Chen
#
# 11/14/2017
# Template for prepping UGA GF budget data that is grouped by "cost category"
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------


prep_detailed_uga_budget = function(dir, inFile, sheet_name, start_date, qtr_num, cashText, grant, disease, period, data_source) {
  
  # ----------------------------------------------
  ##prep functions that will be used in cleaning the code: 
  create_qtr_names = function(qtr_names, cashText){
    for(i in 1:qtr_num+5){
      if(i <6) {
        i=i+1
      } else { 
        qtr_names[i] <- paste("Q", i-5,  cashText, sep="")
      }
      i=i+1
    }
    return(qtr_names)
  }
  
  ##create list of column names: 
  if(start_date=="2018-01-01"){
    qtr_names <- c("Module","Intervention", "Activity Description","Cost Input", "Implementer", rep(1, qtr_num))
  } else {
    qtr_names <- c("Module","Intervention", "Activity Description", "Cost Input","Recipient", rep(1, qtr_num))
  }
  qtr_names <- create_qtr_names(qtr_names, cashText)
  
  # ----------------------------------------------
  ##read the data from excel: 
  gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=sheet_name))
  
  
  
  ##approved budgets are formatted slightly differently - so this code takes care of that: 
  if(start_date=="2018-01-01"){
    gf_data <- gf_data[-c(1:2),]
    colnames(gf_data) <- as.character(gf_data[1,])
    gf_data <- gf_data[-1,]
  }
  
  #3only grab the columns we want (program activity, recipient, and quarterly data) :
  gf_data <- gf_data[,names(gf_data)%in%qtr_names, with=FALSE]
  
  ##rename the columns: 
  colnames(gf_data)[1] <- "module"
  colnames(gf_data)[2] <- "intervention"
  colnames(gf_data)[3] <- "sda_activity"
  colnames(gf_data)[4] <- "cost_category"
  colnames(gf_data)[5] <- "recipient"
  
  ##only keep data that has a value in the "category" column 
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  setDT(gf_data)
  gf_data1<- melt(gf_data,id=c("module","intervention","sda_activity", "cost_category","recipient"), variable.name = "qtr", value.name="budget")
  
  ##create vector that maps quarters to their start dates: 
  dates <- rep(start_date, qtr_num) # 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }
  
  ##if for some reason, we don't have the same number of start dates as quarters, this will stop the function:
  if(length(dates) != length(unique(gf_data1$qtr))){
    stop('quarters were dropped!')
  }
  ##turn the list of dates into a dictionary (but only for quarters!) : 
  dates <- setNames(dates,unique(gf_data1$qtr))
  
  
  ## now match quarters with start dates 
  kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
  budget_dataset <-gf_data1[kDT, on=.(qtr), start_date := i.start_date]
  
  ##add more variables to the data (such as disease and data source)
  budget_dataset$qtr <- NULL
  budget_dataset$start_date <- as.Date(budget_dataset$start_date)
  budget_dataset$period <- period
  budget_dataset$expenditure <- 0 ##change if we get expenditure info 
  budget_dataset$data_source <- data_source
  budget_dataset$grant_number <- grant
  budget_dataset$disease <- disease
  budget_dataset$disbursement <- 0##change if we get disbursement info
  budget_dataset$year <- year(budget_dataset$start_date)
  return(budget_dataset)
}
