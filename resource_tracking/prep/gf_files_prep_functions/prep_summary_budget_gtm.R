# ----------------------------------------------
# Irena Chen
#
# 11/8/2017
# Template for prepping GF GTM budgets where the data is in "summary" form 
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------
##function to clean the data: 
# ----------------------------------------------
prep_summary_budget_gtm = function(dir, inFile, sheet, start_date, qtr_num){
  
  dir = file_dir
  inFile = file_list$file_name[i]
  sheet = file_list$sheet[i]
  start_date = file_list$start_date[i]
  qtr_num = file_list$qtr_number[i]

  
  if(!is.na(sheet)){
    gf_data <- data.table(read.xlsx(paste0(dir, inFile), sheet=as.character(sheet), detectDates=TRUE))
  } else {
    gf_data <- data.table(read.xlsx(paste0(dir, inFile), detectDates=TRUE))
  }
  
  gf_data <- gf_data[, -1]
  colnames(gf_data)[1] <- "module"
  colnames(gf_data)[2] <- "descrip"
  colnames(gf_data)[3] <- "intervention"
  
  gf_data$descrip <- NULL
  ## this type of budget data should always have 13 cost categories
  gf_data <- gf_data[c((grep("service deliv",tolower(gf_data$intervention))):(grep("implementing", tolower(gf_data$intervention)))),]
  
  ## drop the first and last row of the data 
  gf_data <- head(gf_data,-1)
  gf_data <- gf_data[-1,]
  
  toMatch <- c("%", "Phase", "Total", "Year", "RCC")
  drop.cols <- grep(paste(toMatch, collapse="|"), ignore.case=TRUE, gf_data)
  gf_data <- gf_data[, (drop.cols) := NULL]
  
  gf_data[[1]][1] <- "module"
  gf_data[[2]][1] <- "intervention"
  ##only keep data that has a value in the "category" column 
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  gf_data <- gf_data[-1,]
  ## also drop columns containing only NA's
  gf_data<- Filter(function(x) !all(is.na(x)), gf_data)
  
  dates <- rep(start_date, qtr_num) # 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }
  
  col_num = qtr_num+2
  gf_data <- gf_data[, 1:col_num]
  setnames(gf_data, c("module", "intervention", as.character(dates)))
  
  setDT(gf_data)
  budget_dataset<- melt(gf_data,id.vars =c("module", "intervention"), variable.name = "start_date", value.name="budget")
  budget_dataset$start_date <- as.Date(budget_dataset$start_date, "%Y-%m-%d")
  budget_dataset$budget <- as.numeric(budget_dataset$budget)
  
  #Add quarter and year variables. 
  budget_dataset[, quarter:=quarter(start_date)]
  budget_dataset[, year:=year(start_date)]
 
  return(budget_dataset)
  
}