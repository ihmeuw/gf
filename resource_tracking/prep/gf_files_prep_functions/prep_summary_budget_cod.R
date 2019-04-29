# ----------------------------------------------
# Irena Chen
#
# 11/8/2017
# Template for prepping GF summary page only budget data
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------
# 
# dir = file_dir
# sheet_name = "RESUME BUDGET V2 CONSOLIDE"
# inFile = "official_budgets/BUDGET SANRUGF CONSOLIDE  ROUTINE CAMPAGNE.xlsx"
# start_date = "2018-01-01"
# period = 90
# qtr_num = 12


prep_summary_budget_cod = function(dir, inFile, sheet_name, start_date, period, qtr_num){

  
  ######## TROUBLESHOOTING HELP
  ### fill in variables below with information from line where the code breaks (use file list to find variables)
  ### uncomment by "ctrl + shift + c" and run code line-by-line
  ### look at gf_data and find what is being droped where.
  ########
  
  # dir = file_dir
  # inFile = file_list$file_name[i]
  # sheet_name = file_list$sheet[i]
  # start_date = file_list$start_date[i]
  # period = file_list$period[i]
  # disease = file_list$disease[i]
  # qtr_num = file_list$qtr_number[i]
  # language = file_list$language[i]
  
  
  # ----------------------------------------------
  ##set up functions to handle french and english budgets differently
  # ----------------------------------------------
  ## create a vector of start_dates that correspond to each quarter in the budget
  
  dates <- rep(start_date, qtr_num) # 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }
  
  # ----------------------------------------------
  ##read the data: 
  # ----------------------------------------------
  
  if(!is.na(sheet_name)){
    gf_data <- data.table(read.xlsx(paste0(dir, inFile), sheet=as.character(sheet_name), detectDates=TRUE))
  } else {
    gf_data <- data.table(read.xlsx(paste0(dir, inFile), detectDates=TRUE))
  }
  
  colnames(gf_data)[1] <- "cost_category"
  ##only keep data that has a value in the "category" column 
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  
  if(sheet_name == "RESUME BUDGET V2 CONSOLIDE"){
    #grab the sda values
    gf_data <- gf_data[c((grep("Module",gf_data$cost_category)):nrow(gf_data)),]
    #drop Total
    gf_data <- head(gf_data,-1)
    #dropping this becuase it's duplicate of first colum
    gf_data$X__2 = NULL
  
    
  }else{
    ## grab the SDA data
    gf_data <- gf_data[c((grep("Module",gf_data$cost_category)):(grep("Cost Grouping", gf_data$cost_category))),]
    ##drop the last two rows (we don't need them)
    gf_data <- head(gf_data,-2)
  }
  
  
  ## rename the columns 
  colnames(gf_data) <- as.character(gf_data[1,])
  gf_data <- gf_data[-1,]
  toMatch <- c("Ann","Year", "RCC", "%", "Phase", "Implem", "Total")
  drop.cols <- grep(paste(toMatch, collapse="|"), ignore.case=TRUE, colnames(gf_data))
  gf_data <- gf_data[, (drop.cols) := NULL]

  ## also drop columns containing only NA's
  gf_data<- Filter(function(x) !all(is.na(x)), gf_data)
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  ##library(reshape)
  setDT(gf_data)
  if(sheet_name == "RESUME BUDGET V2 CONSOLIDE"){
    #only keep quarters 1 - 12
  gf_data = gf_data[,c(1:13)]
  gf_data1<- melt(gf_data,id="By Module - Intervention" , variable.name = "qtr", value.name="budget")
  }else{
  gf_data1<- melt(gf_data,id="By Module", variable.name = "qtr", value.name="budget")
  }
  
  ## make sure that you have a date for each quarter - will tell you if you're missing any 
  if(length(dates) != length(unique(gf_data1$qtr))){
    stop('Error: quarters were dropped!')
  }
  ##turn the list of dates into a dictionary (but only for quarters!) : 
  dates <- setNames(dates,unique(gf_data1$qtr))
  
  
  ## now match quarters with start dates 
  kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
  budget_dataset <-gf_data1[kDT, on=.(qtr), start_date := i.start_date]
  
  ##rename the category column 
  colnames(budget_dataset)[1] <- "module"
  
  if(sheet_name == "RESUME BUDGET V2 CONSOLIDE"){
    budget_dataset = separate(budget_dataset, module, into=c("module", "intervention"), sep="-")
  } else{
    budget_dataset$intervention <- NA
  }
  
  #Fix formatting on date columns.  
  budget_dataset = budget_dataset[,-c('qtr')]
  budget_dataset[, quarter:=quarter(start_date)]
  budget_dataset[, year:=year(start_date)]
 
  
  return(budget_dataset)
  
}