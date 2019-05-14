#-------------------------------------------------------------
# Emily Linebarger, based off of code written by Irena Chen
# Prepares old detailed budgets in Guatemala for integration 
# into the resource database
# Updated: April 2019
#-------------------------------------------------------------


prep_other_budget_gtm = function(dir, inFile, sheet_name, start_date, qtr_num, period){
  
  # dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
  # inFile = file_list$file_name[i]
  # sheet_name = file_list$sheet[i]
  # start_date = file_list$start_date[i]
  # qtr_num = file_list$qtr_number[i]
  # period = file_list$period[i]

  verified_files = c("GUA-M-MSPAS_SB_Y4-6a_IL8.xlsx")
  verified_sheets = c("Detailed Budget - Year 1", "Detailed Budget - Year 2", "Detailed Budget - Year 3")
  if (!inFile%in%verified_files){
    print(inFile)
    stop("This file has not been run with this function before - Are you sure you want this function? Add file name to verified list within function to proceed.")
  }
  
  ##load the FPM data: 
  if(!is.na(sheet_name)){
    gf_data <- data.table(read.xlsx(paste0(dir, inFile), sheet=as.character(sheet_name), detectDates=TRUE))
  } else {
    gf_data <- data.table(read.xlsx(paste0(dir, inFile), detectDates=TRUE))
  }
  
  #-------------------------------------
  # 1. Subset columns.
  #-------------------------------------
  #First, find Service Delivery Area and activity columns. They're labeled in a different row than budget and expenditure. 
  
  correctly_named = grepl("service delivery area", tolower(names(gf_data)))
  #If there isn't a column named 'module', find the row with the names on it. 
  if (!TRUE%in%correctly_named){
    #Find the row that has the column names in it
    name_row = 1 
    while(is.na(gf_data[name_row, 2])){
      name_row = name_row + 1
    }
    
    subtitle_row = gf_data[name_row, ]
    subtitle_row = tolower(subtitle_row)
  } 
  title_row = tolower(names(gf_data))
  
  #Grab service delivery area and activity rows
  sda_col <- grep("service delivery area|(sda)", subtitle_row)
  activity_col <- grep("activity", subtitle_row)
 
  stopifnot(length(sda_col)==1 & length(activity_col)==1)
  
  #Now, find budget and expenditure columns. 
  budget_cols = grep("total amount", subtitle_row) 
  expenditure_cols = grep("gastos.quarter", title_row)
  if (sheet_name == "Detailed Budget - Year 3"){
    expenditure_cols = grep("gastos", subtitle_row)
    drop_expenditure = grep("compromisos|total", subtitle_row)
    expenditure_cols = expenditure_cols[!expenditure_cols%in%drop_expenditure]
  }
  budget_cols = budget_cols[!budget_cols%in%expenditure_cols]
  
  #Subset to these columns. 
  gf_data = gf_data[, c(sda_col, activity_col, budget_cols, expenditure_cols), with=FALSE]
  
  #Set names
  names(gf_data) <- c('module', 'intervention', 'budget_q1', 'budget_q2', 'budget_q3', 'budget_q4', 'expenditure_q1', 'expenditure_q2', 'expenditure_q3', 'expenditure_q4')
  
  #-------------------------------------
  # 2. Subset rows
  #-------------------------------------
  gf_data = gf_data[!is.na(budget_q1) | is.na(module)]
  gf_data = gf_data[!budget_q1 == 'Total amount']
  
  #-------------------------------------
  # 3. Reshape to the quarter-level. 
  #-------------------------------------
  budget_dataset = melt(gf_data, id.vars=c('module', 'intervention'))
  budget_dataset[, quarter:=tstrsplit(variable, "_", keep=2)] #Pull out a quarter variable
  budget_dataset[, quarter:=gsub("q", "", quarter)]
  budget_dataset[, quarter:=as.numeric(quarter)]
  
  budget_dataset[, variable:=tstrsplit(variable, "_", keep=1)]
  # print(unique(budget_dataset$variable))
  # print(unique(budget_dataset$quarter))
  
  #Add year and start_date variables
  budget_dataset[, year:=year(start_date)]
  #Generate new start date variable. 
  budget_dataset[quarter==1, month:="01"]
  budget_dataset[quarter==2, month:="04"]
  budget_dataset[quarter==3, month:="07"]
  budget_dataset[quarter==4, month:="10"]
  
  budget_dataset[, start_date:=paste0(month, "-01-", year)]
  budget_dataset[, start_date:=as.Date(start_date, "%m-%d-%Y")]
  budget_dataset[, month:=NULL]
  
  #One last cast to make budget and expenditure their own values 
  budget_dataset[, value:=as.numeric(value)]
  budget_dataset = dcast(budget_dataset, module+intervention+start_date+year+quarter~variable, value.var='value', fun.aggregate=sum_na_rm)
  
  return(budget_dataset)  
}

