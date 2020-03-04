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
  
#  colnames(gf_data)[1] <- "cost_category"
  ##only keep data that has a value in the "category" column 
#  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  
if(inFile=="BUDGET SANRUGF CONSOLIDE  ROUTINE CAMPAGNE.xlsx"){
    colnames(gf_data)[1] <- "cost_category"
    
    ## subset rows we want
    start_row <- grep("module", tolower(gf_data$cost_category))
    end_row <- grep("total", tolower(gf_data$cost_category))
    
    x = 1
    while (end_row[x] < start_row){
      x = x + 1
    }
    end_row = end_row[x]
    
    while (length(start_row) > 1){
      start_row = start_row[1]
    } # question--how else to pick the right start-row? Dynamic way?
    
    # validate
    stopifnot(length(start_row)==1 & length(end_row)==1)
    gf_data = gf_data[start_row:end_row, ]


  # drop the year, percent, total columns
  gf_data[,c("X2", "X7", "X12", "X17", "X22", "X23", "X24"):=NULL]
  # rename the columns
  dates <- as.character(dates)
  setnames(gf_data, old=names(gf_data), new=c('module',dates))
  
  # drop total row
  total_rows <- grep("total", tolower(gf_data$module))
  if (length(total_rows) > 0){
    if (verbose == TRUE){
      print(paste0("Total rows being dropped in COD Budget prep function. First column: ", gf_data[total_rows, 1]))
    }
    gf_data <- gf_data[-total_rows, ,drop = FALSE]
  }
  
  # drop extra row
  gf_data <- gf_data[-1, ,drop=FALSE]
  
    # melt long
  budget_dataset = melt(gf_data, id.vars = c('module'), 
                        value.name = "budget", variable.name="start_date")
  budget_dataset$start_date <- as.Date(budget_dataset$start_date)
  
  
 # } else if(sheet_name == "RESUME BUDGET V2 CONSOLIDE"){
 #    colnames(gf_data)[1] <- "cost_category"
 #    module_row <- grep("module", tolower(gf_data$cost_category))
 #    costgrouping_row <- grep("cost grouping", tolower(gf_data$cost_category))
 #    stopifnot(length(module_row)==1 & length(costgrouping_row)==1)
 #     #grab the sda values
 #    gf_data <- gf_data[module_row:costgrouping_row,]
 #    #drop Total
 #    gf_data <- head(gf_data,-1)
 #    #dropping this becuase it's duplicate of first colum
 #    gf_data$X__2 = NULL
 #    
 #    stop("You found the sheet 'RESUME BUDGET V2 CONSOLIDE' but you need the file name. Review the function prep_summary_budget_cod.R")
 #  
  } else if (inFile=="COD-H-SANRU_Summay_Budget.xlsx"){
    colnames(gf_data)[1] <- "cost_category"
    total_rows <- grep("total", tolower(gf_data$cost_category))
    if (length(total_rows) > 0){
      if (verbose == TRUE){
        print(paste0("Total rows being dropped in COD budget prep function. First column: ", gf_data[total_rows, 1]))
      }
      gf_data <- gf_data[-total_rows, ,drop = FALSE]
    }
    # drop the year totals and grant cost category totals
    gf_data[,c("Year.1", "Year.2", "Year.3", "Total"):=NULL]
    dates <- as.character(dates)
    setnames(gf_data, old=names(gf_data), new=c('cost_category',dates)) 
  
    budget_dataset = melt(gf_data, id.vars = c('cost_category'), 
                          value.name = "budget", variable.name="start_date")
    budget_dataset$start_date <- as.Date(budget_dataset$start_date)
 
  } else if (inFile=="COD-M-SANRU_SB2.xlsx") {

    ## subset rows we want
    start_row <- grep("module", tolower(gf_data$`Component:`))
    end_row <- grep("total", tolower(gf_data$`Component:`))
    
    x = 1
    while (end_row[x] < start_row){
      x = x + 1
    }
    end_row = end_row[x]
    
    #Validate that these are correct
    stopifnot(length(start_row)==1 & length(end_row)==1)
    gf_data = gf_data[start_row:end_row, ]
    
    # rename columns
    setnames(gf_data, old=names(gf_data), new=as.character(gf_data[1]))
    
    # drop year columns
    gf_data[,c("Year 1", "Year 2", "Year 3", "Year 4", "Total", "%"):=NULL]
    
    # change column names to proper dates
    dates <- as.character(dates)
    setnames(gf_data, old=names(gf_data), new=c('module',dates))
    
    #Remove 'total' and 'grand total' rows
    total_rows <- grep("total", tolower(gf_data$module))
    if (length(total_rows) > 0){
      if (verbose == TRUE){
        print(paste0("Total rows being dropped in COD Budget prep function. First column: ", gf_data[total_rows, 1]))
      }
      gf_data <- gf_data[-total_rows, ,drop = FALSE]
    }
    
    #Some datasets have an extra title row with "[Module]" in the module column.
    #It's easier to find this by grepping the budget column, though.
    extra_module_row <- grep("module", tolower(gf_data$module))
    if (length(extra_module_row) > 0){
      if (verbose == TRUE){
        print(paste0("Extra rows being dropped in COD budget prep function. First column: ", gf_data[extra_module_row, 1]))
      }
      gf_data <- gf_data[-extra_module_row, ,drop = FALSE]
    }

    # melt long
     
    budget_dataset = melt(gf_data, id.vars = c('module'), 
                          value.name = "budget", variable.name="start_date")
    budget_dataset$start_date <- as.Date(budget_dataset$start_date)
    
  }else{
    stop("A processing function for this file has not been written. Please review code in function prep_summary_budget_cod.R")
    # commented out by Francisco on Feb26 2020 because we're trying to find what files it runs on
    # 
    # ## grab the SDA data
    # module_row <- grep("module", tolower(gf_data$cost_category))
    # costgrouping_row <- grep("cost grouping", tolower(gf_data$cost_category))
    # 
    # stopifnot(length(module_row)==1 & length(costgrouping_row)==1)
    # 
    # gf_data <- gf_data[c((grep("module",gf_data$cost_category)):(grep("Cost Grouping", gf_data$cost_category))),]
    # ##drop the last two rows (we don't need them)
    # gf_data <- head(gf_data,-2)
  }
  

  ## rename the columns 
  # colnames(gf_data) <- as.character(gf_data[1,])
  # gf_data <- gf_data[-1,]
  # toMatch <- c("Ann","Year", "RCC", "%", "Phase", "Implem", "Total")
  # drop.cols <- grep(paste(toMatch, collapse="|"), ignore.case=TRUE, colnames(gf_data))
  # gf_data <- gf_data[, (drop.cols) := NULL]
  # 
  # ## also drop columns containing only NA's
  # gf_data<- Filter(function(x) !all(is.na(x)), gf_data)
  # 
  # ## invert the dataset so that budget expenses and quarters are grouped by category
  # ##library(reshape)
  # setDT(gf_data)
  # if(sheet_name == "RESUME BUDGET V2 CONSOLIDE"){
  #   #only keep quarters 1 - 12
  # gf_data = gf_data[,c(1:13)]
  # gf_data1<- melt(gf_data,id="By Module - Intervention" , variable.name = "qtr", value.name="budget")
  # }else{
  # gf_data1<- melt(gf_data,id="By Module", variable.name = "qtr", value.name="budget")
  # }
  # 
  # ## make sure that you have a date for each quarter - will tell you if you're missing any 
  # if(length(dates) != length(unique(gf_data1$qtr))){
  #   stop('Error: quarters were dropped!')
  # }
  # ##turn the list of dates into a dictionary (but only for quarters!) : 
  # dates <- setNames(dates,unique(gf_data1$qtr))
  # 
  # 
  # ## now match quarters with start dates 
  # kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
  # budget_dataset <-gf_data1[kDT, on=.(qtr), start_date := i.start_date]
  # 
  # ##rename the category column 
  # colnames(budget_dataset)[1] <- "module"
  # 
  # if(sheet_name == "RESUME BUDGET V2 CONSOLIDE"){
  #   budget_dataset = separate(budget_dataset, module, into=c("module", "intervention"), sep="-")
  # } else{
  #   budget_dataset$intervention <- NA
  # }
  # 
  # #Fix formatting on date columns.  
  # budget_dataset = budget_dataset[,-c('qtr')]
  # budget_dataset[, quarter:=quarter(start_date)]
  # budget_dataset[, year:=year(start_date)]
 
  
  return(budget_dataset)
  
}