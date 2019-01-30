# ----------------------------------------------
# Irena Chen
#
# 5/1/52018
# Template for prepping DRC PUDR data:  
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object

# ----------------------------------------------

# start function
prep_pudr_cod = function(dir, inFile, sheet_name, start_date, disease, 
                         period, grant, recipient, source,lang, loc_name) {
  
  
  ######## TROUBLESHOOTING HELP
  ### fill in variables below: inFile, sheet_name, start_date, qtr_num, disease, period, lang, grant, recipient_name 
  ### with information from line where the code breaks, and then uncomment by "ctrl + shift + c" and run code line-by-line
  ### look at gf_data and find what is being droped where.
  ########

  folder = "budgets"
  folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  version = ifelse(file_list$file_iteration[i] == "initial", "iterations", "")
  file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
  if (version != ""){
    file_dir = paste0(file_dir, version, "/")
  }

  dir = file_dir
  inFile = file_list$file_name[i]
  sheet_name = file_list$sheet[i]
  start_date = file_list$start_date[i]
  qtr_num = file_list$qtr_number[i]
  period = file_list$period[i]
  disease = file_list$disease[i]
  lang = file_list$language[i]
  grant = file_list$grant[i]
  source = file_list$data_source[i]
  loc_id = 'cod'

  ##read the data: 
  gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))
  str_replace(start_date, "\\\\", "")
  start_date = substring(start_date, 2, 11)
  start_date = as.Date(start_date)
  
  if(sheet_name == "RFA ALF_7B"){
 ## for the PUDRs w/out SR info
  colnames(gf_data)[1] <- "module"
  colnames(gf_data)[2] <- "intervention"
  colnames(gf_data)[3] <- "budget"
  colnames(gf_data)[5] <- "expenditure"
  gf_data <- gf_data[c(grep("modular approach", tolower(gf_data$module)):.N),]
  budget_dataset <- gf_data[, c("intervention", "budget", "expenditure"),with=FALSE]
  budget_dataset<-  budget_dataset[!is.na(budget_dataset$intervention),]
  budget_dataset<- budget_dataset[!grepl("0",budget_dataset$intervention),]
  ##remove the first and last row of data (it's just the column labels, not actual data)
  budget_dataset <- budget_dataset[-nrow(budget_dataset),]
  budget_dataset <- budget_dataset[-1,]
  
  ##add all of the other RT variables 
  budget_dataset$recipient <- recipient
  budget_dataset$module <- budget_dataset$intervention
  
  } else {
    print(paste0("An else statement was entered in the DRC PUDR prep function. Make sure outputs are correct for ", inFile))
    print(paste0("Sheet name is ", sheet_name))
    colnames(gf_data)[3] <- "module"
    colnames(gf_data)[4] <- "intervention"
    colnames(gf_data)[5] <- "budget"
    colnames(gf_data)[6] <- "expenditure"
    
    start_row <- grep("modular approach", tolower(gf_data$module))
    end_row <- grep("grand total", tolower(gf_data$module))
    x = 1
    while (end_row[x] < start_row){
      x = x + 1
    }
    end_row = end_row[x]
    
    stopifnot(length(start_row)==1 & length(end_row)==1)
    gf_data = gf_data[start_row:end_row, ]
    
    check_drop <- gf_data[((is.na(module) | module == '0') & (is.na(intervention) | intervention == '0') & is.na(module)),]
    if (verbose == TRUE){
      print(paste0("Invalid rows currently being dropped: (only module and intervention columns shown) ", check_drop[, c('module', 'intervention')]))
    }
    gf_data<-  gf_data[!((is.na(module) | module == '0') & (is.na(intervention) | intervention == '0')),]
    
    #Subset to only the columns you want. 
    gf_data = gf_data[, ]
    #Some datasets have an extra title row with "[Module]" in the module column.
    #It's easier to find this by grepping the budget column, though.
    extra_module_row <- grep("modular approach", tolower(gf_data$budget))
    if (length(extra_module_row) > 0){
      if (verbose == TRUE){
        print(paste0("Extra rows being dropped in GTM PU/DR prep function. First column: ", gf_data[extra_module_row, .(module, intervention)]))
      }
      gf_data <- gf_data[-extra_module_row, ,drop = FALSE]
    }
    
    #Remove 'total' and 'grand total' rows
    total_rows <- grep("total", tolower(gf_data$module))
    if (length(total_rows) > 0){
      if (verbose == TRUE){
        print(paste0("Total rows being dropped in GTM PU/DR prep function. First column: ", gf_data[total_rows, .(module, intervention)]))
      }
      gf_data <- gf_data[-total_rows, ,drop = FALSE]
    }
    
    #Replace any modules or interventions that didn't have a pair with "Unspecified".
    gf_data[is.na(module) & !is.na(intervention), module:="Unspecified"]
    gf_data[module == '0' & !is.na(intervention), module:="Unspecified"]
    gf_data[!is.na(module) & is.na(intervention), intervention:="Unspecified"]
    gf_data[!is.na(module) & intervention == '0', intervention:="Unspecified"]
    
  }
  
  budget_dataset$start_date <- start_date
  budget_dataset$data_source <- source
  budget_dataset$period <- period
  budget_dataset$sda_activity <- "Unspecified (Summary budget)"## change if we ever get more detailed PUDR info
  budget_dataset$disease <- disease
  budget_dataset$cost_category <- "all" ## change if we ever get more detailed PUDR info
  budget_dataset$grant_number <- grant
  budget_dataset$loc_name <- loc_name
  budget_dataset$lang <- lang
  budget_dataset$year <- year(start_date)
  
  # return prepped data
  return(budget_dataset)
}


