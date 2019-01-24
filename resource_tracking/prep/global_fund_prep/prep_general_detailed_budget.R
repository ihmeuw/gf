# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen
# PURPOSE: Prep commonly-formatted detailed budgets across countries. 
# DATE: Last updated January 2019. 

# Emily add pre and post conditions.  
# ----------------------------------------------


prep_detailed_uga_budget = function(dir, inFile, sheet_name, start_date, qtr_num, cashText, grant, disease, period, data_source) {

  #TROUBLESHOOTING HELP
  #Uncomment variables below and run line-by-line. 
  # 
  master_file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/", file_list$country[i], "/grants/")

  folder = "budgets"
  folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")

  dir = file_dir
  inFile = file_list$file_name[i]
  sheet_name = file_list$sheet[i]
  start_date = file_list$start_date[i]
  period = file_list$period[i]
  disease = file_list$disease[i]
  grant = file_list$grant[i]
  recipient = file_list$primary_recipient
  source = file_list$data_source[i]
 
  #-------------------------------------
  #Sanity check: Is this sheet name one you've checked before? 
  print(sheet_name)
  verified_sheet_names <- c('Detailed Budget', 'Detailed budget', 'DetailedBudget')
  if (!sheet_name%in%verified_sheet_names){
    stop("This sheet name has not been run with this function before - Are you sure you want this function? Add sheet name to verified list within function to proceed.")
  }
  
  # Load/prep data
  gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  
  #-------------------------------------
  # Remove diacritical marks
  #-------------------------------------
  for (i in 1:ncol(gf_data)){
    gf_data1 = gf_data[, fix_diacritics(gf_data[, i])]
  }
  
  #General function for grants.
  #-------------------------------------
  # 1. Subset columns.
  #-------------------------------------
  #Find the correct column indices based on a grep condition.
  
  #Grab title row 
  #Fix diacritics on title columns 
  module_col <- grep("Module", gf_data)
  intervention_col <- grep("Intervention", gf_data)
  #activity_col
  #cost category 
  

  #Validate these column indices, and assign column names.
  stopifnot(length(budget_col)==1 & length(expenditure_col)==1)
  colnames(gf_data)[budget_col] <- "budget"
  colnames(gf_data)[expenditure_col] <- "expenditure"
  
  #Subset to only these columns.
  gf_data = gf_data[, .(module, intervention, budget, expenditure)]
  
  #-------------------------------------
  # 2. Subset rows
  #-------------------------------------
  #Select only the section of the excel that's broken up by intervention
  start_row <- grep("modular approach", tolower(gf_data$module))
  end_row <- grep("grand total", tolower(gf_data$module))
  
  x = 1
  while (end_row[x] < start_row){
    x = x + 1
  }
  end_row = end_row[x]
  
  #Validate that these are correct
  stopifnot(length(start_row)==1 & length(end_row)==1)
  gf_data = gf_data[start_row:end_row, ]
  
  #Rename data, and remove invalid rows
  check_drop <- gf_data[((is.na(module) | module == '0') & (is.na(intervention) | intervention == '0')),]
  if (verbose == TRUE){
    print(paste0("Invalid rows currently being dropped: (only module and intervention columns shown) ", check_drop[, c('module', 'intervention')]))
  }
  gf_data<-  gf_data[!((is.na(module) | module == '0') & (is.na(intervention) | intervention == '0')),]
  
  #Some datasets have an extra title row with "[Module]" in the module column.
  #It's easier to find this by grepping the budget column, though.
  extra_module_row <- grep("budget for reporting period", tolower(gf_data$budget))
  if (length(extra_module_row) > 0){
    if (verbose == TRUE){
      print(paste0("Extra rows being dropped in GTM PU/DR prep function. First column: ", gf_data[extra_module_row, 1]))
    }
    gf_data <- gf_data[-extra_module_row, ,drop = FALSE]
  }
  
  #Remove 'total' and 'grand total' rows
  total_rows <- grep("total", tolower(gf_data$module))
  if (length(total_rows) > 0){
    if (verbose == TRUE){
      print(paste0("Total rows being dropped in GTM PU/DR prep function. First column: ", gf_data[total_rows, 1]))
    }
    gf_data <- gf_data[-total_rows, ,drop = FALSE]
  }
  
  #Replace any modules or interventions that didn't have a pair with "Unspecified".
  gf_data[is.na(module) & !is.na(intervention), module:="Unspecified"]
  gf_data[module == '0' & !is.na(intervention), module:="Unspecified"]
  gf_data[!is.na(module) & is.na(intervention), intervention:="Unspecified"]
  gf_data[!is.na(module) & intervention == '0', intervention:="Unspecified"]
  
  #-------------------------------------
  # 3. Generate new variables
  #-------------------------------------
  gf_data$start_date <- start_date
  gf_data$data_source <- source
  gf_data$period <- period
  gf_data$disease <- disease
  gf_data$grant_number <- grant
  gf_data$year <- year(gf_data$start_date)
  
  gf_data$cost_category <- "all"
  gf_data$sda_activity <- "all"
  
  #-------------------------------------
  # 4. Validate data
  #-------------------------------------
  budget_dataset = gf_data
  
  #Check to make sure budget and expenditure can be converted to numeric safely,
  # and the total for these columns is not '0' for the file. (may have grabbed wrong column).
  stopifnot(class(budget_dataset$budget) == 'character' & class(budget_dataset$expenditure)=='character')
  
  budget_dataset[, budget:=as.numeric(budget)]
  budget_dataset[, expenditure:=as.numeric(expenditure)]
  
  #Check these by summing the total for the file, and making sure it's not 0.
  check_budgets = budget_dataset[ ,
                                  lapply(.SD, sum, na.rm = TRUE),
                                  .SDcols = c("budget", "expenditure")]
  
  verified_0_expenditure <- c("UGA-C-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", "UGA-M-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", 
                              "UGA-S-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", "GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.xlsx") #These files have 0 for all expenditure.
  
  if(inFile%in%verified_0_expenditure){
    stopifnot(check_budgets[, 1]>0)
  } else {
    stopifnot(check_budgets[, 1]>0 & check_budgets[, 2]>0)
  }
  
  #Check column names, and that you have at least some valid data for the file.
  if (nrow(budget_dataset)==0){
    stop(paste0("All data dropped for ", inFile))
  }
  
  #--------------------------------
  # Note: Are there any other checks I could add here? #EKL
  # -------------------------------
  
  
  return(budget_dataset)
  

  
  return(budget_dataset)
}
