# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen
# PURPOSE: Prep commonly-formatted PU/DRs across countries. 
# DATE: Last updated January 2019. 

# For this function to work properly, a file must have a section labeled "Modular approach" with columns for module, intervention, budget, and expenditure. 
# Returns a file called budget dataset with columns for module, intervention, budget, and expenditure. 
# ----------------------------------------------

#Sheet names that don't work so far: "LFA EFR_7", "LFA_Annex-SR Financials", "LFA_Total PR Cash Outflow_3", "LFA_Total PR Cash Outflow_3A"

# start function
prep_modular_approach_pudr =  function(dir, inFile, sheet_name, start_date, period, qtr_number) {
  
  #TROUBLESHOOTING HELP
  #Uncomment variables below and run line-by-line. 

  # dir = file_dir
  # inFile = file_list$file_name[i]
  # sheet_name = file_list$sheet[i]
  # start_date = file_list$start_date[i]
  # period = file_list$period[i]
  # disease = file_list$disease[i]
  # grant = file_list$grant[i]
  # recipient = file_list$primary_recipient
  # source = file_list$data_source[i]
  # qtr_number = file_list$qtr_number[i]

  # -----------------------------------------------------------------------------
  # Test the inputs to make sure that they are the correct type
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # -----------------------------------------------------------------------------
  # Files and directories
  
  #Sanity check: Is this sheet name one you've checked before? 
  verified_sheet_names <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B')
  if (!sheet_name%in%verified_sheet_names){
    print(sheet_name)
    stop("This sheet name has not been run with this function before - Are you sure you want this function? Add sheet name to verified list within function to proceed.")
  }
  
  # Load/prep data
  gf_data <-data.table(read.xlsx(paste0(dir,inFile), sheet=sheet_name, detectDates=TRUE))

  #General function for grants.
  #-------------------------------------
  # 1. Subset columns.
  #-------------------------------------
  #Find the correct column indices based on a grep condition.
  module_col <- grep("Modular Approach - Modules", gf_data)
  intervention_col <- grep("Modular Approach - Interventions", gf_data)
  budget_col <- grep("Budget for Reporting Period", gf_data)
  expenditure_col <- grep("Actual Expenditure", gf_data)

  #Remove the 'cumulative expenditure' and 'cumulative budget' columns.
  if (length(expenditure_col)!=1){
    cumulative_expenditure_col <- grep("Cumulative Actual Expenditure", gf_data) #Remove the 'cumulative expenditure' column.
    for (i in 1:length(expenditure_col)){
      if (expenditure_col[i] %in% cumulative_expenditure_col){
        expenditure_col = expenditure_col[-i]
      }
    }
  }

  if (length(budget_col)!=1){
    cumulative_budget_col <- grep("Cumulative Budget", gf_data)
    for (i in 1:length(budget_col)){
      if (budget_col[i] %in% cumulative_budget_col){
        budget_col = budget_col[-i]
      }
    }
  }

  #Validate these column indices, and assign column names.
  stopifnot(length(budget_col)==1 & length(expenditure_col)==1)
  colnames(gf_data)[budget_col] <- "budget"
  colnames(gf_data)[expenditure_col] <- "expenditure"

  #Check to see if this file has module and intervention.
  if (is.na(module_col)){
    gf_data$module <- "Unspecified"
  } else {
    stopifnot(length(module_col)==1)
    colnames(gf_data)[module_col] <- "module"
  }

  if (is.na(intervention_col)){
    gf_data$intervention <- "Unspecified"
  } else {
    stopifnot(length(intervention_col)==1)
    colnames(gf_data)[intervention_col] <- "intervention"
  }

  #Subset to only these columns.
  gf_data = gf_data[, .(module, intervention, budget, expenditure)]
  
  #Make budget and expenditure numeric. 
  gf_data[, budget:=as.numeric(budget)]
  gf_data[, expenditure:=as.numeric(expenditure)]

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
  check_drop <- gf_data[((is.na(module) | module == '0') & (is.na(intervention) | intervention == '0') & (is.na(budget)|budget=='0') & (is.na(expenditure)|expenditure=='0')),]
  if (verbose == TRUE){
    print(paste0("Invalid rows currently being dropped: (only module and intervention columns shown) ", check_drop[, c('module', 'intervention')]))
  }
  gf_data<-  gf_data[!((is.na(module) | module == '0') & (is.na(intervention) | intervention == '0')& (is.na(budget)|budget=='0') & (is.na(expenditure)|expenditure=='0')),]

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
  gf_data[is.na(module) | module == '0' , module:="Unspecified"]
  gf_data[is.na(intervention) | intervention == '0' , module:="Unspecified"]

  #-------------------------------------------------------------------------
  # 3. Generate date variables, and expand data to be at the quarter-level. 
  #-------------------------------------------------------------------------
  totals_check = gf_data[, .(budget=sum(budget, na.rm = TRUE), expenditure=sum(expenditure, na.rm=TRUE))]
  
  #Add in date variables 
  gf_data[, quarter:=quarter(start_date)]
  gf_data[, year:=year(start_date)]
  
  gf_data[, period:=period]
  gf_data[, qtr_number:=qtr_number]
  gf_data[, qtr_split:=round((period*qtr_number)/90)]
  gf_data[, split:=round((period*qtr_number)/90)] #Create this variable twice so you can divide budget/expenditure after expansion
 
  #Expand data by the number of days, and generate a variable to iterate over
  gf_data <- expandRows(gf_data, "qtr_split")
  byVars = names(gf_data)
  gf_data[, seq:=sequence(.N), by=byVars]
  gf_data[, seq:=seq-1] #Decrement by 1 because sequence indexes at 1. 
  
  #While seq is not 0, go through the loop below.
  #If seq is greater than or equal to 4, add 1 to year and divide everything by 4. Continue this loop while max(seq) > 4.
  # If month + seq + 1 equals 12, than
  gf_data[, new_qtr:=qtr_number+seq]
  max_quarter = max(gf_data$new_qtr)
  while (max_quarter>4){
    gf_data[new_qtr>4, year:=year+1]
    gf_data[new_qtr>4, new_qtr:=new_qtr-4]
    max_quarter = max(totalGos$new_qtr)
  }

  #Split up budget and expenditure.
  gf_data[, budget:=budget/split]
  gf_data[, expenditure:=expenditure/split]

  #Make sure you haven't changed any budget/expenditure numbers, and clean up
  totals_check2 = gf_data[, .(budget=sum(budget, na.rm = TRUE), expenditure=sum(expenditure, na.rm=TRUE))]
  for (i in 1:nrow(totals_check)){
    stopifnot(totals_check$budget[i]==totals_check2$budget[i] | totals_check$expenditure[i]==totals_check2$expenditure[i])
  }
  gf_data = gf_data[, -c('period', 'qtr_number', 'split', 'seq', 'quarter')]
  setnames(gf_data, 'new_qtr', 'quarter')
  
  #Generate new start date variable. 
  gf_data[quarter==1, month:="01"]
  gf_data[quarter==2, month:="04"]
  gf_data[quarter==3, month:="07"]
  gf_data[quarter==4, month:="10"]
  
  gf_data[, start_date:=paste0(month, "-01-", year)]
  gf_data[, start_date:=as.Date(start_date, "%m-%d-%Y")]
  gf_data[, month:=NULL]

  #-------------------------------------
  # 4. Validate data
  #-------------------------------------
  budget_dataset = gf_data

  #Check these by summing the total for the file, and making sure it's not 0.
  check_budgets = budget_dataset[ ,
                  lapply(.SD, sum, na.rm = TRUE),
                  .SDcols = c("budget", "expenditure")]

  verified_0_expenditure <- c("UGA-C-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", "UGA-M-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", 
                              "UGA-S-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", "GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.xlsx", 
                              "GTM-T-MSPAS_Progress Report jul _31Dec2018_v2  rev LFA.xlsx", "GTM-H-HIVOS_Progress Report_31Dec2018_v1.xlsx") #These files have 0 for all expenditure.
  
  if(inFile%in%verified_0_expenditure){
    stopifnot(check_budgets[, 1]>0)
  } else {
    stopifnot(check_budgets[, 1]>0 & check_budgets[, 2]>0)
  }

  # -------------------------------


  return(budget_dataset)

}


