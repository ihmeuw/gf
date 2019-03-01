# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen
# PURPOSE: Prep commonly-formatted detailed budgets across countries. 
# DATE: Last updated January 2019. 

# Emily add pre and post conditions.  
# ----------------------------------------------


prep_general_detailed_budget = function(dir, inFile, sheet_name, start_date, qtr_num, grant, disease, period, data_source) {

  #TROUBLESHOOTING HELP
  #Uncomment variables below and run line-by-line. 
  # 
  
  ######## TROUBLESHOOTING HELP
  ### fill in variables below with information from line where the code breaks (use file list to find variables)
  ### uncomment by "ctrl + shift + c" and run code line-by-line
  ### look at gf_data and find what is being droped where.
  ########
  
  # folder = "budgets"
  # folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  # file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
  # #
  # dir = file_dir
  # inFile = file_list$file_name[i]
  # sheet_name = file_list$sheet[i]
  # start_date = file_list$start_date[i]
  # period = file_list$period[i]
  # disease = file_list$disease[i]
  # grant = file_list$grant[i]
  # recipient = file_list$primary_recipient
  # source = file_list$data_source[i]
  # qtr_num = file_list$qtr_number[i]

  #-------------------------------------
  #Sanity check: Is this sheet name one you've checked before? 
  verified_sheet_names <- c('Detailed Budget', 'Detailed budget', 'DetailedBudget', 'Recomm_Detailed Budget')
  if (!sheet_name%in%verified_sheet_names){
    stop("This sheet name has not been run with this function before - Are you sure you want this function? Add sheet name to verified list within function to proceed.")
    print(paste0("Sheet name: '", sheet_name, "'"))
  }
  
  # Load/prep data
  gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  
  #-------------------------------------
  # Remove diacritical marks
  #-------------------------------------
  for (i in 1:ncol(gf_data)){
    gf_data1 = gf_data[, fix_diacritics(gf_data[, .(i)])]
  }
  
  #General function for grants.
  #-------------------------------------
  # 1. Subset columns.
  #-------------------------------------
  #Find the correct column indices based on a grep condition. (Want to keep module, intervention, budget, cost category, and activity description)
  
  #Grab module and intervention rows 
  module_col <- grep("Module", gf_data)
  intervention_col <- grep("Intervention", gf_data)
  #Remove "Module ID and Intervention Sub ID columns" 
  mod_id_col = grep("module id", tolower(gf_data))
  intervention_id_col = grep("intervention sub id", tolower(gf_data))
  module_col = module_col[!module_col%in%mod_id_col]
  intervention_col = intervention_col[!intervention_col%in%intervention_id_col] 
  
  stopifnot(length(module_col)==1 & length(intervention_col)==1)
  
  #Grab cost category and activity description
  activity_col = grep("activity description", tolower(gf_data))
  cost_category_col = grep("cost input", tolower(gf_data))
  drop_cost_category = grep("cost input id|cost input sub id|cost input no", tolower(gf_data)) #Drop extra cost category columns
  cost_category_col = cost_category_col[!cost_category_col%in%drop_cost_category]
  
  stopifnot(length(activity_col)==1 & length(cost_category_col)==1)
  
  #Grab all budget rows 
  budget_cols = grep("cash outflow", tolower(gf_data))
  #stopifnot(length(budget_cols) == qtr_num) - This doesn't work here. 
  
  total_subset = c(module_col, intervention_col, activity_col, cost_category_col, budget_cols)
  gf_data = gf_data[, total_subset, with=FALSE]
  
  #Add names to columns - the first row that's not 'NA' in 
  name_row = 1 
  while(is.na(gf_data[name_row, 1])){
    name_row = name_row + 1
  }
  
  new_names = c(gf_data[name_row, ])
  names(gf_data) = tolower(new_names)
  #Do a sanity check here. 
  stopifnot(names(gf_data[, 1]) == 'module' & names(gf_data[, 2]) == 'intervention')
  
  #Do one more subset to grab only the budget columns with quarter-level data. (Keep the first 4 columns and everything that matches this condition)
  quarter_cols = grep("q", names(gf_data))
  second_subset = c(1:4, quarter_cols)
  gf_data = gf_data[, second_subset, with = FALSE]
  
  #Reset names for the whole thing 
  quarters = ncol(gf_data)-4
  old_qtr_names = c()
  for (i in 1:quarters){
    old_qtr_names[i] = paste0("q", i, " cash outflow")
  }
  
  new_qtr_names = c()
  for (i in 1:quarters){
    new_qtr_names[i] = paste0("budget_q", i)
  }
  
  old_names = c('activity description', 'cost input', old_qtr_names)
  new_names = c('activity_description', 'cost_category', new_qtr_names)
  
  setnames(gf_data, old=old_names, new=new_names)
  
  #-------------------------------------
  # 2. Subset rows
  #-------------------------------------
  #Remove the name row and everything before it, because we know the spreadsheet hasn't started before that point
  gf_data = gf_data[-(1:name_row)]
  
  #Remove rows where first 4 variables (module, intervention, activity, and cost category) are NA
  gf_data = gf_data[!(is.na(module) & is.na(intervention) & is.na(activity_description) & is.na(cost_category))]
  
  #Replace any modules or interventions that didn't have a pair with "Unspecified".
  gf_data[is.na(module) & !is.na(intervention), module:="Unspecified"]
  gf_data[!is.na(module) & is.na(intervention), intervention:="Unspecified"]
  
  #-------------------------------------
  # 3. Reshape data long
  #-------------------------------------
  date_range = rep(start_date, quarters) #Make a vector of the date variables the quarters correspond to. 
  for (i in 2:quarters){ 
    date_range[i] = as.Date(start_date + days(period*(i-1))) 
  }
  budget_dataset = melt(gf_data, id.vars = c('module', 'intervention', 'activity_description', 'cost_category'), value.name = "budget")
  budget_dataset[, budget:=as.numeric(budget)] #Go ahead and make budget numeric at this point
  
  #Replace "budget q1", etc. with the actual date it corresponds to. 
  old_quarters = unique(budget_dataset$variable)
  new_quarters = date_range
  
  #For every value of old_quarters, if variable equals old_quarter
  for (i in 1:length(old_quarters)){
    budget_dataset[variable == old_quarters[i], start_date:=new_quarters[i]]
  }
  
  budget_dataset = budget_dataset[, -c('variable')]

  #-------------------------------------
  # 4. Generate new variables
  #-------------------------------------
  budget_dataset$data_source <- source
  budget_dataset$period <- period
  budget_dataset$disease <- disease
  budget_dataset$grant_number <- grant
  budget_dataset$year <- year(budget_dataset$start_date)
  
  #-------------------------------------
  # 5. Validate data
  #-------------------------------------
  #Make sure that the total budget is not 0 (was not converted from numeric correctly, or wrong column was grabbed.)
  check_budgets = budget_dataset[ ,
                                  lapply(.SD, sum, na.rm = TRUE),
                                  .SDcols = c("budget")]
  
  stopifnot(check_budgets[, 1]>0)

  #Check column names, and that you have at least some valid data for the file.
  if (nrow(budget_dataset)==0){
    stop(paste0("All data dropped for ", inFile))
  }
  
  #Make sure you have all the columns you need for analysis
  check_names = c('module', 'intervention', 'cost_category', 'activity_description', 'budget', 'start_date')
  stopifnot(check_names%in%names(budget_dataset))
  
  #--------------------------------
  # Note: Are there any other checks I could add here? #EKL
  # -------------------------------
  
  
  return(budget_dataset)
}
