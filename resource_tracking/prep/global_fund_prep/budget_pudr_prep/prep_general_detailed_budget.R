# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen
# PURPOSE: Prep commonly-formatted detailed budgets across countries. 
# DATE: Last updated January 2019. 

# Emily add pre and post conditions.  
# ----------------------------------------------

prep_general_detailed_budget = function(dir, inFile, sheet_name, start_date, period, qtr_num, language) {

  #TROUBLESHOOTING HELP
  #Uncomment variables below and run line-by-line. 
  # 
  
  ######## TROUBLESHOOTING HELP
  ### fill in variables below with information from line where the code breaks (use file list to find variables)
  ### uncomment by "ctrl + shift + c" and run code line-by-line
  ### look at gf_data and find what is being droped where.
  ########
  #
  dir = file_dir
  inFile = file_list$file_name[i]
  sheet_name = file_list$sheet[i]
  start_date = file_list$start_date[i]
  period = file_list$period[i]
  disease = file_list$disease[i]
  qtr_num = file_list$qtr_number[i]
  language = file_list$language[i]
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
  gf_data = gf_data[, lapply(gf_data, fix_diacritics)]
  
  if (language == 'fr'){
    qtr_text = 'sorties de tresorerie'
  } else if (language == 'eng'){
    qtr_text == 'cash outflow'
  }
  
  #General function for grants.
  #-------------------------------------
  # 1. Subset columns.
  #-------------------------------------
  #Find the correct column indices based on a grep condition. (Want to keep module, intervention, budget, cost category, and activity description)
  
  #Find the row that has the column names in it
  name_row = 1 
  while(is.na(gf_data[name_row, 2])){
    name_row = name_row + 1
  }
  
  names = gf_data[name_row, ]
  names = tolower(names)
  
  #Grab module and intervention rows
  module_col <- grep("module", names)
  intervention_col <- grep("intervention", names)
  #Remove "Module ID and Intervention Sub ID columns" 
  mod_id_col = grep("module id", names)
  intervention_id_col = grep("intervention sub id", names)
  cat_budget_col = grep("catalytic budget", names)
  
  module_col = module_col[!module_col%in%mod_id_col]
  intervention_col = intervention_col[!intervention_col%in%intervention_id_col & !intervention_col%in%cat_budget_col] 
  
  stopifnot(length(module_col)==1 & length(intervention_col)==1)
  
  #Grab cost category and activity description
  if (language == "eng"){
    activity_col = grep("activity description", names)
    cost_category_col = grep("cost input", names)
    drop_cost_category = grep("cost input id|cost input sub id|cost input no", names) #Drop extra cost category columns
    cost_category_col = cost_category_col[!cost_category_col%in%drop_cost_category]
  } else if (language == "fr"){
    activity_col = grep("description de l'activite", names)
    cost_category_col = grep("element de cout", names)
  }
  
  stopifnot(length(activity_col)==1 & length(cost_category_col)==1)
  
  #Grab all budget rows 
  budget_cols = grep(qtr_text, names)
  
  total_subset = c(module_col, intervention_col, activity_col, cost_category_col, budget_cols)
  gf_data = gf_data[, total_subset, with=FALSE]
  
  #Change column names using the name row you found before. 
  names = names[total_subset]
  names(gf_data) = names
  #Do a sanity check here. 
  stopifnot(names(gf_data[, 1]) == 'module' & names(gf_data[, 2]) == 'intervention')
  
  #Do one more subset to grab only the budget columns with quarter-level data. (Keep the first 4 columns and everything that matches this condition)
  budget_periods = names[5:length(names)]
  budget_periods = gsub(qtr_text, "", budget_periods)
  
  if (language == "eng"){
    quarter_cols = grep("q", names(gf_data))
  } else if (language == 'fr'){
    quarter_cols = budget_periods[nchar(budget_periods)<=4] #Don't grab any total rows 
    quarter_cols = quarter_cols[!grepl("a", quarter_cols)]
    quarter_cols = paste0(qtr_text, quarter_cols)
    quarter_cols = as.character(quarter_cols)
    quarter_cols = grep(paste(quarter_cols, collapse="|"), names(gf_data))
  }

  second_subset = c(1:4, quarter_cols) #Only keep module, intervention, activity description, and the budget columns by quarter
  gf_data = gf_data[, second_subset, with = FALSE]
  
  #Reset names for the whole thing 
  quarters = ncol(gf_data)-4
  old_qtr_names = c()
  for (i in 1:quarters){
    if(language == "eng"){
      old_qtr_names[i] = paste0("q", i, qtr_text)
    } else if (language == "fr"){
      old_qtr_names[i] = paste0(qtr_text, " t", i)
    }
  }
  
  new_qtr_names = c()
  for (i in 1:quarters){
    new_qtr_names[i] = paste0("budget_q", i)
  }
  
  if (language == "eng"){
    old_names = c('activity description', 'cost input', old_qtr_names)
  } else if (language == "fr") {
    old_names = c("description de l'activite", "element de cout", old_qtr_names)
  }
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
    month(date_range[i]) = month(date_range[i]) + 3*(i-1) 
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
  # 4. Validate data
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
