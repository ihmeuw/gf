# ----------------------------------------------
# AUTHOR: Audrey Batzel and Francisco Rios Casas based on code by Emily Linebarger and Irena Chen
# PURPOSE: Prep commonly-formatted FR budgets
# DATE: Last updated June 2020.

# TO DO: 
# ----------------------------------------------
# PREP FR BUDGETS code taken directly from the prep general detail budget code and modified slightly

prep_target_populations = function(dir, inFile, sheet_name, start_date, qtr_num, language) {
  
  # # TROUBLESHOOTING HELP
  # dir = file_dir
  # inFile = file_list$file_name[i]
  # sheet_name = "Population"
  # start_date = file_list$start_date_financial[i]
  # qtr_num = file_list$qtr_number[i]
  # language = file_list$language_financial[i]
  # -------------------------------------

  gf_data = data.table(read.xlsx(paste0(dir,inFile), sheet=sheet_name, detectDates=TRUE))

  initial_rows = nrow(gf_data) #Save to run a check on later.
  # 
  # #-------------------------------------
  # # Remove diacritical marks
  # #-------------------------------------
  gf_data = gf_data[, lapply(gf_data, fix_diacritics)]

  # 
  # #General function for grants.
  # #-------------------------------------
  # # 1. Subset columns.
  # #-------------------------------------
  # #Find the correct column indices based on a grep condition. (Want to keep module, intervention, and budget for all subpopuations)

  correctly_named = grepl("modul", tolower(names(gf_data)))
  
  #If there isn't a column named 'module', find the row with the names on it. 
  if (!TRUE%in%correctly_named){
    #Find the row that has the column names in it
    name_row = 1 
    while(is.na(gf_data[name_row, 1]) | is.na(gf_data[name_row, 5]) ){
      name_row = name_row + 1
    }
    
    names = gf_data[name_row, ]
    names = tolower(names)
  } else { #Otherwise, just grab the names of the data table. 
    name_row = 1
    names = tolower(names(gf_data))
    names = fix_diacritics(names)
  }
  
  names=gsub("\r\n", "", names) #Remove extraneous characters
  names = gsub("\\.", " ", names) #Remove periods
  names = gsub(" ", "_", names) #Remove spaces
  
  # reset names for the whole table
  names(gf_data) = names
  
  # find the module and intervention columns
  module_col = grep("mod", names(gf_data))
  intervention_col = grep("intervention|intervencion", names(gf_data))
  
  stopifnot(length(module_col)==1 & length(intervention_col)==1)
  
  #Remove the name row and everything before it, because we know the spreadsheet hasn't started before that point
  if(name_row!=1){
    gf_data = gf_data[-(1:name_row)]
  }
  
  names(gf_data)[module_col] <- "module"
  names(gf_data)[intervention_col] <- "intervention"
  
  #Remove rows where first 2 variables (module, intervention) are NA, checking that their sum is 0 for key variables first. 
  check_na_sum = gf_data[is.na(module) & is.na(intervention)]
  # check_na_sum = melt(check_na_sum, id.vars = c('module', 'intervention', 'activity_description', 'cost_category'), value.name = "budget")
  # check_na_sum[, budget:=as.numeric(budget)]
  # na_budget = check_na_sum[, sum(budget, na.rm = TRUE)]
  # if (na_budget!=0 & inFile!="COD_S_MOH_DetailedBudget_1.xlsx"){
  #   stop("Budgeted line items have NA for all key variables - review drop conditions before dropping NAs in module and intervention")
  # }
  gf_data = gf_data[!(is.na(module) & is.na(intervention))]
  
  # pull out the modular framework columns for later
  mod_frame <- gf_data[,module_col:intervention_col]
  
  # add indicator for number of years in the grant
  year_num <- as.numeric(qtr_num)/4
  
  # add year variables
  date_range = rep(year(start_date), year_num) #Make a vector of the date variables the quarters correspond to. 
  for (i in 2:year_num){ 
    date_range[i] = date_range[i] + (i-1) 
  }
  
  # finding starting column and ending columns to break data down
  if (language=="esp"){
    start_cols <- grep("desglosarse", names(gf_data))
    end_cols <- grep("diferencia", names(gf_data))
  } else if (language=="fr"){
    start_cols <- grep("ventiler", names(gf_data))
    end_cols <- grep("difference", names(gf_data))
  }

  # write a loop that breaks the data into a table for each year
  i=1
  population_data <- list()
  
  for (i in 1:year_num){
    population_data[[i]] <- gf_data[,start_cols[i]: end_cols[i]]
    population_data[[i]] <- cbind(population_data[[i]], budget_year=paste0(date_range[i]))
    population_data[[i]] <- cbind(mod_frame, population_data[[i]])
  }
  
  # rbind the data together
  budget_dataset <- rbindlist(population_data, fill=FALSE, idcol=NULL)
  
  # remove column totals
  total_col <- grep("total", names(budget_dataset))
  difference_col <- grep("dif", names(budget_dataset))
  
  budget_dataset[,(difference_col):=NULL]
  budget_dataset[,(total_col):=NULL]
  
  # melt data long
  # make vector of population groups
  # find new module, intervention, budget_year column
  module_col <- grep("module", names(budget_dataset))
  intervention_col <- grep("intervention", names(budget_dataset))
  budget_year_col <- grep("budget_year", names(budget_dataset))
  
  population_groups <- names(budget_dataset)[-c(module_col, intervention_col, budget_year_col)]
  
  budget.long <- melt(budget_dataset, measure.vars = population_groups,
                      variable.name = "population", value.name = "budget")
  
  # particular guatemala file has an extra column with the same name which is blank i believe
  if (language=="esp"){
    check_empty_col <- budget.long[is.na(grupos_de_poblacion_no_especificos)]
    check_empty_col[, budget:=as.numeric(budget)]
    check_empty_col[, grupos_de_poblacion_no_especificos:=as.numeric(grupos_de_poblacion_no_especificos)]
    na_budget <- check_empty_col[, sum(grupos_de_poblacion_no_especificos, na.rm = TRUE)]
    if (na_budget!=0){
      stop("Budgeted line items have NA for all key variables - review drop conditions before dropping NAs in module and intervention")} else{
        budget.long[,('grupos_de_poblacion_no_especificos'):=NULL]
      }
  } else if (language=="fr"){
    check_empty_col <- budget.long[is.na(groupes_de_population_non_specifies)]
    check_empty_col[, budget:=as.numeric(budget)]
    check_empty_col[, groupes_de_population_non_specifies:=as.numeric(groupes_de_population_non_specifies)]
    na_budget <- check_empty_col[, sum(groupes_de_population_non_specifies, na.rm = TRUE)]
    if (na_budget!=0){
      stop("Budgeted line items have NA for all key variables - review drop conditions before dropping NAs in module and intervention")} else{
        budget.long[,('groupes_de_population_non_specifies'):=NULL]
      }
  }
    
    # sum across module, intervention, budget_year, population
    budget.long[,budget:=as.numeric(budget)]
    budget.long <- budget.long[,.(budget=sum(budget, na.rm = TRUE)), by=c('module', 'intervention', 'population', 'budget_year')]

   return(budget.long)
}
