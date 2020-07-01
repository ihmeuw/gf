# ----------------------------------------------
# AUTHOR: Francisco Rios Casas, based on code written by Emily Linebarger, Irena Chen
# PURPOSE: Prep the qualitative data from commonly-formatted PU/DRs across countries. 
# DATE: Last updated June 2020. 

# For this function to work properly, a file must have a section labeled "Modular approach" with columns for module, intervention, budget, and expenditure. 
# Returns a file called budget dataset with columns for module, intervention, budget, and expenditure. 
# ----------------------------------------------

#Sheet names that don't work so far: "LFA EFR_7", "LFA_Annex-SR Financials", "LFA_Total PR Cash Outflow_3", "LFA_Total PR Cash Outflow_3A"

# start function
prep_modular_approach_pudr_qualitative =  function(dir, inFile, sheet_name, start_date) {
  
  #TROUBLESHOOTING HELP
  #Uncomment variables below and run line-by-line. 
  # Set up file path 
  
  # folder = "budgets"
  # folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  # if (file_list$file_iteration[i]=="initial"){
  #   version = "iterations"
  # } else if (file_list$file_iteration[i]=="revision"){
  #   version= "revisions"
  # } else {
  #   version = ""
  # }
  # grant_period = file_list$grant_period[i]
  # 
  # file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", grant_period, "/", folder, "/")
  # if (version != ""){
  #   file_dir = paste0(file_dir, version, "/")
  # }
  # dir = file_dir
  # inFile = file_list$file_name[i]
  
  # sheet_name = file_list$sheet_financial[i] # change manually if prepping the PR Expenditure sheets
  
  # start_date = file_list$start_date_financial[i]
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
  verified_sheet_names <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B', 'ALF RFR_7')
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
  module_col <- grep("Modular Approach - Modules|Démarche modulaire - Modules", gf_data)
  intervention_col <- grep("Modular Approach - Interventions|Démarche modulaire - Interventions", gf_data)
  budget_col <- grep("Budget for Reporting Period", gf_data)
  expenditure_col <- grep("Actual Expenditure", gf_data)
  lfa_adjustment_col <- grep("Local Fund Agent Adjustment on Expenditures", gf_data)
  cumulative_budget_col = grep("Cumulative Budget", gf_data)
  cumulative_expenditure_col = grep("Cumulative Expenditure|Cumulative Actual Expenditure", gf_data)
  absorption_col = grep("Absorption|absorción", gf_data)
  
  if (sheet_name == "LFA Expenditure_7B"){
    lfa_comment_col <- grep("comments|commentaires|comentarios", gf_data)
   } else if (sheet_name=="PR Expenditure_7A") {
      pr_comment_col <- grep("Explanation|Explication|Explicación", gf_data)
    }
  
  # This might or might not be necessary depending on how far back we are going, but if yes, then the comments_col should be added to each of these different sheet names
  
  # if (sheet_name == "ALF RFR_7"){
  #   module_col = grep("Macrocatégorie", gf_data)
  #   intervention_col = grep("Domaine de prestation de services", gf_data)
  #   budget_col = grep("Budget", gf_data) 
  #   budget_col = budget_col[1] #Just want the first observation of budget. 
  #   if (intervention_col+1 != budget_col){
  #     stop("This file has a different format than ones reviewed before. Review raw data")
  #   }
  #   lfa_adjustment_col = grep("Ajustement au budget par l'ALF", gf_data)
  #   expenditure_col = grep("Dépenses", gf_data)
  #   expenditure_col = expenditure_col[2] #You want the second observation here, to skip LFA adjustment column. 
  #   if (inFile == "Plan TB PUDR 2016.xlsx"){
  #     expenditure_col = 7
  #   }
  #   if (is.na(lfa_adjustment_col) | is.na(expenditure_col)) stop("Expenditure or LFA adjustment columns are NA")
  #   if ((lfa_adjustment_col+1) != expenditure_col){
  #     stop("This file has a different format than ones reviewed before. Review raw data")
  #   }
  # }

  #Remove the 'cumulative expenditure' and 'cumulative budget' columns and the 'cumulative absorption rate'
  if (length(expenditure_col)!=1){
    cumulative_expenditure_drop <- grep("Cumulative Expenditure|Cumulative Actual Expenditure", gf_data) #Remove the 'cumulative expenditure' column.
    for (i in 1:length(expenditure_col)){
      if (expenditure_col[i] %in% cumulative_expenditure_drop){
        expenditure_col = expenditure_col[-i]
      }
    }
  }

  if (length(budget_col)!=1){
    cumulative_budget_drop <- grep("Cumulative Budget", gf_data)
    for (i in 1:length(budget_col)){
      if (budget_col[i] %in% cumulative_budget_drop){
        budget_col = budget_col[-i]
      }
    }
  }
  
  if (length(absorption_col)!=1) {
        absorption_col = absorption_col[1]
      }

  #Validate these column indices, and assign column names.
  stopifnot(length(budget_col)==1 & length(expenditure_col)==1 & length(absorption_col)==1)
  colnames(gf_data)[budget_col] <- "budget"
  colnames(gf_data)[expenditure_col] <- "expenditure"
  colnames(gf_data)[absorption_col] <- "absorption_rate"
  
  if (sheet_name == "LFA Expenditure_7B"){
    colnames(gf_data)[lfa_comment_col] <- "comments"
  } else if (sheet_name=="PR Expenditure_7A") {
    colnames(gf_data)[pr_comment_col] <- "comments"
  }
  
  if (sheet_name!="PR Expenditure_7A"){
    stopifnot(length(lfa_adjustment_col)==1)
    colnames(gf_data)[lfa_adjustment_col] <- "lfa_exp_adjustment"
  }

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
  
  # #Validate cumulative columns 
  # if (length(cumulative_budget_col)>1) cumulative_budget_col = cumulative_budget_col[1]
  # stopifnot(length(cumulative_budget_col)==1)
  # colnames(gf_data)[cumulative_budget_col] <- "cumulative_budget"
  # stopifnot(length(cumulative_expenditure_col)==1)
  # colnames(gf_data)[cumulative_expenditure_col] <- "cumulative_expenditure"
  
  #Subset to only these columns.
  if (sheet_name!="PR Expenditure_7A"){
    gf_data = gf_data[, .(module, intervention, absorption_rate, comments)]
  } else {
    gf_data = gf_data[, .(module, intervention, absorption_rate, comments)]
  }
  
  # #Make budget and expenditure numeric, and make LFA expenditure adjustment column 0 if NA (for subtraction later). 
  # for (var in c('budget', 'expenditure', 'cumulative_budget', 'cumulative_expenditure')){
  #   gf_data[, (var):=as.numeric(get(var))]
  # } 
  # 
  # if (sheet_name!="PR Expenditure_7A"){
  #   gf_data[, lfa_exp_adjustment:=as.numeric(lfa_exp_adjustment)]
  #   gf_data[is.na(lfa_exp_adjustment), lfa_exp_adjustment:=0]
  # }
  
  # # make absorption_rate numeric
  # for (var in ('absorption_rate')){
  #   gf_data[,(var):=as.numeric(get(var))]
  # }
  gf_data[,absorption_rate:=as.numeric(absorption_rate)]
  
  # make absorption_rate a percentage
  gf_data[,absorption_rate:=percent(absorption_rate, accuracy = .1)]
  
  # Add in column to indicate the source of the information
  gf_data[,sheet_name:=sheet_name]

  #-------------------------------------
  # 2. Subset rows
  #-------------------------------------
  # Previously we only cared about the section of the excel that corresponded to interventions, but this version will extract for 
  # cost category and by 
  # implementing entity
  
  # decision -- we can either keep the three types of data seperately and that would involve identifying the rows where the data starts and ends
  # OR
  # we can keep all of the data from the same PUDR sheet together and then add a column for "sheet_name" and modify the identifying columns (intervention, cost category, implementing entity)
  
  # will continue with the second path above
  
  # remove breakdown rows--which will make selecting additional rows easier
  breakdown_rows <- grep("breakdown|récapitulatif", tolower(gf_data$module))
  if (length(breakdown_rows) > 0){
    if (verbose == TRUE){
      print(paste0("Total rows being dropped in PU/DR Qualitative Data prep function. First column: ", gf_data[breakdown_rows, 1]))
    }
    gf_data <- gf_data[-breakdown_rows, ,drop = FALSE]
  }
  
  # I think the safest way to drop rows is to keep only those that have an identifier in the first column (currently called module)
  if ('lfa_exp_adjustment'%in%names(gf_data)){
    check_drop <- gf_data[((is.na(module) | module == '0' | module=="Veuillez sélectionner..." )),]
    #& (is.na(intervention) | intervention == '0' | intervention == "Veuillez sélectionner...") 
    #& (is.na(budget)|budget==0) & (is.na(expenditure)|expenditure==0) & (is.na(lfa_exp_adjustment) | lfa_exp_adjustment==0)), ]
  } else { 
    check_drop <- gf_data[((is.na(module) | module == '0' | module=="Veuillez sélectionner..." )),] 
    # & (is.na(intervention) | intervention == '0' | intervention == "Veuillez sélectionner...") 
    # & (is.na(budget)|budget==0) & (is.na(expenditure)|expenditure==0)), ]
  }
  if (verbose == TRUE){
    print(paste0("Invalid rows currently being dropped: (keeping rows relating to intervention, cost category, and implementing entity). ", check_drop[, c('module', 'intervention')]))
  }
  if ('lfa_exp_adjustment'%in%names(gf_data)){
    gf_data = gf_data[!((is.na(module) | module == '0' | module=="Veuillez sélectionner..." )), ] 
    # & (is.na(intervention) | intervention == '0' | intervention == "Veuillez sélectionner...") 
    # & (is.na(budget)|budget==0) & (is.na(expenditure)|expenditure==0) & (is.na(lfa_exp_adjustment) | lfa_exp_adjustment==0)), ]
  } else {
    gf_data = gf_data[!((is.na(module) | module == '0' | module=="Veuillez sélectionner..." )),] 
    # & (is.na(intervention) | intervention == '0' | intervention == "Veuillez sélectionner...") 
    # & (is.na(budget)|budget==0) & (is.na(expenditure)|expenditure==0)), ]
  }
  
  # #Some datasets have an extra title row with "[absorption rate]" in the absorption_rate column.
  #It's easier to find this by grepping the absorption_rate column, though.
  extra_absorption_row <- grep("absorption rate", tolower(gf_data$absorption_rate))
  extra_absorption_row = c(extra_absorption_row, grep("verified", tolower(gf_data$module))) # and extra rows indicating if LFA verified
  if (length(extra_absorption_row) > 0){
    if (verbose == TRUE){
      print(paste0("Extra rows being dropped in GTM PU/DR prep function. First column: ", gf_data[extra_absorption_row, 1]))
    }
    gf_data <- gf_data[-extra_absorption_row, ,drop = FALSE]
  }
  
  # create an indicator for sheet_name as well as the breakdown_type ("Costing Dimension" or "Modular Approach" or "Implementing Entity")
  
  ######### Select the section of the excel that's broken up by module ########################################
  start_row_m <- grep("modular approach|démarche modulaire", tolower(gf_data$module))
  end_row_m <- grep("grand total|total général", tolower(gf_data$module))

  if (sheet_name == "ALF RFR_7"){
    start_row_m = grep("Macrocatégorie", gf_data$module)
    end_row1_m = grep("Veuillez sélectionner...", gf_data$module)
    end_row2_m = grep("Veuillez sélectionner...", gf_data$intervention)
    end_row_m = end_row1_m[end_row1_m%in%end_row2_m]
  }
  
  x = 1
  while (end_row_m[x] < start_row_m){
    x = x + 1
  }
  end_row_m = end_row_m[x]
  
  # #Validate that these are correct
  stopifnot(length(start_row_m)==1 & length(end_row_m)==1)
  
  gf_data = gf_data[start_row_m:end_row_m,breakdown_type:="Modular_approach"]
  
  #### Cost category   ########################################################
  
  #Select the section of the excel that's broken up by cost category
  start_row_c <- grep("costing dimension|évaluation des coûts", tolower(gf_data$module))
  end_row_c <- grep("grand total|total général", tolower(gf_data$module))
  
  # if (sheet_name == "ALF RFR_7"){
  #   start_row_c = grep("Macrocatégorie", gf_data$module)
  #   end_row1_m = grep("Veuillez sélectionner...", gf_data$module)
  #   end_row2_m = grep("Veuillez sélectionner...", gf_data$intervention)
  #   end_row_m = end_row1_m[end_row1_m%in%end_row2_m]
  # }
  
  x = 1
  while (end_row_c[x] < start_row_c){
    x = x + 1
  }
  end_row_c = end_row_c[x]
  
  # #Validate that these are correct
  stopifnot(length(start_row_c)==1 & length(end_row_c)==1)
  
  gf_data = gf_data[start_row_c:end_row_c,breakdown_type:="Cost_category"]
  
  #### Implementing entity ########################################################
  #Select the section of the excel that's broken up by cost category
  start_row_i <- grep("implementing", tolower(gf_data$module))
  end_row_i <- grep("grand total|total général", tolower(gf_data$module))
  
  # if (sheet_name == "ALF RFR_7"){
  #   start_row_c = grep("Macrocatégorie", gf_data$module)
  #   end_row1_m = grep("Veuillez sélectionner...", gf_data$module)
  #   end_row2_m = grep("Veuillez sélectionner...", gf_data$intervention)
  #   end_row_m = end_row1_m[end_row1_m%in%end_row2_m]
  # }
  
  x = 1
  while (end_row_i[x] < start_row_i){
    x = x + 1
  }
  end_row_i = end_row_i[x]
  
  # #Validate that these are correct
  stopifnot(length(start_row_i)==1 & length(end_row_i)==1)
  
  gf_data = gf_data[start_row_i:end_row_i,breakdown_type:="Implementing_entity"]

  # #Remove 'total' and 'grand total' rows
  total_rows <- grep("total", tolower(gf_data$module))
  if (length(total_rows) > 0){
    if (verbose == TRUE){
      print(paste0("Total rows being dropped in GTM PU/DR prep function. First column: ", gf_data[total_rows, 1]))
    }
    gf_data <- gf_data[-total_rows, ,drop = FALSE]
  }

  #Replace any modules or interventions that didn't have a pair with "Unspecified".
  # gf_data[is.na(module) | module == '0' | module == "Veuillez sélectionner..." , module:="Unspecified"]
  # gf_data[is.na(intervention) | intervention == '0' | intervention == "Veuillez sélectionner...", intervention:="Unspecified"]

  # remove additional rows
  #Some datasets have an extra title row with "[Module]" in the module column.
  #It's easier to find this by grepping the budget column, though.
  extra_module_row <- grep("budget for reporting period", tolower(gf_data$budget))
  extra_module_row = c(extra_module_row, grep("module|macrocatégorie", tolower(gf_data$module)))
  if (length(extra_module_row) > 0){
    if (verbose == TRUE){
      print(paste0("Extra rows being dropped in Qualitative PU/DR data prep function. First column: ", gf_data[extra_module_row, 1]))
    }
    gf_data <- gf_data[-extra_module_row, ,drop = FALSE]
  }
  
  # remove extra header rows
  extra_header_row <- grep("explanation of variances|explication des écarts", tolower(gf_data$comments))
  # extra_header_row <- c(extra_header_row, grep("explanation of variances| explication des écarts", tolower(gf_data$comments)))
  if (length(extra_header_row) > 0){
   if (verbose == TRUE){
     print(paste0("Extra rows being dropped in Qualitative PU/DR data prep function. First column: ", gf_data[extra_header_row, 1]))
   }
   gf_data <- gf_data[-extra_header_row, ,drop = FALSE]
  }

  #-------------------------------------
  # 4. Validate data
  #-------------------------------------
  budget_dataset = gf_data

  # #Check these by summing the total for the file, and making sure it's not 0.
  # check_budgets = budget_dataset[ ,
  #                 lapply(.SD, sum, na.rm = TRUE),
  #                 .SDcols = c("budget", "expenditure")]
  # 
  # verified_0_expenditure <- c("UGA-C-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", "UGA-M-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", 
  #                             "UGA-S-TASO_PU_PEJune2017_LFA_30Nov17.xlsx", "GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.xlsx", 
  #                             "GTM-T-MSPAS_Progress Report jul _31Dec2018_v2  rev LFA.xlsx", "GTM-H-HIVOS_Progress Report_31Dec2018_v1.xlsx", 
  #                             "GTM-T-MSPAS_Progress Report_LFA18Mar19.xlsx", "Core_SANRU_PU_P3141116.xlsm", "PSI PU NFM S1 2016 09102016.xlsm", 
  #                             "Core_PUDR_P30_HivosGT_231116_ LFA Signed.xlsx", "Core_PUDR_MALARIA_P12_03-03-17_Revisado ALF.xlsx",  
  #                             "GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.XLSX", "GTM-M-MSPAS_Progress Report_30Jun2019_REV LFA.xlsx",
  #                             "Informe PUDR P-30 Noviembre 2016_Rev ALF FINAL.xlsx", "GTM-T-MSPAS_Progress Report_31Dec2019_v4.xlsx",
  #                             "GTM-T-MSPAS_Progress Report_31Dec2019_v Rev ALF_02032020.xlsx") #These files have 0 for all expenditure.
  # verified_0_budget <- c("Core_SANRU_PU_P3141116.xlsm", "PSI PU NFM S1 2016 09102016.xlsm", "Core_PUDR_P30_HivosGT_231116_ LFA Signed.xlsx", 
  #                        "Core_PUDR_MALARIA_P12_03-03-17_Revisado ALF.xlsx") #These files have 0 budgeted - maybe this was a draft file. 
  # 
  # if (!inFile%in%verified_0_expenditure){ #If expenditure shouldn't be 0, check it. 
  #   stopifnot(check_budgets[, 2]>0)
  # } else if (!inFile%in%verified_0_budget){ #If budget shouldn't be 0, check it.  
  #   stopifnot(check_budgets[, 1]>0)
  # } else if (!(inFile%in%verified_0_expenditure & inFile%in%verified_0_budget)){ #Check both budget and expenditure 
  #   stopifnot(check_budgets[, 1]>0 & check_budgets[, 2]>0)
  #   if (verbose){
  #     print("File has verified 0 budget and 0 expenditure.")
  #   }
  # } 
  
  # if (verbose){
  #   if (inFile%in%verified_0_expenditure){
  #     print("File has verified 0 expenditure.")
  #   } else if (inFile%in%verified_0_budget){
  #     print("File has verified 0 budget.")
  #   }
  # }
  
  # pending --return

  # -------------------------------


  return(budget_dataset)

}


