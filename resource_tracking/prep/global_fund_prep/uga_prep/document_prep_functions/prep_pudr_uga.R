# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen
# PURPOSE: Prep PU/DRs for Guatemala. 
# DATE: Last updated January 2019. 

# Add in precondition for what type of inputs are acceptable. 
# Add expected result of file here (postcondition)
# ----------------------------------------------


# start function
prep_pudr_uga = function(dir, inFile, sheet_name, start_date, disease, period, grant, recipient, source) {
  
  #TROUBLESHOOTING HELP
  #Uncomment variables below and run line-by-line. 
  
  # folder = "budgets"
  # folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  # file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
  # 
  # dir = file_dir
  # inFile = file_list$file_name[i]
  # sheet_name = file_list$sheet[i]
  # start_date = file_list$start_date[i]
  # period = file_list$period[i]
  # disease = file_list$disease[i]
  # grant = file_list$grant[i]
  # recipient = file_list$primary_recipient
  # source = file_list$data_source[i]
#   
#   # --------------------
#   # Test the inputs to make sure that they are the correct type
#   if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
#   if (class(year)=='character') stop('Error: year argument must be a number!')
#   # ----------------------------------------------
#   # Files and directories
#   
#   # Load/prep data
#   gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
#   
#   #General function for grants. 
#   #-------------------------------------
#   # 1. Subset columns. 
#   #-------------------------------------
#   #Find the correct column indices based on a grep condition.  
#   module_col <- grep("Modular Approach - Modules", gf_data)
#   intervention_col <- grep("Modular Approach - Interventions", gf_data)
#   budget_col <- grep("Budget for Reporting Period", gf_data)
#   expenditure_col <- grep("Actual Expenditure", gf_data)
#   
#   #We don't have modules and interventions for this type of file, and column headers are labeled differently.  
#   if (sheet_name == "LFA_Total PR Cash Outflow_3A"){
#     module_col = NA
#     intervention_col = NA
#     budget_col <- grep("LFA-Verified Budget for Reporting Period", gf_data)
#     expenditure_col <- grep("LFA-Verified Actual for Reporting Period", gf_data)
#   }
#   
#   if(sheet_name == "LFA_Total PR Cash Outflow_3A"){ #For this type of file, keep the first finding from grep. 
#     budget_col = budget_col[1]
#     expenditure_col = expenditure_col[1]
#   }
#   
#   #Remove the 'cumulative expenditure' and 'cumulative budget' columns. 
#   if (length(expenditure_col)!=1){
#     cumulative_expenditure_col <- grep("Cumulative Actual Expenditure", gf_data) #Remove the 'cumulative expenditure' column. 
#     for (i in 1:length(expenditure_col)){
#       if (expenditure_col[i] %in% cumulative_expenditure_col){
#         expenditure_col = expenditure_col[-i]
#       }
#     }
#   }
#   
#   if (length(budget_col)!=1){
#     cumulative_budget_col <- grep("Cumulative Budget", gf_data)
#     for (i in 1:length(budget_col)){
#       if (budget_col[i] %in% cumulative_budget_col){
#         budget_col = budget_col[-i]
#       }
#     }
#   }
#   
#   #Validate these column indices, and assign column names. 
#   stopifnot(length(budget_col)==1 & length(expenditure_col)==1)
#   colnames(gf_data)[budget_col] <- "budget"
#   colnames(gf_data)[expenditure_col] <- "expenditure"
#   
#   #Check to see if this file has module and intervention. 
#   if (is.na(module_col)){
#     gf_data$module <- "Unspecified"
#   } else {
#     stopifnot(length(module_col)==1)
#     colnames(gf_data)[module_col] <- "module"
#   }
#   
#   if (is.na(intervention_col)){
#     gf_data$intervention <- "Unspecified"
#   } else {
#     stopifnot(length(intervention_col)==1)
#     colnames(gf_data)[intervention_col] <- "intervention"
#   }
#   
#   #Subset to only these columns. 
#   gf_data = gf_data[, .(module, intervention, budget, expenditure)][order(module, intervention, budget, expenditure)] 
#   
#   #-------------------------------------
#   # 2. Subset rows 
#   #-------------------------------------
#   #Select only the section of the excel that's broken up by intervention 
#   start_row <- grep("modular approach", tolower(gf_data$module))
#   end_row <- grep("grand total", tolower(gf_data$module))
#   
#   if(sheet_name == "LFA_Total PR Cash Outflow_3A"){
#     
#   }
#   
#   x = 1
#   while (end_row[x] < start_row){
#     x = x + 1
#   }
#   end_row = end_row[x]
#   
#   #Validate that these are correct 
#   stopifnot(length(start_row)==1 & length(end_row)==1)
#   gf_data = gf_data[start_row:end_row, ]
#   
#   #Rename data, and remove invalid rows 
#   check_drop <- gf_data[((is.na(module) | module == '0') & (is.na(intervention) | intervention == '0')),]
#   if (verbose == TRUE){
#     print(paste0("Invalid rows currently being dropped: (only module and intervention columns shown) ", check_drop[, c('module', 'intervention')]))
#   }
#   gf_data<-  gf_data[!((is.na(module) | module == '0') & (is.na(intervention) | intervention == '0')),]
#   
#   #Some datasets have an extra title row with "[Module]" in the module column. 
#   #It's easier to find this by grepping the budget column, though. 
#   extra_module_row <- grep("budget for reporting period", tolower(gf_data$budget))  
#   if (length(extra_module_row) > 0){
#     if (verbose == TRUE){
#       print(paste0("Extra rows being dropped in GTM PU/DR prep function. First column: ", gf_data[extra_module_row, 1]))
#     }
#     gf_data <- gf_data[-extra_module_row, ,drop = FALSE]
#   }
#   
#   #Remove 'total' and 'grand total' rows 
#   total_rows <- grep("total", tolower(gf_data$module))
#   if (length(total_rows) > 0){
#     if (verbose == TRUE){
#       print(paste0("Total rows being dropped in GTM PU/DR prep function. First column: ", gf_data[total_rows, 1]))
#     }
#     gf_data <- gf_data[-total_rows, ,drop = FALSE]
#   }
#   
#   #Replace any modules or interventions that didn't have a pair with "Unspecified". 
#   gf_data[is.na(module) & !is.na(intervention), module:="Unspecified"]
#   gf_data[module == '0' & !is.na(intervention), module:="Unspecified"]
#   gf_data[!is.na(module) & is.na(intervention), intervention:="Unspecified"]
#   gf_data[!is.na(module) & intervention == '0', intervention:="Unspecified"]
#   
#   #-------------------------------------
#   # 3. Generate new variables 
#   #-------------------------------------
#   gf_data$start_date <- start_date
#   gf_data$data_source <- source
#   gf_data$period <- period
#   gf_data$disease <- disease
#   gf_data$grant_number <- grant
#   gf_data$year <- year(gf_data$start_date)
#   
#   gf_data$cost_category <- "all" 
#   gf_data$sda_activity <- "all"
#   
#   #-------------------------------------
#   # 4. Validate data  
#   #-------------------------------------
#   budget_dataset = gf_data 
#   
#   #Check to make sure budget and expenditure can be converted to numeric safely, 
#   # and the total for these columns is not '0' for the file. (may have grabbed wrong column). 
#   stopifnot(class(budget_dataset$budget) == 'character' & class(budget_dataset$expenditure)=='character')
#   #At this point, we know that if we have a line in the budget with NA, there was $0 budgeted for that module/intervention
#   # in this time period. 
#   # budget_dataset[is.na(budget), budget:='0']
#   # budget_dataset[is.na(expenditure), expenditure:='0']
#   
#   budget_dataset[, budget:=as.numeric(budget)]
#   budget_dataset[, expenditure:=as.numeric(expenditure)]
#   
#   #Check these by summing the total for the file, and making sure it's not 0. 
#   check_budgets = budget_dataset[ , 
#                   lapply(.SD, sum, na.rm = TRUE), 
#                   .SDcols = c("budget", "expenditure")]
#   
#   stopifnot(check_budgets[, 1]>0 & check_budgets[, 2]>0)
#   
#   #Check column names, and that you have at least some valid data for the file. 
#   if (nrow(budget_dataset)==0){
#     stop(paste0("All data dropped for ", inFile))
#   }
#   
#   #--------------------------------
#   # Note: Are there any other checks I could add here? #EKL
#   # -------------------------------
#   
#   
#   return(budget_dataset)
# }
  # Load/prep data
    gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))

  
  if (inFile == "UGA-S-TASO_PU_PEJune2017_LFA_30Nov17.xlsx" | inFile == "UGA-M-TASO_PU_PEJune2017_LFA_30Nov17.xlsx"){ #Emily what is this line doing?
    gf_data = gf_data[-1, ]
  }
  ## the malaria PUDRs are set up differently than the HIV ones
  if(grant%in%c("UGD-708-G08-M") & sheet_name=="EFR Malaria Financial Data_3B"){
    gf_data <- gf_data[, -c(1:3)]
    colnames(gf_data)[1] <- "module"
    colnames(gf_data)[2] <- "budget"
    colnames(gf_data)[3] <- "expenditure"
    gf_data <- gf_data[c(grep("service de", tolower(gf_data$module)):grep("type of", tolower(gf_data$module))),]
    gf_data <- gf_data[-nrow(gf_data) ,drop = FALSE]
    budget_dataset <- gf_data[, c("module", "budget", "expenditure"),with=FALSE]
    budget_dataset<- budget_dataset[!is.na(budget_dataset$module),]
    budget_dataset$recipient <- recipient
    budget_dataset$intervention <- "All" ## change if we ever get more detailed PUDR info

    } else if(grant%in%c("UGD-708-G08-M") & sheet_name=="LFA_Annex-SR Financials"){ ## we have SR info for this grant
      gf_data <- gf_data[, -1]
      colnames(gf_data)[1] <- "recipient"
      colnames(gf_data)[5] <- "budget"
      colnames(gf_data)[6] <- "disbursement"
      gf_data <- gf_data[c(grep("name of", tolower(gf_data$recipient)):grep("total", tolower(gf_data$recipient))),]
      gf_data <- gf_data[-nrow(gf_data) ,drop = FALSE]
      budget_dataset <- gf_data[, c("recipient", "budget", "disbursement"),with=FALSE]
      budget_dataset<- budget_dataset[!is.na(budget_dataset$recipient),]
      budget_dataset$module <- "All"
      budget_dataset$intervention <- "All" ## change if we ever get more detailed PUDR info
      ## drop 1st row since it doesn't contain any useful inforamtion:
      budget_dataset <- budget_dataset[-1, ,drop = FALSE]

    } else if (sheet_name%in%c("LFA Expenditure_7B", "PR Expenditure_7A")){ ## for the PUDRs w/out SR info
      if(year(start_date) == 2018){ #This section is okay.
        gf_data = gf_data[, 3:length(gf_data)]
      }

      #Grab columns based on their contents. These indices are not accurate for all files.
      module_col <- grep("Modular Approach - Modules", gf_data)
      intervention_col <- grep("Modular Approach - Interventions", gf_data)
      budget_col <- grep("Budget for Reporting Period", gf_data)
      expenditure_col <- grep("Actual Expenditure", gf_data)

      #Remove the 'cumulative expenditure' column.
      cumulative_expenditure_col <- grep("Cumulative Actual Expenditure", gf_data) #Remove the 'cumulative expenditure' column.
      for (i in 1:length(expenditure_col)){
        if (expenditure_col[i] %in% cumulative_expenditure_col){
          expenditure_col = expenditure_col[-i]
        }
      }
      stopifnot(length(module_col)==1 & length(intervention_col)==1 & length(budget_col)==1 & length(expenditure_col)==1)

      #Assign column names
      colnames(gf_data)[module_col] <- "module"
      colnames(gf_data)[intervention_col] <- "intervention"
      colnames(gf_data)[budget_col] <- "budget"
      colnames(gf_data)[expenditure_col] <- "expenditure"


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
      if (inFile == "UGA-S-TASO_PU_PEJune2017_LFA_30Nov17.xlsx" | inFile == "UGA-M-TASO_PU_PEJune2017_LFA_30Nov17.xlsx"){ #Emily what is this line doing?
        gf_data = gf_data[-1, ]
      }
      if (inFile == "LFA reviewed UGA-T-MoFPED PUDR PE 31 December 2017 25 May 18.xlsx"){ #This file has some extra rows. #Emily can you do this more generally?
        gf_data = gf_data[5:18, ]
      }
      budget_dataset <- gf_data[, c("module","intervention", "budget", "expenditure"),with=FALSE]
      budget_dataset<-  budget_dataset[!is.na(module) & module != '0',]
      budget_dataset<- budget_dataset[!is.na(intervention),]

      budget_dataset$recipient <- recipient
      #Remove extra title rows.
      extra_module_row <- grep("budget for reporting period", tolower(budget_dataset$budget))
      if (length(extra_module_row) > 0){
        #Emily want to add a 'verbose' debugging prompt here.
        budget_dataset <- budget_dataset[-extra_module_row, ,drop = FALSE]
      }
    } else if (sheet_name%in%c('LFA_Total PR Cash Outflow_3A','LFA_Total PR Cash Outflow_3')){ ## for the PUDRs w/out module/intervention detail
      colnames(gf_data)[1] <- "description"
      colnames(gf_data)[3] <- "budget"
      colnames(gf_data)[4] <- "expenditure"
      budget_dataset <- gf_data[, c("description", "budget", "expenditure"),with=FALSE]
      budget_dataset$recipient = ifelse(grepl("PR's total expenditures", budget_dataset$description), "MoFPED",
                                        ifelse(grepl("sub-recipients", budget_dataset$description), "SR", NA))
      budget_dataset<-na.omit(budget_dataset)
      budget_dataset$module <- "All"
      budget_dataset$intervention <- "All"
      budget_dataset$description = NULL
    } else if (inFile == "LFA reviewed UGA-T-MOFPED PUDR PE 31Dec2015_final.xlsx"){
      colnames(gf_data)[1] <- "description"
      colnames(gf_data)[2] <- "module"
      colnames(gf_data)[3] <- "budget"
      colnames(gf_data)[5] <- "expenditure"
      gf_data <- gf_data[c(grep("module", tolower(gf_data$description)):grep(0, tolower(gf_data$module))),]
      budget_dataset <- gf_data[, c("module", "budget", "expenditure"),with=FALSE]
      budget_dataset<- budget_dataset[!is.na(budget_dataset$module),]
      budget_dataset$recipient <- recipient #EKL need to debug this - the lengths of these don't match up. Are we assigning columns randomly?

      #This one has Module and intervention combined -- cleaning it up!
      budget_dataset$module = ifelse(budget_dataset$module == "TB/HIV - Community TB care delivery", "TB/HIV - Community TB/HIV care delivery",  budget_dataset$module)
      dt_budge = budget_dataset[substring(budget_dataset$module, 1, 2) != "MD" & substring(budget_dataset$module, 1, 2) != "HS" & substring(budget_dataset$module, 1, 2) != "Co"]
      dt_budge = separate(data = dt_budge, col = module, into = c("module", "intervention"), sep = "-")
      dt_budge_1 = budget_dataset[substring(budget_dataset$module, 1, 2) == "MD"]
      dt_budge_1$intervention = substring(dt_budge_1$module, 9, nchar(dt_budge_1$module))
      dt_budge_1$module = "MDR-TB"
      dt_budge_2 = budget_dataset[substring(budget_dataset$module, 1, 2) == "HS"]
      dt_budge_2$intervention = substring(dt_budge_2$module, 44, nchar(dt_budge_2$module))
      dt_budge_2$module = "HSS - Health information systems and M&E"
      dt_budge_3 = budget_dataset[substring(budget_dataset$module, 1, 2) == "Co"]
      dt_budge_3$intervention = ifelse(substring(dt_budge_3$module, 35, 43) == "Community", "Community-based monitoring for accountability", "Social mobilization, building community linkages, collaboration and coordination" )
      dt_budge_3$module = "Community systems strengthening"
      budget_dataset = rbind(dt_budge, dt_budge_1, dt_budge_2, dt_budge_3)

      #budget_dataset$intervention <- "All" ## change if we ever get more detailed PUDR info
      ## drop 1st row since it doesn't contain any useful inforamtion:
      budget_dataset <- budget_dataset[-1, ,drop = FALSE]

    } else {  ##change this if we get a new PUDR that doesn't fit any of these three
      print(paste0("An else-loop was triggered in the UGA PUDR prep function - verify outputs are correct for file ", inFile))
    colnames(gf_data)[1] <- "description"
    colnames(gf_data)[2] <- "module"
    colnames(gf_data)[3] <- "budget"
    colnames(gf_data)[5] <- "expenditure"
    gf_data <- gf_data[c(grep("module", tolower(gf_data$description)):grep(0, tolower(gf_data$module))),]
    budget_dataset <- gf_data[, c("module", "budget", "expenditure"),with=FALSE]
    budget_dataset<- budget_dataset[!is.na(budget_dataset$module),]
    budget_dataset$recipient <- recipient
    budget_dataset$intervention <- "All" ## change if we ever get more detailed PUDR info
    ## drop 1st row since it doesn't contain any useful inforamtion:
    budget_dataset <- budget_dataset[-1, ,drop = FALSE]
    }

  #Remove all invalid inputs.
  budget_dataset = budget_dataset[!is.na(module) & module != 0]


  # return prepped data
 return(budget_dataset)
}


