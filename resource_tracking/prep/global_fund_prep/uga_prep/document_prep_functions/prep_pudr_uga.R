# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen
# PURPOSE: Prep PU/DRs for Guatemala. 
# DATE: Last updated January 2019. 

# Add conditions here for what types of files are valid inputs. (precondition) EKL
# Add expected result of file here (postcondition)
# ----------------------------------------------


# start function
prep_pudr_uga = function(dir, inFile, sheet_name, start_date, disease, period, grant, recipient, source) {
  
  #TROUBLESHOOTING HELP
  #Uncomment variables below and run line-by-line. 
  
  dir = file_dir
  inFile = file_list$file_name[i]
  sheet_name = file_list$sheet[i]
  start_date = file_list$start_date[i]
  period = file_list$period[i]
  disease = file_list$disease[i]
  grant = file_list$grant[i]
  recipient = file_list$primary_recipient
  source = file_list$data_source[i]
  
  # --------------------
  # Test the inputs to make sure that they are the correct type
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  
  
  #General function for grants. 
  
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
  
  
  # ## the malaria PUDRs are set up differently than the HIV ones
  # if(grant%in%c("UGD-708-G08-M") & sheet_name=="EFR Malaria Financial Data_3B"){
  #   gf_data <- gf_data[, -c(1:3)]
  #   colnames(gf_data)[1] <- "module"
  #   colnames(gf_data)[2] <- "budget"
  #   colnames(gf_data)[3] <- "expenditure"
  #   gf_data <- gf_data[c(grep("service de", tolower(gf_data$module)):grep("type of", tolower(gf_data$module))),]
  #   gf_data <- gf_data[-nrow(gf_data) ,drop = FALSE]
  #   budget_dataset <- gf_data[, c("module", "budget", "expenditure"),with=FALSE]
  #   budget_dataset<- budget_dataset[!is.na(budget_dataset$module),]
  #   budget_dataset$recipient <- recipient
  #   budget_dataset$intervention <- "All" ## change if we ever get more detailed PUDR info
  # 
  #   } else if(grant%in%c("UGD-708-G08-M") & sheet_name=="LFA_Annex-SR Financials"){ ## we have SR info for this grant
  #     gf_data <- gf_data[, -1]
  #     colnames(gf_data)[1] <- "recipient"
  #     colnames(gf_data)[5] <- "budget"
  #     colnames(gf_data)[6] <- "disbursement"
  #     gf_data <- gf_data[c(grep("name of", tolower(gf_data$recipient)):grep("total", tolower(gf_data$recipient))),]
  #     gf_data <- gf_data[-nrow(gf_data) ,drop = FALSE]
  #     budget_dataset <- gf_data[, c("recipient", "budget", "disbursement"),with=FALSE]
  #     budget_dataset<- budget_dataset[!is.na(budget_dataset$recipient),]
  #     budget_dataset$module <- "All"
  #     budget_dataset$intervention <- "All" ## change if we ever get more detailed PUDR info
  #     ## drop 1st row since it doesn't contain any useful inforamtion:  
  #     budget_dataset <- budget_dataset[-1, ,drop = FALSE]
  #     
  #   } else if (sheet_name%in%c("LFA Expenditure_7B", "PR Expenditure_7A")){ ## for the PUDRs w/out SR info
  #     if(year(start_date) == 2018){ #This section is okay. 
  #       gf_data = gf_data[, 3:length(gf_data)]
  #     }
  # 
  #     #Grab columns based on their contents. These indices are not accurate for all files. 
  #     module_col <- grep("Modular Approach - Modules", gf_data)
  #     intervention_col <- grep("Modular Approach - Interventions", gf_data)
  #     budget_col <- grep("Budget for Reporting Period", gf_data)
  #     expenditure_col <- grep("Actual Expenditure", gf_data)
  #     
  #     #Remove the 'cumulative expenditure' column. 
  #     cumulative_expenditure_col <- grep("Cumulative Actual Expenditure", gf_data) #Remove the 'cumulative expenditure' column. 
  #     for (i in 1:length(expenditure_col)){
  #       if (expenditure_col[i] %in% cumulative_expenditure_col){
  #         expenditure_col = expenditure_col[-i]
  #       }
  #     }
  #     stopifnot(length(module_col)==1 & length(intervention_col)==1 & length(budget_col)==1 & length(expenditure_col)==1)
  #     
  #     #Assign column names 
  #     colnames(gf_data)[module_col] <- "module"
  #     colnames(gf_data)[intervention_col] <- "intervention"
  #     colnames(gf_data)[budget_col] <- "budget"
  #     colnames(gf_data)[expenditure_col] <- "expenditure"
  #     
  #    
  #     #Select only the section of the excel that's broken up by intervention 
  #     start_row <- grep("modular approach", tolower(gf_data$module))
  #     end_row <- grep("grand total", tolower(gf_data$module))
  #     x = 1
  #     while (end_row[x] < start_row){
  #       x = x + 1
  #     }
  #     end_row = end_row[x]
  #     #Validate that these are correct 
  #     stopifnot(length(start_row)==1 & length(end_row)==1)
  #     gf_data = gf_data[start_row:end_row, ]
  #     if (inFile == "UGA-S-TASO_PU_PEJune2017_LFA_30Nov17.xlsx" | inFile == "UGA-M-TASO_PU_PEJune2017_LFA_30Nov17.xlsx"){ #Emily what is this line doing? 
  #       gf_data = gf_data[-1, ]
  #     }
  #     if (inFile == "LFA reviewed UGA-T-MoFPED PUDR PE 31 December 2017 25 May 18.xlsx"){ #This file has some extra rows. #Emily can you do this more generally? 
  #       gf_data = gf_data[5:18, ]
  #     }
  #     budget_dataset <- gf_data[, c("module","intervention", "budget", "expenditure"),with=FALSE]
  #     budget_dataset<-  budget_dataset[!is.na(module) & module != '0',]
  #     budget_dataset<- budget_dataset[!is.na(intervention),]
  #     
  #     budget_dataset$recipient <- recipient
  #     #Remove extra title rows. 
  #     extra_module_row <- grep("budget for reporting period", tolower(budget_dataset$budget))  
  #     if (length(extra_module_row) > 0){
  #       #Emily want to add a 'verbose' debugging prompt here. 
  #       budget_dataset <- budget_dataset[-extra_module_row, ,drop = FALSE]
  #     }
  #   } else if (sheet_name%in%c('LFA_Total PR Cash Outflow_3A','LFA_Total PR Cash Outflow_3')){ ## for the PUDRs w/out module/intervention detail
  #     colnames(gf_data)[1] <- "description"
  #     colnames(gf_data)[3] <- "budget"
  #     colnames(gf_data)[4] <- "expenditure"
  #     budget_dataset <- gf_data[, c("description", "budget", "expenditure"),with=FALSE]
  #     budget_dataset$recipient = ifelse(grepl("PR's total expenditures", budget_dataset$description), "MoFPED", 
  #                                       ifelse(grepl("sub-recipients", budget_dataset$description), "SR", NA))
  #     budget_dataset<-na.omit(budget_dataset)
  #     budget_dataset$module <- "All"
  #     budget_dataset$intervention <- "All" 
  #     budget_dataset$description = NULL
  #   } else if (inFile == "LFA reviewed UGA-T-MOFPED PUDR PE 31Dec2015_final.xlsx"){
  #     colnames(gf_data)[1] <- "description"
  #     colnames(gf_data)[2] <- "module"
  #     colnames(gf_data)[3] <- "budget"
  #     colnames(gf_data)[5] <- "expenditure"
  #     gf_data <- gf_data[c(grep("module", tolower(gf_data$description)):grep(0, tolower(gf_data$module))),]
  #     budget_dataset <- gf_data[, c("module", "budget", "expenditure"),with=FALSE]
  #     budget_dataset<- budget_dataset[!is.na(budget_dataset$module),]
  #     budget_dataset$recipient <- recipient #EKL need to debug this - the lengths of these don't match up. Are we assigning columns randomly? 
  #     
  #     #This one has Module and intervention combined -- cleaning it up!
  #     budget_dataset$module = ifelse(budget_dataset$module == "TB/HIV - Community TB care delivery", "TB/HIV - Community TB/HIV care delivery",  budget_dataset$module)
  #     dt_budge = budget_dataset[substring(budget_dataset$module, 1, 2) != "MD" & substring(budget_dataset$module, 1, 2) != "HS" & substring(budget_dataset$module, 1, 2) != "Co"]
  #     dt_budge = separate(data = dt_budge, col = module, into = c("module", "intervention"), sep = "-")
  #     dt_budge_1 = budget_dataset[substring(budget_dataset$module, 1, 2) == "MD"]
  #     dt_budge_1$intervention = substring(dt_budge_1$module, 9, nchar(dt_budge_1$module))
  #     dt_budge_1$module = "MDR-TB"
  #     dt_budge_2 = budget_dataset[substring(budget_dataset$module, 1, 2) == "HS"]
  #     dt_budge_2$intervention = substring(dt_budge_2$module, 44, nchar(dt_budge_2$module))
  #     dt_budge_2$module = "HSS - Health information systems and M&E"
  #     dt_budge_3 = budget_dataset[substring(budget_dataset$module, 1, 2) == "Co"]
  #     dt_budge_3$intervention = ifelse(substring(dt_budge_3$module, 35, 43) == "Community", "Community-based monitoring for accountability", "Social mobilization, building community linkages, collaboration and coordination" )
  #     dt_budge_3$module = "Community systems strengthening"
  #     budget_dataset = rbind(dt_budge, dt_budge_1, dt_budge_2, dt_budge_3)
  #     
  #     #budget_dataset$intervention <- "All" ## change if we ever get more detailed PUDR info
  #     ## drop 1st row since it doesn't contain any useful inforamtion:  
  #     budget_dataset <- budget_dataset[-1, ,drop = FALSE]
  #     
  #   } else {  ##change this if we get a new PUDR that doesn't fit any of these three 
  #     print(paste0("An else-loop was triggered in the UGA PUDR prep function - verify outputs are correct for file ", inFile))
  #   colnames(gf_data)[1] <- "description"
  #   colnames(gf_data)[2] <- "module"
  #   colnames(gf_data)[3] <- "budget"
  #   colnames(gf_data)[5] <- "expenditure"
  #   gf_data <- gf_data[c(grep("module", tolower(gf_data$description)):grep(0, tolower(gf_data$module))),]
  #   budget_dataset <- gf_data[, c("module", "budget", "expenditure"),with=FALSE]
  #   budget_dataset<- budget_dataset[!is.na(budget_dataset$module),]
  #   budget_dataset$recipient <- recipient
  #   budget_dataset$intervention <- "All" ## change if we ever get more detailed PUDR info
  #   ## drop 1st row since it doesn't contain any useful inforamtion:  
  #   budget_dataset <- budget_dataset[-1, ,drop = FALSE]
  #   }
  
  #Remove all invalid inputs. 
  # budget_dataset = budget_dataset[!is.na(module) & module != 0]
  
  budget_dataset$start_date <- start_date
  budget_dataset$data_source <- source
  budget_dataset$period <- period
  budget_dataset$sda_activity <- "all"## change if we ever get more detailed PUDR info
  budget_dataset$disease <- disease
  budget_dataset$cost_category <- "all" ## change if we ever get more detailed PUDR info
  budget_dataset$grant_number <- grant
  budget_dataset$year <- year(budget_dataset$start_date)

  # return prepped data
  return(budget_dataset)
}


