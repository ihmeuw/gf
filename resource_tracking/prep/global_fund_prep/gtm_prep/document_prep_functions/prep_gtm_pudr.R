# ----------------------------------------------
# Irena Chen
#
# 10/31/2017
# Template for prepping C-COIN budget data 
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object


# ----------------------------------------------
# function to prep the data
# ----------------------------------------------
prep_gtm_pudr = function(dir, inFile, sheet_name, start_date, qtr_num, disease, period, grant, source, loc_name, lang) {
  
  ######## TROUBLESHOOTING HELP
  ### fill in variables below: inFile, sheet_name, start_date, qtr_num, disease, period, lang, grant, recipient_name 
  ### with information from line where the code breaks, and then uncomment by "ctrl + shift + c" and run code line-by-line
  ### look at gf_data and find what is being droped where.
  ########
# 
#   folder = "budgets"
#   folder = ifelse (file_list$data_source[i] == "fpm" , folder, "pudrs")
#   file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
# 
#   dir = file_dir
#   inFile = file_list$file_name[i]
#   sheet_name = file_list$sheet[i]
#   start_date = file_list$start_date[i]
#   qtr_num = file_list$qtr_number[i]
#   period = file_list$period[i]
#   disease = file_list$disease[i]
#   lang = file_list$language[i]
#   grant = file_list$grant[i]
#   source = file_list$data_source[i]
#   loc_name = 'gtm'
  #
  # Load/prep data
  gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  
  #Remove diacritical marks from inFile so it can be used for if-else statements below 
  inFile = fix_diacritics(inFile)
  
  if((grant%in%"GTM-T-MSPAS"&sheet_name!="INTEGRACION"|sheet_name=="LFA Expenditure_7B")){
    colnames(gf_data)[1] <- "module"
    colnames(gf_data)[2] <- "intervention"
    colnames(gf_data)[3] <- "budget"
    colnames(gf_data)[4] <- "expenditure"
    gf_data$sda_activity <- "all"
    gf_data$recipient <- loc_name ##change this when we get SR info
    start_row <- grep("modul", tolower(gf_data$module))
    end_row <- grep("implem", tolower(gf_data$module))
    if (inFile == "GTM-T-MSPAS_Progress Report Disbursement_30Jun2017_EFREnglish_270917.xlsx"){
      start_row <- 29
      end_row <- 48 
    }
    if (inFile == "PUDR_P32_HivosGT_030518.xlsx"){
      start_row <- grep("Modular Approach", gf_data$module)
      end_row <- 106
    }
    if (inFile == "GTM-T-MSPAS_Progress Report_31Dec2017 LFA REVIEW.xlsx"){
      start_row <- grep("Modular Approach", gf_data$module)
      end_row <- 50
    }
    if (!(length(start_row)==1 & length(end_row) == 1)){
      stop(paste0("Incorrect parameters specified for subset. Verify grep condition for file: ", inFile))
    }
    gf_data <- gf_data[start_row:end_row,]
    gf_data <- gf_data[-1, ,drop = FALSE]
    gf_data <- gf_data[, c("module","sda_activity", "intervention", "budget", "expenditure"),with=FALSE]

    } else if (sheet_name=="LFA_Annex-SR Financials"){
      colnames(gf_data)[1] <- "recipient"
      colnames(gf_data)[5] <- "budget"
      colnames(gf_data)[6] <- "disbursement"
      colnames(gf_data)[11] <- "expenditure"
      gf_data <- gf_data[c(grep("entity", tolower(gf_data$recipient)):(grep("total", tolower(gf_data$recipient)))),]
      gf_data$sda_activity <- "all"
      gf_data$module <- "all"
      gf_data$intervention <- "all"
      gf_data <- gf_data[, c("module","sda_activity", "intervention", "budget", "expenditure", "disbursement"),with=FALSE]
  } else if (sheet_name=="INTEGRACION"){
      colnames(gf_data)[2] <- "code"
      colnames(gf_data)[3] <- "module"
      colnames(gf_data)[4] <- "budget"
      colnames(gf_data)[5] <- "expenditure"
      gf_data <- gf_data[c(grep("intid", tolower(gf_data$code)):(grep("costinput", tolower(gf_data$code)))),]
      gf_data <- gf_data[, c("module", "budget", "expenditure"),with=FALSE]
      gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
      setDT(gf_data)[, paste0("module", 1:2) := tstrsplit(module, "-")]
      gf_data$module <- NULL
      setnames(gf_data, c("module1", "module2"), c("module", "intervention"))
      gf_data$recipient <- loc_name
      gf_data$sda_activity <- "all"
      gf_data <- gf_data[, c("module","sda_activity", "intervention", "budget", "expenditure"),with=FALSE]
  } else if (sheet_name=="PR EFR_7A"){
    colnames(gf_data)[2] <- "module"
    colnames(gf_data)[3] <- "sda_activity"
    colnames(gf_data)[4] <- "intervention"
    colnames(gf_data)[5] <- "budget"
    colnames(gf_data)[6] <- "expenditure"
    gf_data$recipient <- loc_name
    gf_data <- gf_data[c(grep("object", tolower(gf_data$sda_activity)):(grep("name", tolower(gf_data$sda_activity)))),]
    gf_data <- gf_data[, c("module","sda_activity", "intervention", "budget", "expenditure"),with=FALSE]
  } else if (inFile == "GUA-M-MSPAS EFR FASE II -2016_Ingles_RevALF.xlsm") {
    colnames(gf_data)[2] <- "module"
    colnames(gf_data)[4] <- "intervention"
    colnames(gf_data)[5] <- "budget"
    colnames(gf_data)[7] <- "expenditure"
    start_row <- 33
    end_row <- 43
    gf_data = gf_data[start_row:end_row, .(module, intervention, budget, expenditure)]
    gf_data$sda_activity <- "all"
    gf_data <- gf_data[, c("module","sda_activity", "intervention", "budget", "expenditure"),with=FALSE]
  } else { 
      print(paste0("An else-statement was entered in GTM PUDR prep function. Review prep code for: ", 
            inFile))
  }
  
  #Emily verify that this isn't dropping budget information! Can we do this another way? 
  if (inFile != "PUDR_P32_HivosGT_030518.xlsx"){
    budget_dataset <- gf_data[-1, drop = FALSE]
    budget_dataset <- budget_dataset[-nrow(budget_dataset) ,drop = FALSE]
  } else {
    budget_dataset = gf_data
  }
  
  #This command makes me nervous everywhere I see it. EKL 1/9/18. Need to be verifying budget totals before dropping NAs. 
  budget_dataset <- na.omit(budget_dataset , cols=1, invert=FALSE)
  
  #Emily what is this prep code doing? 
  toMatch <- c("total", "module")
  budget_dataset <- budget_dataset[!grepl(paste0(toMatch, collapse="|"), tolower(budget_dataset$module)),]
  
  #Need to rewrite this to 1.) Make sure only invalid rows are being dropped and 2.) We have checks in place. 
  #Add numeric budget check here. 
  
  
  if (qtr_num == 1 & !('start_date'%in%colnames(budget_dataset))){
    budget_dataset$start_date <- start_date
  }
  if(!'start_date'%in%colnames(budget_dataset)){
    stop("PUDR with multiple quarters- review GTM prep code.")
  }
  budget_dataset$year <- year(budget_dataset$start_date)
  budget_dataset$period <- period
  budget_dataset$disease <- disease
  budget_dataset$grant_number <- grant
  budget_dataset$lang <- lang
  budget_dataset$cost_category <- "all"
  # 
  # # ----------------------------------------------
  # 
  # return prepped data
  return(budget_dataset)
}


  