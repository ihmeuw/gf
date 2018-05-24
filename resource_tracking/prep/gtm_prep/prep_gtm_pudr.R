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
prep_gtm_pudr = function(dir, inFile, sheet_name, year, qtr_num, disease, period, grant, source, loc_name, lang) {
  
  # Load/prep data
  gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  
  if(grant%in%"GTM-T-MSPAS"&sheet_name!="INTEGRACION"){
    colnames(gf_data)[1] <- "module"
    colnames(gf_data)[2] <- "intervention"
    colnames(gf_data)[3] <- "budget"
    colnames(gf_data)[4] <- "expenditure"
    gf_data$sda_activity <- "all"
    gf_data$disbursement <- 0 
    gf_data$recipient <- loc_name ##change this when we get SR info
    
    gf_data <- gf_data[c(grep("modul", tolower(gf_data$module)):(grep("implem", tolower(gf_data$module)))),]
    gf_data <- gf_data[-1, ,drop = FALSE]
  } else if (sheet_name=="LFA_Annex-SR Financials"){
      colnames(gf_data)[1] <- "recipient"
      colnames(gf_data)[5] <- "budget"
      colnames(gf_data)[6] <- "disbursement"
      gf_data <- gf_data[c(grep("entity", tolower(gf_data$recipient)):(grep("total", tolower(gf_data$recipient)))),]
      gf_data$sda_activity <- "all"
      gf_data$module <- "all"
      gf_data$intervention <- "all"
      gf_data$expenditure <- 0
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
      gf_data$disbursement <- 0 
      gf_data$sda_activity <- "all"
    
  } else if (sheet_name=="PR EFR_7A"){
    colnames(gf_data)[2] <- "module"
    colnames(gf_data)[3] <- "sda_activity"
    colnames(gf_data)[4] <- "intervention"
    colnames(gf_data)[5] <- "budget"
    colnames(gf_data)[6] <- "expenditure"
    gf_data$disbursement <- 0 
    gf_data$recipient <- loc_name
    gf_data <- gf_data[c(grep("object", tolower(gf_data$sda_activity)):(grep("name", tolower(gf_data$sda_activity)))),]
  } else {
      colnames(gf_data)[2] <- "module"
      colnames(gf_data)[3] <- "sda_activity"
      colnames(gf_data)[4] <- "intervention"
      colnames(gf_data)[5] <- "budget"
      colnames(gf_data)[7] <- "expenditure"
      gf_data$disbursement <- 0 
      gf_data$recipient <- loc_name
      gf_data <- gf_data[c(grep("objetivos", tolower(gf_data$sda_activity)):(grep("seleccio", tolower(gf_data$module)))),]
  }
  
  gf_data <- gf_data[, c("module","sda_activity", "intervention", "budget", "expenditure", "disbursement","recipient"),with=FALSE]
  budget_dataset <- gf_data[-1, drop = FALSE]
  budget_dataset <- budget_dataset[-nrow(budget_dataset) ,drop = FALSE]
  
  budget_dataset <- na.omit(budget_dataset , cols=1, invert=FALSE)
  budget_dataset <- budget_dataset[!grepl("total", tolower(budget_dataset$module)),]

  budget_dataset$start_date <- year
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


  