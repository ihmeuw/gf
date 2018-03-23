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

# start function
prep_gtm_pudr = function(dir, inFile, sheet_name, year, qtr_num, disease, period, grant, source) {
  
  # Load/prep data
  gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  
  if(grant%in%"GTM-T-MSPAS"){
    colnames(gf_data)[1] <- "module"
    colnames(gf_data)[2] <- "intervention"
    colnames(gf_data)[3] <- "budget"
    colnames(gf_data)[4] <- "expenditure"
    gf_data$sda_activity <- "All"
    gf_data <- gf_data[c(grep("modul", tolower(gf_data$module)):(grep("implem", tolower(gf_data$module)))),]
    gf_data <- gf_data[-1, ,drop = FALSE]
  } else {
    colnames(gf_data)[2] <- "module"
    colnames(gf_data)[3] <- "sda_activity"
    colnames(gf_data)[4] <- "intervention"
    colnames(gf_data)[5] <- "budget"
    colnames(gf_data)[7] <- "expenditure"
    gf_data <- gf_data[c(grep("objetivos", tolower(gf_data$sda_activity)):(grep("seleccio", tolower(gf_data$module)))),]
  }
  
  gf_data <- gf_data[, c("module","sda_activity", "intervention", "budget", "expenditure"),with=FALSE]
  budget_dataset <- gf_data[-1, drop = FALSE]
  budget_dataset <- budget_dataset[-nrow(budget_dataset) ,drop = FALSE]
  
  budget_dataset <- na.omit(budget_dataset , cols=1, invert=FALSE)
  budget_dataset <- budget_dataset[!grepl("total", tolower(budget_dataset$module)),]

  
  budget_dataset$start_date <- year
  budget_dataset$period <- period
  budget_dataset$disease <- disease
  budget_dataset$grant_number <- grant
  # 
  # # ----------------------------------------------
  # 
  # return prepped data
  return(budget_dataset)
}


  