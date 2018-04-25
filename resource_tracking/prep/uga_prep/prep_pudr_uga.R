# ----------------------------------------------
# Irena Chen
#
# 10/31/2017
# Template for prepping UGA PUDR data:  
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object

# ----------------------------------------------

# start function
prep_pudr_uga = function(dir, inFile, sheet_name, start_date, disease, period, grant, recipient, source) {
  
  # --------------------
  # Test the inputs to make sure that they are the correct type
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  
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
    budget_dataset$disbursement <- 0 
  
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
      budget_dataset$expenditure <- 0 
      
    } else {  ##change this if we get a new PUDR that doesn't fit any of these three 
    colnames(gf_data)[1] <- "description"
    colnames(gf_data)[2] <- "module"
    colnames(gf_data)[3] <- "budget"
    colnames(gf_data)[5] <- "expenditure"
    gf_data <- gf_data[c(grep("module", tolower(gf_data$description)):grep(0, tolower(gf_data$module))),]
    budget_dataset <- gf_data[, c("module", "budget", "expenditure"),with=FALSE]
    budget_dataset<- budget_dataset[!is.na(budget_dataset$module),]
    budget_dataset$recipient <- recipient
    budget_dataset$disbursement <- 0 
  }
  ## drop 1st row since it doesn't contain any useful inforamtion:  
  budget_dataset <- budget_dataset[-1, ,drop = FALSE]
  budget_dataset$start_date <- start_date
  budget_dataset$data_source <- source
  budget_dataset$period <- period
  budget_dataset$intervention <- "All" ## change if we ever get more detailed PUDR info
  budget_dataset$sda_activity <- "All"## change if we ever get more detailed PUDR info
  budget_dataset$disease <- disease
  budget_dataset$cost_category <- "all" ## change if we ever get more detailed PUDR info
  budget_dataset$grant_number <- grant
  budget_dataset$year <- year(budget_dataset$start_date)

  # return prepped data
  return(budget_dataset)
}


