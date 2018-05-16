# ----------------------------------------------
# Irena Chen
#
# 5/1/52018
# Template for prepping DRC PUDR data:  
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object

# ----------------------------------------------

# start function
prep_pudr_cod = function(dir, inFile, sheet_name, start_date, disease, 
                         period, grant, recipient, source,lang, loc_name) {
  
  
  ##read the data: 
  gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))
  
 ## for the PUDRs w/out SR info
  colnames(gf_data)[1] <- "module"
  colnames(gf_data)[2] <- "intervention"
  colnames(gf_data)[3] <- "budget"
  colnames(gf_data)[5] <- "expenditure"
  gf_data <- gf_data[c(grep("modular approach", tolower(gf_data$module)):.N),]
  budget_dataset <- gf_data[, c("intervention", "budget", "expenditure"),with=FALSE]
  budget_dataset<-  budget_dataset[!is.na(budget_dataset$intervention),]
  budget_dataset<- budget_dataset[!grepl("0",budget_dataset$intervention),]
  ##remove the first and last row of data (it's just the column labels, not actual data)
  budget_dataset <- budget_dataset[-nrow(budget_dataset),]
  budget_dataset <- budget_dataset[-1,]
  
  ##add all of the other RT variables 
  budget_dataset$recipient <- recipient
  budget_dataset$module <- budget_dataset$intervention
  budget_dataset$start_date <- start_date
  budget_dataset$data_source <- source
  budget_dataset$period <- period
  budget_dataset$sda_activity <- "all"## change if we ever get more detailed PUDR info
  budget_dataset$disease <- disease
  budget_dataset$cost_category <- "all" ## change if we ever get more detailed PUDR info
  budget_dataset$grant_number <- grant
  budget_dataset$year <- year(budget_dataset$start_date)
  budget_dataset$loc_name <- loc_name
  budget_dataset$lang <- lang
  # return prepped data
  return(budget_dataset)
}


