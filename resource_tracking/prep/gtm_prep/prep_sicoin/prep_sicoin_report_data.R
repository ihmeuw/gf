
# ----------------------------------------------
# Irena Chen
#
# 2/9/2018
# We have one SICOIN report that needs to get prepped, so this is the function that handles the one-off report 
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object

# --------------------------------------------------------------
# Set up R


# start function
prep_report_sicoin = function(inFile, start_date, disease, period, source) {
  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <- data.table(read_excel(inFile))
  ## remove empty columns 
  gf_data$X__6[gf_data$X__6=="TOTAL"] <- NA
  gf_data$X__5[gf_data$X__5=="TOTAL"] <- NA
  gf_data$X__5 <- na.locf(gf_data$X__5, na.rm=FALSE)
  gf_data$X__6 <- na.locf(gf_data$X__6, na.rm=FALSE)
  gf_data$X__15 <- na.locf(gf_data$X__15, na.rm=FALSE)
  gf_data$X__16 <- na.locf(gf_data$X__16, na.rm=FALSE)
  gf_data<- na.omit(gf_data, cols="X__18")
  budget_dataset <- gf_data[, c("X__5","X__6","X__15","X__16","X__18", "X__37", "X__41"), with=FALSE]
  names(budget_dataset) <- c("adm1","adm2", "loc_name", "sda_orig","cost_category", "budget", "disbursement")
    
  ##enforce variable classes 
  if (!is.numeric(budget_dataset$budget)) budget_dataset[,budget:=as.numeric(budget)]
  if (!is.numeric(budget_dataset$disbursement)) budget_dataset[,disbursement:=as.numeric(disbursement)]
    # government resources are split by income (taxes) and "treasury" 
    #we don't care, so sum by just the municipality and SDA: 
    
  budget_dataset <- budget_dataset[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement))),
                                     by=c("adm1","adm2", "loc_name", "sda_orig")]
  
  # ----------------------------------------------
  
  ## Create other variables 
  budget_dataset$source <- source
  budget_dataset$start_date <- start_date
  budget_dataset$period <- period
  budget_dataset$expenditure <- 0 ## change this once we figure out where exp data is
  budget_dataset$disease <- disease
  # ----------------------------------------------
  
  # return prepped data
  return(budget_dataset)
  
}

