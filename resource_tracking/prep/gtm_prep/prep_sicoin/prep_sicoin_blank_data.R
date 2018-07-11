
# ----------------------------------------------
# Irena Chen
#
# 2/9/2018
# Template for prepping SICOIN blank data 
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object


# ----------------------------------------------
# start function
# ----------------------------------------------
prep_blank_sicoin = function(loc_name, adm1, start_date, disease, period, source) {

  budget_dataset <- setnames(data.table(matrix(nrow = 1, ncol = 11)), 
                                          c("sda_orig","adm1", "adm2","loc_name","budget", "disbursement", 
                                            "financing_source", "period",	"start_date", "disease", "expenditure"))

  
  budget_dataset$loc_name<- as.character(budget_dataset$loc_name)
  budget_dataset$loc_name <- loc_name
  budget_dataset$adm1 <- adm1
  budget_dataset$adm2 <- adm1
  ## Create other variables 
  budget_dataset$financing_source <- source
  budget_dataset$start_date <- start_date
  budget_dataset$period <- period 
  budget_dataset$disease <- disease
  # ----------------------------------------------
  
  # Enforce variable classes
  if (!is.numeric(budget_dataset$budget)) budget_dataset[,budget:=as.numeric(budget)]
  if (!is.numeric(budget_dataset$disbursement)) budget_dataset[,disbursement:=as.numeric(disbursement)]
  if (!is.numeric(budget_dataset$expenditure)) budget_dataset[,expenditure:=as.numeric(expenditure)]
  
  # ----------------------------------------------
  
  # return prepped data
  return(budget_dataset)
  
}