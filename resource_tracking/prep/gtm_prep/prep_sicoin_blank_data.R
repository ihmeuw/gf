
# start function
prep_blank_sicoin = function(loc_id, start_date, disease, period, source) {

  budget_dataset <- setnames(data.table(matrix(nrow = 1, ncol = 9)), 
                                          c("sda_orig","loc_id","budget", "disbursement", 
                                            "source", "period",	"start_date", "disease", "expenditure"))

  
  budget_dataset$loc_id<- as.character(budget_dataset$loc_id)
  budget_dataset$loc_id <- loc_id
  ## Create other variables 
  budget_dataset$source <- source
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