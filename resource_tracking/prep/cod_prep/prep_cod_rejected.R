
# ----------------------------------------------
# Irena Chen
#
# 11/8/2017
# This is the iterated budget data that we got from PATH
# It is already in a CSV format, but we just need to shape it into the correct format for the RT database
# Inputs:
# inFile- name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------

prep_cod_rejected <- function(inFile){
    
  ##prep rejected budget: 
  rejected_budgets <- data.table(read.csv(inFile, na.strings=c("","NA"),
                        stringsAsFactors = FALSE, fileEncoding = "latin1"))
  
  setnames(rejected_budgets, c("y1", "y2", "y3"), c("2018", "2019", "2020"))
  setnames(rejected_budgets, "cost_category", "module")
  setnames(rejected_budgets, "grant", "grant_number")
  ## invert the dataset so that budget expenses and quarters are grouped by category
  setDT(rejected_budgets)
  rej_cod <- melt(rejected_budgets,id=c("module", "grant_number", "disease"), variable.name = "year", value.name="budget")
  
  rej_cod$budget <- as.numeric(rej_cod$budget)
  rej_cod$year <-  as.numeric(as.character(rej_cod$year))
  
  rej_cod$intervention <- "All"
  rej_cod$sda_activity <- "All"
  rej_cod$data_source <- 'init_fpm_sep'
  rej_cod$period <- 365
  rej_cod$start_date <- paste0(rej_cod$year, "-01-01")
  rej_cod$start_date  <- as.Date(rej_cod$start_date,"%Y-%m-%d")
  rej_cod$loc_name <- "cod"
  rej_cod$recipient <- rej_cod$grant_number
  rej_cod$lang <- "fr"
  rej_cod$cost_category <- "all"

  return(rej_cod)
}
