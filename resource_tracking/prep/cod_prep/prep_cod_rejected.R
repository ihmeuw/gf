# ----------------------------------------------
# Set up R


prep_cod_rejected <- function(file_name){
    
  ##prep rejected budget: 
  rejected_budgets <- data.table(read.csv(file_name, na.strings=c("","NA"),
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

  return(rej_cod)
}
##RBIND WITH OTHER COD BUDGETS 
# 
# codFpm <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/cod/prepped/prepped_fpm_budgets.csv", fileEncoding = "latin1"))
# codFpm$start_date <- as.Date(codFpm$start_date,"%Y-%m-%d")
# codFpm$year <- year(codFpm$start_date)
# 
# cod_budgets <- rbind(codFpm, rej_cod)
# 
# write.csv(cod_budgets, "J:/Project/Evaluation/GF/resource_tracking/cod/prepped/all_fpm_budgets.csv", fileEncoding = "latin1", row.names = FALSE)
# 
