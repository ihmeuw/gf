# ----------------------------------------------
# Irena Chen
#
# 11/7/2017
# Template for prepping GF budget data that is grouped by "module"
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------
prep_module_budget = function(dir, inFile, extension, sheet_name, start_date, qtr_num) {
  
  col_names <- rep(0, qtr_num+1)
  
  for(i in 1: length(col_names)){
    if (i==1){
      col_names[i] <- "cost_category"
    } else {
      col_names[i] <- paste("Q", i-1, sep="")
    }
  }
  
  
  ghe_data <- data.table(read_excel(paste0(dir, inFile, extension), sheet=sheet_name))
  
  ## this type of budget data should always have 13 cost categories 
  colnames(ghe_data)[3] <- "program_activity"
  ghe_data <- ghe_data[c(grep("Module", ghe_data$program_activity):(grep("Total", ghe_data$program_activity))),]
  
 ## delete 1st two columns 
  ghe_data <- ghe_data[, -c(1:2)]
  
  colnames(ghe_data) <- as.character(ghe_data[1,])

  ## drop the first row now that we renamed the columns 
  ghe_data <- ghe_data[-1,]
  ##we only want the data for each quarter, so remove extraneous values: 
  toMatch <- c("Year", "RCC", "%", "TOTAL", "Phase", "Implem", "Total", "Año")
  drop.cols <- grep(paste(toMatch, collapse="|"), ignore.case=TRUE, colnames(ghe_data))
  ghe_data <- ghe_data[, (drop.cols) := NULL]
  
  
  ghe_data <- ghe_data[-nrow(ghe_data),]

  ## also drop columns containing only NA's
  ghe_data<- Filter(function(x) !all(is.na(x)), ghe_data)
  
  ## rename the columns
  colnames(ghe_data) <- col_names

  ## invert the dataset so that budget expenses and quarters are grouped by category
  setDT(ghe_data)
  melted<- melt(ghe_data,id="cost_category", variable.name = "qtr", value.name="budget")
  return(melted)
}
