# ----------------------------------------------
# Irena Chen
#
# 11/7/2017
# Template for prepping GF budget data that is grouped by "category"
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------

prep_gtm_budget = function(dir, inFile, extension, sheet_name, start_date, qtr_num) {
  col_names <- rep(0, qtr_num+1)
  
  for(i in 1: length(col_names)){
    if (i==1){
      col_names[i] <- "cost_category"
    } else {
      col_names[i] <- paste("Q", i-1, sep="")
    }
  }
  
  

  ghe_data <- data.table(read_excel(paste0(dir, inFile, extension), sheet=sheet_name))
  
  ## the 1st column isn't always the same, so just call it something: 
  colnames(ghe_data)[4] <- "program_activity"
  colnames(ghe_data)[1] <- "first_column"
  ## this type of budget data should always have 13 cost categories 
  ghe_data <- ghe_data[c(grep("Service Deli", ghe_data$program_activity):(grep("Implemen", ghe_data$program_activity)-1)),]
  
  
  # drop 1st three columns (unnecessary)
  ghe_data[,1:3] <- NULL
  
  # drop first row (it's blank)
  ghe_data <- ghe_data[-1,]
  
  
  ghe_data$program_activity[1] <- "cost_category"
  
  
  ## also drop columns containing only NA's
  ghe_data<- Filter(function(x) !all(is.na(x)), ghe_data)
  
  
  ##we only want the data for each quarter, so remove extraneous values: 
  toMatch <- c("Year", "RCC", "%", "TOTAL", "Phase", "Implem")
  ghe_data <- Filter(function(x) !any(grepl(paste(toMatch, collapse="|"), x)), ghe_data)
  
  # workaround to delete columns that have an NA in the first row - 
  #for some reason, ghe_data <- ghe_data[,-is.na(ghe_data[1,])] isn't working. Planning to go to code drop in for help
  
  colnames(ghe_data) <- as.character(ghe_data[1,])
  drop.cols <- grep("X_", colnames(ghe_data))
  ghe_data1 <- ghe_data[, (drop.cols) := NULL]
  
  
  ## drop the first row now that we renamed the columns 
  ghe_data <- ghe_data1[-1,]
  ghe_data <- ghe_data[!is.na(ghe_data$cost_category),]
  
  colnames(ghe_data) <- col_names
  ## invert the dataset so that budget expenses and quarters are grouped by category
  setDT(ghe_data)
  melted<- melt(ghe_data,id="cost_category", variable.name = "qtr", value.name="budget")
  
  return(melted)
}
