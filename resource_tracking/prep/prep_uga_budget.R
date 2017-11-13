# ----------------------------------------------
# Irena Chen
#
# 11/7/2017
# Template for prepping UGA GF budget data that is grouped by "cost category"
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------

prep_gtm_budget = function(dir, inFile, extension, sheet_name, start_date, qtr_num) {

  col_names <- rep(0, qtr_num+1)
  
  for(i in 1: length(col_names)){
    if (i==1){
        col_names[i] <- "category"
      } else {
        col_names[i] <- paste("Q", i-1, sep="")
      }
    }
    
  ghe_data <- data.table(read_excel(paste0(dir, inFile, extension), sheet=sheet_name))
  
  ## the 1st column isn't always the same, so just call it something: 
  colnames(ghe_data)[1] <- "first_column"
  colnames(ghe_data)[4] <- "fourth_column"
  
  ## this type of budget data should always have 13 cost categories 
  ghe_data <- ghe_data[c(grep("Category", ghe_data$X__1):(grep("TOTAL", ghe_data$fourth_column))),]
  
  # drop first row 
  ghe_data <- ghe_data[-1,]
  ghe_data <- ghe_data[,-1]
  
  ghe_data$X__1[1]<- "category"
  
  ##only keep data that has a value in the "category" column 
  ghe_data <- na.omit(ghe_data, cols=1, invert=FALSE)
  

  
  ## also drop columns containing only NA's
  ghe_data<- Filter(function(x) !all(is.na(x)), ghe_data)
  
  
  ##we only want the data for each quarter, so remove extraneous values: 
  toMatch <- c("Ann","Year", "RCC", "%", "TOTAL", "Phase", "Implem", "Total")
  ghe_data <- Filter(function(x) !any(grepl(paste(toMatch, collapse="|"), x)), ghe_data)
  
  colnames(ghe_data) <- as.character(ghe_data[1,])
  ## drop the first row now that we renamed the columns 
  ghe_data <- ghe_data[-1,]
  
  toMatch <- c("cat", "Q")
  ghe_data <- Filter(function(x) any(grepl(paste(toMatch, collapse="|"), x)), ghe_data)
  
  # workaround to delete columns that have an NA in the first row - 
  #for some reason, ghe_data <- ghe_data[,-is.na(ghe_data[1,])] isn't working. Planning to go to code drop in for help
  
  colnames(ghe_data) <- as.character(ghe_data[1,])
  
  toMatch <- c("X_", 0)
  drop.cols <- grep(paste(toMatch, collapse="|"), colnames(ghe_data))
  ghe_data1 <- ghe_data[, (drop.cols) := NULL]
  
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  setDT(ghe_data1)
  melted<- melt(ghe_data1,id="category", variable.name = "qtr", value.name="budget")
  
  return(melted)
}
