
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
  
  ## this type of budget data should always have 13 cost categories 
  ghe_data <- ghe_data[c(grep("Category", ghe_data$X__1):(grep(13, ghe_data$first_column))),]
  
  # drop first row (it's blank)
  ghe_data <- ghe_data[-1,]
  
  # drop 1st column (unnecessary)
  ghe_data[[1]] <- NULL  
  
  ghe_data$X__1[1] <- "category"
  
  
  ## also drop columns containing only NA's
  ghe_data<- Filter(function(x) !all(is.na(x)), ghe_data)
  
  
  ##we only want the data for each quarter, so remove extraneous values: 
  toMatch <- c("Year", "RCC", "%", "TOTAL", "Phase", "Implem")
  ghe_data <- Filter(function(x) !any(grepl(paste(toMatch, collapse="|"), x)), ghe_data)
  
  # workaround to delete columns that have an NA in the first row - 
  #for some reason, ghe_data <- ghe_data[,-is.na(ghe_data[1,])] isn't working.
  
  colnames(ghe_data) <- as.character(ghe_data[1,])
  drop.cols <- grep("X_", colnames(ghe_data))
  ghe_data1 <- ghe_data[, (drop.cols) := NULL]
  
  
  ## rename the columns
  colnames(ghe_data) <- col_names
  ## drop the first row now that we renamed the columns 
  ghe_data <- ghe_data[-1,]
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  setDT(ghe_data)
  melted<- melt(ghe_data,id="category", variable.name = "qtr", value.name="budget")
  
  return(melted)
}
