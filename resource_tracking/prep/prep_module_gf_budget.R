
prep_module_budget = function(dir, inFile, extension, sheet_name, start_date, qtr_num) {
  
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
  colnames(ghe_data)[3] <- "cost_column"
  
  ## this type of budget data should always have 13 cost categories 
  ghe_data <- ghe_data[c(grep("By Cost", ghe_data$cost_column):(grep(13, ghe_data$X__2))),]
  
 ## delete 1st two columns 
  ghe_data <- ghe_data[, -c(1:2)]
  
  
  ##we only want the data for each quarter, so remove extraneous values: 
  toMatch <- c("Year", "RCC", "%", "TOTAL", "Phase", "Implem", "Total")
  ghe_data <- Filter(function(x) !any(grepl(paste(toMatch, collapse="|"), x)), ghe_data)
  
  
  colnames(ghe_data) <- as.character(ghe_data[1,])
  ## drop the first row now that we renamed the columns 
  ghe_data <- ghe_data[-1,]
  
  ## also drop columns containing only NA's
  ghe_data<- Filter(function(x) !all(is.na(x)), ghe_data)
  
  ## rename the columns
  colnames(ghe_data) <- col_names

  ## invert the dataset so that budget expenses and quarters are grouped by category
  setDT(ghe_data)
  melted<- melt(ghe_data,id="category", variable.name = "qtr", value.name="budget")
  
  
  return(melted)
}
