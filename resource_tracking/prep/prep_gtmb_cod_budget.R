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

prep_gtmb_cod_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, loc_id, period) {
  
  col_names <- rep(0, qtr_num+1)
  
  for(i in 1: length(col_names)){
    if (i==1){
      col_names[i] <- "category"
    } else {
      col_names[i] <- paste("Q", i-1, sep="")
    }
  }
  
  ghe_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)), col_names = FALSE)
  
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
  #for some reason, ghe_data <- ghe_data[,-is.na(ghe_data[1,])] isn't working. Planning to go to code drop in for help
  
  colnames(ghe_data) <- as.character(ghe_data[1,])
  drop.cols <- grep("X_", colnames(ghe_data))
  ghe_data <- ghe_data[, (drop.cols) := NULL]
  
  
  ## rename the columns
  colnames(ghe_data) <- col_names
  ## drop the first row now that we renamed the columns 
  ghe_data1<- ghe_data[-1,]
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  setDT(ghe_data)
  ghe_data<- melt(ghe_data,id="category", variable.name = "qtr", value.name="budget")
  
  dates <- rep(start_date, qtr_num) # 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }
  
  
  ##turn the list of dates into a dictionary (but only for quarters!) : 
  dates <- setNames(dates,unique(ghe_data$qtr))
  
  
  ## now match quarters with start dates 
  kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
  budget_dataset <-ghe_data[kDT, on=.(qtr), start_date := i.start_date ]
  budget_dataset$disease <- disease 
  budget_dataset$loc_id <- loc_id
  budget_dataset$period <- period
  
  return( budget_dataset)

}
