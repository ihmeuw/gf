
prep_cod_cat_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, loc_id, period){
  
  if(!is.na(sheet_name)){
  ghe_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name), col_names = FALSE))
  } else {
    ghe_data <- data.table(read_excel(paste0(dir, inFile)))
  }
  ## the 1st column isn't always the same, so just call it something: 
  if(is.na(ghe_data[1,1])){
    ghe_data[1,1] <- "category"
  }

  ##only keep data that has a value in the "category" column 
  ghe_data <- na.omit(ghe_data, cols=1, invert=FALSE)

  
  toMatch <- c("Ann","Year", "RCC", "%", "Phase", "Implem", "Total")
  if(any(colnames(ghe_data)%in% "X_")){
    ##we only want the data for each quarter, so remove extraneous values: 
    ghe_data <- Filter(function(x) !any(grepl(paste(toMatch, collapse="|"), x), ignore.case=TRUE), ghe_data)
    colnames(ghe_data) <- as.character(ghe_data[1,])
    ghe_data <- ghe_data[-1,]
  }
  
  
  drop.cols <- grep(paste(toMatch, collapse="|"), ignore.case=TRUE, colnames(ghe_data))
  ghe_data1 <- ghe_data[, (drop.cols) := NULL]
  
  
  ## also drop columns containing only NA's
  ghe_data<- Filter(function(x) !all(is.na(x)), ghe_data1)
  colnames(ghe_data)[1] <- "category"
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  ##library(reshape)
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
