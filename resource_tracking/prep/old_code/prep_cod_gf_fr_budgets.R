
prep_cod_fr_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, loc_id, period) {
  
  ghe_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name), col_names = FALSE))
  
  ## we need t ograb the budget numbers by quarter - first determine if french or english
  if(lang=="eng"){
    cashText <- " Cash \r\nOutflow"
  } else if (lang=="fr"){
    cashText <- "french"
  }
  
  ## create vector of column names to grab: 
  qtr_names <- rep("Intervention", qtr_num+1)
  create_qtr_names = function(qtr_names, cashText){
    for(i in 1:qtr_num+1){
      if(i==1){
        qtr_names[i] <- qtr_names[i]
      } else{
        qtr_names[i] <- paste("Q", i-1,  cashText, sep="")
      }
     i=i+1
    }
    return(qtr_names)
  }
  qtr_names <- create_qtr_names(qtr_names, cashText)
  ghe_data <- gf_data[,names(gf_data)%in%qtr_names, with=FALSE]
  
  ## this type of budget data should always have 13 cost categories 
  ghe_data <- ghe_data[c((grep("CostGrp", ghe_data$X__2)):(grep(13, ghe_data$X__2))),]
  
  ## delete 1st two columns 
  ghe_data <- ghe_data[, -c(1:2)]
  
  
  ##we only want the data for each quarter, so remove extraneous values: 
  toMatch <- c("Ann","Year", "RCC", "%", "TOTAL", "Phase", "Implem", "Total")
  ghe_data <- Filter(function(x) !any(grepl(paste(toMatch, collapse="|"), x)), ghe_data)
  
  
  colnames(ghe_data) <- as.character(ghe_data[1,])
  ## drop the first row now that we renamed the columns 
  ghe_data <- ghe_data[-1,]
  ## also drop columns containing only NA's
  ghe_data<- Filter(function(x) !all(is.na(x)), ghe_data)
  
  ## rename the columns
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

