#----------------------------------------------
# Irena Chen
#
# 5/16/2018
##This cleans the GUA-M-MSPAS 2014-2016 budget
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object


# ----------------------------------------------
##Function to prepare the budgets: 
# ----------------------------------------------

prep_other_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, period, lang, grant){
  
  ##load the FPM data: 
  if(!is.na(sheet_name)){
    gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))
  } else {
    gf_data <- data.table(read_excel(paste0(dir, inFile)))
  }
  gf_data <- Filter(function(x)!all(is.na(x)), gf_data)
  if(sheet_name=="Detailed Budget - Year 2"){
    gf_data1 <- gf_data[,c("X__3", "X__4", "GASTOS\r\nQuarter 1","GASTOS \r\nQuarter 2", "GASTOS\r\nQuarter 3",  "GASTOS \r\nQuarter 4"), with=FALSE]
  } else if (sheet_name=="Detailed Budget - Year 1"){
    gf_data1 <- gf_data[,c("X__3", "X__4", "Gastos Q1","Gastos Q2", "GASTOS\r\nQuarter 3",  "GASTOS \r\nQuarter 4"), with=FALSE]
  } else {
    gf_data1 <- gf_data[,c("X__3", "X__4", "X__24", "X__26", "X__28", "X__30"), with=FALSE]
  }
  if(sheet_name!="Detailed Budget - Year 3"){
    gf_data1 <- gf_data1[-c(1:3),]
  } else {
    gf_data1 <- gf_data1[-c(1:2),]
  }
    
    ##only keep data that has a value in the "category" column 
  gf_data1<- na.omit( gf_data1 , cols=1, invert=FALSE)
  setnames(gf_data1, c("module", "sda_activity", "Q1", "Q2", "Q3", "Q4"))
  budget_dataset<- melt(gf_data1,id=c("module","sda_activity"), 
                  variable.name = "qtr", value.name="budget")
  
  dates <- rep(start_date, qtr_num) # 
    for (i in 1:length(dates)){
      if (i==1){
        dates[i] <- start_date
      } else {
        dates[i] <- dates[i-1]%m+% months(3)
      }
    }
    if(length(dates) != length(unique(budget_dataset$qtr))){
      stop('quarters were dropped!')
    }
    ##turn the list of dates into a dictionary (but only for quarters!) : 
  dates <- setNames(dates,unique(budget_dataset$qtr))
    ## now match quarters with start dates 
  kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
  budget_dataset <-budget_dataset[kDT, on=.(qtr), start_date := i.start_date ]
  budget_dataset$qtr <- NULL
  budget_dataset$period <- period
  budget_dataset$grant_number <- grant
  budget_dataset$disease <- disease
  budget_dataset$expenditure <- 0 
  budget_dataset$lang <- lang
  budget_dataset$intervention <- "all"
  budget_dataset$cost_category <- "all"
  budget_dataset$recipient <- "MoH"
  
  return(budget_dataset)  
}

