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
  
  # dir = file_dir
  # inFile = file_list$file_name[i]
  # sheet_name = file_list$sheet[i]
  # start_date = file_list$start_date[i]
  # qtr_num = file_list$qtr_number[i]
  # period = file_list$period[i]
  # disease = file_list$disease[i]
  # lang = file_list$language[i]
  # grant = file_list$grant[i]

  
  ##load the FPM data: 
  if(!is.na(sheet_name)){
    gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))
  } else {
    gf_data <- data.table(read_excel(paste0(dir, inFile)))
  }

  
  gf_data <- Filter(function(x)!all(is.na(x)), gf_data)
  setDT(gf_data)
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
    
  setnames(gf_data1, c("module", "sda_activity", "Q1", "Q2", "Q3", "Q4"))
  #Where are other NAs coming from? We should flag these every time 
  
  #Only keep data that has a value in the 'module' column. 
  gf_data1<- gf_data1[!is.na(module)]
  
  #Replace budgets that are NA at this point with 0. EKL Can you do this with a loop? or lapply? 
  gf_data1[is.na(gf_data1$Q1), Q1:=0]
  gf_data1[is.na(gf_data1$Q2), Q2:=0]
  gf_data1[is.na(gf_data1$Q3), Q3:=0]
  gf_data1[is.na(gf_data1$Q4), Q4:=0]
  
  budget_dataset<- melt(gf_data1,id=c("module","sda_activity"), 
                        variable.name = "qtr", value.name="budget")
  
  budget_dataset[, budget:=as.numeric(budget)]
  
  #Make sure the conversions above are working. 
  if(budget_dataset[, sum(budget)]==0){
    stop(paste0("All budget data converted to NA for file: ", inFile, ". Validate prep_fpm_other_budget function."))
  }
  
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
  budget_dataset$disease <- disease
  budget_dataset$lang <- lang
  budget_dataset$intervention <- "all"
  budget_dataset$cost_category <- "all"
  budget_dataset$recipient <- "MoH"
  budget_dataset$grant_number = grant
  
  return(budget_dataset)  
}

