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
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
# ----------------------------------------------
##Function to prepare the detailed budgets: 

# ----------------------------------------------
prep_fpm_detailed_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, period, lang, grant){
  
  ##first determine if budget is in french or english
  if(lang=="eng"){
    cashText <- " Cash \r\nOutflow"
    loc_name <- "Geography/Location"
  } else{
    cashText <- "Salida de efectivo"
    loc_name <-  "Localización"
  }
  
  ## newer budgets use the label "Implementador" and the old ones use "Receptor" 
  if(year(start_date)==2018){
    recipient <- "Implementador"
  } else{
    recipient <- "Receptor"
  }
  
  ##names of the columns we want to ultimately go into our database: 
  if(lang=="eng"){
    qtr_names <- c("Module","Intervention", "Recipient", loc_name, rep(1, qtr_num))
  } else{ 
    qtr_names <- c("Módulo", "Intervención","Descripción de la actividad",	recipient, loc_name, rep(1, qtr_num))
  }
  
  ##add in the quarter names to the list: 
  create_qtr_names = function(qtr_names, cashText, lang){
    for(i in 1:qtr_num+5){
      if(i <6) {
        i=i+1
      } else { 
        qtr_names[i] <- paste("Q", i-5, " ",  cashText, sep="")
        i=i+1
      }
    }
    return(qtr_names)
  }
  qtr_names <- create_qtr_names(qtr_names, cashText, lang)
  
  ##load the FPM data: 
  if(!is.na(sheet_name)){
    gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))
  } else {
    gf_data <- data.table(read_excel(paste0(dir, inFile)))
  }
  
  ## drop the first  two columns (they are unnecessary)
  gf_data <- gf_data[,-c(1:2)]
  
  ##GUA-M-MSPAS formatted differently: 
  if(grant=="GUA-M-MSPAS"){
    gf_data <- gf_data[-1,]
    colnames(gf_data) <- as.character(gf_data[1,])
    gf_data <- gf_data[-c(1:2),]
    budget_dataset <- gf_data[, c("Activity" ,"Total Presupuesto Year 1"), with=FALSE]
    names(budget_dataset) <- c("sda_activity","budget")
    budget_dataset <- na.omit(budget_dataset, cols=c("sda_activity"))
    budget_dataset <- unique(budget_dataset, by=c("sda_activity","budget"))
    
  }else{
    ##new budgets formatted slightly differently: 
    if(year(start_date)==2018){
      gf_data <- gf_data[-c(1:2),]
      colnames(gf_data) <- as.character(gf_data[1,])
      gf_data <- gf_data[-1,]
    }
    
    ##only get the columns that we want
    gf_data <- gf_data[,names(gf_data)%in%qtr_names, with=FALSE]
    
    ##only keep data that has a value in the "category" column 
    gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
    
    colnames(gf_data)[1] <- "module"
    colnames(gf_data)[2] <- "intervention"
    colnames(gf_data)[3] <- "sda_activity"
    
    
    if(!(recipient %in% colnames(gf_data))){
     gf_data$recipient <- grant_number
    } else{
      colnames(gf_data)[4] <- "recipient" 
      }
    if(!(loc_name %in% colnames(gf_data))){
      gf_data$loc_name <- "gtm"
    } else{
      colnames(gf_data)[5] <- "loc_name" 
    }
    
    
    ## invert the dataset so that budget expenses and quarters are grouped by category
    ##library(reshape)
    setDT(gf_data)
    gf_data1<- melt(gf_data,id=c("module", "intervention","sda_activity", "recipient", "loc_name"), 
                      variable.name = "qtr", value.name="budget")
  
    
    dates <- rep(start_date, qtr_num) # 
    for (i in 1:length(dates)){
      if (i==1){
        dates[i] <- start_date
      } else {
        dates[i] <- dates[i-1]%m+% months(3)
      }
    }
    
    if(length(dates) != length(unique(gf_data1$qtr))){
      stop('quarters were dropped!')
    }
    ##turn the list of dates into a dictionary (but only for quarters!) : 
    dates <- setNames(dates,unique(gf_data1$qtr))
    
    
    ## now match quarters with start dates 
    kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
    budget_dataset <-gf_data1[kDT, on=.(qtr), start_date := i.start_date ]
    budget_dataset$qtr <- NULL
  }
  
  budget_dataset$period <- period
  budget_dataset$grant_number <- grant
  budget_dataset$disease <- disease
  budget_dataset$expenditure <- 0 
  budget_dataset$lang <- lang
  
  return(budget_dataset)  
}
