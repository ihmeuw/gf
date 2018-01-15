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

prep_gtm_detailed_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, period, lang, grant){
  ## we need to grab the budget numbers by quarter - first determine if french or english
  if(lang=="eng"){
    cashText <- " Cash \r\nOutflow"
  } else{
    cashText <- "Salida de efectivo"
  }
  
  #3
  if(lang=="eng"){
    qtr_names <- c("Intervention", "Recipient", "Geography/Location", rep(1, qtr_num))
  } else{ 
    qtr_names <- c("Módulo", "Receptor",	"Localización", rep(1, qtr_num))
  }
  
  create_qtr_names = function(qtr_names, cashText, lang){
    for(i in 1:qtr_num+3){
      if(i <4) {
        i=i+1
      } else { 
        qtr_names[i] <- paste("Q", i-3, " ",  cashText, sep="")
        i=i+1
      }
    }
    return(qtr_names)
  }
  qtr_names <- create_qtr_names(qtr_names, cashText, lang)
  
  if(!is.na(sheet_name)){
    gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))
  } else {
    gf_data <- data.table(read_excel(paste0(dir, inFile)))
  }
  ## the 1st column isn't always the same, so just call it something: 
  
  ## drop the first  two columns (they are unnecessary)
  gf_data <- gf_data[,-c(1:2)]
  
  ##only keep data that has a value in the "category" column 
  gf_data <- gf_data[,names(gf_data)%in%qtr_names, with=FALSE]
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  gf_data  <- gf_data[-1,]
  
  colnames(gf_data)[1] <- "cost_category"
  colnames(gf_data)[2] <- "recipient"
  
  ## also drop columns containing only NA's
  gf_data<- Filter(function(x) !all(is.na(x)), gf_data)
  
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  ##library(reshape)
  setDT(gf_data)
  gf_data1<- melt(gf_data,id=c("cost_category", "recipient"), variable.name = "qtr", value.name="budget")
  
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
  budget_dataset$period <- period
  budget_dataset$grant_number <- grant
  budget_dataset$disease <- disease
  
  
  return(budget_dataset)  
}
