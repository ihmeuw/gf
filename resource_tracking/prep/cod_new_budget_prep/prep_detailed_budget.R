
# ----------------------------------------------
# Irena Chen
#
# 12/18/2017
# Template for prepping GF COD new budget data  
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

prep_cod_detailed_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, period, lang, grant, loc_id){
  ## we need to grab the budget numbers by quarter - first determine if french or english
  if(lang=="eng"){
    cashText <- " Cash \r\nOutflow"
  } else{
    cashText <- "Sorties de trésorerie"
  }
  
  if(year(start_date)==2018){
    recipient <- "Implementer"
  } else {
    recipient <- "Récipiendaire"
  }
  
  if(lang=="eng"){
    qtr_names <- c("Module","Intervention","Activity Description", "Recipient", "Geography/Location", rep(1, qtr_num))
  } else { 
    qtr_names <- c("Module","Intervention","Description de l'activité", recipient, "Geography/Location", rep(1, qtr_num))
  }
  
  
  create_qtr_names = function(qtr_names, cashText, lang){
    for(i in 1:qtr_num+5){
      if(i <6) {
        i=i+1
      } else { 
        if(lang=="eng"){
          qtr_names[i] <- paste("Q", i-5,  cashText, sep="")
        } else if(lang=="fr" & qtr_num < 12){
          qtr_names[i] <- paste(cashText, " T", i-(12-qtr_num), sep="")
        } else{
          qtr_names[i] <- paste(cashText, " T", i-5, sep="")
        }
        i=i+1
      }
    }
    return(qtr_names)
  }
  qtr_names <- create_qtr_names(qtr_names, cashText, lang)
  
  ##read the data: 
  gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))
    
  ## drop the first two rows and two columns (they are unnecessary)
  if(year(start_date)==2018){
    gf_data <- gf_data[-c(1:2),]
    colnames(gf_data) <- as.character(gf_data[1,])
    gf_data  <- gf_data[-1,]
  }
  gf_data <- gf_data[,-c(1:2)]
  

  ##only keep data that has a value in the "category" column 
  gf_data <- gf_data[,names(gf_data)%in%qtr_names, with=FALSE]

  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  colnames(gf_data)[1] <- "module"
  colnames(gf_data)[2] <- "intervention"
  colnames(gf_data)[3] <- "sda_activity"
  colnames(gf_data)[4] <- "recipient"
  if(year(start_date)==2018){
    colnames(gf_data)[5] <- "loc_id"
  }

  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  ##library(reshape)
  setDT(gf_data)
  if(year(start_date)==2018){
    gf_data1<- melt(gf_data,id=c("module","intervention","sda_activity", "recipient", "loc_id"), variable.name = "qtr", value.name="budget")
    gf_data1$loc_id <- as.character(gf_data$loc_id)
  } else {
    gf_data1<- melt(gf_data,id=c("module","intervention","sda_activity", "recipient"), variable.name = "qtr", value.name="budget")
    gf_data1$loc_id <- "cod"
  }
  
  
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
  budget_dataset <- na.omit(budget_dataset, cols=1, invert=FALSE)
  budget_dataset$qtr <- NULL
  budget_dataset$period <- period
  budget_dataset$grant_number <- grant
  
  ##separate tb/hiv into either one or the other in order to map programs properly - later we might want to go back and fix this
  sep_hiv_tb <- function(module, loc_id){
    x = "unknown"
    if(loc_id%in%c("VIH", "TB")){
      if(loc_id=="TB"){
        x <- "tb"
      } else{
        x <- "hiv"
      }
    } else{
      if(grepl("tuber", tolower(module))){
        x <- "tb"
      } else {
        x <- "hiv"
      }
    }
    return(x)
  }
  
  ##clean the hiv/tb grants: 
  if(disease!="malaria"){
  budget_dataset$disease <- mapply(sep_hiv_tb, budget_dataset$module, budget_dataset$loc_id)
  } else {
    budget_dataset$disease <- disease
  }
  return(budget_dataset)  
  
  
}


