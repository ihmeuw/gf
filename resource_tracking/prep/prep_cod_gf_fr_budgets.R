# ----------------------------------------------
# Irena Chen
#
# 11/8/2017
# Template for prepping GF COD budget data 
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

prep_cod_fr_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, loc_id, period, lang, grant) {
  ghe_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name), col_names = FALSE))
  
  colnames(ghe_data) <- as.character(ghe_data[1,])
  ## drop the first row now that we renamed the columns 
  ghe_data <- ghe_data[-1,]
  ## we need to grab the budget numbers by quarter - first determine if french or english
  if(lang=="eng"){
    cashText <- " Cash \r\nOutflow"
  } else if (lang=="fr"){
    cashText <- "Sorties de trésorerie"
  }
  
  ## create vector of column names to grab: 
 if(lang=="eng"){
   qtr_names <- c("Module", "Activity Description", "Recipient", rep(1, qtr_num))
 } else{ 
   qtr_names <- c("Module", "Description de l'activité", "Récipiendaire" , rep(1, qtr_num))}
  
  ##create a list of the columns we want to grab: 
  create_qtr_names = function(qtr_names, cashText, lang){
    for(i in 1:qtr_num+3){
      if(i <4) {
        i=i+1
      } else { 
        if(lang=="eng"){
          qtr_names[i] <- paste("Q", i-3,  cashText, sep="")
        } else {
          qtr_names[i] <- paste(cashText, " T", i-3, sep="")
        }
        i=i+1
      }
    }
    return(qtr_names)
  }
  qtr_names <- create_qtr_names(qtr_names, cashText, lang)
  
  ghe_data <- ghe_data[,names(ghe_data)%in%qtr_names, with=FALSE]
  ghe_data <- ghe_data[rowSums(is.na(ghe_data))!=ncol(ghe_data), ]
  ghe_data <- ghe_data[!grep("[[:digit:]]", ghe_data$Module), ]
  
  ## melt the data 
  setDT(ghe_data)
  ghe_data<- melt(ghe_data,id=c(qtr_names[1:3]), variable.name = "qtr", value.name="budget")
  colnames(ghe_data)[1] <- "module"
  colnames(ghe_data)[2] <- "activity_description"
  colnames(ghe_data)[3] <- "recipient"
  
  ## create vector of start dates to map to quarters 
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
  
  ##clean up malaria data: 
  get_malaria_cats <- function(activity_des, module) {
    if(grepl("bed nets", tolower(activity_des))){
      x <- activity_des
    } else {
      x <- module
    }
    return(x)
  }
  
  if(disease=="malaria"){
    budget_dataset$cost_category <- mapply(get_malaria_cats, budget_dataset$activity_description, budget_dataset$module)
    budget_dataset$module <- NULL
  } else {
    colnames(budget_dataset)[colnames(budget_dataset)=="module"] <- "cost_category"
  }
  
  budget_dataset$activity_description <- NULL
  budget_dataset$qtr <- NULL
  budget_dataset$disease <- disease 
  budget_dataset$loc_id <- loc_id
  budget_dataset$period <- period
  budget_dataset$grant_number <- grant
  return(budget_dataset)
}

