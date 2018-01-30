# ----------------------------------------------
# Irena Chen
#
# 11/8/2017
# Template for prepping GF COD Category budget data
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------
##run these packages first: 

library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
# ----------------------------------------------

prep_cod_cat_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, loc_id, period, grant, recipient){

  dates <- rep(start_date, qtr_num) # 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }
  
  if(!is.na(sheet_name)){
  gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name), col_names = FALSE))
  } else {
    gf_data <- data.table(read_excel(paste0(dir, inFile)))
  }
  
  colnames(gf_data)[1] <- "cost_category"
  ##only keep data that has a value in the "category" column 
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)

  ## this type of budget data should always have 13 cost categories
  if(!grant%in%c("ZAR-506-G04-T", "COD-H-MOH-806", "ZAR-708-G06-H")){
    gf_data <- gf_data[c((grep("By Module", gf_data$cost_category)):(grep("Total", gf_data$cost_category))),]
    gf_data <- gf_data[-nrow(gf_data),]
  }
  
  toMatch <- c("Ann","Year", "RCC", "%", "Phase", "Implem", "Total")
  drop.cols <- grep(paste(toMatch, collapse="|"), ignore.case=TRUE, gf_data)
  gf_data <- gf_data[, (drop.cols) := NULL]
  
  ## the 1st column isn't always the same, so just call it something: 
  if(!grant%in%c("ZAR-506-G04-T", "COD-H-MOH-806", "ZAR-708-G06-H")){
    colnames(gf_data) <- as.character(gf_data[1,])
    ## drop the first row now that we renamed the columns 
    gf_data <- gf_data[-1,]
    colnames(gf_data)[1] <- "cost_category"
  }
  ## also drop columns containing only NA's
  gf_data<- Filter(function(x) !all(is.na(x)), gf_data)
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  ##library(reshape)
  setDT(gf_data)
  gf_data1<- melt(gf_data,id="cost_category", variable.name = "qtr", value.name="budget")
  
  if(length(dates) != length(unique(gf_data1$qtr))){
    stop('quarters were dropped!')
  }
  ##turn the list of dates into a dictionary (but only for quarters!) : 
  dates <- setNames(dates,unique(gf_data1$qtr))
  

  ## now match quarters with start dates 
  kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
  budget_dataset <-gf_data1[kDT, on=.(qtr), start_date := i.start_date ]
  budget_dataset$disease <- disease 
  budget_dataset$loc_id <- loc_id
  budget_dataset$period <- period
  budget_dataset$grant_number <- grant
  budget_dataset$qtr <- NULL
  budget_dataset$recipient <- recipient
  
  return(budget_dataset)

}
