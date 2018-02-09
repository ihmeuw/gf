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

library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
# ----------------------------------------------

prep_cat_summary_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, loc_id, period, grant, recipient){
  
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

  ## grab the SDA data
  gf_data <- gf_data[c((grep("Module",gf_data$cost_category)):(grep("Cost Grouping", gf_data$cost_category))),]
  
  ##drop the last two rows (we don't need them)
  gf_data <- head(gf_data,-2)
  
  ## rename the columns 
  colnames(gf_data) <- as.character(gf_data[1,])
  gf_data <- gf_data[-1,]
  toMatch <- c("Ann","Year", "RCC", "%", "Phase", "Implem", "Total")
  drop.cols <- grep(paste(toMatch, collapse="|"), ignore.case=TRUE, colnames(gf_data))
  gf_data <- gf_data[, (drop.cols) := NULL]

  
  ## also drop columns containing only NA's
  gf_data<- Filter(function(x) !all(is.na(x)), gf_data)
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  ##library(reshape)
  setDT(gf_data)
  gf_data1<- melt(gf_data,id="By Module", variable.name = "qtr", value.name="budget")
  
  if(length(dates) != length(unique(gf_data1$qtr))){
    stop('Error: quarters were dropped!')
  }
  ##turn the list of dates into a dictionary (but only for quarters!) : 
  dates <- setNames(dates,unique(gf_data1$qtr))
  
  
  ## now match quarters with start dates 
  kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
  budget_dataset <-gf_data1[kDT, on=.(qtr), start_date := i.start_date]
  
  ##rename the category column 
  colnames(budget_dataset)[1] <- "sda_orig"
  budget_dataset$disease <- disease
  budget_dataset$activity_description <- "none"
  budget_dataset$loc_id <- loc_id
  budget_dataset$period <- period
  budget_dataset$grant_number <- grant
  budget_dataset$recipient <- recipient
  budget_dataset$qtr <- NULL
  
  return(budget_dataset)
  
}