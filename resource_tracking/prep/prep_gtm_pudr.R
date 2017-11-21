# ----------------------------------------------
# Irena Chen
#
# 10/31/2017
# Template for prepping C-COIN budget data 
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------

# start function
prep_pudr = function(dir, inFile, sheet_name, format, year, disease, qtr_num, period) {
  
  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <- data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  
  ##clean the data depending on if in spanish or english
  if(format=="pudr_mspas"){
    colnames(gf_data)[4] <- "cost_category"
    colnames(gf_data)[5] <- "budget"
    colnames(gf_data)[7] <- "expenditure"
    colnames(gf_data)[1] <- "programs"
    gf_data <- gf_data[c(grep("de prestación de servicios", tolower(gf_data$cost_category)):grep("total", tolower(gf_data$programs))),]

    ## drop 1st row: 
    gf_subset <- gf_data[-1, ,drop = FALSE]
    budget_dataset <- gf_subset[, c("cost_category", "budget", "expenditure"),with=FALSE]
    toMatch <- c("mundial", "multire", "seleccion")
    budget_dataset <- budget_dataset[ !grepl(paste(toMatch, collapse="|"), tolower(budget_dataset$cost_category)),]
    budget_dataset<- budget_dataset[!is.na(budget_dataset$cost_category),]
    budget_dataset$start_date <- start_date
    budget_dataset$disbursement <- 0
    } else if (format=="pudr_categories") {
    
    colnames(gf_data)[1] <- "groups"
    colnames(gf_data)[2] <- "cost_category"
    colnames(gf_data)[3] <- "budget"
    colnames(gf_data)[4] <- "expenditure"
    
    gf_data <- gf_data[c(grep("modular approach", tolower(gf_data$groups)):(grep("implementing entity", tolower(gf_data$groups)))),]
    gf_data$groups <- NULL 
    budget_dataset <- gf_data[, c("cost_category", "budget", "expenditure"),with=FALSE]
    budget_dataset<- budget_dataset[!is.na(budget_dataset$cost_category),]
    budget_dataset <- budget_dataset[-c(1:2),]
    budget_dataset$start_date <- start_date
    budget_dataset$disbursement <- 0
    
    } else if (format=="pudr_bud_exp"){
      colnames(gf_data)[1] <- "groups"
      colnames(gf_data)[4] <- "cost_category"
      colnames(gf_data)[5] <- "budget"
      colnames(gf_data)[6] <- "expenditure"
      gf_data <- gf_data[c(grep("service delivery", tolower(gf_data$cost_category)):(grep("total", tolower(gf_data$groups)))),]
      gf_data <- gf_data[, c("cost_category", "budget", "expenditure"),with=FALSE]
      gf_subset <- gf_data[-1, ,drop = FALSE]
      budget_dataset<- gf_subset[!is.na(gf_subset$cost_category),]
      budget_dataset$start_date <- start_date
      budget_dataset$disbursement <- 0
      
      }
    
    }else if (format=="pudr_disbursement"){
      colnames(gf_data)[3] <- "cost_category"
      gf_data <- gf_data[c(grep("interven", tolower(gf_data$cost_category)):grep("total", tolower(gf_data$cost_category))),]
      budget_dataset <- gf_data[, c(3,  grep("desembolso", colnames(gf_data)):ncol(gf_data)),with=FALSE]
      # workaround to delete columns that have an NA in the first row - 
      #for some reason, ghe_data <- ghe_data[,-is.na(ghe_data[1,])] isn't working. Planning to go to code drop in for help
      
      colnames(budget_dataset) <- as.character(budget_dataset[1,])
      drop.cols <- grep(paste("desembolso", collapse="|"), colnames(budget_dataset))
      budget_dataset <- budget_dataset[, (drop.cols) := NULL]
      budget_dataset <- budget_dataset[-1, ,drop = FALSE]
      budget_dataset <- budget_dataset[, c(0:qtr_num+1),with=FALSE]
      budget_dataset <- budget_dataset[-nrow(budget_dataset),] 
      colnames(budget_dataset)[1] <- "cost_category"
      
      setDT(budget_dataset)
      melted<- melt(budget_dataset,id="cost_category", variable.name = "qtr", value.name="disbursement")
      
      dates <- rep(year, qtr_num) # 
      for (i in 1:length(dates)){
        if (i==1){
          dates[i] <- dates[i]
        } else {
          dates[i] <- dates[i-1]%m+% months(3)
        }
      }
      
      
      ##turn the list of dates into a dictionary (but only for quarters!) : 
      dates <- setNames(dates,unique(melted$qtr))
      
      ## now match quarters with start dates 
      kDT = data.table(qtr = names(dates), value = TRUE, start_date = unname(dates))
      budget_dataset <-melted[kDT, on=.(qtr), start_date := i.start_date ]
      budget_dataset$budget <- 0
      budget_dataset$expenditure <- 0
      budget_dataset$qtr <- NULL


      
  }
  # ----------------------------------------------
  ## Code to aggregate into a dataset 
  
  ## now get region + budgeted expenses 
 
  
  ## Create other variables 
  
  budget_dataset$source <- source
  budget_dataset$period <- period
  budget_dataset$disease <- disease
  
  # ----------------------------------------------
  
  # return prepped data
  return(budget_dataset)
}
  
  