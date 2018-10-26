
# ----------------------------------------------
# Irena Chen
#
# 12/18/2017
# Template for prepping GF detailed budget data  
# Inputs:
# inFile - name of the file to be prepped
# Outputs:
# budget_dataset - prepped data.table object
# ----------------------------------------------

prep_detailed_budget = function(dir, inFile, sheet_name, start_date, 
                                qtr_num, disease, period, lang, grant, loc_id, source){
  
  
  ######## TROUBLESHOOTING HELP
  ### fill in variables below with information from line where the code breaks (use file list to find variables)
  ### uncomment by "ctrl + shift + c" and run code line-by-line
  ### look at gf_data and find what is being droped where.
  ########
  
  # file_dir <- 'J:/Project/Evaluation/GF/resource_tracking/cod/gf/'
  # dir = file_dir
  # inFile = ""
  # sheet_name = ""
  # start_date = ""
  # qtr_num =
  # period =
  # disease = ""
  # lang = ""
  # grant = ""
  # source = "fpm"
  # loc_id = "cod"
  

  # ----------------------------------------------
  ##set up functions to handle french and english budgets differently
  # ----------------------------------------------
  ## we need to grab the budget numbers by quarter - first determine if french or english
  if(lang=="eng"){
    cashText <- " Cash \r\nOutflow"
  } else{
    cashText <- "Sorties de trésorerie"
  }
  
  ## the recipient column is named differently, depending on if it's an older or newer budget 
  if(year(start_date)==2018){
    recipient <- "Implementer" 
  } else {
    recipient <- "Récipiendaire"
  }
  
  ##create a vector of the column names we want from the budget 
  if(lang=="eng"){
    qtr_names <- c("Module","Intervention","Activity Description","Cost Input", "Recipient", "Geography/Location", rep(1, qtr_num))
  } else { 
    qtr_names <- c("Module","Intervention","Description de l'activité","Élément de coût", recipient, "Geography/Location", rep(1, qtr_num))
  }
  
  ## create a vector that has the name of each quarter's budget
  create_qtr_names = function(qtr_names, cashText, lang){
    for(i in 1:qtr_num+6){
      if(i <7) {
        i=i+1
      } else { 
        if(lang=="eng"){
          qtr_names[i] <- paste("Q", i-6,  cashText, sep="")
        } else if(lang=="fr" & qtr_num < 12){
          qtr_names[i] <- paste(cashText, " T", i-(12-qtr_num), sep="") ## in french, quarter = "trisemestre" 
        } else{
          qtr_names[i] <- paste(cashText, " T", i-6, sep="")
        }
        i=i+1
      }
    }
    return(qtr_names)
  }
  qtr_names <- create_qtr_names(qtr_names, cashText, lang)
  # ----------------------------------------------
  ##read the data: 
  # ----------------------------------------------
  gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))
    
  ## drop the first two rows and two columns (they are unnecessary)
  if(year(start_date)==2018){  ## for the newer budgets, the first two rows aren't necessary
    gf_data <- gf_data[-c(1:2),]
    colnames(gf_data) <- as.character(gf_data[1,])
    gf_data  <- gf_data[-1,]
  }
  gf_data <- gf_data[,-c(1:2)]
  
  gf_data <- gf_data[,names(gf_data)%in%qtr_names, with=FALSE]
  ##only keep data that has a value in the "category" column 
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  
  ##rename the columns to RT variables 
  colnames(gf_data)[1] <- "module"
  colnames(gf_data)[2] <- "intervention"
  colnames(gf_data)[3] <- "sda_activity"
  colnames(gf_data)[4] <- "cost_category"
  colnames(gf_data)[5] <- "recipient"
  if(year(start_date)==2018){
    colnames(gf_data)[6] <- "loc_name" ##the newer budgets sometimes have geo-location data 
  }

  # ----------------------------------------------
  ## invert the dataset so that budget expenses and quarters are grouped by category
  # ----------------------------------------------
  setDT(gf_data)
  if(year(start_date)==2018){
    gf_data1<- melt(gf_data,id=c("module","intervention","sda_activity","cost_category", "recipient", "loc_name"), variable.name = "qtr", value.name="budget")
    gf_data1$loc_name <- as.character(gf_data$loc_name)
  } else {
    gf_data1<- melt(gf_data,id=c("module","intervention","sda_activity","cost_category", "recipient"), variable.name = "qtr", value.name="budget")
    gf_data1$loc_name <- "cod"
  }
  
  ## create a vector of start_dates that correspond to each quarter in the budget 
  dates <- rep(start_date, qtr_num) # 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }
  
  ## make sure that you have a date for each quarter - will tell you if you're missing any 
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
  budget_dataset$expenditure <- 0 
  budget_dataset$grant_number <- grant
  if(grant=="COD-M-PSI"&lang=="eng"){
    lang <- "fr"
  }
  budget_dataset$lang <- lang
  budget_dataset$data_source <- source
  # ----------------------------------------------
  ##separate tb/hiv into either one or the other in order to map programs properly - later we might want to go back and change 
  # ----------------------------------------------
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
  budget_dataset$disease <- mapply(sep_hiv_tb, budget_dataset$module, budget_dataset$loc_name)
  } else {
    budget_dataset$disease <- disease
  }
  return(budget_dataset)  
  
  
}


