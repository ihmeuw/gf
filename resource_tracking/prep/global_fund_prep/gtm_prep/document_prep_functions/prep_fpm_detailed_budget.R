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
##Function to prepare the detailed budgets: 
# ----------------------------------------------
prep_fpm_detailed_budget = function(dir, inFile, sheet_name, start_date, qtr_num, disease, period, lang, grant_name, recipient_name){
  
  ######## TROUBLESHOOTING HELP
  ### fill in variables below: inFile, sheet_name, start_date, qtr_num, disease, period, lang, grant, recipient_name 
  ### with information from line where the code breaks, and then uncomment by "ctrl + shift + c" and run code line-by-line
  ### look at gf_data and find what is being droped where.
  ########
  
  # file_dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/grants/active/GTM-H-INCAP/budgets/'
  # dir = file_dir
  # inFile = "06 FR100-GTM-H_DB_INCAP_05feb2018.xlsx"
  # sheet_name = "Detailed Budget"
  # start_date = "2018-10-01"
  # qtr_num = 9
  # period = 90
  # disease = "hiv"
  # lang = "esp"
  # grant_name = "GTM-H-INCAP"
  # recipient_name = "INCAP"

  
  ##first determine if budget is in spanish or english
  if(lang=="eng"){
    cashText <- " Cash \r\nOutflow"
    loc_id <- "Geography/Location"
  } else{
    cashText <- "Salida de efectivo"
    loc_id <-  "Localizacion"
  }
  
  ## newer budgets use the label "Implementador" and the old ones use "Receptor" 
  if(year(start_date)>=2018){
    recipient <- "Implementador"
  } else{
    recipient <- "Receptor"
  }
  
  ##names of the columns we want to ultimately go into our database: 
  if(lang=="eng"){
    qtr_names <- c("Module","Intervention", "Recipient", loc_id, rep(1, qtr_num))
  } else{ 
    qtr_names <- c("Modulo", "Intervencion","Descripcion de la actividad","Categoria de Gastos"	, recipient, loc_id, rep(1, qtr_num))
  }
  
  ##add in the quarter names to the list: 
  create_qtr_names = function(qtr_names, cashText){
    for(i in 1:qtr_num+6){
      if(i <7) {
        i=i+1
      } else { 
        qtr_names[i] <- paste("Q", i-6, " ",  cashText, sep="")
        i=i+1
      }
    }
    return(qtr_names)
  }
  qtr_names <- create_qtr_names(qtr_names, cashText)
  
  ##load the FPM data: 
  if(!is.na(sheet_name)){
    gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))
  } else {
    gf_data <- data.table(read_excel(paste0(dir, inFile)))
  }
  

  gf_data <- gf_data[,-c(1:2)]
  ##GUA-M-MSPAS formatted differently: 
  if(grant_name=="GUA-M-MSPAS"){
    gf_data <- gf_data[-1,]
    colnames(gf_data) <- as.character(gf_data[1,])
    gf_data <- gf_data[-c(1:2),]
    budget_dataset <- gf_data[, c("Activity" ,"Total Presupuesto Year 1"), with=FALSE]
    names(budget_dataset) <- c("sda_activity","budget")
    budget_dataset <- na.omit(budget_dataset, cols=c("sda_activity"))
    budget_dataset <- unique(budget_dataset, by=c("sda_activity","budget"))
  } else {
    ##new budgets formatted slightly differently: 
    if(year(start_date)>=2018){
      gf_data <- gf_data[-c(1:2),]
      colnames(gf_data) <- as.character(gf_data[1,])
      gf_data <- gf_data[-1,]
      }
  
  setnames(gf_data, fix_diacritics(names(gf_data)))
  ##only get the columns that we want
  gf_data <- gf_data[,names(gf_data)%in%qtr_names, with=FALSE]
  
  ##only keep data that has a value in the "category" column 
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  
  colnames(gf_data)[1] <- "module"
  colnames(gf_data)[2] <- "intervention"
  colnames(gf_data)[3] <- "sda_activity"
  colnames(gf_data)[4] <- "cost_category"
  
  if(!(recipient %in% colnames(gf_data))){
   gf_data$recipient <- grant_name
  } else{
    colnames(gf_data)[5] <- "recipient" 
    gf_data$recipient = recipient_name
    }
  if(!(loc_id %in% colnames(gf_data))){
    gf_data$loc_id <- "gtm"
  } else{
    colnames(gf_data)[6] <- "loc_id" 
  }
  
  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  ##library(reshape)
  setDT(gf_data)
  gf_data1<- melt(gf_data,id=c("module", "intervention","sda_activity","cost_category", "recipient", "loc_id"), 
                    variable.name = "qtr_num", value.name="budget")

  
  dates <- rep(ymd(start_date), qtr_num) # 
  for (i in 1:length(dates)){
    if (i==1){
      dates[i] <- start_date
    } else {
      dates[i] <- dates[i-1]%m+% months(3)
    }
  }

  if(length(dates) != length(unique(gf_data1$qtr_num))){
    stop('quarters were dropped!')
  }
  ##turn the list of dates into a dictionary (but only for quarters!) : 
  dates <- setNames(dates,unique(gf_data1$qtr_num))
  
  
  ## now match quarters with start dates 
  kDT = data.table(qtr_num = names(dates), value = TRUE, start_date = unname(dates))
  budget_dataset <-gf_data1[kDT, on=.(qtr_num), start_date := i.start_date ]
  budget_dataset$qtr_num <- NULL
  }
  budget_dataset$grant_number <- grant_name
  budget_dataset$period <- period
  budget_dataset$disease <- disease
  budget_dataset$expenditure <- 0 
  budget_dataset$lang <- lang
  budget_dataset$loc_id <- NULL

  return(budget_dataset)  
}
