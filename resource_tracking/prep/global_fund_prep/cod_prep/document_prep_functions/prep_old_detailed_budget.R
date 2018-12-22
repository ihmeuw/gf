
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

prep_old_detailed_budget = function(dir, inFile, sheet_name, start_date, 
                                qtr_num, disease, period, lang, grant, loc_id, source, recipient){
  
  # dir = file_dir
  # inFile = "official_budgets/10Jul12_Final  Budget SSF_ ZAR-H-CORDAID.xlsm"
  # sheet_name = "Budget détaillé - Année 4"
  # start_date = "2013-01-01"
  # qtr_num = 4
  # disease = "hiv"
  # period = 90
  # lang = "fr"
  # grant = "COD-H-CORDAID"
  # loc_id = "cod"
  # source = "old_detailed"
  
  
  ##read the data: 
  gf_data <- data.table(read_excel(paste0(dir, inFile), sheet=as.character(sheet_name)))
  sheet_name = fix_diacritics(sheet_name)
  str_replace(start_date, "\\\\", "")
  start_date = substring(start_date, 2, 11)
  start_date = as.Date(start_date)
  
  gf_data <- gf_data[,-c(1:2)]
  if(sheet_name=="Budget detaille - Annee 3."){
    qtr_names <- c("X__6" ,"X__7")
  } else {
    qtr_names <- c("X__4", "X__5", "X__6" ,"X__7")
  }
  col_names <- c("Domaine de prestation de services (DPS)","Activite","Categorie de cout" ,qtr_names)
  
  setnames(gf_data, fix_diacritics(names(gf_data)))
  
  ##only keep data that has a value in the "category" column 
  gf_data <- gf_data[,names(gf_data)%in%col_names, with=FALSE]
  
  gf_data <- na.omit(gf_data, cols=1, invert=FALSE)
  colnames(gf_data)[1] <- "module"
  colnames(gf_data)[2] <- "sda_activity"
  colnames(gf_data)[3] <- "cost_category"

  
  ## invert the dataset so that budget expenses and quarters are grouped by category
  ##library(reshape)
  setDT(gf_data)
  gf_data1<- melt(gf_data,id=c("module","sda_activity","cost_category"), variable.name = "qtr", value.name="budget")
  
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
  budget_dataset$intervention <- "all"
  budget_dataset$loc_name <- loc_id
  budget_dataset$recipient <- recipient
  budget_dataset$disease <- disease
  budget_dataset$expenditure <- 0 
  budget_dataset$grant_number <- grant
  budget_dataset$lang <- lang
  budget_dataset$data_source <- source
  budget_dataset$year <- year(budget_dataset$start_date)
  return(budget_dataset)
  
}


