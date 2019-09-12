# Validate PUDR hand-entered dates, both for programmatic and financial data
library(XLConnect)
setwd("C:/Users/elineb/Documents/gf")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")
source("./resource_tracking/prep/_common/shared_functions.r", encoding="UTF-8")
verbose=T

dir = "J:/Project/Evaluation/GF/resource_tracking/"

file_list = data.table(read_excel(paste0(dir, "_gf_files_gos/master_file_list.xlsx")))
file_list[, start_date_financial:=as.Date(as.numeric(start_date_financial), origin="1899-12-30")]
file_list[, start_date_programmatic:=as.Date(as.numeric(start_date_programmatic), origin="1899-12-30")]
file_list[, end_date_programmatic:=as.Date(as.numeric(end_date_programmatic), origin="1899-12-30")]

gos_year=2015
file_list = prioritize_gos(file_list)

#Drop out strangely formatted GTM PUDRs 
file_list = file_list[!(sheet_financial%in%c('INTEGRACION', "LFA EFR_7", 'PR EFR_7A') & loc_name=="gtm")]

#Only keep the PUDRs you want to look at. 
file_list = file_list[data_source=="pudr" & file_iteration=="final" & (function_financial=="pudr" | function_performance=="master")]
  
pudr_mod_approach_sheet_financials <- c('LFA Expenditure_7B', 'LFA AFR_7B', 'PR Expenditure_7A', 'RFA ALF_7B', 'ALF RFR_7')

for(i in 1:nrow(file_list)){
    if (!file_list$sheet_financial[i]%in%pudr_mod_approach_sheet_financials){
      print(paste0("Sheet is not in accepted sheets", file_list$sheet_financial[i]))
    }
    # Set up file path 
    folder = "budgets"
    folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
    if (file_list$file_iteration[i]=="initial"){
      version = "iterations"
    } else if (file_list$file_iteration[i]=="revision"){
      version= "revisions"
    } else {
      version = ""
    }
    grant_period = file_list$grant_period[i]
    
    file_dir = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", file_list$loc_name[i], "/raw_data/", file_list$grant_status[i], "/", 
                      file_list$grant[i], "/", grant_period, "/", folder, "/")
    if (version != ""){
      file_dir = paste0(file_dir, version, "/")
    }
    
    #Load the workbook, and pull the available sheet names. 
    wb = loadWorkbook(paste0(file_dir, file_list$file_name[i]))
    sheets = getSheets(wb)
    coverSheetNames = c('Portada', 'CoverSheet')
    sheets = sheets[sheets%in%coverSheetNames]
    if (length(sheets==1)){
      print("Success!")
      # read_excel(paste0(dir, file_list$file_name[i]))
    } else {
      print(paste0("No cover sheet found for: ", file_list$file_name[i], "."))
    }
    wb=NULL
    
    print((paste0(i, " ", file_list$file_name[i])))
} 