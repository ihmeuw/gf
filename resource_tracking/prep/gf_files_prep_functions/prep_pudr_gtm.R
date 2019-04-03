# ------------------------------------------------------------------------
# AUTHOR: Emily Linebarger, based off of code by Irena Chen
# PURPOSE: Prep special-case PUDRs for Guatemala that don't fit into the 
#   standard modular approach format. 
#`  Files should be added to the if-statement logic below with FILE NAME ONLY 
#   to prevent the the wrong logic being used on the wrong file. 
# DATE: Last updated April 2019.
# ------------------------------------------------------------------------

# ----------------------------------------------
# function to prep the data
# ----------------------------------------------
prep_pudr_gtm = function(dir, inFile, sheet_name, start_date, qtr_num, disease, period, grant, source, loc_name, lang) {
  
  ######## TROUBLESHOOTING HELP
  ### fill in variables below: inFile, sheet_name, start_date, qtr_num, disease, period, lang, grant, recipient_name 
  ### with information from line where the code breaks, and then uncomment by "ctrl + shift + c" and run code line-by-line
  ### look at gf_data and find what is being droped where.
  ########

  folder = "budgets"
  folder = ifelse (file_list$data_source[i] == "fpm" , folder, "pudrs")
  file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")

  dir = file_dir
  inFile = file_list$file_name[i]
  sheet_name = file_list$sheet[i]
  start_date = file_list$start_date[i]
  qtr_num = file_list$qtr_number[i]
  period = file_list$period[i]
  disease = file_list$disease[i]
  lang = file_list$language[i]
  grant = file_list$grant[i]
  source = file_list$data_source[i]
  loc_name = 'gtm'
  
  # Load/prep data
  gf_data <-data.table(read_excel(paste0(dir,inFile), sheet=sheet_name))
  
  #Remove diacritical marks from inFile so it can be used for if-else statements below 
  inFile = fix_diacritics(inFile)
  
  # -----------------------------------------------------------------------------
  # Test the inputs to make sure that they are the correct type
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # -----------------------------------------------------------------------------
  # Files and directories
  
  #------------------------------------------------------------
  # 1. Decide which category of file this document belongs to. 
  #-------------------------------------------------------------
  cat1 = c("GASTOS SUBVENCION DE TUBERCULOSIS JULIO A DICIEMBRE 2016_RevALF.xls") #Files similar to: "J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gtm/raw_data/active/GTM-T-MSPAS/pudrs/GASTOS SUBVENCION DE TUBERCULOSIS JULIO A DICIEMBRE 2016_RevALF.xls"
  
  #Sanity check: Is this sheet name one you've checked before? 
  if (!inFile%in%cat1){
    print(inFile)
    stop("This file has not been run with this function before - Are you sure you want this function? Add file name to verified list within function to proceed.")
  }
  
  #-------------------------------------
  # 1. Prep category 1 type files. 
  #-------------------------------------
  if (inFile%in%cat1){
    #Grab module-intervention block of rows - first find the columns you need
    start_col <- grep("By Module - Intervention", gf_data)
    end_col <- grep("Total Ejecutado", gf_data)
    
    stopifnot(length(start_col)==1 & length(end_col)==1)
    
    #Then find the rows you need by grepping those columns 
    start_row <- grep("By Module - Intervention", gf_data[[start_col]])
    end_row <- grep("Total", gf_data[[start_col]])
    x = 1
    while (end_row[x] < start_row){ #Grab the next 'total' row past your start column. 
      x = x + 1
    }
    end_row = end_row[x]
    stopifnot(length(start_row)==1 & length(end_row)==1)
    
    #Subset to this section. 
    gf_data = gf_data[start_row:end_row, start_col:end_col]
    
    #Set the names, and remove the first and last rows (We know that the first one is names, and the last one is total)
    names(gf_data) = as.character(gf_data[1, ])
    gf_data = gf_data[2:(nrow(gf_data)-1), ]
    
    #Only keep the intervention, budget, and expenditure columns. 
    gf_data = gf_data[, c(1:3, 7)]
    names(gf_data) = c('intervention', 'budget_q3', 'budget_q4', 'expenditure')
    gf_data[, intervention:=gsub("MDR-TB", "MDRTB", intervention)]
    split = strsplit(gf_data$intervention, "-")
    gf_data[, module:=sapply(split,`[`,1)]
    gf_data[, intervention:=sapply(split,`[`,2)]
    
  }

  #Subset to only these columns.
  gf_data = gf_data[, .(module, intervention, budget, expenditure)]
    

  if(!'start_date'%in%colnames(budget_dataset)){
    stop("PUDR with multiple quarters- review GTM prep code.")
  }
  budget_dataset$period <- period
  
  #-----------------------------------------------------------
  # Return prepped data
  #-----------------------------------------------------------
  return(budget_dataset)
}


  