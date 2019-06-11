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
prep_pudr_gtm = function(dir, inFile, sheet_name, start_date, qtr_number, period) {
  
  ######## TROUBLESHOOTING HELP
  ### fill in variables below: inFile, sheet_name, start_date, qtr_num, disease, period, lang, grant, recipient_name 
  ### with information from line where the code breaks, and then uncomment by "ctrl + shift + c" and run code line-by-line
  ### look at gf_data and find what is being droped where.
  ########
# 

  dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", folder, "/")
  inFile = file_list$file_name[i]
  sheet_name = file_list$sheet[i]
  start_date = file_list$start_date[i]
  qtr_number = file_list$qtr_number[i]
  period = file_list$period[i]
  # 
  # Load/prep data
  gf_data <-data.table(read.xlsx(paste0(dir,inFile), sheet=sheet_name))
  
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
  cat1 = c("GASTOS SUBVENCION DE TUBERCULOSIS JULIO A DICIEMBRE 2016_RevALF.xlsx") #Files similar to: "J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gtm/raw_data/active/GTM-T-MSPAS/pudrs/GASTOS SUBVENCION DE TUBERCULOSIS JULIO A DICIEMBRE 2016_RevALF.xls"
  cat2 = c("GUA-311-G06-H EFR_Form_2016_EFR ENGLISH VERSION.xlsx", "GUA-M-MSPAS EFR FASE II -2016_Ingles_RevALF.xlsm") #Files similar to: J:\Project\Evaluation\GF\resource_tracking\_gf_files_gos\gtm\raw_data\not_active\GUA-311-G06-H\pudrs\GUA-311-G06-H EFR_Form_2016_EFR ENGLISH VERSION.xls
  
  #Sanity check: Is this sheet name one you've checked before? 
  if (!inFile%in%c(cat1, cat2)){
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
    names(gf_data) = c('intervention', 'budget_q3', 'budget_q4', 'expenditure') #EKL CAN WE TAKE OUT THIS HARD-CODING?? 
    
    #Split the intervention column into module and intervention. 
    gf_data[, intervention:=gsub("MDR-TB", "MDRTB", intervention)]
    split = strsplit(gf_data$intervention, "-")
    gf_data[, module:=sapply(split,`[`,1)]
    gf_data[, intervention:=sapply(split,`[`,2)]
    
    budget_dataset = melt(gf_data, id.vars=c('module', 'intervention'), measure.vars=c('budget_q3', 'budget_q4', 'expenditure'))
    budget_dataset[variable%in%c('budget_q3', 'budget_q4'), split:=1]
    budget_dataset[variable=='expenditure', split:=2]
    
    budget_dataset <- expandRows(budget_dataset, "split")
    
    #Divide expenditure by 2 because you've expanded rows to the quarter-level. 
    budget_dataset[, value:=as.numeric(value)]
    budget_dataset[variable=='expenditure', value:=value/2]
    
    #Assign a quarter variable to budget and expenditure
    budget_dataset[, quarter:=seq(3, 4, by=1), by=c('module', 'intervention', 'variable', 'value')]
    budget_dataset[variable=='budget_q3', quarter:=3]
    budget_dataset[variable=='budget_q4', quarter:=4]
    budget_dataset[, year:=year(start_date)]
    
    #Rename the budget values, and then reshape 
    budget_dataset[variable=='budget_q3'|variable=='budget_q4', variable:='budget']
    
    budget_dataset = dcast(budget_dataset, module+intervention+quarter+year~variable, value.var="value", fun.aggregate=sum_na_rm)
    
    #Add 'start date' variable
    budget_dataset[quarter==1, month:="01"]
    budget_dataset[quarter==2, month:="04"]
    budget_dataset[quarter==3, month:="07"]
    budget_dataset[quarter==4, month:="10"]
    
    budget_dataset[, start_date:=paste0(month, "-01-", year)]
    budget_dataset[, start_date:=as.Date(start_date, "%m-%d-%Y")]
    
    budget_dataset = budget_dataset[, -c('month')]
    
  }
  #-------------------------------------
  # 2. Prep category 2 type files. 
  #-------------------------------------
  if (inFile%in%cat2){
    #Grab module-intervention block of rows - first find the columns you need
    module_col <- 2
    intervention_col <- 4
    budget_col <- 5
    expenditure_col <- 7 #Just hand-coding this one; there are several columns. 
    
    stopifnot(length(module_col)==1 & length(intervention_col)==1 & length(budget_col)==1 & length(expenditure_col)==1)
    
    #Then find the rows you need by grepping those columns 
    start_row <- grep("Macrocategor", gf_data[[module_col]])
    end_row <- grep("TOTAL", gf_data[[1]])
    
    #Subset to this section, and fix names
    gf_data = gf_data[start_row:end_row, c(module_col, intervention_col, budget_col, expenditure_col), with=FALSE]
    names(gf_data) <- c('module', 'intervention', 'budget', 'expenditure')
    
    #Drop extra rows that don't have module specified, and drop title row
    gf_data = gf_data[!module=="Seleccione…", ]
    gf_data = gf_data[!is.na(module) & !is.na(intervention)]
    gf_data = gf_data[!module=="Macrocategoría"|module=="macrocategoria" | module=="Macrocategoria"]
    
    #Make budget and expenditure numeric
    gf_data[, budget:=as.numeric(budget)]
    gf_data[, expenditure:=as.numeric(expenditure)]
    
    #Reshape to the quarter-level. 
    totals_check = gf_data[, .(budget=sum(budget, na.rm = TRUE), expenditure=sum(expenditure, na.rm=TRUE))] #Add a totals check for later. 
    gf_data[, quarter:=quarter(start_date)]
    gf_data[, year:=year(start_date)]
    
    gf_data[, qtr_split:=round((period*qtr_number)/90)]
    gf_data[, split:=round((period*qtr_number)/90)] #Create this variable twice so you can divide budget/expenditure after expansion
    
    #Expand data by the number of days, and generate a variable to iterate over
    gf_data <- expandRows(gf_data, "qtr_split")
    byVars = names(gf_data)
    gf_data[, seq:=sequence(.N), by=byVars]
    gf_data[, seq:=seq-1] #Decrement by 1 because sequence indexes at 1. 
    
    #While seq is not 0, go through the loop below.
    #If seq is greater than or equal to 4, add 1 to year and divide everything by 4. Continue this loop while max(seq) > 4.
    # If month + seq + 1 equals 12, than
    gf_data[, new_qtr:=qtr_number+seq]
    max_quarter = max(gf_data$new_qtr)
    while (max_quarter>4){
      gf_data[new_qtr>4, year:=year+1]
      gf_data[new_qtr>4, new_qtr:=new_qtr-4]
      max_quarter = max(gf_data$new_qtr)
    }
    
    #Split up budget and expenditure.
    gf_data[, budget:=budget/split]
    gf_data[, expenditure:=expenditure/split]
    
    #Fix names 
    gf_data = gf_data[, -c('quarter', 'split', 'seq')]
    setnames(gf_data, 'new_qtr', 'quarter')
    
    #Generate new start date variable. 
    gf_data[quarter==1, month:="01"]
    gf_data[quarter==2, month:="04"]
    gf_data[quarter==3, month:="07"]
    gf_data[quarter==4, month:="10"]
    
    gf_data[, start_date:=paste0(month, "-01-", year)]
    gf_data[, start_date:=as.Date(start_date, "%m-%d-%Y")]
    gf_data[, month:=NULL]
    
    #Make sure you haven't changed any budget/expenditure numbers, and clean up
    totals_check2 = gf_data[, .(budget=sum(budget, na.rm = TRUE), expenditure=sum(expenditure, na.rm=TRUE))]
    for (i in 1:nrow(totals_check)){
      stopifnot(totals_check$budget[i]==totals_check2$budget[i] | totals_check$expenditure[i]==totals_check2$expenditure[i])
    }
    
    budget_dataset = copy(gf_data)
    #Drop out unneeded rows 
    budget_dataset = budget_dataset[!grep("Macrocategor", module)]
  }
 
  
  #-----------------------------------------------------------
  # Return prepped data
  #-----------------------------------------------------------
  return(budget_dataset)
}


  