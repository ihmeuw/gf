# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chem
#DATE: Written Nov. 2017, modified May 2019
#
# 
# Template for prepping SICOIN detailed data (most of the GHE data is formatted like this)
# Inputs:
# inFile - name of the file to be prepped
# year - which year the dataset corresponds to
#
# Outputs:
# budget_dataset - prepped data.table object

# ----------------------------------------------

# start function
prep_detailed_sicoin = function(inFile) {
  
  #Inputs for debugging
  # inFile = paste0(sicoinDir, file_list$file_path[i], file_list$file_name[i])

  # --------------------
  # Test the inputs
  if (class(inFile)!='character') stop('Error: inFile argument must be a string!')
  if (class(year)=='character') stop('Error: year argument must be a number!')
  # ----------------------------------------------
  # Files and directories
  
  # Load/prep data
  gf_data <- data.table(read_excel(inFile))
  
  # ----------------------------------------------
  #Need to get - location out
  
  #---------------------------------------------------
  # 1. Subset columns 
  #---------------------------------------------------
  #First, find the row that has the names in it. 
  description_col = grep("descripcion", tolower(gf_data)) #We don't actually save this one but it helps us get the columns we need. 
  stopifnot(length(description_col)==1)
  department_col = description_col + 2 #These 3 columns are shifted over one box. 
  municip_col = description_col + 3
  activity_col = description_col + 4
  
  budget_col = grep("asignado", tolower(gf_data))
  modified_col = grep("modificado", tolower(gf_data)) #We actually want the budget column between 'asignado' and 'modificado' that has an activity associated with it. 
  stopifnot(length(budget_col)==1 & length(modified_col)==1)
  budget_cols = budget_col:modified_col
  
  expenditure_col = grep("devengado", tolower(gf_data))
  paid_col = grep("pagado", tolower(gf_data))
  stopifnot(length(expenditure_col)==1 & length(paid_col)==1)
  expenditure_cols = expenditure_col:paid_col
  
  subset1 = c(department_col, municip_col, activity_col, budget_cols, expenditure_cols)
  gf_data = gf_data[, subset1, with=FALSE]
  names(gf_data)[1:3] <- c('department', 'municipality', 'activity') #Set names. 
  
  #Do a quick drop of unneeded rows - this makes the next subset easier. 
  #Drop the rows you don't need at the very bottom. 
  drop_text = "otros recursos|donaciones|prestamos externos|colocaciones internas|ingresos corrientes   government resources|ingresos propios  government resources"
  drop_col = grep(drop_text, tolower(gf_data)) #VERIFY WITH DAVID - ARE WE OKAY TO DROP THESE SECTIONS? 
  if (length(drop_col)!=0){ # (the drop_col changes from file to file, based on the section staggering, so needed to set this dynamically)
    drop_rows = integer()
    for (col in drop_col){
      drop_rows = c(drop_rows, grep(drop_text, tolower(gf_data[[col]])))
    }
  }
  if (verbose){
    print("Dropping the following rows from data:")
    print(gf_data[drop_rows])
  }
  # if (length(drop_rows)> 3) stop("There are more than three extraneous rows dropping - review logic condition.")
  gf_data = gf_data[!drop_rows]
  
  #Now, find the actual budget and expenditure columns - the ones where activity description has an amount associated with it. 
  # This makes sure you don't actually include a total row! 
  budget_start = grep("asignado", tolower(gf_data)) #First, reset your column indices after subsetting above. 
  exp_start = grep("devengado", tolower(gf_data))
  stopifnot(length(budget_start)==1 & length(exp_start)==1)
  
  #Find the row where activity description is not NA, and make sure you have a budget and expenditure amount associated with it. 
  activities = gf_data[!is.na(activity)]
  # *You want the first column after the 'budget start' that doesn't have NAs to represent the final budget, and the same for expenditure.
  budget_cols = budget_start:(exp_start-1)
  exp_cols = exp_start:ncol(activities)
  
  #Find the first column in the budget section that doesn't have "NA's" as values. 
  budget_col = budget_start
  while(anyNA(activities[[budget_col]]) & budget_col%in%budget_cols){ #Only check within a certain subset! 
    budget_col = budget_col + 1
  }
  #Find the first column in the expenditure section that doesn't have "NA's" as values. 
  exp_col = exp_start
  while(anyNA(activities[[exp_col]]) & exp_col%in%exp_cols){ #Only check within a certain subset! 
    exp_col = exp_col + 1
  }
  
  stopifnot(length(budget_col)==1 & length(exp_col)==1)
  
  #Subset to the correct budget and expenditure columns. 
  subset2 = c(1, 2, 3, budget_col, exp_col)
  gf_data = gf_data[, subset2, with=FALSE]
  
  names(gf_data)[4:5] = c('budget', 'expenditure')
  
  #----------------------------------------------------------
  # 2. Fill in the rows, and only keep the rows you need. 
  #---------------------------------------------------------
  #Fill in the columns so the data is not staggered. 
  #First, set a 'start point' in the first row 
  check_first_row = is.na(gf_data[1])
  if (FALSE%in%check_first_row){
    stop("Start point creates an error - review values in first row.")
  }
  #Fill the staggered columns using the na.locf function 
  gf_data[1, department:="BLANK"]
  gf_data[, department_new:=na.locf(department)]
  
  gf_data[1, municipality:="BLANK"]
  gf_data[, municipality_new:=na.locf(municipality)]
  
  gf_data[1, activity:="BLANK"]
  gf_data[, activity_new:=na.locf(activity)]
  
  #Subset rows 
  gf_data = gf_data[!is.na(budget) | !is.na(expenditure), .(department_new, municipality_new, activity_new, budget, expenditure)]
  names(gf_data) = c('department', 'municipality', 'activity', 'budget', 'expenditure')
  
  #Drop title row with "blanks" in categories - this row has nothing for budget/expenditure 
  gf_data = gf_data[!department=='BLANK']
  
  # ----------------------------------------------
  # 3. Data quality checks 
  # ----------------------------------------------
  gf_data[, budget:=as.numeric(budget)]
  gf_data[, expenditure:=as.numeric(expenditure)]
  
  #Flag if budget or expenditure total to 0. 
  budget_check = gf_data[, sum(budget)]
  exp_check = gf_data[, sum(expenditure)]
  if(budget_check==0) stop("Budget total 0 - review file prep.")
  if (verbose & exp_check==0){
    print("Expenditure total is 0 - this may not signify an error, but review file prep.")
  }
  
  # Flag if there is not both budget and expenditure for a single line item. 
  na_check = gf_data[(is.na(budget) & !is.na(expenditure)) | (!is.na(budget) & is.na(expenditure))]
  if (nrow(na_check)!=0) stop("Rows have NAs for budget but values for expenditure, or vice versa. Review file prep.")
  
  # return prepped data
  stopifnot(names(gf_data)%in%c('department', 'municipality', 'activity', 'budget', 'expenditure'))
  return(gf_data)
  
}

