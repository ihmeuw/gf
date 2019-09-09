#-----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Load and validate Master GF File List 
# DATE: August 2019 
# ----------------------------------------------

#---------------------------------------
# TO-DO: Add a check to make sure primary and secondary recipients are standardized across grants. 


load_master_list = function(purpose=NULL) {
  require(data.table)
  require(readxl) 
  
  if (is.null(purpose)) stop("Please specify the 'purpose' option. Options are 'financial' or 'performance indicators'")
  stopifnot(purpose%in%c('financial', 'performance indicators'))
  
  #Read in data. 
  dt = data.table(read_excel(paste0(dir, "_gf_files_gos/master_file_list.xlsx")))
  #*** Note that NA's entered by hand in the excel will be imported as strings! ("NA")
  
  #------------------------------------------------
  #Run some validation checks before you start. 
  #------------------------------------------------
  #Certain columns should never have missing values. 
  core_cols = c('loc_name', 'grant', 'grant_period', 'grant_status', 'file_name', 'disease', 'data_source', 'primary_recipient', 
                'file_currency', 'file_iteration') #Note that secondary_recipient and geography_detail aren't included in this list at the moment, 
                                                  # because they aren't really used in the prep pipeline at the moment. EL 9.9.2019
  for (col in core_cols) {
    if (verbose){
      print(paste0('Checking for NAs in ', col))
    } 
    stopifnot(nrow(dt[is.na(get(col))])==0)
  } 
  
  #Certain columns should only have specific values. 
  #First, check data source and function columns so they can be used to filter rows. 
  stopifnot(unique(dt$data_source)%in%c('budget', 'pudr', 'document', 'performance_framework'))
  stopifnot(unique(dt$function_financial)%in%c('detailed', 'detailed_other', 'module', 'old_detailed', 'pudr', 'summary', 'unknown', 'NA'))
  stopifnot(unique(dt$function_performance)%in%c('master', 'unknown', 'NA'))
  
  #Then, drop out data types that aren't being processed. 
  if (purpose=="financial") dt = dt[data_source%in%c('budget', 'pudr') & !function_financial%in%c('NA', 'unknown')]
  if (purpose=="performance indicators") dt = dt[data_source%in%c('pudr', 'performance_framework') & !function_performance%in%c('NA', 'unknown')]
  #*** Note that 'unknown' typed into a column means data is there, but there's not a function that can process it yet. 
  # 'NA' typed into a column means that extraction type doesn't apply here. 
  
  #Check remaining columns - you should have no NA values for these now. 
  stopifnot(unique(dt$loc_name)%in%c('cod', 'uga', 'gtm', 'sen'))
  stopifnot(unique(dt$grant_status)%in%c('active', 'not_active'))
  stopifnot(unique(dt$disease%in%c('hiv', 'tb', 'malaria', 'rssh', 'hiv/tb')))
  stopifnot(unique(dt$file_currency)%in%c('USD', 'EUR', 'LOC'))
  stopifnot(unique(dt$geography_detail)%in%c('NATIONAL', 'SUBNATIONAL'))
  stopifnot(unique(dt$file_iteration)%in%c('final', 'initial', 'revision'))
  
  #Drop out data types that aren't being processed. 
  if (purpose=="financial") dt = dt[data_source%in%c('budget', 'pudr')]
  if (purpose=="performance indicators") dt = dt[data_source%in%c('pudr', 'performance_framework')]
  
  #Correct date formats
  dt[, start_date_financial:=as.Date(as.numeric(start_date_financial), origin="1899-12-30")]
  dt[, start_date_programmatic:=as.Date(as.numeric(start_date_programmatic), origin="1899-12-30")]
  dt[, end_date_programmatic:=as.Date(as.numeric(end_date_programmatic), origin="1899-12-30")]
  
  #Validate columns based on the type of extraction you're doing. 
  #-------------------------------
  # Financial 
  #-------------------------------
  if (purpose=="financial") {
    keep_cols = c('function_financial', 'sheet_financial', 'start_date_financial', 'period_financial', 'qtr_number_financial', 'language_financial', 
                  'pudr_semester', 'update_date', 'mod_framework_format')
    keep_cols = c(core_cols, keep_cols)
    dt = dt[, c(keep_cols), with=F]
    
    for (col in names(dt)[!names(dt)%in%c('start_date_financial', 'update_date', 'pudr_semester')]){ #Check all applicable string columns. PUDR semester is OK to be NA if the line-item is a budget.  
      if (verbose){
        print(paste0("Checking for NA values in ", col))
      }
      stopifnot(nrow(dt[get(col)=="NA" | is.na(get(col))])==0)
    }
    
    #Check date variables, and special string variables. 
    stopifnot(nrow(dt[is.na(start_date_financial)])==0)
    stopifnot(nrow(dt[is.na(update_date) & file_iteration=="revision"])==0)
    stopifnot(nrow(dt[data_source=="pudr" & (pudr_semester=="NA" | is.na(pudr_semester))])==0) #Check PUDR semester. 
  }
  
  
  #-------------------------------
  # Performance indicators 
  #-------------------------------
  
  if (purpose=="performance indicators") {
    keep_cols = c('function_performance', 'sheet_impact_outcome_1a', 'sheet_impact_outcome_1a_disagg', 'sheet_coverage_1b', 'sheet_coverage_1b_disagg', 
                  'start_date_programmatic', 'end_date_programmatic', 'language_programmatic')
    keep_cols = c(core_cols, keep_cols)
    dt = dt[, c(keep_cols), with=F]
    
    for (col in names(dt)[!names(dt)%in%c('start_date_programmatic', 'end_date_programmatic')]){ #Check all applicable string columns. PUDR semester is OK to be NA if the line-item is a budget.  
      if (verbose){
        print(paste0("Checking for NA values in ", col))
      }
      stopifnot(nrow(dt[get(col)=="NA" | is.na(get(col))])==0)
    }
    
    #Check date variables, and special string variables. 
    stopifnot(nrow(dt[is.na(start_date_programmatic)])==0)
    stopifnot(nrow(dt[is.na(end_date_programmatic)])==0)
  }
  
  
  #--------------------------------------------------------
  # Make sure that hand-entered information matches with GF metadata. 
  #--------------------------------------------------------
  metadata = fread(paste0(dir, "_gf_files_gos/metadata/grant_agreement_implementation_periods_dataset_201963.csv"))
  correct_periods = metadata[GeographicAreaCode_ISO3%in%c('COD', 'GTM', 'SEN', 'UGA'), .(GrantAgreementNumber, ImplementationPeriodStartDate, ImplementationPeriodEndDate)]
  names(correct_periods) = c('grant', 'grant_period_start', 'grant_period_end')
  
  #Format dates correctly 
  correct_periods[, grant_period_start:=tstrsplit(grant_period_start, " ", keep=1)][, grant_period_start:=as.Date(grant_period_start, format="%m/%d/%Y")]
  correct_periods[, grant_period_end:=tstrsplit(grant_period_end, " ", keep=1)][, grant_period_end:=as.Date(grant_period_end, format="%m/%d/%Y")]
  
  #Extract grant period 
  correct_periods[, grant_period:=paste0(year(grant_period_start), "-", year(grant_period_end))]
  correct_periods[, correct_grant_period:=grant_period]
  correct_periods = correct_periods[, .(grant, grant_period, correct_grant_period)]
  
  #Merge data together
  our_periods = unique(dt[data_source%in%c('fpm', 'pudr', 'performance_framework'), .(grant, grant_period)])
  #EMILY WE SHOULD FLAG WHEN GRANT PERIODS ARE NA!! 
  our_periods = our_periods[!is.na(grant_period)]
  check = merge(our_periods, correct_periods, by=c('grant', 'grant_period'), all.x=T)
  if (nrow(check[is.na(correct_grant_period)]) != 0){
    print(check[is.na(correct_grant_period), .(grant, grant_period)])
    warning("These grant periods are incorrectly tagged. Match grant periods in Global Fund metadata. (correct_periods)")
  }
  
  incorrect_grants = unique(our_periods$grant[!our_periods$grant%in%correct_periods$grant])
  if (length(incorrect_grants)>0) { 
    print(incorrect_grants) 
    stop("There are grant names that don't match with GF metadata.")
  }
  
  
  #----------------------------------------------------------------------------------
  #So that you always get consistent ordering, even if the excel beneath is filtered. 
  #----------------------------------------------------------------------------------
  dt = dt[order(loc_name, grant_period, grant_period, data_source, file_name)] 
  
  return(dt) 
}