# ----------------------------------------------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chen
# PURPOSE: Shared functions for the resource tracking database. All of these functions 
# perform calculations, for string-prep and file-loading functions 
# review 'shared_prep_functions'. 
# DATE: Last updated March 2019. 
#-----------------------------------------------------------------------------------

#----------------------------------------------
#Functions to verify budgets, used in step 5. 
#----------------------------------------------
#Sums budget by key variables
check_budgets_pudrs = function(dt){
  keyVars = c("start_date", "file_name", "grant", "data_source")
  #Deciding not to split by disease here because we just want the total for the whole quarter. 
  dt[, budget:=as.numeric(budget)]
  dt[, expenditure:=as.numeric(expenditure)]
  
  dt[is.na(budget), budget:=0]
  dt[is.na(expenditure), expenditure:=0]
  #Replacing budget and expenditure as NA 
  budgets = dt[ , 
               lapply(.SD, sum), 
                by = keyVars, 
               .SDcols = c("budget", "expenditure")]
  budgets <- unique(budgets)
  return(budgets)
}

check_SICOIN_dates = function(dt){
  # ouptut can be used to compare to "Data Seeking Spreadsheets"
  dt = dt[data_source == "sicoin"]
  dt$budget <- as.numeric(dt$budget)
  dt[,bug_sum := sum(budget, na.rm = TRUE), by = c("start_date", "grant_number", "end_date", "period", "fileName", "grant_period", "disease")]
  dt$sdaDetail = ifelse(dt$financing_source == 'gf', "Summary", "None")
  dt$geog = ifelse(dt$adm1 == 100, "National", "Municipality")
  sicoin_dt = unique(dt[, c("data_source", "financing_source","grant_period", "start_date", "end_date", 'sdaDetail', 'geog', "period", "grant_number", "disease", "fileName", "bug_sum")])
  sicoin_dt$financing_source = ifelse(sicoin_dt$financing_source == "other_dah", "donacions", sicoin_dt$financing_source)
  sicoin_dt$data_source = paste0(sicoin_dt$data_source, "-", sicoin_dt$financing_source)
  sicoin_dt$financing_source = NULL
  sicoin_dt$grant_period = "none"
  
  # files with "Municiaplity" also have "National estimates, removing those for geographic detail purposes
  dt_muni = sicoin_dt[geog == 'Municipality']
  dt_national = sicoin_dt[geog == 'National']
  dt_national <- subset(dt_national, !fileName %in% dt_muni$fileName)
  sicoin_dt = rbind(dt_muni, dt_national)
  sicoin_dt$start_date = as.Date(sicoin_dt$start_date)
  sicoin_dt$end_date = as.Date(sicoin_dt$end_date)
  return(sicoin_dt)
}

total_budget_by_grantPeriod = function(dt_g){
  dt_g[,bug_sum := sum(budget, na.rm = TRUE), by = c('grant_number', "grant_period", "fileName")]
  return(unique(dt_g[data_source == "fpm", c("grant_number", "grant_period", "bug_sum", "fileName")]))
}

#-------------------------- 
# Sum, removing NA's (simple function to add into data tables) 
#---------------------------
sum_na_rm <- function(col){
  col = sum(col, na.rm = TRUE)
  return(col)
}


#--------------------------------------------------------------------------------
# Orders a dataset to be at the quarter-level given a start and an end date. - EMILY WANT TO ADD THIS
#--------------------------------------------------------------------------------

#---------------------------------------------------------------------------------
# Converts from euros to USD for a given year. 
# Based off of the FGH team's currency conversion function, but with euros added 
# (and much simpler). 
#---------------------------------------------------------------------------------

convert_eur_usd = function(dt, yearVar=NULL, valueVars=NULL){
  if (is.null(yearVar)) stop("year argument is NULL")
  if (is.null(valueVars)) stop("value argument is NULL")
  if (class(yearVar)!='character') stop("year must be a character")
  if (!yearVar %in% names(dt)) stop("year argument is not a variable in data table")
  if (any(!valueVars %in% names(dt))) stop("value argument is not a variable in data table")
  
  #Pulled out from FGH team's currency conversion function - Emily Linebarger, 5/7/19 
  oecd_xrate = fread(paste0(j, "/Project/IRH/HIV/01_data/raw_data/xrates_oecd_121118.csv")) #This data includes 1995-2017. 
  oecd_xrate = oecd_xrate[LOCATION == "EA19", .(TIME, Value)]
  setnames(oecd_xrate, c('TIME','Value'), c(yearVar, 'eur_usd'))
  oecd_xrate[, (yearVar):= as.integer(get(yearVar))]
  
  #Re-extracted from the OECD's website to grab 2018 as well. 
  oecd_xrate2 = fread(paste0(code_dir, "_common/oecd_conversion_rates_2018.csv"))
  oecd_xrate2 = oecd_xrate2[LOCATION == "EA19", .(TIME, Value)]
  setnames(oecd_xrate2, c('TIME','Value'), c(yearVar, 'eur_usd'))
  oecd_xrate2[, (yearVar):= as.integer(get(yearVar))]
  
  #Bind these two time series together. 
  oecd_xrate = rbind(oecd_xrate, oecd_xrate2)
  
  converted_to_USD <- merge(dt, oecd_xrate, 
                            by = yearVar, 
                            all.x = T)
  for(v in valueVars) converted_to_USD[, (v):=get(v)/eur_usd]
  # converted_to_USD[, lapply(.SD, function(v) { get(v)/eur_usd }), .SDcols=valueVars]
  converted_to_USD[, orig_currency:= 'EUR']
  
  return(converted_to_USD)
  
  
  
  #Attempts to use FGH team's currency function - these were unsuccessful. If we can debug this function it would be better to return to it. 
  # The common function was returning all budget and expenditure values as NA.
  # Emily Linebarger 5/7/19 
  {
    # source(paste0(j, '/Project/IRH/HIV/code/currency_conversion.R')) #FGH team's currency conversion function. 
    # 
    # #How to use the FGH team's currency conversion function - an example from Miranda Tao
    # test <- data.table(iso3 = c('UGA','UGA','GTM','GTM','COD','COD'),
    #                    year = c(2000, 2001, 2003, 2004, 2003,2004),
    #                    currency_year = c(2000, 2001, 2003, 2004, 2003,2004),
    #                    val1 = runif(n = 6,min = 4000, max = 8000),
    #                    val2 = runif(n = 6, min = 300, max = 1000))
    # 
    # test_new_1 <- currency_conversion(data = test,
    #                                   col.loc = 'iso3',
    #                                   col.currency.year = 'currency_year',
    #                                   currency = 'EUR',
    #                                   col.value = c('val1','val2'),
    #                                   base.year = 2018,
    #                                   base.unit = 'USD',
    #                                   simplify = T,
    #                                   converter.version = 3)
    # 
    # `col.loc` is location column(must be iso3 code) in your data,
    # `col.value` is value column that you want to be converted (can take multiple value columns)
    # `currency` is the raw currency of the value you want to be converted
    # `col.currency.year` is the column for year of the currency
    # `base.year` is 2018 in your case
    # `base.unit` is usd in your case
    # data = needs_conversion 
    # col.loc = 'loc_name'
    # col.currency.year='year'
    # currency='EUR'
    # col.value=c('budget', 'expenditure')
    # base.year=2018
    # base.unit='USD'
    # simplify=T
    # converter.version=3
    # converted_to_USD = currency_conversion(data = needs_conversion,
    #                                        col.loc = 'loc_name',
    #                                        col.currency.year = 'year',
    #                                        currency = 'EUR',
    #                                        col.value = c('budget','expenditure'),
    #                                        base.year = 2018,
    #                                        base.unit = 'USD',
    #                                        simplify = T,
    #                                        converter.version = 3)
  }

}

