# ----------------------------------------------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chen
# PURPOSE: Shared functions for the resource tracking database. 
# DATE: Last updated July 2019 
#-----------------------------------------------------------------------------------


# ----------------------------------------------
#Fixes diacritical marks
# ----------------------------------------------

fix_diacritics = function(x){
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  #print(names(replacement_chars))
  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))
}

#--------------------------
#Replaces common acronyms 
#--------------------------
replace_acronyms = function(x) {
  x = gsub('fortgs', 'fortransgenderpeople', x)
  x = gsub('pwid', 'peoplewhoinjectdrugs', x)
  x = gsub('msm', 'menwhohavesexwithmen', x)
  x = gsub('stis', 'sexuallytransmittedinfections', x)
  return(x)
}

#-------------------------------------------------------
# Split HIV/TB combined grants  
# ------------------------------------------------------
split_hiv_tb = function(dt){
  tb_mods <- c('Multidrug-resistant TB', 'TB care and prevention')
  hiv_mods <- c('Comprehensive prevention programs for men who have sex with men', 'Comprehensive prevention programs for sex workers and their clients', 'Comprehensive prevention programs for transgender people',
                'HIV Testing Services', 'Prevention of mother-to-child transmission', 'Prevention programs for adolescents and youth, in and out of school', 'Prevention programs for general population',
                'Programs to reduce human rights-related barriers to HIV services', 'Treatment, care and support', 'Comprehensive prevention programs for people who inject drugs and their partners')
  rssh_mods <- c('Community responses and systems', 'Integrated service delivery and quality improvement', 'Health management information system and monitoring and evaluation',
                 'Human resources for health, including community health workers', 'Procurement and supply chain management systems')
  
  #Make sure all diseases are spelled the same 
  dt[disease == "tb/hiv", disease := "hiv/tb"]
  
  #Reclassify based on gf_module 
  dt[gf_module %in% tb_mods & disease == "hiv/tb", disease:="tb"]
  dt[gf_module %in% hiv_mods & disease == "hiv/tb", disease:="hiv"]
  dt[gf_module %in% rssh_mods & disease == "hiv/tb", disease:="rssh"]
  
  if (nrow(dt[disease == 'hiv/tb'])!= 0){
    print("Alert: The following modules were not split. They will be relabeled as HIV.")
    print(unique(dt[disease == "hiv/tb", .(gf_module, gf_intervention)]))
  }
  #Right now, just reclassifying all other modules that don't fit in these categories to be "hiv". 
  dt[disease == "hiv/tb", disease:= 'hiv']
  
  #Check to make sure all modules were caught in the edit above - Should still have Program management; TB/HIV; and Unspecified. 
  stopifnot(nrow(dt[disease == "hiv/tb"])==0)
  return(dt)
}

# ------------------------------------------------------------
# Function to clean up the mods/interventions in the RT data 
# ------------------------------------------------------------

##function that takes three parameters: the dataset you want cleaned, and the two vectors we created above: 
strip_chars <- function(dt){
  
  ## vector dictionary of special characters to regular characters
  unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='C', '?'='E', '?'='E',
                            '?'='E', '?'='E', '?'='I', '?'='I', '?'='I', '?'='I', '?'='N', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='U',
                            '?'='U', '?'='U', '?'='U', '?'='Y', '?'='B', '?'='Ss', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='c',
                            '?'='e', '?'='e', '?'='e', '?'='e', '?'='i', '?'='i', '?'='i', '?'='i', '?'='o', '?'='n', '?'='o', '?'='o', '?'='o', '?'='o',
                            '?'='o', '?'='o', '?'='u', '?'='u', '?'='u', '?'='y', '?'='y', '?'='b', '?'='y', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  
  # vector of characters or phrases to remove
  remove_chars <- c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                    , "[[:punct:]]", "[^[:alnum:]]","\"", ",") 
  
  
  #Save an original copy of module and intervention
  dt$orig_module <- copy(dt$module)
  dt$orig_intervention <- copy(dt$intervention)
  
  ##remove special characters and blank spaces
  dt$module <-tolower(dt$module)
  dt$module <-gsub(paste(remove_chars, collapse="|"), "",dt$module)
  
  dt$intervention  <-tolower(dt$intervention)
  dt$intervention <-gsub(paste(remove_chars, collapse="|"), "",dt$intervention)
  
  dt$module <- chartr(paste(names(unwanted_array), collapse=''),
                      paste(unwanted_array, collapse=''),
                      dt$module)
  dt$intervention <- chartr(paste(names(unwanted_array), collapse=''),
                            paste(unwanted_array, collapse=''),
                            dt$intervention)
  
  return(dt)
}

# ------------------------------------------------------------
# Function to clean up the population column in the RT data 
# ------------------------------------------------------------

##function that takes three parameters: the dataset you want cleaned, and the two vectors we created above: 
strip_chars_pop <- function(dt){
  
  ## vector dictionary of special characters to regular characters
  unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='C', '?'='E', '?'='E',
                            '?'='E', '?'='E', '?'='I', '?'='I', '?'='I', '?'='I', '?'='N', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='U',
                            '?'='U', '?'='U', '?'='U', '?'='Y', '?'='B', '?'='Ss', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='c',
                            '?'='e', '?'='e', '?'='e', '?'='e', '?'='i', '?'='i', '?'='i', '?'='i', '?'='o', '?'='n', '?'='o', '?'='o', '?'='o', '?'='o',
                            '?'='o', '?'='o', '?'='u', '?'='u', '?'='u', '?'='y', '?'='y', '?'='b', '?'='y', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  
  # vector of characters or phrases to remove
  remove_chars <- c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                    , "[[:punct:]]", "[^[:alnum:]]","\"", ",") 
  
  
  #Save an original copy of population and intervention
  dt$orig_population <- copy(dt$population)
  
  ##remove special characters and blank spaces
  dt$population <-tolower(dt$population)
  dt$population <-gsub(paste(remove_chars, collapse="|"), "",dt$population)
  
  dt$population <- chartr(paste(names(unwanted_array), collapse=''),
                      paste(unwanted_array, collapse=''),
                      dt$population)

  return(dt)
}

# --------------------------------------------------------------------------------
#Given a country's file list, only keeps the files that will be kept after GOS data 
# is prioritized in step 4. Right now, drop everything before 2017. 
# --------------------------------------------------------------------------------

prioritize_gos = function(file_list){
  file_list = file_list[!(data_source=="fpm" & file_iteration=="initial")] #Drop out initial budgets, you don't need these. 
  
  file_list[, qtr_number_financial:=as.numeric(qtr_number_financial)]
  file_list[, period_financial:=as.numeric(period_financial)]
  file_list[, days_in_budget:=period_financial*qtr_number_financial]
  file_list[, end_date:=start_date_financial+days_in_budget]
  
  file_list[, end_year:=year(end_date)]
  file_list = file_list[!end_year<=gos_year] #The variable 'gos_year' is set in global variables. 
  
  file_list= file_list[, -c('end_date', 'end_year', 'days_in_budget')]
  
  return(file_list)
}


#-------------------------- 
# Sum, removing NA's (simple function to add into data tables) 
#---------------------------
sum_na_rm <- function(col){
  col = sum(col, na.rm = TRUE)
  return(col)
}


#--------------------------------------------------------------------------------
# Orders a dataset to be at the quarter-level given a start and an end date. 
#--------------------------------------------------------------------------------
expand_to_quarter = function(dt, idVars=NULL, finVars=NULL, startDateVar=NULL, periodVar=NULL){
  totals_check = dt[, .(budget=sum(budget, na.rm = TRUE), expenditure=sum(expenditure, na.rm=TRUE))]
  
  #Add in date variables 
  dt[, quarter:=quarter(start_date)]
  dt[, year:=year(start_date)]
  
  dt[, period:=period]
  dt[, qtr_number:=qtr_number]
  dt[, qtr_split:=round((period*qtr_number)/90)]
  dt[, split:=round((period*qtr_number)/90)] #Create this variable twice so you can divide budget/expenditure after expansion
  
  #Expand data by the number of days, and generate a variable to iterate over
  dt <- expandRows(dt, "qtr_split")
  byVars = names(dt)
  dt[, seq:=sequence(.N), by=byVars]
  dt[, seq:=seq-1] #Decrement by 1 because sequence indexes at 1. 
  
  #While seq is not 0, go through the loop below.
  #If seq is greater than or equal to 4, add 1 to year and divide everything by 4. Continue this loop while max(seq) > 4.
  # If month + seq + 1 equals 12, than
  dt[, new_qtr:=quarter+seq]
  max_quarter = max(dt$new_qtr)
  while (max_quarter>4){
    dt[new_qtr>4, year:=year+1]
    dt[new_qtr>4, new_qtr:=new_qtr-4]
    max_quarter = max(dt$new_qtr)
  }
  
  #Split up budget and expenditure.
  dt[, budget:=budget/split]
  dt[, expenditure:=expenditure/split]
  if (sheet_name!="PR Expenditure_7A"){
    dt[, lfa_exp_adjustment:=lfa_exp_adjustment/split]
  }
  
  #Make sure you haven't changed any budget/expenditure numbers, and clean up
  totals_check2 = dt[, .(budget=sum(budget, na.rm = TRUE), expenditure=sum(expenditure, na.rm=TRUE))]
  for (i in 1:nrow(totals_check)){
    stopifnot(totals_check$budget[i]==totals_check2$budget[i] | totals_check$expenditure[i]==totals_check2$expenditure[i])
  }
  dt = dt[, -c('period', 'qtr_number', 'split', 'seq', 'quarter')]
  setnames(dt, 'new_qtr', 'quarter')
  
  #Generate new start date variable. 
  dt[quarter==1, month:="01"]
  dt[quarter==2, month:="04"]
  dt[quarter==3, month:="07"]
  dt[quarter==4, month:="10"]
  
  dt[, start_date:=paste0(month, "-01-", year)]
  dt[, start_date:=as.Date(start_date, "%m-%d-%Y")]
  dt[, month:=NULL]
}
#---------------------------------------------------------------------------------
# Converts from euros to USD for a given year. 
# Based off of the FGH team's currency conversion function, but with euros added 
# (and much simpler). 
#---------------------------------------------------------------------------------

convert_currency = function(dt, yearVar=NULL, convertFrom, convertTo, finVars = c('budget', 'expenditure', 'disbursement')){
  if (is.null(yearVar)) stop("year argument is NULL")
  if (class(yearVar)!='character') stop("year must be a character")
  if (!yearVar %in% names(dt)) stop("year argument is not a variable in data table")
  if (!convertFrom%in%c('EUR', 'USD')) stop("convertFrom must be either EUR or USD")
  if (!convertTo%in%c('EUR', 'USD')) stop("convertTo must be either EUR or USD")
  if (!any(finVars%in%names(dt))) stop("Financial variables to convert are not in data. Modify the 'finVars' option.")
  
  #Pulled out from FGH team's currency conversion function - Emily Linebarger, 5/7/19 
  oecd_xrate = fread("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/oecd_currency_conversion/xrates_oecd_121118.csv") #This data includes 1995-2017. 
  oecd_xrate = oecd_xrate[LOCATION == "EA19", .(TIME, Value)]
  setnames(oecd_xrate, c('TIME','Value'), c(yearVar, 'eur_usd'))
  oecd_xrate[, (yearVar):= as.integer(get(yearVar))]
  
  #Re-extracted from the OECD's website to grab 2018 and 2019 as well, last downloaded on 2/13/2020 by Emily Linebarger 
  oecd_xrate2 = fread("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/oecd_currency_conversion/oecd_conversion_rates_2019.csv")
  oecd_xrate2 = oecd_xrate2[LOCATION == "EA19", .(TIME, Value)]
  setnames(oecd_xrate2, c('TIME','Value'), c(yearVar, 'eur_usd'))
  oecd_xrate2[, (yearVar):= as.integer(get(yearVar))]
  
  # Only keep 2018 and 2019 from the second OECD x-rate table. EL 3/11/2019
  max_year = max(oecd_xrate$year)
  oecd_xrate2 = oecd_xrate2[year>max_year]
  
  #Bind these two time series together. 
  oecd_xrate = rbind(oecd_xrate, oecd_xrate2)
  
  #Create one more data table that has several years into the future, with the latest exchange rate available. 
  #This is a temporary patch until more data (or a better method) becomes available! 
  latest_rate = oecd_xrate[get(yearVar)==max(get(yearVar)), .(eur_usd)]
  xrate_extension = data.table(year = seq(2020, 2024, by=1), eur_usd = rep(latest_rate, 5)) #Extend 5 years into the future - this should catch all of our current data. 
  oecd_xrate = rbind(oecd_xrate, xrate_extension)
  
  #Round OECD exchange rates to 6 significant figures 
  oecd_xrate[, eur_usd:=as.numeric(eur_usd)]
  oecd_xrate[, eur_usd:=round(eur_usd, 6)]
  
  #Merge data 
  dt_converted <- merge(dt, oecd_xrate, 
                            by = yearVar, 
                            all.x = T)
  #Validate data 
  stopifnot(nrow(dt_converted[is.na(yearVar)])==0)
  stopifnot(nrow(dt_converted[is.na(eur_usd)])==0)
  
  #Convert financial variables. 
  for (var in finVars){
    dt_converted[is.na(get(var)), (var):=0]
    if (convertTo=="USD"){
      dt_converted[, (var):=get(var)/eur_usd]
      dt_converted[, file_currency:="USD"]
    } else if (convertTo=="EUR"){
      dt_converted[, (var):=get(var)*eur_usd]
      dt_converted[, file_currency:="EUR"]
    }
  } 
  
  dt_converted$eur_usd <- NULL #Drop the conversion rate
  
  return(dt_converted)
}

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


#--------------------------------------------------------------------
#Recursively find the terminal directories within a given filepath
#Useful for mapping unclassified files
#--------------------------------------------------------------------
get_dirs = function(path, list){
  setwd(path) #Navigate to the given directory. 
  dirs = list.dirs(path, recursive=FALSE) #List the directories in this folder, non-recursively. 
  #If there is more than one directory, keep recursing.
  if (length(dirs)>1){ 
    #You want to grab the first directory name, add to a path, and keep recursing. 
    for (next_dir in dirs){
      list = get_dirs(next_dir, list)  
    }
  } else { #If there are no more directories, then add your current path to the final list and return it.  
    return(c(list, path))
  }
  #If you've made it to the end, and there are no more subfolders, then return the list of terminal file paths in this folder. 
  return(list)
}

#Once you've found a terminal directory, grab the file names that live in it, and return as a list
get_files = function(path){
  list = character()
  setwd(path)
  files = list.files()
  for (file in files){
    new_path = paste0(path, "/", file) 
    list = c(list, new_path)
  }
  return(list)
}

######
add_fr_es_to_dt = function(dt, module_merge_col= 'gf_module', intervention_merge_col= 'gf_intervention'){
  map_fr_es = readRDS("\\\\ihme.washington.edu/ihme/snfs/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/2018_2020_MF.rds")
  map_fr_es = map_fr_es[, .(disease, gf_module, gf_intervention, gf_module_fr, gf_intervention_fr, gf_module_esp, gf_intervention_esp)]
  
  dt = merge(dt, map_fr_es, all.x = TRUE, by.x = c(module_merge_col, intervention_merge_col, 'disease'), by.y = c('gf_module', 'gf_intervention', 'disease'))
  
  return(dt)
}
