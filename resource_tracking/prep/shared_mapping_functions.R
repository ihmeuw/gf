# ----------------------------------------------
# Irena Chen
#
# 3/27/2018

### This code contains functions that map RT data to the GF Modular Framework 

# ----------------------------------------------
##### Fixes diacritical marks
# ----------------------------------------------

fix_diacritics = function(x){
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
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


# ----------------------------------------------
##### Function to clean up the mods/interventions in the RT data #####
# ----------------------------------------------

##function that takes three parameters: the dataset you want cleaned, and the two vectors we created above: 
strip_chars <- function(gfData, unwanted_array, remove_chars){
  
  ## vector dictionary of special characters to regular characters
  unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='A', '?'='C', '?'='E', '?'='E',
                            '?'='E', '?'='E', '?'='I', '?'='I', '?'='I', '?'='I', '?'='N', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='O', '?'='U',
                            '?'='U', '?'='U', '?'='U', '?'='Y', '?'='B', '?'='Ss', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='a', '?'='c',
                            '?'='e', '?'='e', '?'='e', '?'='e', '?'='i', '?'='i', '?'='i', '?'='i', '?'='o', '?'='n', '?'='o', '?'='o', '?'='o', '?'='o',
                            '?'='o', '?'='o', '?'='u', '?'='u', '?'='u', '?'='y', '?'='y', '?'='b', '?'='y', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  
  # vector of characters or phrases to remove
  remove_chars <- c(" ","hss", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                    , "[[:punct:]]", "[^[:alnum:]]","\"", ",") 
  
  
  ##remove special characters and blank spaces
  gfData$orig_module <- copy(gfData$module)
  gfData$orig_intervention <- copy(gfData$intervention)
  
  
  gfData$module <-tolower(gfData$module)
  gfData$module <-gsub(paste(remove_chars, collapse="|"), "",gfData$module)
  
  gfData$intervention  <-tolower(gfData$intervention)
  gfData$intervention <-gsub(paste(remove_chars, collapse="|"), "",gfData$intervention)
  
  
  gfData$module <- chartr(paste(names(unwanted_array), collapse=''),
                                paste(unwanted_array, collapse=''),
                                gfData$module)
  gfData$intervention <- chartr(paste(names(unwanted_array), collapse=''),
         paste(unwanted_array, collapse=''),
         gfData$intervention)
  
  gfData$intervention[is.na(gfData$intervention)] <- "all" #Why are we doing this here?? Or at all??? EKL 12/3/18 

return(gfData)
}

# ----------------------------------------------
##### Function to load the mapping data #####
# parameters are the mapping_file name 
# and boolean that indicates if you want to repeat each RSSH module/intervention for each disease
# set the boolean to false when mapping data to GF modular framework
# but set to true when you want to graph RSSH modules in each disease 
# ----------------------------------------------
load_mapping_list <- function(mapping_file, include_rssh_by_disease){ 
  tab_names <- c("HIV Interventions", "TB Interventions", "Malaria Interventions", "RSSH Interventions")
  
  for(i in 1:length(tab_names)){
    tmpData <- data.table(read_excel(mapping_file, sheet = tab_names[i], trim_ws = TRUE))
    if(grepl("HIV", tab_names[i])){
      tmpData$disease <- "hiv"
    } else if (grepl("Mal", tab_names[i])){
      tmpData$disease <- "malaria"
    } else if (grepl("TB", tab_names[i])){
      tmpData$disease <- "tb"
    } else {
      tmpData$disease <- "hss"
    }
    if(i==1){
      indicator_mapping <- tmpData
    } else {
      indicator_mapping <- rbind(indicator_mapping, tmpData)
    }
  }
  
  ##we don't need this for mapping, but we need this for plotting interventions by modules per disease
 if(include_rssh_by_disease){
  rsshData <- data.table(read_excel(mapping_file, sheet = "RSSH Interventions", trim_ws = TRUE))
  diseases <- c("hiv", "malaria", "tb")
  for(i in 1:length(diseases)){
    tmp <- copy(rsshData)
    tmp$disease <- diseases[i]
    indicator_mapping <- rbind(indicator_mapping, tmp)
  }
 }
  ##change the dataset names
  setnames(indicator_mapping, c("code","module", "intervention", "abbrev_module", "abbrev_intervention", "disease"))
  indicator_mapping <- unique(indicator_mapping)
  ##this will make it easier to map everything by removing spaces, punctuation, etc. 
  return(indicator_mapping)
}
  
# ----------------------------------------------
##### Function that cleans the special chars/white space from the mapping tab #####
# ----------------------------------------------
total_mapping_list <- function(file_name, indicator_mapping, unwanted_array, remove_chars){
  
  old_modules <- data.table(read_excel(file_name, sheet = "module_mapping", trim_ws = TRUE))
  
  ##remove duplicates: 
  old_modules<- unique(old_modules)
  
  ##rbind with the GF modular framework: 
  mapping_for_gf <- rbind(old_modules, indicator_mapping)
  
  ##this will make it easier to map everything by removing spaces, punctuation, etc. 
  mapping_for_gf$module <- chartr(paste(names(unwanted_array), collapse=''),
                          paste(unwanted_array, collapse=''),
                          mapping_for_gf$module)
  mapping_for_gf$intervention <- chartr(paste(names(unwanted_array), collapse=''),
                                paste(unwanted_array, collapse=''),
                                mapping_for_gf$intervention)
  
  mapping_for_gf$intervention  <-tolower(mapping_for_gf$intervention)
  mapping_for_gf$module <-tolower(mapping_for_gf$module)
  
  mapping_for_gf$module <-gsub(paste(remove_chars, collapse="|"), "",mapping_for_gf$module)
  mapping_for_gf$intervention <-gsub(paste(remove_chars, collapse="|"), "",mapping_for_gf$intervention)
  
  ##remove any duplicates: 
  mapping_for_gf <- unique(mapping_for_gf)
  return(mapping_for_gf)
}

#Splits modules and interventions into separate text fields based on a substring "keyword". 
#I.e. preventionotherpreventiondistributionofcondoms with the keyword "prevention" passed will 
# become module "prevention" and intervention "otherpreventiondistributionofcondoms". 
#This function will only remove modules that occur at the beginning of the word. 
split_mods_interventions <- function(dt, mod, keyword){
  new_intervention = substr(mod, start = nchar(keyword) + 1, stop = nchar(mod))
  dt = dt[module == mod, intervention:=new_intervention]
  dt = dt[module == mod, module:=keyword]
  paste0("Function will return ", keyword, " as a module and ", new_intervention, " as an intervention.")
  return(dt)
}

#----------------------------------------------
#Functions to verify budgets, used in step 5. 
#----------------------------------------------
#Sums budget by key variables
check_budgets_pudrs = function(dt){
  keyVars = c("start_date", "fileName", "grant_number", "data_source")
  #Deciding not to split by disease here because we just want the total for the whole quarter. 
  
  budgets = dt[ , 
               lapply(.SD, .(sum, na.rm = TRUE)), 
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


