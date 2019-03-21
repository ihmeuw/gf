# ----------------------------------------------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chen
# PURPOSE: Shared functions for the resource tracking database. These are all string-
# based functions or functions that load files; for functions used in calculation steps 
# review 'shared_budget_functions'. 
# DATE: Last updated March 2019. 
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

# --------------------------------------------------------------------------------
#Given a country's file list, only keeps the files that will be kept after GOS data 
# is prioritized in step 4. Right now, drop everything before 2017. 
# --------------------------------------------------------------------------------

prioritize_gos = function(file_list){
  file_list = file_list[file_iteration == 'final'] 
  
  file_list[, qtr_number:=as.numeric(qtr_number)]
  file_list[, period:=as.numeric(period)]
  file_list[, days_in_budget:=period*qtr_number]
  file_list[, end_date:=start_date+days_in_budget]
  
  file_list[, end_year:=year(end_date)]
  file_list = file_list[!end_year<=gos_year]
  
  file_list= file_list[, -c('end_date', 'end_year', 'days_in_budget')]
  
  return(file_list)
}