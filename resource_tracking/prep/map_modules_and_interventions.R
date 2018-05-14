# ----------------------------------------------
# Irena Chen
#
# 3/27/2018

### This code contains functions that map RT data to the GF Modular Framework 



# ----------------------------------------------
##### Function to clean up the mods/interventions in the RT data #####
# ----------------------------------------------
## vector dictionary of special characters to regular characters
unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )


# vector of characters or phrases to remove
remove_chars <- c(" ","rssh","hss", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                  , "[[:punct:]]", "[^[:alnum:]]","\"", ",")


##function that takes three parameters: the dataset you want cleaned, and the two vectors we created above: 
strip_chars <- function(gfData, unwanted_array, remove_chars){
  ##remove special characters and blank spaces
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
  
  gfData$intervention[is.na(gfData$intervention)] <- "all"

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
  setnames(indicator_mapping, c("code","module", "intervention", "disease"))
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



