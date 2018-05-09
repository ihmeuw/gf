# ----------------------------------------------
# Irena Chen
#
# 3/27/2018

### This code is to map RT data to the GF framework modules and interventions 

# ----------------------------------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(readxl)

# ----------------------------------------------
##### Function to clean up the mods/interventions in the RT data #####

##create vector of unwanted characters:
unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

remove_chars <- c(" ","rssh","hss", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                  , "[[:punct:]]", "[^[:alnum:]]","\"", ",")


##remove accents
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
##load the mapping data: 
load_mapping_list <- function(mapping_file){
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
  ##change the dataset names
  setnames(indicator_mapping, c("code","module", "intervention", "disease"))
  ##this will make it easier to map everything by removing spaces, punctuation, etc. 
  return(indicator_mapping)
}
  
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



# ----------------------------------------------

##sum to make sure that budget numbers aren't dropped:
# data_check1 <- gfData[, sum(budget, na.rm = TRUE),by = c("country", "module","intervention","disease")]
# data_check2 <- gf_data_mapped[, sum(budget, na.rm = TRUE),by = c("country","module", "intervention","disease")]
# data_check1[!module%in%data_check2$module]
# data_check1$ind <- "pre"
# data_check2$ind <- "post"
# data_check <- rbind(data_check1, data_check2)
# write.csv(data_check, "data_check.csv", row.names = FALSE)

# ----------------------------------------------

write.csv(mappedData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/rt_data_mapped.csv"
          , fileEncoding="latin1", row.names=FALSE)





