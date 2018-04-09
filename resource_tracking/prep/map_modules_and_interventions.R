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
##load the data: 

totalData <- data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv',
                                 fileEncoding = "latin1"))

##we only care about GF data for now - the other donations and GHE are from sicoin
gfData <- totalData[source=="gf"]
##ignore sicoin for now; we will work on mapping later: 
gfData <- gfData[data_source!="sicoin"]

##clean this up: 
remove_chars <- c(" ","rssh","hss", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                      , "[[:punct:]]", "[^[:alnum:]]","\"", ",")
gfData$module <-tolower(gfData$module)
gfData$module <-gsub(paste(remove_chars, collapse="|"), "",gfData$module)
gfData$intervention  <-tolower(gfData$intervention)
gfData$intervention <-gsub(paste(remove_chars, collapse="|"), "",gfData$intervention)


unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

gfData$module <- chartr(paste(names(unwanted_array), collapse=''),
                              paste(unwanted_array, collapse=''),
                              gfData$module)
gfData$intervention <- chartr(paste(names(unwanted_array), collapse=''),
       paste(unwanted_array, collapse=''),
       gfData$intervention)

gfData$intervention[is.na(gfData$intervention)] <- "all"
gfData$concat <- paste0(gfData$module, gfData$intervention)

# ----------------------------------------------
##load the mapping data: 

tab_names <- c("HIV Interventions", "TB Interventions", "Malaria Interventions", "RSSH Interventions")

for(i in 1:length(tab_names)){
  tmpData <- data.table(read_excel("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx"
                                   , sheet = tab_names[i], trim_ws = TRUE))
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

# ----------------------------------------------
##change the dataset names
setnames(indicator_mapping, c("code","module", "intervention", "disease"))

## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
final_mapping <- copy(indicator_mapping)
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))

##this will make it easier to map everything by removing spaces, punctuation, etc. 
indicator_mapping$coefficient <- 1

old_modules <- data.table(read_excel("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx"
                                     , sheet = "module_mapping", trim_ws = TRUE))

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

# USE THIS TO CHECK FOR ANY MODULE/INTERVENTION COMBOS IN THE DATA THAT AREN'T IN THE MAPPING
# mapping_for_gf$concat <- paste0(mapping_for_gf$module, mapping_for_gf$intervention)
# gfData$concat <- paste0(gfData$module, gfData$intervention)
# unmapped_mods <- gfData[!concat%in%mapping_for_gf$concat]

##if this works correctly, we should be able to drop the unmapped_mods from our dataset since they are junk categories:
# gfData<- gfData[!module%in%unmapped_mods$module]

##if categories get dropped or miscalculated during the mapping, we can figure out which ones they were: 
data_check1 <- as.data.frame(gfData[, sum(budget, na.rm = TRUE),by = c("country", "disease")])
data_check2 <- as.data.frame(gfData[, sum(budget, na.rm = TRUE),by = c("country","grant_number", "disease")])

mapped_gf <- merge(gfData, mapping_for_gf, by=c("module", "intervention", "disease"), all.x=TRUE,allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
# dropped_gf <- mapped_gf[is.na(mapped_gf$code)]

gf_data_mapped <- merge(mapped_gf, final_mapping, by="code")
gf_data_mapped$budget <- gf_data_mapped$budget*gf_data_mapped$coefficient
gf_data_mapped$expenditure <- gf_data_mapped$expenditure*gf_data_mapped$coefficient
gf_data_mapped$disbursement <- gf_data_mapped$disbursement*gf_data_mapped$coefficient

##sum to make sure that budget numbers aren't dropped:
# hiv_data_check1 <- hivData[, sum(budget, na.rm = TRUE),by = c("country", "module","intervention","disease")]
# hiv_data_check2 <- hivMapped[, sum(budget, na.rm = TRUE),by = c("country","module", "intervention","disease")]
# hiv_data_check1[!module%in%hiv_data_check2$module]


write.csv(mappedData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/rt_data_mapped.csv"
          , fileEncoding="latin1", row.names=FALSE)





