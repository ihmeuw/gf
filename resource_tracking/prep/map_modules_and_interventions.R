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
                                 fileEncoding = "latin1", trimws("both")))

##we only care about GF data for now - the other donations and GHE are from sicoin
gfData <- totalData[source=="gf"]
##ignore sicoin for now; we will work on mapping later: 
gfData <- gfData[data_source!="sicoin"]

##clean this up: 
remove_chars <- c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                      , "[[:punct:]]", "[^[:alnum:]]","\"", ",")
gfData$module <-gsub(paste(remove_chars, collapse="|"), "",gfData$module)
gfData$module <-tolower(gfData$module)
gfData$intervention <-gsub(paste(remove_chars, collapse="|"), "",gfData$intervention)
gfData$intervention  <-tolower(gfData$intervention)


gfData$intervention[is.na(gfData$intervention)] <- "all"
# ----------------------------------------------
##load the mapping data: 

tab_names <- c("HIV Interventions", "TB Interventions", "Malaria Interventions", "RSSH Interventions")

for(i in 1:length(tab_names)){
  tmpData <- data.table(read_excel("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx"
                                   , sheet = tab_names[i], trim_ws = TRUE))
  if(i==1){
    indicator_mapping <- tmpData
  } else {
    indicator_mapping <- rbind(indicator_mapping, tmpData)
  }
}

# ----------------------------------------------

setnames(indicator_mapping, c("code","module", "intervention"))

final_mapping <- copy(indicator_mapping)

setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))

##this will make it easier to map everything by removing spaces, punctuation, etc. 
indicator_mapping$module <-gsub(paste(remove_chars, collapse="|"), "",indicator_mapping$module)
indicator_mapping$module <-tolower(indicator_mapping$module)
indicator_mapping$intervention <-gsub(paste(remove_chars, collapse="|"), "",indicator_mapping$intervention)
indicator_mapping$intervention  <-tolower(indicator_mapping$intervention)

indicator_mapping$coefficient <- 1

old_modules <- data.table(read_excel("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx"
                                     , sheet = "old_module_categories", trim_ws = TRUE))
old_modules <- old_modules[,c(1, 3:5)]

setnames(old_modules, c("module", "intervention", "code", "coefficient"))


##this will make it easier to map everything by removing spaces, punctuation, etc. 
old_modules$module <-gsub(paste(remove_chars, collapse="|"), "",old_modules$module)
old_modules$module <-tolower(old_modules$module)
old_modules$intervention <-gsub(paste(remove_chars, collapse="|"), "",old_modules$intervention)
old_modules$intervention  <-tolower(old_modules$intervention)

##remove duplicates: 

mapping_for_gf <- rbind(old_modules, indicator_mapping)
mapping_for_gf <- unique(mapping_for_gf)

##split the GF Data into three datasets: one where modules and interventions both in the framework

unmapped_mods <- gfData[!module%in%mapping_for_gf$module&!(intervention%in%mapping_for_gf$intervention)]
##if this works correctly, we should be able to drop the unmapped_mods from our dataset since they are junk categories:

gfData<- gfData[!module%in%unmapped_mods$module]

##if categories get dropped or miscalculated during the mapping, we can figure out which ones they were: 
data_check1 <- as.data.frame(gfData[, sum(budget, na.rm = TRUE),by = c("country", "disease")])
data_check2 <- as.data.frame(gfData[, sum(budget, na.rm = TRUE),by = c("country","grant_number", "disease")])


dataset_names <- list()
disease_list <- unique(gfData$disease)

for(i in 1:length(disease_list)){
  dataset_names[[i]] <- gfData[disease==disease_list[i]]
}

hivData <- gfData[disease=="hiv"]
malData <- gfData[disease=="malaria"]
tbData <- gfData[disease=="tb"]
hssData <- gfData[disease=="hss"]

hivMapping <- mapping_for_gf[grepl("H", mapping_for_gf$code),]
malMapping <- mapping_for_gf[grepl("M", mapping_for_gf$code),]
tbMapping <- mapping_for_gf[grepl("T", mapping_for_gf$code),]
hssMapping <- mapping_for_gf[grepl("R", mapping_for_gf$code),]

hiv_only <- hivData[(module%in%hivMapping$module)&(intervention%in%hivMapping$intervention)]
hiv_rssh <- hivData[(!module%in%hivMapping$module)|(!intervention%in%hivMapping$intervention)]

mapped_rssh <- merge(hiv_rssh, hssMapping , by=c("module", "intervention"), allow.cartesian = TRUE)
rssh_data_mapped <- merge(mapped_rssh, final_mapping, by="code")
rssh_data_mapped$budget <- rssh_data_mapped$budget*rssh_data_mapped$coefficient

mapped_hiv <- merge(hiv_only, hivMapping , by=c("module", "intervention"), all.x=TRUE,allow.cartesian = TRUE)

dropped_hiv <- mapped_hiv[is.na(mapped_hiv$code)]

unique(dropped_hiv[list(module, intervention), nomatch=0])



hiv_data_mapped <- merge(mapped_hiv, final_mapping, by="code")
hiv_data_mapped$budget <- hiv_data_mapped$budget*hiv_data_mapped$coefficient

##sum to make sure that budget numbers aren't dropped:
hiv_data_check1 <- hiv_only[, sum(budget, na.rm = TRUE),by = c("country", "module","intervention","disease")]
hiv_data_check2 <- hiv_data_mapped[, sum(budget, na.rm = TRUE),by = c("country","module", "intervention","disease")]
hiv_data_check1[!module%in%hiv_data_check2$module]



mapped_mal <- merge(malData, malMapping , by=c("module", "intervention"), allow.cartesian = TRUE)
mal_data_mapped <- merge(mapped_mal, final_mapping, by="code")

##check DRC - sex workers and clients


mapped_gf <- merge(gfData, mapping_for_gf, by=c("module", "intervention"), allow.cartesian = TRUE)
gf_data_mapped <- merge(mapped_gf, final_mapping, by="code")
gf_data_mapped$budget <- gf_data_mapped$budget*gf_data_mapped$coefficient
gf_data_mapped$expenditure <- gf_data_mapped$expenditure*gf_data_mapped$coefficient
gf_data_mapped$disbursed <- gf_data_mapped$disbursed*gf_data_mapped$coefficient


data_check3 <- as.data.frame(gf_data_mapped[, sum(budget, na.rm = TRUE),by = c("country", "disease")])
data_check3 <- as.data.frame(gf_data_mapped[, sum(budget, na.rm = TRUE),by = c("grant_number", "disease")])



write.csv()






