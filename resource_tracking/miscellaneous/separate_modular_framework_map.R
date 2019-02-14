#Temporary task - find all of the rows in the map that are mapping to files after 2017. Save these in their own file, and remove all RSSH (?)

#To do- split files into english, french, and spanish training data sets. Include activity descriptions. 

#Eventually just need to have post-2017 maps for French, English, and Spanish. 

rm(list=ls())
library(data.table)
source("C:/Users/elineb/Documents/gf/resource_tracking/prep/shared_prep_functions.R", encoding = "UTF-8")

module_map <- fread("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/gf_mapping.csv")

#Read in file lists, and keep only the files that are from 2017 or later  ``
cod_filelist <- fread("J:/Project/Evaluation/GF/resource_tracking/cod/grants/cod_budget_filelist.csv")
pre_mf_cod = cod_filelist[grant_period == "2018-2020"]
post_mf_cod = cod_filelist[grant_period != "2018-2020"]

gtm_filelist <- fread("J:/Project/Evaluation/GF/resource_tracking/gtm/grants/gtm_budget_filelist.csv")
#Is this all? How about 2016-2019? 
pre_mf_gtm <- gtm_filelist[grant_period != "2019-2022" & grant_period != "2018-2020" & grant_period != "2019-2021" & grant_period != "2018"]
post_mf_gtm <- gtm_filelist[grant_period == "2019-2022" | grant_period == "2018-2020" | grant_period == "2019-2021" | grant_period == "2018"]

uga_filelist <- fread("J:/Project/Evaluation/GF/resource_tracking/uga/grants/uga_budget_filelist.csv")
pre_mf_uga = uga_filelist[grant_period != "2018-2020"]
post_mf_uga = uga_filelist[grant_period == "2018-2020"]

pre_mf_files <- rbind(pre_mf_cod, pre_mf_gtm, pre_mf_uga, fill = TRUE) #Output this for documentation. 
post_mf_files <- rbind(post_mf_cod, post_mf_gtm, post_mf_uga, fill = TRUE)

#Read in raw prepped data and only keep the files that apply
cod_data <- readRDS("J:/Project/Evaluation/GF/resource_tracking/cod/prepped/raw_bound_gf_files.RDS")
pre_mf_cod_data = cod_data[fileName%in%pre_mf_cod$file_name]
post_mf_cod_data = cod_data[fileName%in%post_mf_cod$file_name]

gtm_data <- readRDS("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/raw_bound_gf_files.RDS")
pre_mf_gtm_data = gtm_data[fileName%in%pre_mf_gtm$file_name]
post_mf_gtm_data = gtm_data[fileName%in%post_mf_gtm$file_name]

uga_data <- readRDS("J:/Project/Evaluation/GF/resource_tracking/uga/prepped/raw_bound_gf_files.RDS")
pre_mf_uga_data = uga_data[fileName%in%pre_mf_uga$file_name]
post_mf_uga_data = uga_data[fileName%in%post_mf_uga$file_name]

#pre_mf_data <- rbind(pre_mf_cod_data, pre_mf_gtm_data, pre_mf_uga_data, fill = TRUE) 
mod_framework_data <- rbind(post_mf_cod_data, post_mf_gtm_data, post_mf_uga_data, fill = TRUE) 
mod_framework_data <- strip_chars(mod_framework_data)

#Correct common acronyms in the resource database and the module map. 
mod_framework_data[, module:=replace_acronyms(module)]
mod_framework_data[, intervention:=replace_acronyms(intervention)]

module_map[, module:=replace_acronyms(module)]
module_map[, intervention:=replace_acronyms(intervention)]
mod_framework_data = mod_framework_data[, .(module, intervention, disease)] 
mod_framework_data = unique(mod_framework_data)

#You should keep all of the rows in the module map that match these interventions and modules. Make sure all are included. 
post_2017_map <- merge(mod_framework_data, module_map, by = c('module', 'intervention', 'disease'), all.x = TRUE) 

#Drop rows that have NA for module and intervention 
post_2017_map = post_2017_map[!is.na(module) & !is.na(intervention)]

#Validate this data 
check_nas <- post_2017_map[is.na(code)]
#These observations need to be filled in by hand! 
  
  
#Remove all the rows from the 2017 map that map to RSSH; it's better to do this by grepping. 
post_2017_map = post_2017_map[substring(code, 1, 1)!='R']

saveRDS(post_2017_map, "J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/post_2017_map.rds")
