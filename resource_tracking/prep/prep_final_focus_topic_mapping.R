# Creates a codebook to identify focus topics across all budgets and grants!
# by Francisco Rios
# August 5, 2020

# The current working directory should be the root of this repository
# -----------------------------------------------
rm(list=ls())
# -----------------------------------------------
# SET UP R
# -----------------------------------------------
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}

# source files with other functions and common resource tracking filepaths
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")
verbose = FALSE

# read in the modules, interventions and activities of all the PCE grants 
all_data <- fread(paste0(dir, 'modular_framework_mapping/PCE2020_FocusTopicAreas_activityLevel_wFRs.csv'))
cols_trim <- c("loc_name","gf_module","gf_intervention","activity_description")
all_data[,(cols_trim) :=lapply(.SD,trimws),.SDcols = cols_trim]

# clean up column names
# remove the V1 and set columns to NA
if ('V1'%in%names(all_data)){
  all_data[, c('V1'):=NULL]
}

all_data_subset <- all_data[,.(loc_name, disease, gf_module, gf_intervention, activity_description)]

#-------------------------------
##### PRIMARY ###############
#-------------------------------

# merge onto it the old modules and interventions already identified in this file:
# identifyTopicAreas_PCE2020_forSubsetting
primary_id_budgets <- fread(paste0(dir,"modular_framework_mapping/archive/identifyTopicAreas_PCE2020_forSubsetting.csv"))
primary_id_fr <- fread(paste0(dir, "modular_framework_mapping/identifyTopicAreas_PCE2020_forSubsettingFRs.csv"))

primary_id <- rbind(primary_id_budgets, primary_id_fr)

primary_focus_topics <- merge(all_data_subset, primary_id, by=c('loc_name', 'disease', 'gf_module', 'gf_intervention'), all.x=TRUE)

# Country-specific edits to since some of the module names changed in the FR and approved budgets causing a few errors in the mapping--need to implement this as a fix throuhgout the code

# primary_focus_topics <- primary_focus_topics[gf_module=="Health management information systems and monitoring and evaluation" & loc_name=="Senegal", isTopicArea:=TRUE]
# primary_focus_topics <- primary_focus_topics[gf_module=="Health management information systems and monitoring and evaluation" & loc_name=="Senegal", topicAreaDesc:="DHIS2"]
# 
# primary_focus_topics <- primary_focus_topics[gf_intervention=="Analysis, evaluations, reviews and transparency" & loc_name=="DRC", isTopicArea:=TRUE]
# primary_focus_topics <- primary_focus_topics[gf_intervention=="Analysis, evaluations, reviews and transparency" & loc_name=="DRC", topicAreaDesc:="DHIS2 and Data Quality"]
# primary_focus_topics <- primary_focus_topics[gf_intervention=="Program and data quality" & loc_name=="DRC", isTopicArea:=TRUE]
# primary_focus_topics <- primary_focus_topics[gf_intervention=="Program and data quality" & loc_name=="DRC", topicAreaDesc:="DHIS2 and Data Quality"]
# primary_focus_topics <- primary_focus_topics[gf_intervention=="Routine reporting" & loc_name=="DRC", isTopicArea:=TRUE]
# primary_focus_topics <- primary_focus_topics[gf_intervention=="Routine reporting" & loc_name=="DRC", topicAreaDesc:="DHIS2 and Data Quality"]
# 
# primary_focus_topics <- primary_focus_topics[gf_intervention=="Analysis, evaluations, reviews and transparency" & loc_name=="Guatemala", isTopicArea:=TRUE]
# primary_focus_topics <- primary_focus_topics[gf_intervention=="Analysis, evaluations, reviews and transparency" & loc_name=="Guatemala", topicAreaDesc:="Health Information System"]
# primary_focus_topics <- primary_focus_topics[gf_intervention=="Surveys" & loc_name=="Guatemala", isTopicArea:=TRUE]
# primary_focus_topics <- primary_focus_topics[gf_intervention=="Surveys" & loc_name=="Guatemala", topicAreaDesc:="Health Information System"]

#-------------------------------
##### SECONDARY ###############
#-------------------------------
# merge onto this master file new activities that are identified by each of the four PCE countries

###############----------------------
################ GUATEMALA
###############----------------------

# guatemala can be done with a merging onto the FR comments they sent

# bind together guatemala fr 2017, fr 2020, and other budgets together
gtm_fr_17 <- fread(paste0(dir, "modular_framework_mapping/keyword_search/GTM/Guatemala_fr17_additional_ft_activities_073120_final.csv"))
gtm_fr_17 <- gtm_fr_17[,.(loc_name, gf_module, gf_intervention, activity_description, final_decision, topicAreaDesc)]

gtm_fr_18 <- fread(paste0(dir, "modular_framework_mapping/keyword_search/GTM/Guatemala_fr20_additional_ft_activities_prelimdecisions_final.csv"))
gtm_fr_18 <- gtm_fr_18[,.(loc_name, gf_module, gf_intervention, activity_description, final_decision, topicAreaDesc)]

gtm_all <- fread(paste0(dir, "modular_framework_mapping/keyword_search/GTM/gtm_focus_topic_activities_10jul2020.csv"))
gtm_all <- gtm_all[,.(loc_name, gf_module, gf_intervention, activity_description, final_decision, topicAreaDesc)]

gtm_secondary <- rbind(gtm_fr_17, gtm_fr_18, gtm_all)

setnames(gtm_secondary, old=c('final_decision', 'topicAreaDesc'), new=c('isTopicAreaActivity', 'topicAreaActivityDesc'))
gtm_ft_selection <- merge(primary_focus_topics[loc_name=="Guatemala"], gtm_secondary, by=c('loc_name', 'gf_module', 'gf_intervention', 'activity_description'), all.x = TRUE)

###############----------------------
################ SENEGAL
###############----------------------

## bind all countries data together
ft_selection_codebook <- rbind(gtm_ft_selection, primary_focus_topics[loc_name%in%c('Uganda', 'Senegal', 'DRC')], fill=TRUE)

# Clean up the file columns
ft_selection_codebook[is.na(isTopicArea), isTopicArea:=FALSE]
ft_selection_codebook[isTopicArea==FALSE, topicAreaDesc:=NA]
ft_selection_codebook[is.na(isTopicAreaActivity), isTopicAreaActivity:=FALSE]
ft_selection_codebook[isTopicAreaActivity==FALSE, topicAreaActivityDesc:=NA]

## save the final file that will be read in to select focus topics in all of Tableau data
write.csv(ft_selection_codebook, paste0(dir, "modular_framework_mapping/PCE2020_FocusTopicAreas_activityLevel_wFRs_FINAL.csv"))
