# --------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Review a few key examples of activity classifications 
# That aren't mapped correctly to GF modular framework. 
# DATE: July 22, 2019 
#----------------------------------------------

rm(list=ls())
library(data.table) 

#Read in file 
dt = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/budget_pudr_iterations.rds")
dt = dt[grant_period%in%c('2018-2020', '2018', '2019-2021', '2016-2019', '2019-2022') & data_source=="fpm"]

# ---------------------------
# KVP 
#----------------------------
msm = dt[grep("msm|hsh", tolower(activity_description)), .(grant, grant_period, activity_description, orig_module, orig_intervention, gf_module, gf_intervention)]
msm = unique(msm)
#Search for errors by excluding the MSM module 
msm = msm[gf_module!="Comprehensive prevention programs for men who have sex with men"]
msm[, count:=1]
msm[, sum(count), by='gf_module']

write.csv(msm[, .(grant, grant_period, orig_module, orig_intervention, activity_description)], "C:/Users/elineb/Desktop/msm.csv", row.names=F)

# Finding 1: Funding for salaries is classified a number of different ways, even within the same grant. 
salaries = msm[grepl("salario", activity_description)]
View(salaries[grant=="GTM-H-HIVOS", .(orig_module, orig_intervention, activity_description)])

# ---------------------------
# GeneXpert
#----------------------------

xpert = dt[grep("xpert", tolower(activity_description)), .(grant, grant_period, orig_module, orig_intervention, gf_module, gf_intervention, activity_description, grant_disease)]
xpert = xpert[grant_disease%in%c('tb', 'rssh')]
xpert = unique(xpert)

write.csv(xpert, 'C:/Users/elineb/Desktop/xpert.csv', row.names=F)

# ---------------------------
# Salaries 
#----------------------------
salary = dt[grep("salaire|salario|salary", tolower(activity_description))]
salary = unique(salary[, .(grant, grant_period, orig_module, orig_intervention, gf_module, gf_intervention, activity_description)])

write.csv(salary, "C:/Users/elineb/Desktop/salaries.csv", row.names=F)

#-----------------------------
# Program management - kind of a dead end so far. 
#-----------------------------
management = dt[grep("gestion|management|gestión", tolower(activity_description))]
management = unique(management[, .(grant, grant_period, orig_module, orig_intervention, gf_module, gf_intervention, activity_description)])
management = management[gf_module!="Program management"]

#--------------------------------
# Human Rights 
#--------------------------------
hr = dt[gf_module=="Programs to reduce human rights-related barriers to HIV services"]
hr = unique(hr[, .(grant, grant_period, orig_module, orig_intervention, gf_module, gf_intervention, activity_description)])
write.csv(hr, "C:/Users/elineb/Desktop/hr.csv", row.names=F)

#--------------------------------
# Human Rights 
#--------------------------------
crs = dt[gf_module == "Community responses and systems"]
crs = unique(crs[, .(grant, grant_period, orig_module, orig_intervention, gf_module, gf_intervention, activity_description)])

#Can you go at this another way? Search for 'rights' and see what you turn up? EMILY 

#-----------------------------------------
# What are dead giveaway keywords?
#-----------------------------------------
# Surveys 
surveys = dt[grep("survey|enquete|encuesta", tolower(activity_description))]
surveys = unique(surveys[, .(grant, grant_period, orig_module, orig_intervention, gf_module, gf_intervention, activity_description)])
write.csv(surveys, "C:/Users/elineb/Desktop/surveys.csv", row.names=F)

#-----------------------------------------
# Can you easily classify drug-buying? 
#-----------------------------------------
art = dt[grep("art|antiretroviral|anti-retroviral", tolower(activity_description))]
art = unique(art[, .(grant, grant_period, orig_module, orig_intervention, gf_module, gf_intervention, activity_description)])

#------------------------------------------------------
# Do you have any coefficient-splitting in these files? 
#------------------------------------------------------
merge = unique(dt[, .(file_name, grant, grant_period, orig_module, orig_intervention, grant_disease)])
merge[, ]
setnames(merge, c('orig_module', 'orig_intervention', 'grant_disease'), c('module', 'intervention', 'disease'))
map = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/gf_mapping.rds")

merge = merge(merge, map, by=c('module', 'intervention', 'disease'), all.x=T)
stopifnot(nrow(merge[is.na(gf_module)])==0)
