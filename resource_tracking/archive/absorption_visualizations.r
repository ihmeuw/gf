#-------------------------------------------------
# AUTHOR: Emily Linebarger 
# DATE: June 2019 
# PURPOSE: Some absorption visualizations 
#--------------------------------------------------

rm(list=ls())
library(data.table)
library(ggplot2) 


#J:drive filepaths
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/resource_tracking/')
inputDir = paste0(dir, "_gf_files_gos/combined_prepped_data/")
outputDir = paste0(dir, "visualizations/")

#Read in absorption data 
sen = readRDS(paste0(dir, "_gf_files_gos/sen/prepped_data/absorption.rds"))
cod = readRDS(paste0(dir, "_gf_files_gos/cod/prepped_data/absorption.rds"))
gtm = readRDS(paste0(dir, "_gf_files_gos/gtm/prepped_data/absorption.rds"))
uga = readRDS(paste0(dir, "_gf_files_gos/uga/prepped_data/absorption.rds"))


#Visualize absorption at the country-level by module
country_module_absorption = function(dt){
  dt = dt[, .(expenditure=sum(expenditure), budget=sum(budget)), by=c('gf_module', 'semester', 'grant_period')]
  dt[, absorption:=(expenditure/budget)*100]
  graph = ggplot(dt, aes(x=gf_module, y=absorption, fill=semester)) + 
    geom_bar(stat="identity", position="dodge") + 
    theme_bw() + coord_flip() + 
    facet_wrap(~grant_period) + 
    labs(title="Absorption for all grants, by module", caption="*There are overlapping date periods in PUDRs", fill="PUDR Semester", 
         y="Absorption (%)", x="Module")
  return(graph) 
}

#Visualize absorption at the grant-level by module
grant_module_absorption = function(dt){
  dt = dt[, .(expenditure=sum(expenditure), budget=sum(budget)), by=c('gf_module', 'semester', 'grant_period', 'grant')]
  dt[, absorption:=(expenditure/budget)*100]
  graph = ggplot(dt, aes(x=gf_module, y=absorption, fill=semester)) + 
    geom_bar(stat="identity", position="dodge") + 
    theme_bw() + coord_flip() + 
    facet_wrap(~grant) + #Is this going to be okay if it isn't facet-wrapped by grant_period? 
    labs(title="Absorption for all grants, by module", caption="*There are overlapping date periods in PUDRs", fill="PUDR Semester", 
         y="Absorption (%)", x="Module")
  return(graph) 
}

sen1 = country_module_absorption(sen)
sen1

sen2 = grant_module_absorption(sen)
sen2
