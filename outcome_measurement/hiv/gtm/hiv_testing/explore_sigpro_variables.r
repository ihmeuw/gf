# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
rm(list=ls())
library(data.table)
library(readxl)
library(xlsx)

#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#--------------------------------------------
# Set directories 
#--------------------------------------------
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro')
prep_dir <- paste0(dir, '/raw_sigpro')


sigpro_aa = data.table(read.csv(paste0(prep_dir, "/sigpro_f4_JanNov2018 - AA.csv"), stringsAsFactors = F))
sigpro_its = data.table(read.csv(paste0(prep_dir, "/sigpro_f4_JanNov2018 - ITS.csv"), stringsAsFactors = F))
sigpro_pb_tvc = data.table(read.csv(paste0(prep_dir, "/sigpro_f4_JanNov2018 - PB_TVC.csv"), stringsAsFactors = F))
sigpro_psico = data.table(read.csv(paste0(prep_dir, "/sigpro_f4_JanNov2018 - Psico.csv"), stringsAsFactors = F))
sigpro_links = data.table(read.csv(paste0(prep_dir, "/sigpro_f4_JanNov2018 - Vinculacion.csv"), stringsAsFactors = F))


names_aa = names(sigpro_aa)
names_its= names(sigpro_its)
names_pbtvc = names(sigpro_pb_tvc)
names_psico = names(sigpro_psico)
names_links = names(sigpro_links)

#What variables are shared across all files?  
commonNames = names_aa[names_aa %in% names_pbtvc]
length(commonNames)
commonNames = names_its[names_its %in% commonNames]
length(commonNames)
commonNames = names_psico[names_psico %in% commonNames] 
length(commonNames)
commonNames = names_links[names_links %in% commonNames]
length(commonNames)

#What are the unique variables in each file that we can analyse? 
unique_aa = setdiff(names_aa, commonNames)
unique_its = setdiff(names_its, commonNames)
unique_pbtvc = setdiff(names_pbtvc, commonNames)
unique_psico = setdiff(names_psico, commonNames)
unique_links = setdiff(names_links, commonNames)

unique_aa
unique_its #I'm not sure what the difference is between the aa and its data? 
unique_pbtvc 
unique_psico 
unique_links 