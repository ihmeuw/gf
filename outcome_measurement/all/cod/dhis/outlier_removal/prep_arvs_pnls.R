# Prep the ARV data set in PNLS
# Final prep file for usable data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/28/19
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(openxlsx)
# --------------------

# shell script for working on the cluster
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 10 -P snis_download

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
setwd(dir)

#---------------------------------------
# load the file that represents a subset (no sex or )

dt = readRDS(paste0(dir, 'prepped/pnls_sets/pnls_arv_2017_01_01_2018_12_01.rds'))

#----------------------
# begin element collapse

# drop unecessary elements
dt[ ,unique(stock_category)]
dt[ ,c('set', 'stock_category', 'health_area', 'element_eng'):=NULL]

# drop the post-script categories
# this information is included in the subpop variable
dt[ , element:=gsub("-Femmes Allaitantes", "", element)]
dt[ , element:=gsub("-Femmes Allaitantess", "", element)] # one misspelling with additional s
dt[ , element:=gsub("-Femmes Enceintes", "", element)]
dt[ , element:=gsub("-Femmes enceintes", "", element)]
dt[ , element:=gsub("-Autres", "", element)]
dt[ , element:=gsub("-Autres PPVIH", "", element)]
dt[ , element:=gsub("-Partenaire Masc", "", element)]
dt[ , element:=gsub("-Partenaires Masc", "", element)]

# list the categories in elements to check that the elimination worked
dt[ , element1:=unlist((lapply(strsplit(element, "-"), "[", 2)))]
dt[ ,unique(element1)] # should not include any key or sub-populations
dt[ ,element1:=NULL]

#----------------------
# export for corrected translations

# feed into onlinedoctranslator.com and hand correct
elements = dt[ ,.(element=unique(element)), by=element_id]
elements[ ,count:=.N, by=element]
write.xlsx(elements, paste0(dir, 'catalogues/pnls_sets/pnls_arv.xlsx'))

# read in the translated elements
elements_eng = data.table(read.xlsx(paste0(dir, 'catalogues/pnls_sets/pnls_arv_translated.xlsx')))
elements_eng[ ,count:=NULL]

# merge in the english elements
dt = merge(dt, elements_eng, by='element_id', all.x=T)
#----------------------
# merge in the svs elements - svs is listed under subpop

dt[element_id=='M58t1xKXIXD' , element_eng:='HIV-exposed Persons who received a PEP Kit']
dt[element_id=='mnDamvtzKp4', element_eng:="HIV-exposed Persons who received a PEP Kit within 72 hours"]
dt[element_id=='yYraWmPVFWP', element_eng:='Received medical care within 72 hours']
#----------------------
# add IPT to TB elements
dt[grep("IPT", element_eng), tb:=TRUE]
#----------------------
# collapse on the english elements
sumVars = c("org_unit_id", "org_unit", "date", "element_eng", "subpop", "sex", "age",
            "org_unit_type", "level", "dps", "health_zone", "mtk", "maternity",  
            "case", "tb")

dt = dt[ ,.(value=sum(value)), by=sumVars]

# rename element_eng 
setnames(dt, 'element_eng', 'element')

#----------------------
# save the output 

saveRDS(dt, paste0(dir, 'prepped/pnls_arv.rds'))
#----------------------
