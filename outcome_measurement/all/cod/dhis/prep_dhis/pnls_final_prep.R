# Final cleaning on PNLS
# Use this to manually aggregate the final variables
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
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 2 

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
setwd(dir)

#---------------------------------------
# load the file that represents a subset (no sex or )

dt = readRDS(paste0(dir, 'prepped/pnls_sets/pnls_pmtct_2017_01_01_2018_12_01.rds'))

#----------------------------------------------
#Add in subpops that weren't captured before
dt[grep("AMF", element), subpop:='AMF']
dt[element_id=='eVULOZGDgFZ', subpop:='plw']
dt[element_id=='hSZGwtEX9pt', subpop:='serodisc']
#--------------------------------------------

#-----------------------------
# check the subpops and sexes in the indicators are all captured
# then strip them from the indicators

dt[, unique(element)]
unique(dt[is.na(subpop), .(element)])

#Remove subpop strings
dt[ ,element1:=tolower(element)]
dt[ ,element1:=gsub("femmes enceintes ou allaitantes", "", element1)]
dt[, element1:=gsub("femmes enc. ou allaitantes", "", element1)]
dt[, element1:=gsub("femmes enceintes et allaitantes", "", element1)]
dt[, element1:=gsub("femmes enceintes ouallaitantes", "", element1)]
dt[, element1:=gsub("femmes enceintes", "", element1)]
dt[ ,element1:=gsub("eev", "", element1)]
dt[, element1:=gsub("partenaires masculins", "", element1)]
dt[, element1:=gsub("enfants exposés", "", element1)]
dt[, element1:=gsub("couples discordants", "", element1)]
dt[, element1:=gsub("amf", "", element1)]

#

dt[, element1:=trimws(element1)]

dt[, unique(element1)]

#-----------------------------

# export the abbreviated elements for translation

# to do this on the cluster, you must export as an RDS, then use local code to save
elements = dt[ ,.(element = unique(element)), by=.(element_id)]
set = dt[ ,tolower(unique(set))]

# save the list as an excel file 
write.xlsx(elements, paste0(dir,'meta_data/translate/pnls_elements_to_translate', set, '.xlsx' ))

# translate using onlinedoctranslator.com and save as file path below
#---------------------

# import the translated elements
new_elements = read.xlsx(paste0(dir,
                'meta_data/translate/pnls_elements_translations_', set, '.xlsx' ))

# reset the variable name for the merge and merge on element id
setnames(new_elements, 'element', 'element_eng')

#Putting this in here for now - couldn't get read .xlsx to work, but these were the files I used. 
# elements = read.csv("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/meta_data/translate/pnls_elements.csv")
# new_elements = read.csv("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/meta_data/translate/pnls_elements_translated_pmtct.csv")

# be sure 
x = merge(elements, new_elements, by='element_id', all.x=T )
setDT(x)
#---------------------------------------

#PMTCT edits 
x[, element_eng:=tolower(element_eng)]

x[, first_word:=word(element_eng, 1)]
x[, second_word:=word(element_eng, 2)]
x[, last_word:=word(element_eng, -1)]

#How can you make sure you're not overwriting any cases? Do any of these overlap? 


#Variables for children
# x[grep("eev", element_eng), new_var:='Children exposed to HIV']
# x[grep("children exposed", element_eng), new_var:='Children exposed to HIV']

#Variables for pregnant and lactating women
x[element=='Femmes enceintes ou allaitantes VIH+ ayant bénéficié d’un appui nutritionnel', new_var:='Pregnant or lactating women who are HIV+ and receiving supplemental nutrition']
x[element=='Femmes enceintes ou allaitantes testées' | element=='Femmes enc. ou allaitantes testées-service', new_var:='Pregnant or lactating women who were tested']
x[element=='Femmes allaitantes qui sont passées de l’Option A à l’Option B+', new_var:='Pregnant or lactating women who were moved from Option A to Option B+']
x[element=='Femmes enceintes ou allaitantes VIH+' | element == 'Femmes enc. ou allaitantes VIH+ service', new_var:='Pregnant or lactating women who are HIV+']
x[element=='Femmes enc. ou allaitantes informées des résultats-service' | element=='Femmes enceintes ou allaitantes informées des résultats', new_var:='Pregnant or lactating women who were informed of their results']
x[element=='Femmes enceintes ou allaitantes VIH+ et informées de leurs résultats' | element=='Femmes enc. ou allaitantes VIH+ et informées de leurs résultats-service', 
  new_var:='Pregnant or lactating women who are HIV+ and were informed of their results']
x[element=='Total femmes enceintes ouallaitantes reçues dans la structure', new_var:='Total pregnant or lactating women received in the health system']
x[element=="Femmes enceintes ou allaitantes ne connaissant leur statut VIH+ avant d'arriver dans la structure sous TAR" |
    element=="Femmes enceintes ou allaitantes ne connaissant pas leur statut VIH avant toute intervention dans la structure", new_var:=
    "Pregnant or lactating women who don't know their HIV status before arriving in the facility for ART"]
x[element=='Femmes enceintes ou allaitantes conseillées' | element=='Femmes enc. ou allaitantes conseillées-service', new_var:='Pregnant or lactating women who were counseled']
x[element=='Femmes enceintes ou allaitantes VIH+ ayant bénéficié d’une évaluation et conseils nutritionnels', new_var:='Pregnant or lactating women who are HIV+ and have received nutritional counseling']
x[element=='Femmes enceintes ou allaitantes VIH+ ayant bénéficié du screening SGBV', new_var:=
    'Pregnant or lactating women who are HIV+ who have received a screening for survivors of sexual violence']
x[element=='Femmes enceintes ou allaitantes mises sous ARV le mois passé et qui sont encore sous TARV le mois suivant', new_var:=
    'Pregnant or lactating women who were on ARV during the past month and are still on ARV the following month']
x[element=='Femmes enceintes ou allaitantes VIH+ dépistées malnutries', new_var:='Pregnant or lactating women who were tested for malnutrition']
x[element=="Femmes enceintes et allaitantes connaissant leur statut VIH+ avant d'arriver dans la structure sous TAR", new_var:=
    'Pregnant or lactating women who know their HIV status before arriving at the health facility for ART']

#Make a subset of variables regarding pregnant and lactating women 
preg = x[grep("allaitantes", element), .(element, new_var, first_word, second_word)]
preg = preg[is.na(new_var)]
View(preg)


#General cases
# x[grep("counseled and tested", element_eng), new_var:='Counseled and Tested']
# x[grep("couples tested", element_eng), new_var:='Couples tested']
# x[grep('hiv +', element_eng), new_var:='HIV positive']
x[grep('PVVIH sous ARVavec une charge virale indétectable', element), new_var:='PLHIV with an undetectable viral load']
# x[grep('PVVIH sous prophylaxie à l’INH', element), new_var:='PLHIV under isoniazid profylaxis']
x[grep('plwha with active search of tb in the month', element_eng), new_var:='PLHIV screened for TB in the month']
x[grep('plwha with active search of tb during mois', element_eng), new_var:='PLHIV screened for TB in the month']
# x[grep('plwha benefiting cd4', element_eng), new_var:='PLHIV benefiting from CD4']
x[grep('plwha on arvs benefiting from a viral-load', element_eng), new_var:='PLHIV with an undetectable viral load']

duplicate_words = x[is.na(new_var) & duplicated(first_word) & duplicated(second_word), .(first_word, second_word, element_eng)] #This might help you highlight groups. 
duplicate_words = unique(duplicate_words)

duplicate_words = duplicate_words[order(first_word, second_word)]
View(duplicate_words)
#Review cases that have the same first and second word, but different last words. 

#Make sure that there are unique subgroups among each new_var above before you collapse. 


