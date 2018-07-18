# Prep the COD DHIS2 data for Tableau
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/17/2018
#
# Upload the RDS data from DHIS2 and merge with the meta data 
# prep the data sets for analysis and the Tableau Dashboard

# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(stringr)
library(xlsx)
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/tableau/')

# --------------------
# import the PNLS data for Tableau and prep





















#--------------------
# Import base services data set and convert to a data table

base <- readRDS(paste0(dir, 'prepped_data/base.rds'))
base <- data.table(base)

#--------------------
# label "category" for the graphs
base$category <- factor(base$category, levels=c(">5 ans", "<5 ans", "default", "Féminin, 5 ans et plus",  
                                                "Masculin, Moins de 5 ans",
                                                "Féminin, Moins de 5 ans", "Masculin, 5 ans et plus" ),
                        labels=c("5 and over", "Under 5", "default", "Female, 5 and over", "Male, under 5",
                                 "Female, under 5", "Male, 5 and over"))
#--------------------
# subset to only the indicators in Base Services needed for Tableau

# subset to 2017/18 malaria indicators
base <- base[type=='malaria']
base <- base[year=='2017' | year=='2018']

#---------------------------
# fix the english translations for the malaria indicators

# RDTs
base[element_id=='CIzQAR8IWH1', element:='A 1.4 RDT - completed']
base[element_id=='SpmQSLRPMl4', element:='A 1.4 RDT - positive']

# Cases
base[element_id=='aK0QXqm8Zxn', element:='A 1.4 Presumed malaria']
base[element_id=='JXm8J8GRJxI', element:='A 1.4 Presumed malaria, treated']
base[element_id=='rfeqp2kdOGi', element:='A 1.4 Confirmed simple malaria']
base[element_id=='nRm30I4w9En', element:='A 1.4 Confirmed simple malaria, treated']
base[element_id=='AxJhIi7tUam', element:='A 1.4 Severe malaria']
base[element_id=='CGZbvJchfjk', element:='A 1.4 Severe malaria treated']

# alternate 1.5 indicators 
base[element_id=='jocZb4TE1U2', element:='A 1.5 Confirmed simple malaria - pregnant woman']
base[element_id=='wleambjupW9', element:='A 1.5 Confirmed simple malaria treated - pregnant woman']

# SP indicators phrased differently (translation OK)
base[element_id=='okUsVMBrhZC', element:='A 2.1 Sulfadox. + Pyrimét 1st dose']   
base[element_id=='IhUBOQkWKoo', element:='A 2.1 Sulfadox. + Pyrimét 4th dose']   

# LLIN indicators phrased differently
base[element_id=='jrtkdRjvNKv', element:='A 2.1 LLINs distributed - ANC 2nd visit+']   
base[element_id=='uV53nh3MrYl', element:='A 2.1 LLINs distributed - 1st ANC visit']  

#----------------------------------------
# subset to only the relevant elements  

base[ ,.(unique(element), unique(element_id))]


CIzQAR8IWH1
SpmQSLRPMl4

jocZb4TE1U2
wleambjupW9

nRm30I4w9En

#---------------------------
# organize the data in an intuitive way

malaria <- malaria[ ,.(count=sum(value)), by=.(data_set, element_fr, 
                                               element, date, category, type,
                                               level, dps=province, mtk)]

#---------------------------
# organize the data for Tableau

# rename the binary for Maniema, Kinshasa, or Tshopo
malaria[ ,mtk:=as.character(mtk)]
malaria[mtk==0, mtk:='No']
malaria[mtk==1, mtk:='Yes']


#-----------------------------
# add umlauts for the maps 

malaria[dps=='Kasai', dps:='Kasaï']
malaria[dps=='Kasai Central', dps:='Kasaï Central']
malaria[dps=='Kasai Oriental', dps:='Kasaï Oriental']
malaria[dps=='Mai-Ndombe', dps:='Maï-Ndombe']

#-----------------------------

#--------------------
# save the RDS file to be merged with the SIGL data
# only use if you want to save the interim outputs
# saveRDS(malaria, paste0(dir, 'tableau/tableau_base.rds'))

#--------------------------------------------------
# SIGL

# import the SIGL data set
sigl <- readRDS(paste0(dir, 'sigl.rds'))
sigl <- data.table(sigl)

#------------------------------
# subset to the elements needed for Tableau

elements <- c('KEv4JxAgpFK', 'HfCvAwRmGFf', 'QvVGcIERRFc', 'jm3jeYdVkBl', 'Wo3vNpLXPPm',  'ovziGhkDOKb', 'aeTpblK0SVC', 'WjEIQZvEFqa')
tabl <- sigl[element_id %in% elements]

#------------------------------
# fix the english on the tableau indicators 

tabl[,.(unique(element), unique(element_id))]

tabl[element_id=='HfCvAwRmGFf', element:='C2 12.2 HIV Test Kit for PMTCT']
tabl[element_id=='KEv4JxAgpFK', element:='C2 12.2 Determine HIV 1+2 Test Kit']
tabl[element_id=='QvVGcIERRFc', element:='Artesunate-amodiaquine C1 12.1 (12-59 months) 50mg + 135mg tablet - amount consumed']
tabl[element_id=='Wo3vNpLXPPm', element:='Artesunate-amodiaquine C1 12.1 (2-11 months) + 25mg tablet 67.5mg - amount consumed']
tabl[element_id=='jm3jeYdVkBl', element:='Artesunate-amodiaquine C1 12.1 (14 years, 6 and over) 100mg + 270mg tablet - amount consumed']
tabl[element_id=='ovziGhkDOKb', element:='Artesunate-amodiaquine C1 12.1 (6-13 years, 3 and over) 100mg + 270mg tablet - amount consumed']

# LA English translated correctly
#------------------------------
# subset to only the relevant variables and aggregate ASAQ

tabl <- tabl[ ,.(count=sum(value)), by=.(data_set, element_fr, 
                                               element, date, type,
                                               level, dps=province, mtk)]

#------------------------------
#umlauts

tabl[dps=='Kasai', dps:='Kasaï']
tabl[dps=='Kasai Central', dps:='Kasaï Central']
tabl[dps=='Kasai Oriental', dps:='Kasaï Oriental']
tabl[dps=='Mai-Ndombe', dps:='Maï-Ndombe']

#------------------------------
# rename the binary for Maniema, Kinshasa, or Tshopo
tabl[ ,mtk:=as.character(mtk)]
tabl[mtk==0, mtk:='No']
tabl[mtk==1, mtk:='Yes']

#------------------------------

#-------------------------------
# merge the two data sets

vars <- c('data_set', 'element_fr', 'element', 'date', 'type', 'level', 'dps', 'mtk', 'count')
tableau <- merge(tabl, malaria, by=vars, all=TRUE)

#-------------------------------
# set the names for tableau

setnames(tableau, c("data_set", "element_fr", "element", "date", "type",
                             "level", "dps", "mtk", "category", "count"), c("Data Set", "Element - French", 
                        "Element - English", "Date", "Disease",
                      "Facility type", "DPS", "Maniema, Kinshasa, Tshopo",
                      "Category", "Count"))

#-------------------------------
# Export as an Excel document 

# save as a CSV to upload to Basecamp
write.csv(tableau, paste0(dir, 'tableau_01_2017_04_2018.csv'))

# save as a RDS file for future manipulation
saveRDS(tableau, paste0(dir, 'tableau/tableau.rds'))

#-------------------------------

#------------------------------
# load the hiv indicator from PNLS
# first line regimen of ART

# save an RDS file that is just ART first line regimen
pnls <- readRDS(paste0(dir, 'prepped_data/pnls.rds'))
art <- pnls[element_id=='Ua57G6vbmMq' | element_id=='prnCi6GwYzL']
DXz4Zxd4fKq
gHBcPOF5y3z

saveRDS(art, paste0(dir, 'tableau/art.rds'))






