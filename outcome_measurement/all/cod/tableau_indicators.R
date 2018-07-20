# Prep the COD DHIS2 data for Tableau
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/19/2018
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

#--------------------
# Import base services data set and convert to a data table

base <- readRDS(paste0(dir, 'tabl_base.rds'))
base <- data.table(base)

#--------------------
# label "category" for the graphs
base$category <- factor(base$category, levels=c(">5 ans", "<5 ans", "default"),
                        labels=c("5 and over", "Under 5", "default"))

#---------------------------
# fix the english translations for the malaria indicators

# RDTs
base[element_id=='CIzQAR8IWH1', element_eng:='A 1.4 RDT - performed']
base[element_id=='SpmQSLRPMl4', element_eng:='A 1.4 RDT - positive']

# Cases
base[element_id=='rfeqp2kdOGi', element_eng:='A 1.4 Confirmed simple malaria']
base[element_id=='nRm30I4w9En', element:='A 1.4 Confirmed simple malaria, treated']

# alternate 1.5 indicators 
base[element_id=='jocZb4TE1U2', element_eng:='A 1.5 Confirmed simple malaria - pregnant woman']
base[element_id=='wleambjupW9', element_eng:='A 1.5 Confirmed simple malaria treated - pregnant woman']

#---------------------------
# organize the data in an intuitive way and subset to necessary variables

base <- base[ ,.(count=sum(value, na.rm=T)), by=.(data_set, element, element_eng, date, category, type,
                                               level, dps, mtk)]

base[ , category:=as.character(category)]

#---------------------------
# organize the data for Tableau
#-----------------------------
# test graph - test that the values are rational 

confirm <- base[ ,.(count=sum(count)), by=.(element_eng, date, category)]

ggplot(confirm, aes(x=date, y=count, color=category, group=category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

#--------------------------------------------------
# SIGL

# import the SIGL data set
sigl <- readRDS(paste0(dir, 'tabl_sigl.rds'))
sigl <- data.table(sigl)

# check that first line tb frugs are included
sigl[element_id=='ncXfF8VViSh', tableau:=1]
#------------------------------
# subset to the elements needed for Tableau

sigl <- sigl[tableau==1 & (year==2017 | year==2018)]

#------------------------------
# fix the english on the tableau indicators 

sigl[,.(unique(element_eng), unique(element_id))]

sigl[element_id=='HfCvAwRmGFf', element_eng:='C2 12.2 HIV Test Kit for PMTCT']
sigl[element_id=='KEv4JxAgpFK', element_eng:='C2 12.2 Determine HIV 1+2 Test Kit']
sigl[element_id=='QvVGcIERRFc', element_eng:='Artesunate-amodiaquine C1 12.1 (12-59 months) 50mg + 135mg tablet - amount consumed']
sigl[element_id=='Wo3vNpLXPPm', element_eng:='Artesunate-amodiaquine C1 12.1 (2-11 months) + 25mg tablet 67.5mg - amount consumed']
sigl[element_id=='jm3jeYdVkBl', element_eng:='Artesunate-amodiaquine C1 12.1 (14 years, 6 and over) 100mg + 270mg tablet - amount consumed']
sigl[element_id=='ovziGhkDOKb', element_eng:='Artesunate-amodiaquine C1 12.1 (6-13 years, 3 and over) 100mg + 270mg tablet - amount consumed']
sigl[element_id=='ncXfF8VViSh', element_eng:='C1 12.1 Rifampicin isoniazid + Pyrimetham Ethamb (RHZE) + 75mg 150mg + 400mg + 275mg these - amount consumed']



#------------------------------
# subset to only the relevant variables 

sigl <- sigl[ ,.(count=sum(value, na.rm=T)), by=.(data_set, element, 
                                               element_eng, date, category, type, 
                                               level, dps, mtk)]


#-----------------------------
# test graph - test that the values are rational 

drugs <- sigl[ ,.(count=sum(count)), by=.(element_eng, date, category)]

ggplot(drugs, aes(x=date, y=count)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)


#-----------------------------
# merge the two data sets

idVars <- c('data_set', 'element', 'element_eng', 'date', 'category', 'type', 'level', 'dps', 'mtk', 'count')
tableau_base_sigl <- merge(base, sigl, by=idVars, all=TRUE)

#------------------------------
# save an interim RDS file 

#saveRDS(tableau_base_sigl, paste0(dir, 'tableau_base_sigl.rds' ))

#------------------------------
# load the hiv indicator from PNLS
# first line regimen of ART

# save an RDS file that is just ART first line regimen
pnls <- readRDS(paste0(dir, 'tabl_pnls.rds'))
pnls <- data.table(pnls)
pnls <- pnls[element_id!='prnCi6GwYzL']

pnls[element_id=='DXz4Zxd4fKq', element_eng:='Pregnant or lactating women tested for HIV']
pnls[element_id=='gHBcPOF5y3z', element_eng:='Pregnant or lactating women tested HIV+']
pnls[element_id=='zxn95tkbnCv', element_eng:='Pregnant or lactating women HIV+ and informed of their results']

# remove a single outlier
pnls[org_unit=='kr Christ Roi Centre de Santé' & date=='2017-09-01' & 
       type=='pmtct' & element_id=='gHBcPOF5y3z' & category=='CPN, Moins de 15 ans',
     value:=0]

pnls[element=='PNLS-DRUG-ABC + 3TC + LPV/r sex' & date=='2017-05-01' &
       org_unit=="hk Mumbunda II Centre de Santé de Réference", value:=0]

pnls <- pnls[ ,.(count=sum(value, na.rm=T)), by=.(data_set, element, element_eng, 
                                                  date, category, type,
                                                  level, dps, mtk)]


#------------------------------
# find the outlier
# 
# wut <- pnls[date=='2017-09-01' & element_id=='gHBcPOF5y3z' & category=='CPN, Moins de 15 ans', .(value=sum(value)), by=org_unit]
# wut[value > 5, .(org_unit, value)]
# krc <- pnls[ org_unit=='kr Christ Roi Centre de Santé', .(value=sum(value)), by=.(date, element, category)]
# 
# pnls[org_unit=='kr Christ Roi Centre de Santé' & date=='2017-09-01' & 
#        type=='pmtct' & element_id=='gHBcPOF5y3z' & category=='CPN, Moins de 15 ans',
#         value]

#------------------------------
# create test graphs 

hiv <- pnls[ ,.(count=sum(count)), by=.(element_eng, date, category, type)]

ggplot(hiv[type=='pmtct'], 
       aes(x=date, y=count, color=category, group=category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng, scales='free_y') +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

ggplot(hiv[type=='drugs'], 
       aes(x=date, y=count, color=category, group=category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng, scales='free_y') +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

hiv2 <- pnls[ ,.(count=sum(count)), by=.(element_eng, date, type)]

ggplot(hiv2[type=='drugs'], 
       aes(x=date, y=count)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng, scales='free_y') +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

#--------------------------------

tabl <- merge(tableau_base_sigl, pnls, by=idVars, all=T)

#------------------------------

#------------------------------
# add umlauts to merge with tableau

tabl[dps=='Kasai', dps:='Kasaï']
tabl[dps=='Kasai Central', dps:='Kasaï Central']
tabl[dps=='Kasai Oriental', dps:='Kasaï Oriental']
tabl[dps=='Mai-Ndombe', dps:='Maï-Ndombe']

#-------------------------------
# add age and sex categories and merge in pregnant women 




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


