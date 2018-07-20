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
base[element_id=='nRm30I4w9En', element_eng:='A 1.4 Confirmed simple malaria, treated']

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

pnls[element_id=='DXz4Zxd4fKq', element_eng:='Pregnant or lactating women tested for HIV']
pnls[element_id=='gHBcPOF5y3z', element_eng:='Pregnant or lactating women tested HIV+']
pnls[element_id=='zxn95tkbnCv', element_eng:='Pregnant or lactating women HIV+ and informed of their results']

# remove a single outlier
pnls[org_unit=='kr Christ Roi Centre de Santé' & date=='2017-09-01' & 
       type=='pmtct' & element_id=='gHBcPOF5y3z' & category=='CPN, Moins de 15 ans',
     value:=0]

pnls <- pnls[ ,.(count=sum(value, na.rm=T)), by=.(data_set, element, element_eng, element_id,
                                                  date, category, type,
                                                  level, dps, mtk)]
#------------------------------
# find the outliers
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

ggplot(hiv2[type=='drugs'], 
       aes(x=date, y=count, color=element_eng)) +
  geom_point() +
  geom_line(alpha=0.2) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

#--------------------------------
# drop out first line ART regimen by sex
pnls[ ,unique(element_eng), element_id]
pnls <- pnls[element_id!='wLBRFksBtou']
pnls[,element_id:=NULL]

#---------------------------------
# merge the three data sets into one 
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

tabl[ ,unique(category)]

tabl[category=='5 and over', age:='5 +']
tabl[category=='Under 5', age:='Under 5']
tabl[category=='Féminin, 1 et 4 ans', age:='1 - 4 years']
tabl[category=='Féminin, 10 et 14 ans', age:='10 - 14 years']
tabl[category=='Féminin, 15 et 19 ans', age:='15 - 19 years']
tabl[category=='Féminin, 20 et 24 ans', age:='20 - 24 years']
tabl[category=='Féminin, 25 et 49 ans', age:='25 - 49 years']
tabl[category=='Féminin, 5 et 9 ans', age:='5 - 9 years']
tabl[category=='Féminin, 50 ans et plus', age:='50 +']
tabl[category=='Féminin, Moins d\'un an', age:='< 1 year']

tabl[category=='Masculin, 1 et 4 ans', age:='1 - 4 years']
tabl[category=='Masculin, 10 et 14 ans', age:='10 - 14 years']
tabl[category=='Masculin, 15 et 19 ans', age:='15 - 19 years']
tabl[category=='Masculin, 20 et 24 ans', age:='20 - 24 years']
tabl[category=='Masculin, 25 et 49 ans', age:='25 - 49 years']
tabl[category=='Masculin, 5 et 9 ans', age:='5 - 9 years']

tabl[category=='Masculin, 50 ans et plus', age:='50 +']
tabl[category=='Masculin, Moins d\'un an', age:='']
tabl[category=='CPN, 15 et 19 ans', age:='< 1 year']
tabl[category=='CPN, 20 et 24 ans', age:='15 - 19 years']
tabl[category=='CPN, 25 et 49 ans', age:='25 - 49 years']
tabl[category=='CPN, 50 ans et plus', age:='50 +']
tabl[category=='CPN, Moins de 15 ans', age:='< 15 years']
tabl[category=='SA/PP, 15 et 19 ans', age:='15 - 19 years']
tabl[category=='SA/PP, 20 et 24 ans', age:='20 - 24 years']
tabl[category=='SA/PP, 25 et 49 ans', age:='25 - 49 years']
tabl[category=='SA/PP, 50 ans et plus', age:='50+']
tabl[category=='SA/PP, Moins de 15 ans', age:='< 15 years'] 

#-------------------------------
# add a sex category
tabl[ ,unique(category)]

tabl[category=='Féminin, 1 et 4 ans', sex:='Female']
tabl[category=='Féminin, 10 et 14 ans', sex:='Female']
tabl[category=='Féminin, 15 et 19 ans', sex:='Female']
tabl[category=='Féminin, 20 et 24 ans', sex:='Female']
tabl[category=='Féminin, 25 et 49 ans', sex:='Female']
tabl[category=='Féminin, 5 et 9 ans', sex:='Female']
tabl[category=='Féminin, 50 ans et plus', sex:='Female']
tabl[category=='Féminin, Moins d\'un an', sex:='Female']

tabl[category=='CPN, 15 et 19 ans', sex:='Female']
tabl[category=='CPN, 20 et 24 ans', sex:='Female']
tabl[category=='CPN, 25 et 49 ans', sex:='Female']
tabl[category=='CPN, 50 ans et plus', sex:='Female']
tabl[category=='CPN, Moins de 15 ans', sex:='Female']


tabl[category=='Masculin, 1 et 4 ans', sex:='Male']
tabl[category=='Masculin, 10 et 14 ans', sex:='Male']
tabl[category=='Masculin, 15 et 19 ans', sex:='Male']
tabl[category=='Masculin, 20 et 24 ans', sex:='Male']
tabl[category=='Masculin, 25 et 49 ans', sex:='Male']
tabl[category=='Masculin, 5 et 9 ans', sex:='Male']
tabl[category=='Masculin, 50 ans et plus', sex:='Male']
tabl[category=='Masculin, Moins d\'un an', sex:='Male']

#--------------------------------------------------------

tabl[, unique(element_eng)]


# alternate 1.5 indicators 
tabl[element_eng=='A 1.5 Confirmed simple malaria - pregnant woman', category:='Pregnant woman']
tabl[element_eng=='A 1.5 Confirmed simple malaria - pregnant woman', sex:='Female']
tabl[element_eng=='A 1.5 Confirmed simple malaria - pregnant woman', element:='A 1.4 Paludisme simple confirmé']
tabl[element_eng=='A 1.5 Confirmed simple malaria - pregnant woman', element_eng:='A 1.4 Confirmed simple malaria']

tabl[element_eng=='A 1.5 Confirmed simple malaria treated - pregnant woman', category:='Pregnant woman']
tabl[element_eng=='A 1.5 Confirmed simple malaria treated - pregnant woman', sex:='Female']
tabl[element_eng=='A 1.5 Confirmed simple malaria treated - pregnant woman', element:='A 1.4 Paludisme simple confirmé traité [PN]']
tabl[element_eng=='A 1.5 Confirmed simple malaria treated - pregnant woman', element_eng:='A 1.4 Confirmed simple malaria, treated']


#--------------------------------------------------------

# change type to disease
tabl[type=="malaria ", disease:='malaria']
tabl[type=="malaria", disease:='malaria']
tabl[type=="hiv", disease:='hiv']
tabl[type=="pmtct", disease:='hiv']
tabl[type=='drugs', disease:='hiv']

tabl[,type:=NULL]

#----------------------------------------------------
# test graphs

malaria <- tabl[disease=='malaria', .(value=sum(count)), by=.(category, element_eng, date)]

ggplot(malaria, aes(x=date, y=value, color=category, group=category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng) +
  theme_bw()
   

hiv <- tabl[disease=='hiv', .(value=sum(count)), by=.(category, element_eng, date)]

ggplot(hiv, aes(x=date, y=value, color=category, group=category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng, scale='free_y') +
  theme_bw()

#---------------------------------------------

# set the names for tableau
setnames(tabl, c("data_set", "element", "element_eng", "date", "category", 
                             "level", "dps", "mtk", "age", "sex", "disease", "count"),
         c("Data Set", "Element - French", "Element - English", "Date", "Category",  
                      "Facility type", "DPS", "Maniema, Kinshasa, Tshopo", "Age",
                     "Sex", "Disease", "Count"))

#-------------------------------
# Export as an Excel document 

# save as a CSV to upload to Basecamp
write.csv(tableau, paste0(dir, 'tableau_01_2017_04_2018.csv'))

# save as a RDS file for future manipulation
saveRDS(tableau, paste0(dir, 'tableau/tableau.rds'))

#-------------------------------


