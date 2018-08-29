# Prep the COD DHIS2 data for Tableau
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/23/2018
#
# Upload the RDS data from SNIS-DHIS2 and merge with the meta data 
# Prep the data sets for the Tableau Dashboard

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
library(openxlsx)
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
base[, unique(category)]

base$category <- factor(base$category, levels=c(">5 ans", "<5 ans", "default"),
                        labels=c("5 and over", "Under 5", "default"))

base[ , category:=as.character(category)]
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

base <- base[ ,.(count=sum(value, na.rm=T)), by=.(data_set, element, 
                                        element_eng, date, category, type,
                                        level, dps, mtk)]


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

# check that first line tb drugs are included
sigl[element_id=='ncXfF8VViSh']

#------------------------------
# fix the english on the tableau indicators 

sigl[,.(unique(element_eng), unique(element_id))]

sigl[element_id=='HfCvAwRmGFf', element_eng:='C2 12.2 HIV Test Kit for PMTCT site']
sigl[element_id=='KEv4JxAgpFK', element_eng:='C2 12.2 Determine HIV 1+2 Test Kit Pce']
sigl[element_id=='QvVGcIERRFc', element_eng:='C1 12.1 Artesunate-amodiaquine (12-59 months) 50mg + 135mg tablet - amount consumed']
sigl[element_id=='Wo3vNpLXPPm', element_eng:='C1 12.1 Artesunate-amodiaquine (2-11 months) 25mg + 67.5mg tablet  - amount consumed']
sigl[element_id=='jm3jeYdVkBl', element_eng:='C1 12.1 Artesunate-amodiaquine (14 years, 6 and over) 100mg + 270mg tablet - amount consumed']
sigl[element_id=='ovziGhkDOKb', element_eng:='C1 12.1 Artesunate-amodiaquine (6-13 years, 3 and over) 100mg + 270mg tablet - amount consumed']
sigl[element_id=='ncXfF8VViSh', element_eng:='C1 12.1 Rifampicin isoniazid + Pyrimetham Ethamb (RHZE) 150mg + 75mg + 400mg + 275mg these - amount consumed']

sigl[element_id=='WjEIQZvEFqa', element_eng:='C1 12.1 Lumefantrine+ Artemether 80mg + 480mg - amount consumed' ]
sigl[element_id=='aeTpblK0SVC', element_eng:='C1 12.1 Lumefantrine+ Artemether 40mg + 240mg - amount consumed' ]


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
  facet_wrap(~element_eng, scale='free_y') +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)


tb <- drugs <- sigl[type=='tb' ,.(count=sum(count)), by=.(element_eng, date, category)]

ggplot(tb, aes(x=date, y=count, color=category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element_eng) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

#-----------------------------
# bind the two data sets together

tableau_base_sigl <- rbind (base, sigl)

#------------------------------
# save an interim RDS file 

#saveRDS(tableau_base_sigl, paste0(dir, 'tableau_base_sigl.rds' ))

#------------------------------
# load the hiv indicator from PNLS
# first line regimen of ART

# save an RDS file that is just ART first line regimen
pnls <- readRDS(paste0(dir, 'tabl_pnls.rds'))
pnls <- data.table(pnls)

# drop out the drug element where the totals don't make sense and STI test indicator
pnls <- pnls[element_id!="jJuipTLZK4o"]
pnls <- pnls[element_id!="DAbWpraDg43"]
pnls <- pnls[element_id!="ZqM4AyJW42Q"]
pnls <- pnls[element_id!="Gv1UQdMw5wL"]

# drop two large outliers
pnls[org_unit=="kn Molende Centre de Santé" & date=='2018-02-01' & element_id=='fdc1v0PSUZe' & category=='Féminin', value:=NA]
pnls[org_unit=='kr Christ Roi Centre de Santé' & date=='2017-09-01' & 
       type=='pmtct' & element_id=='gHBcPOF5y3z' & category=='CPN, Moins de 15 ans',
     value:=NA]

pnls <- pnls[!is.na(value)]

# fix the english translations
pnls[element_id=='DXz4Zxd4fKq', element_eng:='Pregnant or lactating women tested for HIV']
pnls[element_id=='gHBcPOF5y3z', element_eng:='Pregnant or lactating women tested HIV+']
pnls[element_id=='zxn95tkbnCv', element_eng:='Pregnant or lactating women HIV+ and informed of their results']

# sum the values at the dps level
pnls <- pnls[ ,.(count=sum(value, na.rm=T)), by=.(data_set, element, element_eng, 
                                                  date, category, type,
                                                  level, dps, mtk)]

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

ggplot(hiv[type=='drugs' | type=='sti'], 
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



#---------------------------------
# rbind the three data sets into one 

tabl <- rbind(tableau_base_sigl, pnls)

#------------------------------

#------------------------------
# add umlauts to merge with tableau

tabl[dps=='Kasai', dps:='Kasaï']
tabl[dps=='Kasai Central', dps:='Kasaï Central']
tabl[dps=='Kasai Oriental', dps:='Kasaï Oriental']
tabl[dps=='Mai-Ndombe', dps:='Maï-Ndombe']

#-------------------------------
# add age and sex categories  

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

tabl[category=="Féminin, Moins de 14 ans", age:='< 14 years']
tabl[category=="Féminin, 15 et 24 ans", age:='15 - 24 years']
tabl[category=="Féminin, 25 ans et plus", age:='25+ years']

tabl[category=="Masculin, Moins de 14 ans", age:='< 14 years']
tabl[category=="Masculin, 15 et 24 ans", age:='15 - 24 years']
tabl[category=="Masculin, 25 ans et plus", age:='25+ years']

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

tabl[is.na(age), unique(category)]
#-------------------------------
# add a sex category
tabl[ ,unique(category)]

fems <- grep(pattern="Féminin", x=tabl$category)
tabl[fems, sex:='Female']

# code anc visits as female patients
cpn <- grep(pattern="CPN", x=tabl$category)
tabl[cpn, sex:='Female']

gents <- grep(pattern="Masculin", x=tabl$category)
tabl[gents, sex:='Male']

tabl[is.na(sex), unique(category)]
#--------------------------------------------------------
# merge pregnant women into malaria cases 

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
tabl[type=='sti', disease:='hiv']
tabl[type=='tb', disease:='tb']

tabl[ is.na(disease), type]
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


tb <- tabl[disease=='tb', .(value=sum(count)), by=.(category, element_eng, date)]

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

# save as a RDS file 
saveRDS(tabl, paste0(dir, 'tableau_01_2017_07_2018.rds'))

# read in the RDS file so you don't have to rerun the code
#tabl <- readRDS(paste0(dir, 'tableau_01_2017_04_2018.rds'))

#------------------------------
# use xlsx instead of csv because it preserves the french special characters
# use the openxlsx package (not xlsx!) as the java in xlsx cannot accomodate size

write.xlsx(tabl, paste0(dir, 'tableau_01_2017_04_2018.xlsx'))

#-------------------------------
