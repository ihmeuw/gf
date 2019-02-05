# Use prepped SNIS data to create a Tableau data set

# ----------------------------------------------
# Caitlin O'Brien-Carelli
# 1/25/2019
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(openxlsx) # does not work on the cluster
library(stringr) 
# --------------------

# --------------------
# set working directories 

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# set the folder for 
import_folder = 'pre_prep/merged/'

# input data sets
base_data <- 'base_2016_01_01_2018_12_01.rds'
pnls_data <- 'pnls_subset_2014_11_01_2018_12_01.rds'
sigl_data <- 'sigl_2016_01_01_2018_12_01.rds'

# ---------------------------------------------
# import the tableau data sets and rbind them together 

# working on this to make this script automatically update the tableau data with the most recent files
# data.list = list() # list of data tables to rbind together
# 
# # list all the files in the import_folder
# files = list.files(paste0(dir, import_folder))
# 
# # create a subset of files with just base, sigl, and pnls data
# files_subset = c()
# i = 1
# for(f in files) {
#   if( unlist(strsplit(f, "_"))[1] %in% c("base", "sigl", "pnls")  ){
#     files_subset[i] = f
#   } else { next }
#   i = i + 1
# }  
# # get second to last element of the file name
# date = unlist(strsplit(f, "_"))[ length( strsplit(f, "_")[[1]] ) - 1 ]


# vector of the variables to subset to
elements = c("aZwnLALknnj", "AxJhIi7tUam", "CGZbvJchfjk",
             "aK0QXqm8Zxn", "JXm8J8GRJxI", "rfeqp2kdOGi", "nRm30I4w9En",
             "SpmQSLRPMl4", "CIzQAR8IWH1", "scjrnpQxZ85", "ukhWLb7dPET",
             "jocZb4TE1U2", "wleambjupW9", "uV53nh3MrYl", "jrtkdRjvNKv",
             "jJuipTLZK4o", "DXz4Zxd4fKq", "gHBcPOF5y3z", "jm3jeYdVkBl", 
             "QvVGcIERRFc", "Wo3vNpLXPPm", "ovziGhkDOKb", "aeTpblK0SVC",
             "WjEIQZvEFqa", "ncXfF8VViSh", "KEv4JxAgpFK")


# import the data sets and subset to the relevant variables
base = readRDS(paste0(dir, import_folder, base_data))
base = base[year(date)=='2017' | year(date)=='2018']
base = base[element_id %in% elements]

pnls = readRDS(paste0(dir, import_folder, pnls_data))
pnls = pnls[year(date)=='2017' | year(date)=='2018']
pnls = pnls[element_id %in% elements]

sigl = readRDS(paste0(dir, import_folder, sigl_data))
sigl = sigl[year(date)=='2017' | year(date) =='2018']
sigl = sigl[element_id %in% elements]

# create a data set identifier
base[ ,set:='base']
pnls[ ,set:='pnls']
sigl[ ,set:='sigl']

pnls[, country:= "République Démocratique du Congo"]
drop_cols <- c("coordinates")
base <- base[ , !(drop_cols), with = FALSE]
sigl <- sigl[ , !(drop_cols), with = FALSE]

# the code works up to here - you will need to edit after this
dt = rbindlist( list(base, sigl, pnls), use.names = TRUE, fill= TRUE )
if (nrow(base) + nrow(sigl) + nrow(pnls) != nrow(dt)) stop ("rbind did not work correctly")
# ---------------------------------------------

# ---------------------------------------------
# save the interim data set so you don't need to load all the data every time

saveRDS(dt, paste0(dir, 'pre_prep/merged/tableau_interim.rds'))
# ---------------------------------------------

# ---------------------------------------------
# check the english translations of the elements
# for future downloads, change the english translations in the csv so they are correct after merge

# change order of drug names and dosage to mirror the french elements
dt[element_id=='KEv4JxAgpFK', element_eng:='C2 12.2 Determine HIV 1 + 2 test kit - quantity consumed']
dt[element_id=='QvVGcIERRFc', element_eng:='C1 12.1 Artesunate-amodiaquine (12-59 months) 50mg + 135mg tablet - amount consumed']
dt[element_id=='Wo3vNpLXPPm', element_eng:='C1 12.1 Artesunate-amodiaquine (2-11 months) 25mg + 67.5mg tablet  - amount consumed']
dt[element_id=='jm3jeYdVkBl', element_eng:='C1 12.1 Artesunate-amodiaquine (14 years, 6 and over) 100mg + 270mg tablet - amount consumed']
dt[element_id=='ovziGhkDOKb', element_eng:='C1 12.1 Artesunate-amodiaquine (6-13 years, 3 and over) 100mg + 270mg tablet - amount consumed']
dt[element_id=='ncXfF8VViSh', element_eng:='C1 12.1 Rifampicin isoniazid + Pyrimetham Ethamb (RHZE) 150mg + 75mg + 400mg + 275mg these - amount consumed']

dt[element_id=='WjEIQZvEFqa', element_eng:='C1 12.1 Lumefantrine + Artemether 80mg + 480mg - amount consumed' ]
dt[element_id=='aeTpblK0SVC', element_eng:='C1 12.1 Lumefantrine + Artemether 40mg + 240mg - amount consumed' ]

# fix the english translations
dt[element_id=='DXz4Zxd4fKq', element_eng:='Pregnant or lactating women tested for HIV']
dt[element_id=='gHBcPOF5y3z', element_eng:='Pregnant or lactating women who tested HIV+']

# change amount to quantity
dt$element_eng <- gsub("amount", "quantity", dt$element_eng)

#---------------------------------------
# create a variable for the number of facilities reporting at the health zone level

facilities = dt[ ,.(facilities_reporting=length(unique(org_unit))), 
                  by=.(set, element, date, health_zone, dps, category, level)]

#-----------------------------------
# sum over health zone to change data to the health facility type level 
dt$value <- as.numeric(dt$value) # the NAs introduced here were listed as "NULL" before when the value was character type
dt <- dt[ ,.(value=sum(value)), by=.(set, data_set, element_id, element, element_eng, date,
                                     category,  health_zone, dps, level)] 
# Note: th

# merge in the number of facilities
dt = merge(dt, facilities, by=c('set', 'element', 'date', 
                           'health_zone', 'dps', 'category', 'level'), all = TRUE)

#----------------------------
# create age and sex variables 
dt[grep(category, pattern='>5'), age:='5+ years']
dt[grep(category, pattern='<5'), age:='Under 5 years']

# code anc visits as female patients
dt[grep(category, pattern='CPN'), sex:='Female']
dt[grep(category, pattern='SA/PP'), sex:='Female']
# dt[category=='5 and over', age:='5 +']
# dt[category=='Under 5', age:='Under 5']
# dt[category=='Féminin, 1 et 4 ans', age:='1 - 4 years']
# dt[category=='Féminin, 10 et 14 ans', age:='10 - 14 years']
# dt[category=='Féminin, 15 et 19 ans', age:='15 - 19 years']
# dt[category=='Féminin, 20 et 24 ans', age:='20 - 24 years']
# dt[category=='Féminin, 25 et 49 ans', age:='25 - 49 years']
# dt[category=='Féminin, 5 et 9 ans', age:='5 - 9 years']
# dt[category=='Féminin, 50 ans et plus', age:='50 +']
# dt[category=='Féminin, Moins d\'un an', age:='< 1 year']
# 
# dt[category=="Féminin, Moins de 14 ans", age:='< 14 years']
# dt[category=="Féminin, 15 et 24 ans", age:='15 - 24 years']
# dt[category=="Féminin, 25 ans et plus", age:='25+ years']
# 
# dt[category=="Masculin, Moins de 14 ans", age:='< 14 years']
# dt[category=="Masculin, 15 et 24 ans", age:='15 - 24 years']
# dt[category=="Masculin, 25 ans et plus", age:='25+ years']
# 
# dt[category=='Masculin, 1 et 4 ans', age:='1 - 4 years']
# dt[category=='Masculin, 10 et 14 ans', age:='10 - 14 years']
# dt[category=='Masculin, 15 et 19 ans', age:='15 - 19 years']
# dt[category=='Masculin, 20 et 24 ans', age:='20 - 24 years']
# dt[category=='Masculin, 25 et 49 ans', age:='25 - 49 years']
# dt[category=='Masculin, 5 et 9 ans', age:='5 - 9 years']
# 
# dt[category=='Masculin, 50 ans et plus', age:='50 +']
# dt[category=='Masculin, Moins d\'un an', age:='< 1 year']
dt[category=='CPN, 15 et 19 ans', age:='15 - 19 years']
dt[category=='CPN, 20 et 24 ans', age:='20 - 24 years']
dt[category=='CPN, 25 et 49 ans', age:='25 - 49 years']
dt[category=='CPN, 50 ans et plus', age:='50 +']
dt[category=='CPN, Moins de 15 ans', age:='< 15 years']
dt[category=='SA/PP, 15 et 19 ans', age:='15 - 19 years']
dt[category=='SA/PP, 20 et 24 ans', age:='20 - 24 years']
dt[category=='SA/PP, 25 et 49 ans', age:='25 - 49 years']
dt[category=='SA/PP, 50 ans et plus', age:='50+']
dt[category=='SA/PP, Moins de 15 ans', age:='< 15 years'] 

dt$element_eng <- gsub("FE", "pregnant women", dt$element_eng)
dt[grep(element, pattern='FE'), sex:='Female']
dt[grep(element, pattern='CPN'), sex:='Female']

# only stock data should be missing sex
dt[is.na(sex), unique(category)]

# fix type
dt[set=='pnls', type:='hiv']
dt[set=="base", type:='malaria']
dt[grep(element, pattern='Artesunate'), type:='malaria']
dt[grep(element, pattern='Lumefantrine'), type:='malaria']
dt[grep(element, pattern='RHZE'), type:='tb']
dt[grep(element, pattern='VIH'), type:='hiv']
#-------------------------------
# test graphs to confirm it worked

# pdf(paste0(dir,'tableau/tableau_indicators.pdf'), height=6, width=12)
# 
# base <- dt [set=='base',.(count=sum(value, na.rm=T)), by=.(element_eng, date)]
# 
# ggplot(base, aes(x=date, y=count, color=element_eng)) +
#   geom_point() +
#   geom_line() +
#   theme_bw() +
#   scale_y_continuous(labels = scales::comma)
# 
# sigl <- dt [set=='sigl',.(count=sum(value, na.rm=T)), by=.(element_eng, date)]
# 
# ggplot(sigl, aes(x=date, y=count, color=element_eng)) +
#   geom_point() +
#   geom_line() +
#   theme_bw() +
#   scale_y_continuous(labels = scales::comma)
# 
# 
# pnls <- dt [set=='pnls',.(count=sum(value, na.rm=T)), by=.(element_eng, element_id, date, category)]
# 
# ggplot(pnls[element_id=='jJuipTLZK4o' | element_id=='Gv1UQdMw5wL'], aes(x=date, y=count, color=category)) +
#   geom_point() +
#   geom_line() +
#   theme_bw() +
#   facet_wrap(~element_eng) +
#   scale_y_continuous(labels = scales::comma)
# 
# ggplot(pnls[element_id=="DXz4Zxd4fKq"], aes(x=date, y=count, color=category)) +
#   geom_point() +
#   geom_line() +
#   theme_bw() +
#   facet_wrap(~element_eng) +
#   scale_y_continuous(labels = scales::comma)
# 
# ggplot(pnls[element_id=="gHBcPOF5y3z"], aes(x=date, y=count, color=category)) +
#   geom_point() +
#   geom_line() +
#   theme_bw() +
#   facet_wrap(~element_eng) +
#   scale_y_continuous(labels = scales::comma)
# 
# dev.off()

#------------------------------
# add umlauts to merge with tableau

dt[dps=='Kasai', dps:='Kasaï']
dt[dps=='Kasai Central', dps:='Kasaï Central']
dt[dps=='Kasai Oriental', dps:='Kasaï Oriental']
dt[dps=='Mai-Ndombe', dps:='Maï-Ndombe']

#--------------------------------------------------------

# change type to disease
setnames(dt, 'type', 'disease')
#----------------------------------------------------

#-------------------------------
# Export as an Excel document 

# save as a RDS file 
saveRDS(dt, paste0(dir, 'tableau/tableau_01_2017_12_2018_updated_01_2019.rds'))

# read in the RDS file so you don't have to rerun the code
# tabl <- readRDS(paste0(dir, 'tableau/tableau_01_2017_12_2018_updated_01_2019.rds'))

#------------------------------
# use xlsx instead of csv because it preserves the french special characters
# use the openxlsx package (not xlsx!) as the java in xlsx cannot accomodate size
# openxlsx works on the cluster (no need to install)

# subset dates to just through September due to lag in update of facilites from the time it was downloaded
dt_date_subset <- dt[ date <= "2018-09-01", ]

# start and end date are both inclusive
write.xlsx(dt_date_subset, paste0(dir, 'tableau/tableau_01_2017_09_2018_updated_01_2019.xlsx'))
#-------------------------------




