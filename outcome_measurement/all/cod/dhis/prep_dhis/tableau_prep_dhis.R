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
prepped_folder = 'prepped/'

# input data sets
base_data <- 'base_2018_01_01_2019_01_01.rds'
pnls_data <- 'pnls_subset_2014_11_01_2018_12_01.rds'
sigl_data <- 'sigl_2018_01_01_2019_01_01.rds'
pati_data <- 'tb_pati_v_registered_2017_01_01_2018_10_01.rds'
ssc_data <- 'ssc_prepped.rds'
supervisions_data <- 'supervisions_prepped.rds'

# archived data for 2017 data:
base_archive <- 'prepped/archive/base_services_drc_01_2017_09_2018_prepped.rds'
sigl_archive <- 'prepped/archive/sigl_drc_01_2015_07_2018_prepped.rds'
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
             "WjEIQZvEFqa", "ncXfF8VViSh", "KEv4JxAgpFK", "kGmdIbd3mbn",
             "fsBcCCKHGUx", "pOZcn5GNnRD", "okUsVMBrhZC", "IhUBOQkWKoo",
             "pgHWKBydVPw", "B7A1sbUixQL", "SP3Kv0MybgA", "QoJw73JZCkb",
             "MMU2mFrLpBb", "hLVx9BdEkVZ", "AIlNCyG5OOX", "ti2YQSNVmw1",
             "nB8OBp7TIVR", "sDKDNVBDNYZ", "Jy7o0gHyA1W", "fIVevsf0eej",
             "ANzWXWvvbX1", "OF6Jci2VF2x", "BtF9VwZSqdz", "rEtqA3DojjN", 
             "AxeUO0gRsAR", "HJVFrQH0Tn4", "S40yEtRViXS", "zN8t1RPspMj", 
             "jxW9eJlwooQ", "RWdaG7J6RiH")
           #  "eeE4fgP5fml", "DQkITx9AQjm",  # TB-HIV
           #  "QStu7Ux1Loa", "sJRCCgJJBHD", "eO8nthpRoT4", "FMLmCNm7QTf",
           #  "dg8e1fPD4pb")


# import the data sets and subset to the relevant variables
base = readRDS(paste0(dir, import_folder, base_data))
pnls = readRDS(paste0(dir, import_folder, pnls_data))
sigl = readRDS(paste0(dir, import_folder, sigl_data))
pati = readRDS(paste0(dir, import_folder, pati_data))
ssc = readRDS(paste0(dir, prepped_folder, ssc_data))
supervisions = readRDS(paste0(dir, prepped_folder, supervisions_data))

base_2017 = readRDS(paste0(dir, base_archive))
sigl_2017 = readRDS(paste0(dir, sigl_archive))

# subset base_2017 and sigl_2017 to just the 2017 data and rbind with base and sigl
base_2017 = base_2017[year == 2017, ]
sigl_2017 = sigl_2017[year == 2017, ]
base = rbindlist(list(base, base_2017), use.names = TRUE, fill = TRUE)
sigl = rbindlist(list(sigl, sigl_2017), use.names = TRUE, fill = TRUE)

# create a data set identifier
base[ ,set:='base']
pnls[ ,set:='pnls']
sigl[ ,set:='sigl']
pati[ ,set:='pati']
ssc[, set:='ssc']
supervisions[, set:='supervisions']

# subset elements before combining since data sets are so large
pnls = pnls[element_id %in% elements, ]
pnls[, country:= "R?publique D?mocratique du Congo"]
base = base[element_id %in% elements, ]
sigl = sigl[element_id %in% elements, ]
pati = pati[element_id %in% elements, ]
ssc = ssc[element_id %in% elements, ]
supervisions = supervisions[element_id %in% elements, ]

drop_cols <- c("coordinates", "download_number", "last_update")
base <- base[ , !(drop_cols), with = FALSE]
sigl <- sigl[ , !(drop_cols), with = FALSE]
pati <- pati[ , !(drop_cols), with = FALSE]
ssc <- ssc[ , !(drop_cols), with = FALSE]
supervisions <- supervisions[ , !(drop_cols), with = FALSE]

dt = rbindlist( list(base, sigl, pnls, pati, ssc, supervisions), use.names = TRUE, fill= TRUE )
if (nrow(base) + nrow(sigl) + nrow(pnls) + nrow(pati) + nrow(ssc) + nrow(supervisions)!= nrow(dt)) stop ("rbind did not work correctly")

# ---------------------------------------------

# ---------------------------------------------
# save the interim data set so you don't need to load all the data every time
saveRDS(dt, paste0(dir, 'pre_prep/merged/tableau/tableau_interim_3_4_19_(sigl_updated_2017_included).rds'))
# dt = readRDS(paste0(dir, 'pre_prep/merged/tableau/tableau_interim_3_4_19_(sigl_updated).rds'))
# ---------------------------------------------

# ---------------------------------------------
# check the english translations of the elements
# read in csv of data elements with name changes:
names = read.csv(paste0(dir, import_folder, 'tableau/rename_vars_tableau.csv'))
names = as.data.table(names)
names = names[, .(element_id, rename)]

dt = merge(dt, names, by = "element_id")
dt[, element_eng := NULL]
setnames(dt, "rename", "element_eng")
# ---------------------------------------------

# ---------------------------------------------
# only keep data from 2017 on:
dt = dt[date >= "2017-01-01",]

# test unique identifiers:
if (nrow(dt) != nrow( unique( dt[, .(date, org_unit_id, element, category)]))) stop('Unique identifiers do not uniquely identify rows in dt')

# create a variable for the number of facilities reporting at the health zone level
facilities = dt[, .(facilities_reporting = length(unique(org_unit))), 
                 by=.(set, element, element_eng, date, dps, health_zone, category, level)]

# sum over health zone to change data to the health facility type level 
dt$value <- as.numeric(dt$value) # the NAs introduced here were listed as "NULL" before when the value was character type
dt <- dt[, .(value = sum(value)), 
          by=.(set, element, element_eng, date, dps, health_zone, category, level)] 

# merge in the number of facilities
dt = merge(dt, facilities, by=c('set', 'element', 'element_eng', 'date', 'health_zone', 'dps', 'category', 'level'), all = TRUE)
# ---------------------------------------------

# ---------------------------------------------
# create age and sex variables 
dt[grep(category, pattern='>5'), age:='5+ years']
dt[grep(category, pattern='<5'), age:='Under 5 years']

# code anc visits as female patients
dt[grep(category, pattern='CPN'), sex:='Female']
dt[grep(category, pattern='SA/PP'), sex:='Female']
# dt[category=='5 and over', age:='5 +']
# dt[category=='Under 5', age:='Under 5']
# dt[category=='F?minin, 1 et 4 ans', age:='1 - 4 years']
# dt[category=='F?minin, 10 et 14 ans', age:='10 - 14 years']
# dt[category=='F?minin, 15 et 19 ans', age:='15 - 19 years']
# dt[category=='F?minin, 20 et 24 ans', age:='20 - 24 years']
# dt[category=='F?minin, 25 et 49 ans', age:='25 - 49 years']
# dt[category=='F?minin, 5 et 9 ans', age:='5 - 9 years']
# dt[category=='F?minin, 50 ans et plus', age:='50 +']
# dt[category=='F?minin, Moins d\'un an', age:='< 1 year']
# 
# dt[category=="F?minin, Moins de 14 ans", age:='< 14 years']
# dt[category=="F?minin, 15 et 24 ans", age:='15 - 24 years']
# dt[category=="F?minin, 25 ans et plus", age:='25+ years']
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
dt[grep(element, pattern='FE'), sex:='Female']
dt[grep(element, pattern='CPN'), sex:='Female']
dt[grep(element_eng, pattern='LLIN'), sex:=NA]

# fix type
dt[set=='pnls', type:='hiv']
dt[set=="base", type:='malaria']
dt[set=="pati", type:='tb']
dt[grep(element, pattern='Artesunate'), type:='malaria']
dt[grep(element, pattern='Lumefantrine'), type:='malaria']
dt[grep(element, pattern='RHZE'), type:='tb']
dt[grep(element, pattern='VIH'), type:='hiv']
dt[grep(element_eng, pattern='LLIN'), type:='malaria']
# ---------------------------------------------

# ---------------------------------------------
# test graphs to confirm it worked
# ---------------------------------------------
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
# ---------------------------------------------

# ---------------------------------------------
# add umlauts to merge with tableau
dt[dps=='Kasai', dps:='Kasa?']
dt[dps=='Kasai Central', dps:='Kasa? Central']
dt[dps=='Kasai Oriental', dps:='Kasa? Oriental']
dt[dps=='Mai-Ndombe', dps:='Ma?-Ndombe']

# change type to disease
setnames(dt, 'type', 'disease')
# ---------------------------------------------

# ---------------------------------------------
# Export tableau data set

# save as a RDS file 
saveRDS(dt, paste0(dir, 'tableau/tableau_prepped_updated_03_04_2019.rds'))

# use xlsx instead of csv because it preserves the french special characters
# use the openxlsx package (not xlsx!) as the java in xlsx cannot accomodate size
# openxlsx works on the cluster (no need to install)

# subset dates to just through September due to lag in update of facilites from the time it was downloaded
dt_date_subset <- dt[ date <= "2018-09-01", ]

# start and end date are both inclusive
write.xlsx(dt_date_subset, paste0(dir, 'tableau/tableau_01_2017_09_2018_updated_03_04_2019.xlsx'))
saveRDS(dt_date_subset, paste0(dir, 'tableau/tableau_01_2017_09_2018_updated_03_04_2019.rds'))
#-------------------------------




