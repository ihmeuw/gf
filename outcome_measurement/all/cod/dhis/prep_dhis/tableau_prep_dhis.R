# Use prepped SNIS data to create a Tableau data set

# ----------------------------------------------
# Caitlin O'Brien-Carelli / Audrey Batzel
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

# ---------------------------------------------
# directories and files
# ---------------------------------------------
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
# set the directory for input and output
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# input data sets

facilities = readRDS(paste0(dir, 'meta_data/master_facilities.rds'))

# base_data <- 'base_2018_01_01_2019_01_01.rds'
base_data = 'prepped/base_services_prepped_outliers_removed.rds'
pnls_data = 'pre_prep/merged/pnls_subset_2014_11_01_2018_12_01.rds'
sigl_data = 'pre_prep/merged/sigl_2018_01_01_2019_01_01.rds'
sigl_commodities = 'prepped/sigl/sigl_prepped_drugs_received.rds'
pati_data = 'pre_prep/merged/tb_pati_v_registered_2017_01_01_2018_10_01.rds'
ssc_data = 'prepped/ssc_prepped.rds'
supervisions_data = 'prepped/supervisions_prepped.rds'
# archived data for 2017 data:
base_archive = 'prepped/archive/base_services_drc_01_2017_09_2018_prepped.rds'
sigl_archive = 'prepped/archive/sigl_drc_01_2015_07_2018_prepped.rds'

# out files:
out_interim = 'pre_prep/merged/tableau/tableau_interim_4_30_19.rds'
out_final = 'tableau/tableau_prepped_updated_05_07_19.rds'
out_final_basecamp_xlsx = 'tableau/tableau_01_2017_09_2018_updated_05_07_2019.xlsx'
out_final_basecamp_rds = 'tableau/tableau_01_2017_09_2018_updated_05_07_2019.rds'

#functions:
source("./core/standardizeHZNames.R")
source("./core/standardizeDPSNames.R")
# ---------------------------------------------

# ---------------------------------------------
# import the DRC data sets and rbind them together 
# ---------------------------------------------
# import the data sets and subset to the relevant variables
base = readRDS(paste0(dir, base_data))
pnls = readRDS(paste0(dir, pnls_data))
sigl = readRDS(paste0(dir, sigl_data))
pati = readRDS(paste0(dir, pati_data))
ssc = readRDS(paste0(dir, ssc_data))
supervisions = readRDS(paste0(dir, supervisions_data))
base_2017 = readRDS(paste0(dir, base_archive))
sigl_2017 = readRDS(paste0(dir, sigl_archive))
sigl_drugs = readRDS(paste0(dir, sigl_commodities))
base_cpn = readRDS(paste0(dir, "prepped/base_services_prepped.rds")) # temporary while Caitlin adds this back into the data

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
elements_pnls = c('DXz4Zxd4fKq', 'gHBcPOF5y3z', 'jJuipTLZK4o')
elements_ssc = unique(ssc$element_id)
elements_sup = unique(supervisions$element_id)
elements_tbhiv = unique(pati[grepl(element, pattern= "VIH", ignore.case = TRUE), element_id]) 
elements = c(elements_pnls, elements_ssc, elements_sup, elements_tbhiv, elements)

# subset base_2017 and sigl_2017 to just the 2017 data and rbind with base and sigl
base_2017 = base_2017[year == 2017, ]
sigl_2017 = sigl_2017[year == 2017, ]

# subset elements before combining on some of the data sets since data sets are so large
pnls = pnls[element_id %in% elements, ]
base = base[element_id %in% elements, ]
base_2017 = base_2017[element_id %in% elements, ]
sigl_2017 = sigl_2017[element_id %in% elements, ]
sigl = sigl[element_id %in% elements, ]
pati = pati[element_id %in% elements, ]
base_cpn = base_cpn[element_eng == "A 2.1 CPN 1", ]

base = rbindlist(list(base, base_2017), use.names = TRUE, fill = TRUE)
rm(base_2017)
sigl = rbindlist(list(sigl, sigl_2017), use.names = TRUE, fill = TRUE)
rm(sigl_2017)

# create a data set identifier
base[ ,set:='base']
base_cpn[, set := 'base']
pnls[ ,set:='pnls']
sigl[ ,set:='sigl']
pati[ ,set:='pati_tb']
ssc[, set:='secondary_services_(ssc)']
supervisions[, set:='supervisions']

drop_cols <- c("coordinates", "download_number", "last_update", "mtk", "tableau", "opening_date", "type", "country", "drug", "month", "year")
base <- base[ , !(drop_cols), with = FALSE]
base_cpn <- base_cpn[ , !(drop_cols), with = FALSE]
sigl <- sigl[ , !(drop_cols), with = FALSE]
pati <- pati[ , !(drop_cols), with = FALSE]
ssc <- ssc[ , !(drop_cols), with = FALSE]
supervisions <- supervisions[ , !(drop_cols), with = FALSE]
pnls <- pnls[ , !(drop_cols), with = FALSE]

ssc[, date := as.Date(date)]
supervisions[, date := as.Date(date)]

dt = rbindlist( list(base, sigl, pnls, pati, ssc, supervisions, base_cpn), use.names = TRUE, fill= TRUE )
if (nrow(base) + nrow(sigl) + nrow(pnls) + nrow(pati) + nrow(ssc) + nrow(supervisions)!= nrow(dt)) stop ("rbind did not work correctly")
dt$year = year(dt$date)
# ---------------------------------------------

# ---------------------------------------------
# save the interim data set so you don't need to load all the data every time
saveRDS(dt, paste0(dir, out_interim))
dt = readRDS(paste0(dir, out_interim))
dt = dt[!data_set %in% c('ssc', 'supervisions')]
# ---------------------------------------------

# ---------------------------------------------
# level is na in random places - fix this:
# ---------------------------------------------
dt[is.na(health_zone) & org_unit_type %in% c("health_zone", "health zone"), level := "health_zone"]
dt[is.na(health_zone) & org_unit_type %in% c("health_zone", "health zone"), health_zone := org_unit]

dt[, c('org_unit', 'org_unit_type', 'level', 'data_set'):= NULL] 
facilities[, c('health_area', 'health_zone', 'dps', 'opening_date', 'coordinates', 'country') := NULL ]
facilities[ is.na(level) & org_unit_type == "health_area", level := "health_area"]
facilities[ is.na(level) & org_unit_type == "health_zone", level := "health_zone"]
facilities[ is.na(level) & org_unit_type == "dps", level := "dps"]

dt = merge(dt, facilities, all.x = TRUE, by = c('org_unit_id'))

dt[is.na(level) & grepl(org_unit, pattern = "polyc", ignore.case= TRUE), level := "polyclinic"]
dt[is.na(level) & grepl(org_unit, pattern = "posta", ignore.case= TRUE), level := "health_post"]
dt[is.na(level) & grepl(org_unit, pattern = "dical", ignore.case= TRUE), level := "medical_center"]
dt[is.na(level) & grepl(org_unit, pattern = "baringa", ignore.case= TRUE), level:="reference_health_center"]
dt[is.na(level) & grepl(org_unit, pattern = "hosp", ignore.case= TRUE), level:="hospital_center"]
dt[is.na(level) & grepl(org_unit, pattern = "centre * de", ignore.case= TRUE), level:="health_center"]
dt[is.na(level) & grepl(org_unit, pattern = "clin", ignore.case= TRUE), level:="clinic"]
dt[is.na(level) & grepl(org_unit, pattern = "CS", ignore.case= TRUE), level:="health_center"]
dt[is.na(level) & grepl(org_unit, pattern = "Centre * San", ignore.case= TRUE), level:="health_center"]
dt[is.na(level) & grepl(org_unit, pattern = "Centrede", ignore.case= TRUE), level:="health_center"]
dt[is.na(level) & grepl(org_unit, pattern = "Cente de san", ignore.case= TRUE), level:="health_center"]
dt[is.na(level) & grepl(org_unit, pattern = "CH", ignore.case= FALSE), level:="hospital_center"]
dt[is.na(level) & grepl(org_unit, pattern = "spital", ignore.case= FALSE), level:="hospital"]

# bind ssc and supervisions back on
dt = rbindlist( list(dt, ssc, supervisions), use.names = TRUE, fill= TRUE )

dt[, health_zone := standardizeHZNames(health_zone)]
dt[, dps := standardizeDPSNames(dps)]

# hacky fix for now - the only health zone that didn't work was 'mont-ngafula' (because normally it is either mont-ngafula-ii or mont-ngafula-i) 
dt[is.na(health_zone), health_zone := 'mont-ngafula']

# sum over health zone because standardize function creates duplicates of masisi and uvira in SNIS
dt$value <- as.numeric(dt$value) # the NAs introduced here were listed as "NULL" before when the value was character type
check = dt[, .(value = sum(value)), by = .(set, element, element_eng, element_id, date, dps, health_zone, category, level, org_unit_id, org_unit, 
                                         health_area, quarter, org_unit_type)]
# ---------------------------------------------

# ---------------------------------------------
# check the english translations of the elements
# ---------------------------------------------
# read in csv of data elements with name changes:
names = read.csv(paste0(dir, 'pre_prep/merged/tableau/rename_vars_tableau.csv'))
names = as.data.table(names)
names = names[, .(element_id, rename)]

dt = merge(dt, names, all.x= TRUE, by = "element_id")
dt[is.na(rename), rename:= element_eng]
dt[, element_eng := NULL]
setnames(dt, "rename", "element_eng")
# ---------------------------------------------

# ---------------------------------------------
# only keep data from 2017 on:
dt = dt[date >= "2017-01-01",]

# test unique identifiers:
if (nrow(dt) != nrow( unique( dt[, .(date, org_unit_id, health_zone, dps, element, category)]))) stop('Unique identifiers do not uniquely identify rows in dt')

# create a variable for the number of facilities reporting at the health zone level
fac_reporting = dt[, .(facilities_reporting = length(unique(org_unit))), by=.(set, element, element_eng, date, dps, health_zone, category, level)]
fac_reporting[ level == "health_zone", facilities_reporting:=1]

# sum over level to change data to the health facility type level 
dt <- dt[, .(value = sum(value, na.rm = TRUE)), by=.(set, date, year, quarter, dps, health_zone, level, element, element_eng, category)] 

# merge in the number of facilities
dt = merge(dt, fac_reporting, by=c('set', 'element', 'element_eng', 'date', 'health_zone', 'dps', 'category', 'level'), all = TRUE)
setnames(dt, "facilities_reporting", "units_reporting")
# ---------------------------------------------

# ---------------------------------------------
# create age and sex variables 
dt[grep(category, pattern='>5'), age:='5+ years']
dt[grep(category, pattern='<5'), age:='Under 5 years']

# code anc visits as female patients
dt[grep(category, pattern='CPN'), sex:='Female']
dt[grep(category, pattern='SA/PP'), sex:='Female']

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
dt[grep(element, pattern='Artesunate'), type:='malaria']
dt[grep(element, pattern='Lumefantrine'), type:='malaria']
dt[grep(element, pattern='RHZE'), type:='tb']
dt[grep(element, pattern='VIH'), type:='hiv']
dt[set=="pati_tb", type:='tb']
dt[grep(element_eng, pattern='LLIN'), type:='malaria']
# ---------------------------------------------

# ---------------------------------------------
# # add umlauts to merge with tableau
# dt[dps=='Kasai', dps:='Kasaï']
# dt[dps=='Kasai Central', dps:='Kasaï Central']
# dt[dps=='Kasai Oriental', dps:='Kasaï Oriental']
# dt[dps=='Mai-Ndombe', dps:='Maï-Ndombe']

# change type to disease
setnames(dt, 'type', 'disease')
# ---------------------------------------------

# ---------------------------------------------
# Export tableau data set

# save as a RDS file 
saveRDS(dt, paste0(dir, out_final))

# use xlsx instead of csv because it preserves the french special characters
# use the openxlsx package (not xlsx!) as the java in xlsx cannot accomodate size
# openxlsx works on the cluster (no need to install)

# subset dates to just through September due to lag in update of facilites from the time it was downloaded
dt_date_subset <- dt[ date <= "2018-09-01", ]

# start and end date are both inclusive
write.xlsx(dt_date_subset, paste0(dir, out_final_basecamp_xlsx))
saveRDS(dt_date_subset, paste0(dir, out_final_basecamp_rds))
# ---------------------------------------------

# ---------------------------------------------
# ---------------------------------------------
# other code:
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
# ---------------------------------------------