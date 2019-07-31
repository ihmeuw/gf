#---------------------------------------------------
# AUTEUR: Emily Linebarger & Audrey Batzel
# EMAIL: elineb@uw.edu / abatzel@uw.edu
# OBJECTIF: Finaliser les donn√©es pr√©par√©es pour 2016-2017 et 2018
#    VIH/TB du PNLT
# DATE: Juin / Juillet 2019
#---------------------------------------------------

#---------------------------------------------------
# Configurer l'espace de travail R 
#---------------------------------------------------
rm(list=ls())
library(data.table) 
library(openxlsx)
library(zoo)

# Mettre en place des r√©pertoires
raw_dir = "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/aggregated_data/" #D√©fini o√π les deux fichiers de donn√©es pr√©par√©s sont enregistr√©s
save_dir = "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/aggregated_data/" #O√π souhaitez-vous sortir les donn√©es finalis√©es?

# Output files:
outFile18 = "TB_2018_FINAL.csv"

# DÈfinissez le rÈpertoire de travail dans le dossier o˘ vous avez enregistrÈ le fichier de code appelÈ 'standardizeHZNames.R'
setwd("C:/local/gf/")

# Source le code ‡ renommer les zones de santÈ
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')

check_hzs = readRDS('J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/imputedData_run_0_001_aggVars_lagsLeads_condensed_hz_median.rds')
check_hzs = unique(check_hzs[, .(dps, health_zone)])
num_hzs_by_dps = check_hzs[, .(num_hzs = .N), by = 'dps']
num_hzs_by_dps[ dps == 'bas-congo', dps := 'kongo-central']
#---------------------------------------------------

#---------------------------------------------------
#                DonnÈes 2018 
#---------------------------------------------------
# Lire dans les donn√©es
#dt_2018_orig = data.table(read.xlsx(paste0(raw_dir, "TB DATA 2018.xlsx")))
#dt_2018_add = data.table(read.xlsx(paste0(raw_dir, "DATA TB 6 dps 2018.xlsx")))
dt_2018 = data.table(read.xlsx(paste0(raw_dir, 'tb data 2018 all dps with health zone column.xlsx'))) # data Constant updated manually

# Supprimez les quatre premi√®res lignes de noms maintenant qu'elles ne sont plus n√©cessaires.
dt_2018_orig = dt_2018_orig[5:nrow(dt_2018_orig), ]
dt_2018_add = dt_2018_add[5:nrow(dt_2018_add), ]

dt_2018 = rbindlist(list(dt_2018_orig, dt_2018_add), use.names = TRUE)

# Corriger les noms de variables
#----
new_names = c('trimestre', 'csdt', 'population_totale', 'population_couverte', 'presume_tb_teste_microscope', 
              'presume_tb_teste_microscope', 'presume_tb_positif_microscope', 'presume_tb_teste_xpert', 'presume_tb_positive_xpert', 
              'frottis_effectue', 'frottis_positif', 'csdt_participe_cq', 'cas_enreg_bac_nouveau', 'cas_enreg_bac_rechute', 
              'cas_enreg_bac_hors_rechutes', 'cas_enreg_bac_enfants', 'cas_enreg_clinique_noveau', 'cas_enreg_clinique_rechute', 
              'cas_enreg_clinique_hors_rechutes', 'cas_enreg_clinique_enfants', 'cas_extrapul_enreg_nouveau', 
              'cas_extrapul_enreg_rechute', 'cas_extrapul_enreg_hors_rechute', 'cas_extrapul_enreg_enfants', 
              'autre_patient_deja_traite', 'total_de_cas_incident', 'total_de_cas', 
              'tb_teste_vih', 'tb_vih_positif', 
              'tb_vih_positif_cotri', 'tb_vih_positif_tarv', 'pvvih_avec_tb', 'pvvih_sans_tb', 'pvvih_sous_inh', 
              'enfant_0_5_vivant_maison_avec_tb', 'enfant_0_5_teste_tb', 'enfant_0_5_positif_tb', 'enfant_0_5_sous_inh', 
              'prisionniers_positif_tb', 'prisionniers_traite_tb', 'mineurs_positif_tb', 'mineurs_traite_tb', 'cas_contact_positif_tb', 
              'cas_contact_traite_tb', 'cas_oriente_reco_oac', 'cas_recu_soutien_reco_oac', 'sous_traitement_initial_enfant', 
              'sous_traitement_initial_adulte', 'sous_retraitement_enfant', 'sous_retraitement_adulte', 'csdt_rupture_7_jour')
#----

stopifnot(length(new_names)==ncol(dt_2018)) #Le nombre de nouveaux noms est-il identique √† celui des colonnes?

names(dt_2018) = new_names

# Faire DPS variable
dt_2018 = dt_2018[trimestre != 'TOTAL  CPLT ',  ] # enlever cette rangÈe
dt_2018[, dps:=substr(trimestre, 1, nchar(trimestre)-8)]
dt_2018[dps == 'EQU', dps := 'EQUATEUR']
if (length(unique(dt_2018$dps)) != 27) stop('Certaines DPS manquent!') #Il y a 27 dps parce que Kongo Cetnral est divisÈ en Est et Ouest

# Extraire l'ann√©e et le trimestre et cr√©er une variable de date
dt_2018[, year:=2018]

dt_2018[grepl(trimestre, pattern = "T1"), date:=as.Date("01-01-2018", format="%m-%d-%Y")]
dt_2018[grepl(trimestre, pattern = "T2"), date:=as.Date("04-01-2018", format="%m-%d-%Y")]
dt_2018[grepl(trimestre, pattern = "T3"), date:=as.Date("07-01-2018", format="%m-%d-%Y")]
dt_2018[grepl(trimestre, pattern = "T4"), date:=as.Date("10-01-2018", format="%m-%d-%Y")]

# VÈrifier que tous les DPS ont chaque trimestre de donnÈes
check = dt_2018[, length(unique(date)), by = 'dps']
if (length(check[V1 != 4, dps]) != 0) stop("Au moins un quart d'au moins un dps manqueant dans les donnÈes")

# Supprimer les lignes contenant les totaux DPS ou de la ZS
total_rows = grep("cplt", tolower(dt_2018$csdt))
dt_2018[total_rows, unique(csdt)] # Revoir visuellement
dt_2018 = dt_2018[!total_rows]
# Copiez la colonne 'cdst' dans 'zone_sante' afin d'essayer de sÈparer les lignes correspondant aux totaux de la zone de santÈ.
dt_2018[, zone_sante := tolower(csdt)]
dt_2018[, zone_sante := trimws(zone_sante)]

#--------------------------------------------------------
# CHECKING HZs MANUALLY:
#-----------------
# print(paste("Iteration:", i))
# if (grepl(unique(dt_2018$dps)[i], pattern = 'kongo central', ignore.case = TRUE)) {
#   print(unique(dt_2018$dps)[i])
#   zs_pnlt = dt_2018[ grepl('kongo central', dps, ignore.case = TRUE) & date == '2018-01-01' & grepl('zs', zone_sante, ignore.case=TRUE), zone_sante]
#   print(zs_pnlt)
#   zs_pnlt = trimws(zs_pnlt)
#   zs_all = check_hzs[ dps == 'bas-congo', health_zone]
#   print(zs_all)
# } else {
#   print(unique(dt_2018$dps)[i])
#   zs_pnlt = dt_2018[ dps == unique(dt_2018$dps)[i] & date == '2018-01-01', zone_sante]
#   print(zs_pnlt)
#   zs_pnlt = trimws(zs_pnlt)
#   if (unique(dt_2018$dps)[i] != 'KWILU' & unique(dt_2018$dps)[i] != 'SUD KIVU' & unique(dt_2018$dps)[i] != 'KASAI ORIENTAL') zs_pnlt = zs_pnlt[ grepl('zs', zs_pnlt, ignore.case=TRUE)]
#     if (unique(dt_2018$dps)[i] == 'SUD KIVU') {
#       zs_pnlt = zs_pnlt[ zs_pnlt %in% c('bagira', 'ibanda', 'bunyakiri', 'kalonge', 'fizi', 'idjwi', 'kabare', 'katana', 'kalehe', 'miti-murhesa', 'kaziba', 'mwana', 'lemera', 'minova', 'mwenga',
#                   'kamituga', 'kitutu', 'nundu', 'minembwe', 'itombwe', 'nyangezi', 'nyantende', 'shabunda', 'kalole', 'lulingu', 'uvira', 'ruzizi', 'hauts plateaux',
#                   'walungu', 'mubumbano', 'kaniola', 'kimbilulenge ', 'mulungu', 'kadutu')]
#     }
#     if (unique(dt_2018$dps)[i] == 'KASAI ORIENTAL') {
#       zs_pnlt = zs_pnlt[zs_pnlt %in% c('bipemba', 'bonzola', 'dibindi', 'diulu', 'kansele', 'lubilanji', 'lukelenge', 'muya', 'mpokolo', 'nzaba', 'bibanga', 'kabeya kamuanga', 'kasansa', 'miabi', 'mukumbi',
#                   'tshilenge', 'tshilundu', 'tshitenge', 'tshishimbi')]
#     }
#     if (unique(dt_2018$dps)[i] == 'KWILU') zs_pnlt = zs_pnlt[ !grepl(zs_pnlt, pattern = 'csdt')]
#     if (unique(dt_2018$dps)[i] == 'KWILU') zs_pnlt = zs_pnlt[ !zs_pnlt %in% c('fatunda', 'lukuni', 'misay')]
#     if (unique(dt_2018$dps)[i] == 'MAI NDOMBE') zs_pnlt = c(zs_pnlt, 'banzow', 'bokoro')
#     if (unique(dt_2018$dps)[i] == 'MANIEMA') zs_pnlt = c(zs_pnlt, 'samba')
#     if (unique(dt_2018$dps)[i] == 'NORD KIVU') zs_pnlt = c(zs_pnlt, '21. manguredjipa')
#     if (unique(dt_2018$dps)[i] == 'KINSHASA') zs_pnlt = c(zs_pnlt, 'bandalungwa', 'nsele')
# 
#   zs_all = check_hzs[ dps == standardizeDPSNames(unique(dt_2018$dps)[i]), health_zone]
#   print(zs_all)
#   print(zs_pnlt)
# }
# # develop rules here to identify hz rows
# zs_pnlt = gsub("zsr", "", zs_pnlt)
# zs_pnlt = gsub("zs", "", zs_pnlt)
# zs_pnlt = gsub("wambaluadi", 'wamba luadi', zs_pnlt)
# zs_pnlt = gsub("maswika", 'musuika', zs_pnlt)
# zs_pnlt = gsub("tot ", "", zs_pnlt)
# zs_pnlt = gsub("total ", "", zs_pnlt)
# zs_pnlt = gsub("de ", "", zs_pnlt)
# zs_pnlt = gsub('[0-9]+', '', zs_pnlt)
# zs_pnlt = gsub('[.]', '', zs_pnlt)
# zs_pnlt = gsub('/', '', zs_pnlt)
# zs_pnlt = trimws(zs_pnlt)
# zs_pnlt = (standardizeHZNames(zs_pnlt))
# 
# zs_all = (standardizeHZNames(zs_all))
# zs_pnlt[!zs_pnlt %in% zs_all]
# zs_all[!zs_all %in% zs_pnlt]
# 
# i = i + 1
#--------------------------------------------------------
test = copy(dt_2018)
# test = test[999:2000, c(1:2, 6)]

ptm <- proc.time()
for(group in unique(test$trimestre)){ 
  counter = min(test[, which(trimestre == group)])
  test[trimestre == group, sum := shift(cumsum(presume_tb_teste_microscope))]
  if ( nrow(test[trimestre == group & presume_tb_teste_microscope == sum, ]) == 0 ) next
  while( counter < max(test[, which(trimestre == group)]) ) { 
    test[c(counter: max(test[, which(trimestre == group)]) ), sum := shift(cumsum(presume_tb_teste_microscope))]
    counter = (max(which(test$presume_tb_teste_microscope == test$sum))+1)
    print(counter)
  }
  test[, is_zs:=(sum==presume_tb_teste_microscope)]  
}
proc.time() - ptm

data = copy(test)
setnames(data, 'trimestre', 'group')
setnames(data, 'presume_tb_teste_microscope', 'value')

ptm <- proc.time()
data[, idx:=seq_len(.N), by=group]
data[, latest_id:=0]
n_caught_previously=0
n_caught=1
while(n_caught>n_caught_previously) { 
  data[idx>latest_id, is_total:=(value==shift(cumsum(value)) & !is.na(shift(cumsum(value)))), by=group]
  data = merge(data[, -'latest_id'], data[is_total==TRUE, .(latest_id=max(idx)), by=group])
  n_caught_previously = n_caught
  n_caught = sum(data$is_total, na.rm=TRUE) }
proc.time() - ptm

# identify which rows are health zones:
dt_2018[grepl(zone_sante, pattern = 'zs'), is_zs := TRUE]

# # check number of health zones per province and quarter 
# check = dt_2018[, sum(is_zs, na.rm = T), by = c('dps', 'date')]
# check = dcast.data.table(check, dps ~ date )
# check[, dps := standardizeDPSNames(dps)]
# check[is.na(dps), dps := 'kongo-central']
# check = merge(check, num_hzs_by_dps, all = TRUE)

# specialized rules for different conditions in different province data
dt_2018[dps == 'KWILU' & !grepl(zone_sante, pattern = 'csdt'), is_zs := TRUE]
dt_2018[dps == 'KWILU' & zone_sante %in% c('fatunda', 'lukuni', 'misay'), is_zs := NA] # these are in Kikongo, are not health zones. 
dt_2018[dps == 'MAI NDOMBE' & zone_sante %in% c('banzow', 'bokoro'), is_zs := TRUE] # these are hz rows
dt_2018[dps == 'MANIEMA' & zone_sante %in% c('samba'), is_zs := TRUE] 
dt_2018[dps == 'NORD KIVU' & zone_sante %in% c('21. manguredjipa'), is_zs := TRUE]
dt_2018[dps == 'SUD KIVU' & zone_sante %in% c('bagira', 'ibanda', 'bunyakiri', 'kalonge', 'fizi', 'idjwi', 'kabare', 'katana', 'kalehe', 'miti-murhesa', 'kaziba', 'mwana', 'lemera', 'minova', 'mwenga', 
                                              'kamituga', 'kitutu', 'nundu', 'minembwe', 'itombwe', 'nyangezi', 'nyantende', 'shabunda', 'kalole', 'lulingu', 'uvira', 'ruzizi', 'hauts plateaux', 
                                              'walungu', 'mubumbano', 'kaniola', 'kimbilulenge '), is_zs := TRUE]
sud_kivu_zs = dt_2018[, c(which(dps == 'SUD KIVU' & zone_sante == 'kadutu')[2], which(dps == 'SUD KIVU' & zone_sante == 'mulungu')[2]) ]
dt_2018[sud_kivu_zs, is_zs := TRUE]
dt_2018[dps == 'SUD KIVU' & zone_sante == 'kimbilulenge', zone_sante := 'kimbi-lulenge']

dt_2018[dps == 'KASAI ORIENTAL' & zone_sante %in% c('bipemba', 'bonzola', 'dibindi', 'diulu', 'kansele', 'lubilanji', 'lukelenge', 'muya', 'mpokolo', 'bibanga', 'kabeya kamuanga', 'miabi', 'mukumbi', 
                                                    'tshilenge', 'tshilundu', 'tshitenge', 'tshishimbi'), is_zs := TRUE]
kasai_ori_zs = dt_2018[, c(which(dps == 'KASAI ORIENTAL' & zone_sante == 'nzaba')[c(2,4,6,8)], which(dps == 'KASAI ORIENTAL' & zone_sante == 'kasansa')[c(2,4,6,8)]) ]
dt_2018[kasai_ori_zs, is_zs := TRUE]
dt_2018[dps == 'KINSHASA' & zone_sante %in% c('nsele', 'bandalungwa'), is_zs := TRUE] 

# check number of health zones per province and quarter 
check_num_hz = dt_2018[, sum(is_zs, na.rm = T), by = c('dps', 'date')]
check_num_hz = dcast.data.table(check_num_hz, dps ~ date )
check_num_hz[, dps := standardizeDPSNames(dps)]
check_num_hz[is.na(dps), dps := 'kongo-central']
check_num_hz = merge(check_num_hz, num_hzs_by_dps, all = TRUE)
check_num_hz

# TO DO - GET MAI NDOMBE DATA FOR Q3 and Q4 - (it was actually Mongala copied over twice)... for now remove it:
dt_2018[, date := as.character(date)]
dt_2018 = dt_2018[!(dps == 'MAI NDOMBE' & date %in% c('2018-07-01', '2018-10-01')), ]
dt_2018[, date := as.Date(date)]

# keep only health zone names in the zone sante column
dt_2018[is.na(is_zs), is_zs := FALSE]
dt_2018[is_zs == FALSE, zone_sante := NA]  

# standardize the health zone names:
dt_2018[, zone_sante := gsub("zsr", "", zone_sante)]
dt_2018[, zone_sante := gsub("zs", "", zone_sante)]
dt_2018[dps == 'KWANGO', zone_sante := gsub("wambaluadi", 'wamba luadi', zone_sante)]
dt_2018[, zone_sante := gsub("tot ", "", zone_sante)]
dt_2018[, zone_sante := gsub("total ", "", zone_sante)]
dt_2018[, zone_sante := gsub("de ", "", zone_sante)]
dt_2018[dps == 'KASAI CENTRAL', zone_sante := gsub("maswika", 'musuika', zone_sante)]
dt_2018[, zone_sante := gsub('[0-9]+', '', zone_sante)]
dt_2018[, zone_sante := gsub('[.]', '', zone_sante)]
dt_2018[, zone_sante := gsub('/', '', zone_sante)]
dt_2018[, zone_sante := trimws(zone_sante)]
dt_2018[, zone_sante := standardizeHZNames(zone_sante) ] # TO DO: figure out what RIBA is supposed to be in ITURI
dt_2018[is_zs == TRUE & is.na(zone_sante), zone_sante := 'missing'] # to prevent backfilling where it should be something else

# use the function na.locf to backfill health zone names
    # test = dt_2018[ !all(is)]
    # dt_2018[, tmp := sum(is_zs), by= c('dps', 'date')]
    # test = dt_2018[tmp!=0,]

test = copy(dt_2018)
test[nrow(test),zone_sante:='empty']
test[, new_hz:=na.locf(zone_sante, fromLast = TRUE)]
test[zone_sante=='empty', zone_sante:=NA]

# then, delete health zone total rows  
  #TO DO - check that totals for non-hz rows equal totals for hz rows (by province)

# Sauvegarder les donn√©es finales
write.csv(dt_2018, paste0(save_dir, outFile18), row.names = FALSE)
#---------------------------------------------------

#---------------------------------------------------
#               DonnÈes 2016-2017
#---------------------------------------------------
#Lire dans les donn√©es
dt_2016_2017 = data.table(read.xlsx(paste0(raw_dir, "TB DATA 2016_2017.xlsx")))

#1. Corriger les noms de variables
new_names = c('trimestre', 'zone_sante', 'population_totale', 'population_couverte', 'presume_tb_teste_microscope', 
              'frottis_effectue', 'frottis_positif', 'presume_tb_teste_microscope_ziehl', 'presume_tb_teste_microscope_ziehl_positif', 
              'presumes_xpert', 'xpert_mtb_pos_rif_neg', 'xpert_mtb_pos_rif_pos', 'xpert_invalide',
              'tb_bac_nouveau', 'tb_bac_recurrente_rechute', 'tb_bac_recurrente_echec', 'tb_bac_recurrente_perdu', 
              'tb_clinique_nouveau', 'tb_clinique_recurrente_rechute', 'tb_clinique_recurrente_hors_rechute', 
              'cas_extrapul_nouveau', 'cas_extrapul_rechute', 'cas_extrapul_hors_rechute',
              'autre_patient_deja_traite', 'total_de_cas_incident', 'total_de_cas', 'cas_oriente_par_communaute', 
              'cas_recu_soutien_communitaire', 
              'nouveau_tb_connait_vih_pos', 'nouveau_tb_vih_pos', 'nouveau_tb_vih_pos_cotri', 'nouveau_tb_vih_tarv', 
              'autre_tb_connait_vih_pos', 'autre_tb_vih_pos', 'autre_tb_vih_pos_cotri', 'autre_tb_vih_tarv', 
              'pvvih_avec_tb', 'pvvih_sans_tb', 'pvvih_sous_inh', 
              'tbmr_nouveau_presumes', 'tbmr_nouveau_confirme_tbmr_rr', 'tbmr_nouveau_confirme_tb_xdr', 
              'tbmr_recurrente1_presumes', 'tbmr_recurrente1_confirme_tbmr_rr', 'tbmr_recurrente1_confirme_xdr',
              'tbmr_recurrente2_presumes', 'tbmr_recurrente2_confirme_tbmr_rr', 'tbmr_recurrente2_confirme_xdr',
              'tbmr_traitement_presumes', 'tbmr_traitement_confirme_tbmr_rr', 'tbmr_traitement_confirme_xdr',
              'tb_sensible_traitement1_zs', 'tb_sensible_traitement1_hzs', 'tb_sensible_traitement1_transfron',
              'tb_sensible_traitement2_zs', 'tb_sensible_traitement2_hzs', 'tb_sensible_traitement2_transfron',
              'enfant_0_5_sous_inh', 
              'prisionniers', 'miniers', 'cas_contact', 'autres', 'populations_speciales_total', 'appartenance')

stopifnot(length(new_names)==ncol(dt_2016_2017)) #Le nombre de nouveaux noms est-il identique √† celui des colonnes?

names(dt_2016_2017) = new_names

#Supprimez les quatre premi√®res lignes de noms maintenant qu'elles ne sont plus n√©cessaires.
dt_2016_2017 = dt_2016_2017[5:nrow(dt_2016_2017), ]

# 2. Supprimer le nombre total de lignes 
total_rows = grep("total", tolower(dt_2016_2017$zone_sante))
dt_2016_2017[total_rows, .(zone_sante)] #Review visuellement
dt_2016_2017 = dt_2016_2017[!total_rows]

#3. Extrait DPS et d√©partement
dt_2016_2017[, dps:=substr(trimestre, 1, nchar(trimestre)-8)]

#4. Extraire l'ann√©e et le trimestre et cr√©er une variable de date
dt_2016_2017[, year:=substr(trimestre, nchar(trimestre)-3, nchar(trimestre))]
dt_2016_2017[, year:=as.numeric(year)]

dt_2016_2017[grepl(trimestre, "T1"), date:=as.Date(paste0("01-01-", year), format="%m-%d-%Y")]
dt_2016_2017[grepl(trimestre, "T2"), date:=as.Date(paste0("04-01-", year), format="%m-%d-%Y")]
dt_2016_2017[grepl(trimestre, "T3"), date:=as.Date(paste0("07-01-", year), format="%m-%d-%Y")]
dt_2016_2017[grepl(trimestre, "T4"), date:=as.Date(paste0("10-01-", year), format="%m-%d-%Y")]

#Sauvegarder les donn√©es finales
write.csv(dt_2016_2017, paste0(save_dir, "TB_2016_2017_FINAL.csv"), row.names=F)


