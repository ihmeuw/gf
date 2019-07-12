#---------------------------------------------------
# AUTEUR: Emily Linebarger
# EMAIL: elineb@uw.edu
# OBJECTIF: Finaliser les données préparées pour 2016-2017 et 2018
#    VIH/TB du PNLT
# DATE: Juin 2019
#---------------------------------------------------

#Configurer l'espace de travail R 
rm(list=ls())
library(data.table) 
library(openxlsx)

#Mettre en place des répertoires
raw_dir = "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/aggregated_data/" #Défini où les deux fichiers de données préparés sont enregistrés
save_dir = "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/aggregated_data/" #Où souhaitez-vous sortir les données finalisées?

# Set working directory to the folder where your code is stored:
setwd("C:/local/gf/core/")
# Souce code to rename health zones
source('standardizeHZNames.R')
#----------------------------
#           2018
#----------------------------
#Lire dans les données
dt_2018 = data.table(read.xlsx(paste0(raw_dir, "TB DATA 2018.xlsx")))

#1. Corriger les noms de variables
new_names = c('trimestre', 'csdt', 'population_totale', 'population_couverte', 'presumes_tb', 
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

stopifnot(length(new_names)==ncol(dt_2018)) #Le nombre de nouveaux noms est-il identique à celui des colonnes?

names(dt_2018) = new_names

#Supprimez les quatre premières lignes de noms maintenant qu'elles ne sont plus nécessaires.
dt_2018 = dt_2018[5:nrow(dt_2018), ]

# 2. Faire DPS variable
dt_2018[, dps:=substr(trimestre, 1, nchar(trimestre)-8)]

# 3. Supprimer les lignes contenant les totaux DPS ou de la ZS
total_rows = grep("cplt", tolower(dt_2018$csdt))
dt_2018[total_rows, unique(csdt)] #Review visually 
dt_2018 = dt_2018[!total_rows]
# copy over column csdt to "zone_sante" to try to separate out which rows are health zone totals
dt_2018[, zone_sante := tolower(csdt)]
dt_2018[grepl(zone_sante, pattern = 'zs'), is_zs := TRUE]

#4. Extraire l'année et le trimestre et créer une variable de date
dt_2018[, year:=2018]

dt_2018[grepl(trimestre, "T1"), date:=as.Date("01-01-2018", format="%m-%d-%Y")]
dt_2018[grepl(trimestre, "T2"), date:=as.Date("04-01-2018", format="%m-%d-%Y")]
dt_2018[grepl(trimestre, "T3"), date:=as.Date("07-01-2018", format="%m-%d-%Y")]
dt_2018[grepl(trimestre, "T4"), date:=as.Date("10-01-2018", format="%m-%d-%Y")]

#Sauvegarder les données finales
write.csv(dt_2018, paste0(save_dir, "TB_2018_FINAL.csv"), row.names=F)

#----------------------------
#         2016-2017
#----------------------------
#Lire dans les données
dt_2016_2017 = data.table(read.xlsx(paste0(raw_dir, "TB DATA 2016_2017.xlsx")))

#1. Corriger les noms de variables
new_names = c('trimestre', 'zone_sante', 'population_totale', 'population_couverte', 'presumes_tb', 
              'frottis_effectue', 'frottis_positif', 'presumes_tb_ziehl', 'presumes_tb_ziehl_positif', 
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

stopifnot(length(new_names)==ncol(dt_2016_2017)) #Le nombre de nouveaux noms est-il identique à celui des colonnes?

names(dt_2016_2017) = new_names

#Supprimez les quatre premières lignes de noms maintenant qu'elles ne sont plus nécessaires.
dt_2016_2017 = dt_2016_2017[5:nrow(dt_2016_2017), ]

# 2. Supprimer le nombre total de lignes 
total_rows = grep("total", tolower(dt_2016_2017$zone_sante))
dt_2016_2017[total_rows, .(zone_sante)] #Review visuellement
dt_2016_2017 = dt_2016_2017[!total_rows]

#3. Extrait DPS et département
dt_2016_2017[, dps:=substr(trimestre, 1, nchar(trimestre)-8)]

#4. Extraire l'année et le trimestre et créer une variable de date
dt_2016_2017[, year:=substr(trimestre, nchar(trimestre)-3, nchar(trimestre))]
dt_2016_2017[, year:=as.numeric(year)]

dt_2016_2017[grepl(trimestre, "T1"), date:=as.Date(paste0("01-01-", year), format="%m-%d-%Y")]
dt_2016_2017[grepl(trimestre, "T2"), date:=as.Date(paste0("04-01-", year), format="%m-%d-%Y")]
dt_2016_2017[grepl(trimestre, "T3"), date:=as.Date(paste0("07-01-", year), format="%m-%d-%Y")]
dt_2016_2017[grepl(trimestre, "T4"), date:=as.Date(paste0("10-01-", year), format="%m-%d-%Y")]

#Sauvegarder les données finales
write.csv(dt_2016_2017, paste0(save_dir, "TB_2016_2017_FINAL.csv"), row.names=F)


