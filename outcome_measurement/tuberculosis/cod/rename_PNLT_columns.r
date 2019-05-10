# ----------------------------------------------
# David Phillips
# Code to work on for cleaning PNLT data

# Instructions:
# 1. Explore the output from "PNLT_prep_code_example.R"
# 2. Identify a group of sheets that have an identical format
# 3. List the column names in a new vector "names2" (make them match names1)
# 4. Set the names of those sheets to be the cleaned names
# 5. Repeat
# ----------------------------------------------


# --------------------
# Set up R / install packages
# note: if a package is not installed, run install.packages('package.name'); then, rerun library(package.name) 
rm(list=ls())
library(data.table)  
library(reshape2)
library(stringr)
library(readxl)
library(zoo)
# --------------------


# --------------------------------------------------------
# Files and Directories

# directory where you stored the data
dir = 'C:/local/Basecamp_PNLT_Data/'  # where the TB files are stored 

# which sheet type do you want to prep?
# This code preps “DEP”, “EVAL”, “AGE SEX”, “SUIVI BACT” separately
sheet_type = 'DEP'

# input file
inFile = paste0(dir, 'appended_', sheet_type, '_data.csv') 

# output file
outFile = paste0(dir, 'cleaned_', sheet_type, '_data.csv') 
# --------------------------------------------------------


# -------------------------------------------------------------------------------
# Set column names for each individual sheet

# load list of sheets from previous code
cleanedSheets = readRDS(inFile)

# define a bunch of lists of possible name arrangements (done manually)
names1 = c('n', 'csdt_zs', 'population_totale', 'population_couverte', 'presumes_tb', 'frottis_effectues_np_et_rech', 'frottis_positifs_np_et_rech', 'presumes_tb_soumis_a_lexamen_ziehl_auramine', 'presumes_tb_soumis_a_lexamen_ziehl_auramine_positif', 'presumes_soumis_au_genexpert', 'mtb_detecte_resistance_a_rifampicine_non_detectee_mtb_rif', '_rr', 'invalide_aucun_resultat_erreur', 'nouveau_patient', 'rechute', 'apres_echec', 'apres_perdu_de_vue', 'nouveau_patient', 'rechute', 'hors_rechute', 'nouveau_patient', 'rechute', 'hors_rechute', 'autre_patient_deja_traite', 'total_des_cas_incident_np_et_rech', 'total_des_cas', 'nombre_de_cas_ayant_ete_orientes_par_la_communaute', 'total_patients_en_traitement_et_qui_ont_reçu_une_forme_de_soutien_a_lobservance_du_traitement_de_la_communaute', 'teste_au_vih_connaissant_leur_statut', 'vih', 'vih_sous_cotri', 'vih_sous_tarv', 'teste_au_vih_connaissant_leur_statut', 'vih', 'vih_sous_cotri', 'vih_sous_tarv', 'nombre_des_pvvih_avec_recherche_de_la_tb', 'nombre_des_pvvih_exclus_de_la_tb', 'nombre_des_pvvih_mis_sous_linh', 'presumes_tb_mr_rr', 'confirmes_tbmr_rr', 'confirmes_tb_xdr', 'presumes_tb_mr_rr', 'confirmes_tbmr_rr', 'confirmes_tb_xdr', 'presumes_tb_mr_rr', 'confirmes_tbmr_rr', 'confirmes_tb_xdr', 'presumes_tb_mr_rr', 'confirmes_tbmr_rr', 'confirmes_tb_xdr', 'zs', 'hzs', 'transfrontalier', 'zs', 'hzs', 'transfrontalier', 'enfant_de_0_5_ans_sous_inh', 'prisonniers', 'miniers', 'cas_contact', 'autres', 'total', 'appartenance', 'cas', 'file', 'sheet', 'year', 'quarter', 'dps')
names2 = c(KEEP GOING)

# set names for sheets that have identical formats (please double check)
for(s in c(1:8, 11, 12, 17:44, 51, 52, 100, 105, 107:116, 134:143)) setnames(cleanedSheets[[s]], names1)
for(s in c(OTHER SHEETS)) setnames(cleanedSheets[[s]], names2)
# -------------------------------------------------------------------------------


# --------------------------------------------
# Append all sheets into one dataset and save

# append
data = rbindlist(cleanedSheets)

# save a csv file
write.csv(data, outFile, row.names=FALSE)
# --------------------------------------------
