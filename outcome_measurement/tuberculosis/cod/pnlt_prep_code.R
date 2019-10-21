#---------------------------------------------------
# Audrey Batzel
# Prep the updated, appended PNLT data for analysis
# August 2019
#---------------------------------------------------

#---------------------------------------------------
# Set up R workspace
#---------------------------------------------------
# Load packages
# install.packages('data.table') # run these two lines of code if you have not
# install.packages('openxlsx')   # already installed these packages
library(data.table)
library(openxlsx)

# Set directories - change these file paths to where you have saved/will save your data
# in_dir = "C:/local/PNLT_data/raw_data/"  # This is the file path where the raw data file(s) are saved on your computer; the input of this R script
# out_dir = "C:/local/PNLT_data/prepped_data/" # This is the file path where you will save the prepped data; the output of this R script

j_dir = "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/hand_appended_data/"
out_dir = paste0(j_dir, 'prepped/')

# Input files:
inFile_2018 = "tb data 2018 all dps with health zone column.xlsx"
inFile_2017 = "tb data 2017 all dps with health zone column.xlsx"
inFile_2016 = "tb_data_2016_2017.xlsx"

# Output files:
outFile = "PNLT_2016_to_2018_all_data_prepped.csv"
outFile_tbhiv = "PNLT_2016_to_2018_TBHIV_prepped.csv" 

# Définissez le répertoire de travail dans le dossier où vous avez enregistré le fichier de code appelé 'standardizeHZNames.R'
setwd("C:/local/gf/")

# Source le code à renommer les zones de santé
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
#---------------------------------------------------

#---------------------------------------------------
# 2018 data
#---------------------------------------------------
# Read in the data
dt_2018 = read.xlsx(paste0(j_dir, inFile_2018))
# Make the data a "data table" 
dt_2018 = data.table(dt_2018) 
# Remove the first four lines of the data - now that the data is in R, we don't want the column names in the rows of data
dt_2018 = dt_2018[5:nrow(dt_2018), ] 

# Change the column names to be easier to use in R: 
new_names = c('dps', 'trimestre','zs', 'csdt', 'population_totale', 'population_couverte', 
              'presumes_tb', 'presumes_tb_teste_microscope', 'presumes_tb_positif_microscope', 'presumes_tb_teste_xpert', 'presumes_tb_positif_xpert', 
              'frottis_effectue', 'frottis_positif', 'csdt_participe_cq', 
              'cas_tb_bac_nouveau', 'cas_tb_bac_rechute', 'cas_tb_bac_horsRechutes', 'cas_tb_bac_enfants', 
              'cas_tb_clinique_noveau', 'cas_tb_clinique_rechute', 'cas_tb_clinique_horsRechutes', 'cas_tb_clinique_enfants', 
              'cas_tb_extrapul_nouveau', 'cas_tb_extrapul_rechute', 'cas_tb_extrapul_horsRechute', 'cas_tb_extrapul_enfants', 
              'autre_patient_deja_traite', 'total_cas_incident', 'total_cas', 
              'tb_teste_vih', 'tb_vih_positif', 'tb_vih_positif_cotri', 'tb_vih_positif_tarv', 
              'pvvih_avec_tb', 'pvvih_sans_tb', 'pvvih_sous_inh', 
              'enfant_0_5_vivant_maison_avec_tb', 'enfant_0_5_teste_tb', 'enfant_0_5_positif_tb', 'enfant_0_5_sous_inh', 
              'prisionniers_positif_tb', 'prisionniers_traite_tb', 'mineurs_positif_tb', 'mineurs_traite_tb', 'cas_contact_positif_tb', 'cas_contact_traite_tb', 
              'cas_oriente_reco_oac', 'cas_recu_soutien_reco_oac', 
              'sous_traitement_initial_enfant', 'sous_traitement_initial_adulte', 'sous_retraitement_enfant', 'sous_retraitement_adulte', 
              'csdt_rupture_7_jour')
names(dt_2018) = new_names

# Make a column for the year
dt_2018[, year := 2018]
# Make a column for the date from the 'trimestre' variable - this will make graphing the data easier
dt_2018[grepl(trimestre, pattern = "T1"), date:="2018-01-01"]
dt_2018[grepl(trimestre, pattern = "T2"), date:="2018-04-01"]
dt_2018[grepl(trimestre, pattern = "T3"), date:="2018-07-01"]
dt_2018[grepl(trimestre, pattern = "T4"), date:="2018-10-01"]
# R has a special variable type for dates, so we want to set our variable called "date", to be the type 'Date'. 
dt_2018[, date := as.Date(date)]

# remove "de " before health zone names in kinshasa
dt_2018[ dps == 'KINSHASA', zs := gsub("de ", "", zs, ignore.case = TRUE)]

# run tolower() and trimws() functions and gsub "_" for " "- on dps, zs, and csdt in order to format them consistently across quarters and years of data
dt_2018[, dps := tolower(dps)]
dt_2018[, zs := tolower(zs)]
dt_2018[, csdt := tolower(csdt)]

dt_2018[, dps := trimws(dps)]
dt_2018[, zs := trimws(zs)]
dt_2018[, csdt := trimws(csdt)]

dt_2018[, dps := gsub(" ", "_", dps)]
dt_2018[, zs := gsub(" ", "_", zs)]
dt_2018[, csdt := gsub(" ", "_", csdt)]

# change one mistake that resulted form the above: 
dt_2018[ zs == 'ngiri_-ngiri', zs := 'ngiri_ngiri']

# REMOVE REMAINING EMBEDDED TOTALS
# remove health zone total rows in T1 kinshasa
dt_2018[ dps == 'kinshasa' & grepl('zs_', csdt), remove_row := TRUE ]
# and remove some remaining total rows elsewhere in the data:
dt_2018[grepl('zs', csdt), remove_row := TRUE]
dt_2018 = dt_2018[is.na(remove_row)]
dt_2018[, remove_row := NULL]
#---------------------------------------------------

#---------------------------------------------------
# 2018 ZS and CSDT fixes
#---------------------------------------------------
# Fix where "monieka" facilities were accidentally labeled 'mbandaka' in 2018 Q4
# It's the last three rows of MBANDAKA in T4 2018 - note be careful here; you can't just use the names of facilities in Monieka 
# to identify these rows, because one is CSDT HGR which is a generic name, and is present in a lot of other health zones;
# however, the names of facilities in monieka and mbandaka ARE mutually exclusive, so that helps!
monieka_fac = dt_2018[ trimestre == 'T3 2018' & zs == 'monieka', unique(csdt)] 
dt_2018[ trimestre == 'T4 2018' & zs == 'mbandaka' & csdt %in% monieka_fac, zs := 'monieka']

# Fix where "aba" facilities were accidentally labeled "boma_mangbetu" in 2018 Q1
aba_fac = dt_2018[ trimestre == 'T2 2018' & zs == 'aba', unique(csdt)] 
dt_2018[ trimestre == 'T1 2018' & csdt %in% aba_fac, zs:= 'aba']

# Fix where "katoyi" facilities were accidentally labeled "kirotshe" or "masisi" in 2018 Q1
katoyi_fac = dt_2018[ trimestre == 'T2 2018' & zs == 'katoyi', unique(csdt)] 
dt_2018[ trimestre == 'T1 2018' & csdt %in% katoyi_fac, zs := 'katoyi']

# Fix where "barumbu" facilities were accidentally labeled "kalamu_ii" in 2018 Q1
barumbu_fac = dt_2018[ trimestre == 'T2 2018' & zs == 'barumbu', unique(csdt)] 
dt_2018[ trimestre == 'T1 2018' & csdt %in% barumbu_fac, zs := 'barumbu']

# Fix where "nsele" facilities were accidentally labeled "kikimi" in 2018 all quarters, and remove nsele total rows
nsele_fac = c('etonga', 'notre_dame_du_jardin_(_ex._ste_angele)', 'emerode_kinkole', 'cs_centenaire', 'hgr_kinkole')
dt_2018[ csdt %in% nsele_fac, zs := 'nsele']
dt_2018 = dt_2018[ csdt != 'nsele'] # this removes the totals embedded in facilities (and will also remove where csdt is NA - checked this and it looks right)

opienge_fac = c('ndrekoko', 'hgr_opienge')
dt_2018[ csdt %in% opienge_fac & zs == 'tshopo', zs := 'opienge']

# standardize hz names within pnlt data
dt_2018[zs == 'riba', zs := 'rimba']
dt_2018[zs == 'kamane', zs := 'kamana']
dt_2018[zs == 'kalamaba', zs := 'kalamba']
dt_2018[zs == 'pendjwa', zs := 'penzwa']
dt_2018[zs == 'dingila', zs := 'ganga']
dt_2018[zs == 'kilelabalanda', zs := 'kilela_balanda']
dt_2018[zs == 'kabondo-d', zs := 'kabondo-dianda']
dt_2018[zs == 'kikwitnord', zs := 'kikwit_nord']
dt_2018[zs == 'kikwitsud', zs := 'kikwit_sud']
dt_2018[zs == 'paykongila', zs := 'pay_kongila']
dt_2018[zs == 'yasabonga', zs := 'yasa_bonga']
dt_2018[zs == 'kamana', zs := 'kamane']
dt_2018[zs == 'bosomanzi', zs := 'boso_manzi']
dt_2018[zs == 'bosomodanda', zs := 'boso_modanda']
dt_2018[zs == 'utshuru', zs := 'rutshuru']
dt_2018[zs == 'kamana', zs := 'kamane']
dt_2018[zs == 'bosomanzi', zs := 'boso_manzi']
dt_2018[zs == 'bosomodanda', zs := 'boso_modanda']
dt_2018[zs == 'wanguba', zs := 'rwanguba']

# Some samba and kasongo facilities in maniema got mixed up:
samba_fac = c('hgr_samba', 'lusangay', 'ch_samba', 'malela')
dt_2018[ csdt %in% samba_fac, zs := 'samba']
kasongo_fac = c('hgr_kasongo', 'mwanandeke', 'km_18', 'ch_uzima', 'lububula')
dt_2018[ csdt %in% kasongo_fac, zs := 'kasongo']
#---------------------------------------------------

#---------------------------------------------------
# 2016 data
#---------------------------------------------------
# Read in the data
dt_2016 = read.xlsx(paste0(j_dir, inFile_2016)) # note, this data has both 2016 and 2017 - we want to subset to just 2016
# Make the data a "data table" 
dt_2016 = data.table(dt_2016)
# For some reason, there are 1000s of columns, we want to just keep the ones with the data; do this first so it doesn't take up too much space (everything is slow/taking forever)
dt_2016 = dt_2016[, 1:65]
# Remove the first four lines of the data - now that the data is in R, we don't want the column names in the rows of data
dt_2016 = dt_2016[5:nrow(dt_2016), ] 

# Change the column names to be easier to use in R: 
new_names = c('dps', 'trimestre', 'zs', 'csdt', 'population_totale', 'population_couverte', 
              'presumes_tb', 'frottis_effectue', 'frottis_positif', 'microscope_teste', 'microscope_positif', 'xpert_teste', 'xpert_positif', 'xpert_positif_resistance', 'xpert_invalide',
              'cas_tb_bac_nouveau', 'cas_tb_bac_rechute', 'cas_tb_bac_apresEchec', 'cas_tb_bac_apresPerduDeVue', 
              'cas_tb_clinique_noveau', 'cas_tb_clinique_rechute', 'cas_tb_clinique_horsRechutes', 
              'cas_tb_extrapul_nouveau', 'cas_tb_extrapul_rechute', 'cas_tb_extrapul_horsRechute',
              'autre_patient_deja_traite', 'total_cas_incident', 'total_cas', 
              'cas_oriente_par_communaute', 'cas_recu_soutien_communitaire', 
              'tb_teste_vih_nouveau', 'tb_vih_positif_nouveau', 'tb_vih_positif_cotri_nouveau', 'tb_vih_positif_tarv_nouveau', 
              'tb_teste_vih_autres', 'tb_vih_positif_autres', 'tb_vih_positif_cotri_autres', 'tb_vih_positif_tarv_autres', 
              'pvvih_avec_tb', 'pvvih_sans_tb', 'pvvih_sous_inh', 
              'tbmr_nouveau_presumes', 'tbmr_nouveau_confirme', 'tbmr_nouveau_confirme_xdr', 
              'tbmr_dejaTraitePremiere_presumes', 'tbmr_dejaTraitePremiere_confirme', 'tbmr_dejaTraitePremiere_confirme_xdr',
              'tbmr_dejaTraiteDeuxieme_presumes', 'tbmr_dejaTraiteDeuxieme_confirme', 'tbmr_dejaTraiteDeuxieme_confirme_xdr',
              'tbmr_traitement_presumes', 'tbmr_traitement_confirme', 'tbmr_traitement_confirme_xdr',
              'tb_sensible_traitement1_zs', 'tb_sensible_traitement1_hzs', 'tb_sensible_traitement1_transfron',
              'tb_sensible_traitement2_zs', 'tb_sensible_traitement2_hzs', 'tb_sensible_traitement2_transfron',
              'enfant_0_5_sous_inh', 'prisionniers_positif_tb', 'mineurs_positif_tb', 'cas_contact_positif_tb', 'populations_speciales_autres', 'populations_speciales_total')

names(dt_2016) = new_names

# Make a column for the year 
dt_2016[, year := 2016]
# Subset to just 2016
dt_2016 = dt_2016[grepl(trimestre, pattern = '2016|0216')] # there's a typo for date, so include these rows too
# Make a column for the date from the 'trimestre' variable - this will make graphing the data easier
dt_2016[grepl(trimestre, pattern = "T1"), date:="2016-01-01"]
dt_2016[grepl(trimestre, pattern = "T2"), date:="2016-04-01"]
dt_2016[grepl(trimestre, pattern = "T3"), date:="2016-07-01"]
dt_2016[grepl(trimestre, pattern = "T4"), date:="2016-10-01"]
# R has a special variable type for dates, so we want to set our variable called "date", to be the type 'Date'. 
dt_2016[, date := as.Date(date)]

# run tolower() and trimws() functions and gsub "_" for " "- on dps, zs, and csdt in order to format them consistently across quarters and years of data
dt_2016[, dps := tolower(dps)]
dt_2016[, zs := tolower(zs)]
dt_2016[, csdt := tolower(csdt)]

dt_2016[, zs := gsub('total', '', zs)]
dt_2016[, zs := gsub('zs', '', zs)]

dt_2016[, dps := trimws(dps)]
dt_2016[, zs := trimws(zs)]
dt_2016[, csdt := trimws(csdt)]

dt_2016[, dps := gsub(" ", "_", dps)]
dt_2016[, zs := gsub(" ", "_", zs)]
dt_2016[, csdt := gsub(" ", "_", csdt)]

# change one mistake that resulted form the above: 
dt_2016[ zs == 'ngiri_-ngiri', zs := 'ngiri_ngiri']
#---------------------------------------------------

#---------------------------------------------------
# 2016 ZS and CSDT fixes
#---------------------------------------------------
# standardize hz names within pnlt data
dt_2016[zs == 'de_mandima', zs := 'mandima']
dt_2016[zs == 'moanda_a', zs := 'muanda']
dt_2016[zs == 'boma_bung', zs := 'boma_bungu']
dt_2016[zs == 'kyongo', zs := 'kyondo']
dt_2016[zs == 'makiso-_kis', zs := 'makiso_kisangani']
dt_2016[zs == 'mufunga', zs := 'mufunga_sampwe']
dt_2016[zs == 'maswika', zs := 'masuika']
dt_2016[zs == 'wambaluadi', zs := 'wamba_luadi']
dt_2016[zs == 'kimbilulenge', zs := 'lulenge']
#---------------------------------------------------

#---------------------------------------------------
# 2017 data
#---------------------------------------------------
# Read in the data
dt_2017 = read.xlsx(paste0(j_dir, inFile_2017))
# Make the data a "data table" 
dt_2017 = data.table(dt_2017)
# Remove the first four lines of the data - now that the data is in R, we don't want the column names in the rows of data
dt_2017 = dt_2017[5:nrow(dt_2017), ]

# Change the column names to be easier to use in R: 
new_names = c('dps', 'trimestre', 'zs', 'csdt', 'population_totale', 'population_couverte', 
              'presumes_tb', 'frottis_effectue', 'frottis_positif', 'microscope_teste', 'microscope_positif', 'xpert_teste', 'xpert_positif', 'xpert_positif_resistance', 'xpert_invalide',
              'cas_tb_bac_nouveau', 'cas_tb_bac_rechute', 'cas_tb_bac_apresEchec', 'cas_tb_bac_apresPerduDeVue', 
              'cas_tb_clinique_noveau', 'cas_tb_clinique_rechute', 'cas_tb_clinique_horsRechutes', 
              'cas_tb_extrapul_nouveau', 'cas_tb_extrapul_rechute', 'cas_tb_extrapul_horsRechute',
              'autre_patient_deja_traite', 'total_cas_incident', 'total_cas', 
              'cas_oriente_par_communaute', 'cas_recu_soutien_communitaire', 
              'tb_teste_vih_nouveau', 'tb_vih_positif_nouveau', 'tb_vih_positif_cotri_nouveau', 'tb_vih_positif_tarv_nouveau', 
              'tb_teste_vih_autres', 'tb_vih_positif_autres', 'tb_vih_positif_cotri_autres', 'tb_vih_positif_tarv_autres', 
              'pvvih_avec_tb', 'pvvih_sans_tb', 'pvvih_sous_inh', 
              'tbmr_nouveau_presumes', 'tbmr_nouveau_confirme', 'tbmr_nouveau_confirme_xdr', 
              'tbmr_dejaTraitePremiere_presumes', 'tbmr_dejaTraitePremiere_confirme', 'tbmr_dejaTraitePremiere_confirme_xdr',
              'tbmr_dejaTraiteDeuxieme_presumes', 'tbmr_dejaTraiteDeuxieme_confirme', 'tbmr_dejaTraiteDeuxieme_confirme_xdr',
              'tbmr_traitement_presumes', 'tbmr_traitement_confirme', 'tbmr_traitement_confirme_xdr',
              'tb_sensible_traitement1_zs', 'tb_sensible_traitement1_hzs', 'tb_sensible_traitement1_transfron',
              'tb_sensible_traitement2_zs', 'tb_sensible_traitement2_hzs', 'tb_sensible_traitement2_transfron',
              'enfant_0_5_sous_inh', 'prisionniers_positif_tb', 'mineurs_positif_tb', 'cas_contact_positif_tb', 'populations_speciales_autres', 'populations_speciales_total')

names(dt_2017) = new_names

# Make a column for the year 
dt_2017[, year := 2017]
# Make a column for the date from the 'trimestre' variable - this will make graphing the data easier
dt_2017[grepl(trimestre, pattern = "T1"), date:="2017-01-01"]
dt_2017[grepl(trimestre, pattern = "T2"), date:="2017-04-01"]
dt_2017[grepl(trimestre, pattern = "T3"), date:="2017-07-01"]
dt_2017[grepl(trimestre, pattern = "T4"), date:="2017-10-01"]
# R has a special variable type for dates, so we want to set our variable called "date", to be the type 'Date'. 
dt_2017[, date := as.Date(date)]

# remove "de " before health zone names in kinshasa
dt_2017[ dps == 'KINSHASA', zs := gsub("de ", "", zs, ignore.case = TRUE)]
dt_2017[ dps == 'KWILU', zs := gsub("de ", "", zs, ignore.case = TRUE)]
dt_2017[ dps == 'KWILU', zs := gsub("d'", "", zs, ignore.case = TRUE)]

# run tolower() and trimws() functions and gsub "_" for " "- on dps, zs, and csdt in order to format them consistently across quarters and years of data
dt_2017[, dps := tolower(dps)]
dt_2017[, zs := tolower(zs)]
dt_2017[, csdt := tolower(csdt)]

dt_2017[, dps := trimws(dps)]
dt_2017[, zs := trimws(zs)]
dt_2017[, csdt := trimws(csdt)]

dt_2017[, dps := gsub(" ", "_", dps)]
dt_2017[, zs := gsub(" ", "_", zs)]
dt_2017[, csdt := gsub(" ", "_", csdt)]

# change one mistake that resulted form the above: 
dt_2017[ zs == 'ngiri_-ngiri', zs := 'ngiri_ngiri']

# fix tanganyika spelling
dt_2017[ dps == 'tanganyka', dps := 'tanganyika']

# REMOVE REMAINING EMBEDDED TOTALS
# and remove some remaining total rows elsewhere in the data:
dt_2017[grepl('zs', csdt), remove_row := TRUE]
dt_2017 = dt_2017[is.na(remove_row)]
dt_2017[, remove_row := NULL]
#---------------------------------------------------

#---------------------------------------------------
# 2017 ZS and CSDT fixes
#---------------------------------------------------
# Fix where "jiba" facilities were accidentally labeled "kilo" in 2017 all quarters
jiba_fac = c('cs_laudjo', 'hgr_jiba', 'cs_gobunji')
dt_2017[ csdt %in% jiba_fac, zs := 'jiba']
# Remove "jiba" total rows
dt_2017 = dt_2017[ csdt != 'jiba']

# Fix where "nsele" facilities were accidentally labeled "kikimi" in 2017 all quarters, and remove nsele total rows
nsele_fac = c('etonga', 'notre_dame_du_jardin_(_ex._ste_angele)', 'emerode_kinkole', 'cs_centenaire', 'hgr_kinkole')
dt_2017[ csdt %in% nsele_fac, zs := 'nsele']
# Remove "nsele" total rows
dt_2017 = dt_2017[ csdt != 'nsele'] 

# Fix where "manguredjipa" facilities were accidentally labeled "masereka" in 2018 Q1
manguredjipa_fac = dt_2017[ trimestre == 'T2 2017' & zs == 'manguredjipa', unique(csdt)] 
dt_2017[ trimestre %in% c('T1 2017', 'T3 2017', 'T4 2017') & csdt %in% manguredjipa_fac, zs:= 'manguredjipa']

# Fix where "katako_kombe" facilities were accidentally labeled "kole" in 2018 Q1
katako_kombe_fac = dt_2017[ trimestre == 'T2 2017' & zs == 'katako_kombe', unique(csdt)] 
katako_kombe_fac = katako_kombe_fac[!katako_kombe_fac %in% 'hgr'] # because one of the facilities is just 'hgr' we have to rename that row of data by the specific values, unfortunately
dt_2017[ trimestre == 'T1 2017' & zs == 'kole' & csdt %in% katako_kombe_fac, zs:= 'katako_kombe']
dt_2017[ trimestre == 'T1 2017' & zs == 'kole' & csdt == 'hgr' & population_totale == '53891', zs:= 'katako_kombe'] # look at it first to confirm it is the right row of data compared to raw data (it is!)

# Fix where "bena_dibele" facilities were accidentally labeled "dikungu" in 2018 Q1
bena_dibele_fac = dt_2017[ trimestre == 'T2 2017' & zs == 'bena_dibele', unique(csdt)] 
dt_2017[ trimestre == 'T1 2017' & csdt %in% bena_dibele_fac & zs == 'dikungu', zs:= 'bena_dibele']

# standardize hz names within pnlt data
dt_2017[zs == 'de_mandima', zs := 'mandima']
dt_2017[zs == 'lukalenge', zs := 'lukelenge']
dt_2017[zs == 'boma_bu', zs := 'boma_bungu']
dt_2017[zs == 'moanda_a', zs := 'muanda']
dt_2017[zs == 'muanda_a', zs := 'muanda']
dt_2017[zs == 'kyongo', zs := 'kyondo']
dt_2017[zs == 'mobayi', zs := 'mobayi_mbongo']
dt_2017[zs == 'makiso-_kis', zs := 'makiso_kisangani']
dt_2017[zs == 'wanierukula', zs := 'wanie_rukula']
dt_2017[zs == 'total_yakusu', zs := 'yakusu']
#---------------------------------------------------

#---------------------------------------------------
# Combine all years of data where columns are the same
#---------------------------------------------------
# first, combine 2016 and 2017:
dt = rbindlist(list(dt_2016, dt_2017), use.names = TRUE, fill = TRUE)

# Make all of the tb/hiv indicator variables the 'numeric' type in R:
dt[, total_cas_incident := as.numeric(total_cas_incident)]
dt[, total_cas := as.numeric(total_cas)]
dt[, pvvih_sous_inh := as.numeric(pvvih_sous_inh)]
dt[, tb_teste_vih_nouveau := as.numeric(tb_teste_vih_nouveau)]
dt[, tb_teste_vih_autres := as.numeric(tb_teste_vih_autres)]
dt[, tb_vih_positif_nouveau := as.numeric(tb_vih_positif_nouveau)]
dt[, tb_vih_positif_autres := as.numeric(tb_vih_positif_autres)]
dt[, tb_vih_positif_tarv_nouveau := as.numeric(tb_vih_positif_tarv_nouveau)]
dt[, tb_vih_positif_tarv_autres := as.numeric(tb_vih_positif_tarv_autres)]
dt[, tb_vih_positif_cotri_nouveau := as.numeric(tb_vih_positif_cotri_nouveau)]
dt[, tb_vih_positif_cotri_autres := as.numeric(tb_vih_positif_cotri_autres)]
dt[, cas_tb_bac_apresEchec := as.numeric(cas_tb_bac_apresEchec)]
dt[, cas_tb_bac_apresPerduDeVue := as.numeric(cas_tb_bac_apresPerduDeVue)]

# Combine some columns for 2016/2017 to get the 2018 equivalent
dt[, tb_teste_vih := tb_teste_vih_nouveau + tb_teste_vih_autres]
dt[, tb_vih_positif := tb_vih_positif_nouveau + tb_vih_positif_autres]
dt[, tb_vih_positif_tarv := tb_vih_positif_tarv_nouveau + tb_vih_positif_tarv_autres]
dt[, tb_vih_positif_cotri := tb_vih_positif_cotri_nouveau + tb_vih_positif_cotri_autres]
dt[, cas_tb_bac_horsRechutes := cas_tb_bac_apresEchec + cas_tb_bac_apresPerduDeVue]

dt_2018[, total_cas_incident := as.numeric(total_cas_incident)]
dt_2018[, total_cas := as.numeric(total_cas)]
dt_2018[, tb_teste_vih := as.numeric(tb_teste_vih)]
dt_2018[, tb_vih_positif := as.numeric(tb_vih_positif)]
dt_2018[, tb_vih_positif_tarv := as.numeric(tb_vih_positif_tarv)]
dt_2018[, pvvih_sous_inh := as.numeric(pvvih_sous_inh)]
dt_2018[, cas_tb_bac_horsRechutes := as.numeric(cas_tb_bac_horsRechutes)]
dt_2018[, tb_vih_positif_cotri := as.numeric(tb_vih_positif_cotri)]

keep_vars = names(dt)[names(dt) %in% names(dt_2018)]
dt = dt[, keep_vars, with = FALSE]
dt_2018 = dt_2018[, keep_vars, with = FALSE]
dt_all = rbindlist(list(dt, dt_2018), use.names = TRUE, fill = TRUE)

dt_all[, zs:=gsub('_', '-', zs)]
dt_all[zs == 'mufunga', zs := 'mufunga-sampwe']
dt_all[zs == 'maswika', zs := 'masuika']
dt_all[zs == 'wambaluadi', zs := 'wamba-luadi']
dt_all[zs == 'kimbilulenge', zs := 'lulenge']
dt_all[, zs:=standardizeHZNames(zs)]

# save the full time series
write.csv(dt_all, paste0(out_dir, outFile), row.names=F)
#---------------------------------------------------

#---------------------------------------------------
# subset to TB/HIV indicators
#---------------------------------------------------
# Subset the data to include only the id variables and tb/hiv indicators
id_vars = c('year', 'date', 'dps', 'trimestre','zs', 'csdt', 'population_totale', 'population_couverte')
tb_hiv_vars = c('total_cas_incident', 'total_cas', 'tb_teste_vih', 'tb_vih_positif', 'tb_vih_positif_tarv', 'pvvih_sous_inh') 

dt = dt_all[, c(id_vars, tb_hiv_vars), with = FALSE]

# Save the final data as a .csv file 
write.csv(dt, paste0(out_dir, outFile_tbhiv), row.names=F)
#---------------------------------------------------