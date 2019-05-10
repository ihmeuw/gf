# ----------------------------------------------
# Audrey Batzel
# Example code to assemble the PNLT data

# Instructions:
# 1. Store the 2016-2018 TB data in a folder identical to Basecamp (one sub-folder for each year) 
# 2. Change the 'dir' object to reflect the location of the data on your computer
# 3. Change the sheet_type object to reflect the sheet within each excel file you want to extract
# 4. Run the whole script
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
dir = 'C:/local/Basecamp_PNLT_Data'  # where the TB files are stored 

# which sheet type do you want to prep?
# This code preps “DEP”, “EVAL”, “AGE SEX”, “SUIVI BACT” separately
sheet_type = 'DEP'

# output file
outFile = paste0(dir, 'cleaned_', sheet_type, '_data.csv') 
# --------------------------------------------------------


# ----------------------------------------------------------------------------------
# Loop over files and get the necessary sheets out of them

# list all the files in this directory (excluding synthesis files)
files = list.files(dir, recursive=TRUE)
files = files[!grepl('synth', files, ignore.case=TRUE)]

# set up a list to store all the data in
sheetList = list()

# loop over files (note: for DEP this takes about an hour on my computer)
i=1
for (f in files[1:36]) { 
	# display the name of the current file to monitor progress
	print(f)

	# look up the names of the sheets in this file
    sheets = excel_sheets(paste0(dir, '/', f))
	
	# get the sheet specified by sheet_type (excluding synthesis sheets)
    sheets = sheets[grepl(sheet_type, sheets, ignore.case=TRUE)]
    sheets = sheets[!grepl('synth', sheets, ignore.case=TRUE)]

	# loop over sheets and load them into R (even if they are empty)
	for(s in sheets) { 
		# load the sheet and convert to data.table for convenience
		currentSheet = read_excel(paste0(dir, '/', f), sheet=s, col_names=FALSE)
		currentSheet = data.table(currentSheet)
		
		# identify which file and sheet this came from
		currentSheet[, file:=f]
		currentSheet[, sheet:=s]
			
		# store the data in the list
		sheetList[[i]] = currentSheet
		i=i+1
	}
}
# ----------------------------------------------------------------------------------


# -------------------------------------------------------------------------------
# Clean each individual sheet

# set up a list to store all the data in
cleanedSheets = list()

# loop over sheets
for(s in seq(length(sheetList))) {

	# carry down the column names
	tmp = copy(sheetList[[s]])
	tmp = tmp[, lapply(.SD, na.locf, na.rm=F)]
	
	# drop rows with no data
	tmp = tmp[, n_not_missing:=rowSums(!is.na(.SD))]
	tmp = tmp[n_not_missing>0]
	tmp$n_not_missing = NULL
	
	# drop columns with no data
	tmp = tmp[,colSums(is.na(tmp))<nrow(tmp), with=FALSE]
	
	# set data to NA if I carried down the column name too far
	for(v in names(tmp)) tmp[get(v)==v, (v):=NA]
	
	# get DPS, year and quarter from file name and sheet name
	for(y in seq(2016,2018)) tmp[grepl(as.character(y),file), year:=y]
	for(q in seq(4)) tmp[grepl(as.character(q),sheet), quarter:=q]
	tmp[, dps:=gsub(year, '', file)]
	tmp[, dps:=gsub(quarter, '', dps)]
	for(x in c('Data TB /', ' T .xlsx', ' T .xls')) tmp[, dps:=gsub(x, '', dps)]
	
	# store
	cleanedSheets[[s]] = copy(tmp)
}
# -------------------------------------------------------------------------------


# -------------------------------------------------------------------------------
# Set column names for each individual sheet

# define a bunch of lists of possible name arrangements (done manually)
names1 = c('N', 'CSDT_ZS', 'Population_Totale', 'Population_couverte', 'Presumes_TB', 'Frottis_effectues_NP_et_rech', 'Frottis_positifs_NP_et_Rech', 'Presumes_TB_soumis_a_lexamen_Ziehl_Auramine', 'Presumes_TB_soumis_a_lexamen_Ziehl_Auramine_positif', 'Presumes_soumis_au_Genexpert', 'MTB_detecte_resistance_a_rifampicine_non_detectee_MTB_RIF', '_RR', 'Invalide_Aucun_resultat_Erreur', 'Nouveau_patient', 'Rechute', 'Apres_echec', 'Apres_perdu_de_vue', 'Nouveau_patient', 'Rechute', 'Hors_Rechute', 'Nouveau_patient', 'Rechute', 'Hors_Rechute', 'Autre_patient_deja_traite', 'Total_des_cas_incident_NP_et_Rech', 'Total_des_cas', 'Nombre_de_cas_ayant_ete_orientes_par_la_communaute', 'Total_patients_en_traitement_et_qui_ont_reçu_une_forme_de_soutien_a_lobservance_du_traitement_de_la_communaute', 'Teste_au_VIH_connaissant_leur_statut', 'VIH', 'VIH_sous_Cotri', 'VIH_sous_TARV', 'Teste_au_VIH_connaissant_leur_statut', 'VIH', 'VIH_sous_Cotri', 'VIH_sous_TARV', 'Nombre_des_PVVIH_avec_recherche_de_la_TB', 'Nombre_des_PVVIH_exclus_de_la_TB', 'Nombre_des_PVVIH_mis_sous_lINH', 'Presumes_TB_MR_RR', 'Confirmes_TBMR_RR', 'confirmes_TB_XDR', 'Presumes_TB_MR_RR', 'Confirmes_TBMR_RR', 'confirmes_TB_XDR', 'Presumes_TB_MR_RR', 'Confirmes_TBMR_RR', 'confirmes_TB_XDR', 'Presumes_TB_MR_RR', 'Confirmes_TBMR_RR', 'confirmes_TB_XDR', 'ZS', 'HZS', 'Transfrontalier', 'ZS', 'HZS', 'Transfrontalier', 'Enfant_de_0_5_ans_sous_INH', 'Prisonniers', 'Miniers', 'Cas_contact', 'Autres', 'Total', 'Appartenance', 'Cas', 'file', 'sheet', 'year', 'quarter', 'dps')
names2 = c(KEEP GOING)

# set names for sheets that have identical formats (please double check)
for(s in c(1:8, 11, 12, 17:44, 51, 52, 100, 105, 107:116, 134:143)) setnames(cleanedSheets[[s]], names1)
for(s in c(OTHER ROWS)) setnames(cleanedSheets[[s]], names2)
# -------------------------------------------------------------------------------


# ----------------------------------------------
# Save the data
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------------