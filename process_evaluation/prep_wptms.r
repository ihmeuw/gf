# ---------------------------
# David Phillips
# 
# 1/26/2019
# Extract WPTM info from PUDRs
# ---------------------------


# ---------------------------
# Set up R
rm(list=ls())
library(data.table)
library(readxl)
library(stringr)
# ---------------------------


# ---------------------------
# Files and directories

# root directory
dir = 'C:/local/GF_copy/resource_tracking/'

# locate PUDR directories by country
ugaDirs = list.dirs(paste0(dir, 'uga/grants/active/'))
ugaDirs = ugaDirs[grepl('pudr',ugaDirs)]
codDirs = list.dirs(paste0(dir, 'cod/grants/active/'))
codDirs = codDirs[grepl('pudr',codDirs) & !grepl('iteration', codDirs)]
gtmDirs = list.dirs(paste0(dir, 'gtm/grants/active/'))
gtmDirs = gtmDirs[grepl('pudr',gtmDirs)]
allDirs = c(ugaDirs, codDirs, gtmDirs)

# locate files
files = list.files(allDirs, full.names=TRUE)

# drop invalid file types
types = c('xlsx', 'xlsm', '.xls', 'XLSX')
files = files[str_sub(files,-4,nchar(files)) %in% types]

# output file
outFile = paste0(dir, '../process_evaluation/WPTM_Sheets.csv')
# ---------------------------


# ---------------------------
# Loop over files and extract WPTMs
i=1
for(file in files) {
	print(paste('Extracting:', file))
	
	# get file name and grant name
	fileName = file
	for(d in allDirs) fileName = gsub(d, '', fileName)
	grantName = str_split(file, '/')[[1]][9]
	
	# skip MS office temp files
	if (str_sub(fileName,2,2)=='~') { 
		print('File starts with ~. Skipping...')
		next 
	}
	
	sheets = excel_sheets(file)
	
	# confirm make a blank if sheet doesn't exist
	if (!'WPTM_1C' %in% sheets) {
		print('File does not have a sheet named WPTM_1C. Adding blank row')
		currData = data.table(grant=grantName, file_name=fileName)
	}	
	
	# load if sheet exists
	if ('WPTM_1C' %in% sheets) {
		# load wptm sheet assuming normal formatting
		currData = data.table(read_excel(file, sheet='WPTM_1C', skip=8, col_types='text'))
		
		# keep trying to load data until correct variables found
		j=1
		while (!'Module' %in% names(currData) & !'Module Name' %in% names(currData)) { 
			currData = data.table(read_excel(file, sheet='WPTM_1C', skip=j, col_types='text'))
			j=j+1
		}
		
		# drop unnecessary columns
		dropVars = c('X__1', 'X__2', 'X__3', 'X__4', 'X__5', 'X__6', 'X__7')
		for(v in dropVars) currData[[v]] = NULL
		
		# standardize names
		if ('Module Name' %in% names(currData)) setnames(currData, 'Module Name', 'Module')
		if ('Activité' %in% names(currData)) setnames(currData, 'Activité', 'Activity')
		if ('Détails de l\'activité- étapes clés/cibles' %in% names(currData)) { 
			setnames(currData, 'Détails de l\'activité- étapes clés/cibles', 
			'Activity details- milestones/ targets')
		}
		if ('Critère de réalisation' %in% names(currData)) { 
			setnames(currData, 'Critère de réalisation', 'Criterion for Completion')
		}
		if ('Pays (pour les subventions multipays)' %in% names(currData)) { 
			setnames(currData, 'Pays (pour les subventions multipays)', 
			'Country (relevant for multi-country grants)')
		}
		if ('Étapes clés/cible pour la période actuelle' %in% names(currData)) { 
			setnames(currData, 'Étapes clés/cible pour la période actuelle', 
			'Milestones/Target for the Current Period')
		}
		if ('Motifs de l\'écart par rapport aux activités et étapes clés du plan de travail' %in% names(currData)) { 
				setnames(currData, 'Motifs de l\'écart par rapport aux activités et étapes clés du plan de travail', 
				'Reasons for deviation from workplan activities and milestones')
		}
		if ('État d\'avancement' %in% names(currData)) setnames(currData, 'État d\'avancement', 'Progress Status')
		if ('Activité' %in% names(currData)) setnames(currData, 'Activité', 'Activity')
		
		# drop unnecessary rows
		dropRows = c('[Module Name - FR]', '[Module Name]')
		currData = currData[!Module %in% dropRows]
		for(v in names(currData)) currData[get(v)=='-', (v):=NA]
		
		# drop duplicates and all completely missing rows except one
		currData = unique(currData)
		
		# label
		currData[, grant:=grantName]
		currData[, file_name:=fileName]
	}
	
	# append to the rest
	if (i==1) data = copy(currData)
	if (i>1) data = rbind(data, currData, fill=TRUE, use.names=TRUE)
	i=i+1
}
# ---------------------------


# ------------------------------------------------------------------------
# Clean up and save

# drop complete duplicates
data = unique(data)

# identify completely missing rows
data[, Completely_Missing:=FALSE]
data[which(rowSums(is.na(data))==ncol(data)-3), Completely_Missing:=TRUE]

write.csv(data, outFile, row.names=FALSE)
# ------------------------------------------------------------------------
