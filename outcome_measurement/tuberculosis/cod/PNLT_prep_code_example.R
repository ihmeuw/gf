# ----------------------------------------------
# Audrey Batzel
# Code that appends a set of sheets from many PNLT datasets

# Instructions:
# 1. Store the 2016-2018 TB data in a folder identical to Basecamp (one sub-folder for each year) 
# 2. Change the 'dir' object to reflect the location of the data on your computer
# 3. Change the sheet_type object to reflect the sheet within each excel file you want to extract
# 4. Run the whole script (it saves a list containing all the sheets)
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

# output file
outFile = paste0(dir, 'appended_', sheet_type, '_data.csv') 
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
for (f in files) { 
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
		currentSheet[, item:=i]
			
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


# ----------------------------------------------
# Save the data
saveRDS(data, outFile)
# ----------------------------------------------