# ----------------------------------------------
# David Phillips
#
# 2/3/2017
# Print out some GBD stats for TERG presentations
# ----------------------------------------------



# ------------------
# Set up R
rm(list=ls())
library(data.table)
# ------------------


# ----------------------------------------------------------------------------
# Files and directories

# main directory
root = 'J:/Project/Evaluation/GF/miscellaneous/IHME-GBD_2015_DATA-1f6365b4-1'

# data file, this is an extraction from the GBD Results Tool
inFile = paste0(root, '/IHME-GBD_2015_DATA-1f6365b4-1.csv')
# ----------------------------------------------------------------------------



# ---------------------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# subset to the preferred metric
data = data[metric=='Rate']

# subset columns
vars = c('measure', 'location', 'year', 'cause', 'val')
data = data[, vars, with=FALSE]

# "reshape" year wide
y1 = data[year==2010, c('measure', 'location', 'cause', 'val'), with=FALSE]
y2 = data[year==2015, c('measure', 'location', 'cause', 'val'), with=FALSE]
data = merge(y1, y2, by=c('measure', 'location', 'cause'), suffixes=c('_2010', '_2015'))

# percent change
data[, pct_change:=(val_2015/val_2010*100)-100]

# round
data[, val_2015:=round(val_2015, 1)]
data[, val_2010:=round(val_2010, 1)]
data[, pct_change:=round(pct_change,1)]
# ---------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------------------------------------------
# Print numbers

# DRC
	# HIV
	data[location=='Democratic Republic of the Congo' & cause=='HIV/AIDS', c('measure','val_2015', 'pct_change'), with=FALSE]

	# TB
	data[location=='Democratic Republic of the Congo' & cause=='Tuberculosis', c('measure','val_2015', 'pct_change'), with=FALSE]

	# Malaria
	data[location=='Democratic Republic of the Congo' & cause=='Malaria', c('measure','val_2015', 'pct_change'), with=FALSE]

# GTM
	# HIV
	data[location=='Guatemala' & cause=='HIV/AIDS', c('measure','val_2015', 'pct_change'), with=FALSE]

	# TB
	data[location=='Guatemala' & cause=='Tuberculosis', c('measure','val_2015', 'pct_change'), with=FALSE]

	# Malaria
	data[location=='Guatemala' & cause=='Malaria', c('measure','val_2015', 'pct_change'), with=FALSE]

# UGA
	# HIV
	data[location=='Uganda' & cause=='HIV/AIDS', c('measure','val_2015', 'pct_change'), with=FALSE]

	# TB
	data[location=='Uganda' & cause=='Tuberculosis', c('measure','val_2015', 'pct_change'), with=FALSE]

	# Malaria
	data[location=='Uganda' & cause=='Malaria', c('measure','val_2015', 'pct_change'), with=FALSE]

# ------------------------------------------------------------------------------------------------------------------------------------------
