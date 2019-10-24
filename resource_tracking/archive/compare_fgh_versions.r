# --------------------------------------------------------------------------------------------
# David Phillips
# 
# 7/16/2019
# Compare the old version of the FGH team's DAH data to the new version for major differences
# --------------------------------------------------------------------------------------------

# ------------------
# Set up R
rm(list=ls())
library(data.table)
# ------------------


# ------------------------------------------------------------------
# Files and directories

# data directory
dir = 'J:/Project/Evaluation/GF/resource_tracking/_fgh/raw_data'

newFile = paste0(dir, '/FGH_EZ_2018.rds')
oldFile = paste0(dir, '/archive/ihme_dah_cod_uga_gtm_1990_2016.csv')
# ------------------------------------------------------------------


# -------------------------------------------------------------------------------
# Load/prep data

# load data
newData = readRDS(newFile)
oldData = fread(oldFile)

# store variable lists
byVars = c('iso3_rc','source','channel','year')
numVars = names(sapply(newData, class)[sapply(newData, class)=='numeric'])
keepVars = names(newData)[grepl('hiv|mal|tb',names(newData))]
keepVars = c(keepVars,names(oldData)[grepl('hiv|mal|tb',names(oldData))])
subTotals = c('total_hiv', 'total_mal', 'total_tb', 'hiv_dah', 
		'hiv_hss_dah', 'mal_dah', 'mal_hss_dah', 'tb_dah', 'tb_hss_dah')

# collapse out donor names from new file
newData = newData[, lapply(.SD, sum), by=byVars, .SDcols=numVars]

# test unique ids
nrow(newData)==nrow(unique(newData[,byVars,with=F]))
nrow(oldData)==nrow(unique(oldData[,byVars,with=F]))

# merge old and new data
data = merge(newData, oldData, by=c('iso3_rc','source','channel','year'), all=T)

# keep only necessary variables
data = data[, c(byVars, keepVars), with=F]

# melt categories long
data = melt(data, byVars)

# harmonize category names
data[, version:=ifelse(grepl('18',variable), 'new', 'old')]
data[, variable:=gsub('_18|_17','',variable)]

# drop subtotals
data = data[!variable %in% subTotals]

# reshape version wide
form = as.formula(paste(paste(c(byVars, 'variable'), collapse='+'), '~ version'))
data = dcast(data, form)
# -------------------------------------------------------------------------------


# ---------------------------------------------
# Check for discrepancies

# differences
data[, diff:=new-old]

# display biggest increases since last version
data[order(-diff)][1:10]

# display biggest decreases since last version
data[order(diff)][1:10]
# ---------------------------------------------

