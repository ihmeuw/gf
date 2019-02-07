# ---------------------------------------------------------------
# David Phillips
# 
# 11/4/2018
# Populate grant info synthesis spreadsheet for Nov 2018 workshop
# ---------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
# ------------------


# ---------------------------------------------------------------
# Files and directories

# main directory
dir = 'C:/Users/davidp6/Google Drive/Work/IHME Work/GF/Workshops/london_nov2018/Data/'

# resource tracking file
inFile = paste0(dir, 'total_resource_tracking_data.csv')
inFile = "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/final_budgets.csv"

# codebook for categories
codebookFile = paste0(dir, 'category_codebook.csv')

# output file to paste into spreadsheet
outFile = paste0(dir, 'summary_table.csv')
# ---------------------------------------------------------------


# ---------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# subset to current budgets
grant_periods = c('2018-2020', '2018', '2016-2019', '2019-2022')
data = data[data_source=='fpm' & grant_period %in% grant_periods]

# load file containing new categories
codebook = fread(codebookFile)
codebook = codebook[, c('code','category'), with=FALSE]

# create new categories in data
data = merge(data, codebook, 'code', all.x=TRUE)
# ---------------------------------------------------------------


# -----------------------------------------------------------------------
# Aggregate to grant-category level

# sum
byVars = c('country','grant_number','grant_period','category')
data = data[, .(budget=sum(budget)), by=byVars]

# reshape
data = dcast.data.table(data, country+grant_number+grant_period~category)

# fill in zeroes when no modules for that grant existed
for(v in unique(codebook$category)) {
	data[is.na(get(v)), (v):=0]
}
# -----------------------------------------------------------------------


# ---------------------------------------------------------------
# Order columns and rows

# order columns
orderVars = c('country','grant_number','grant_period','RSSH',
		'Program management','Treatment','Prevention','Other')
data = data[, orderVars, with=FALSE]

# insert blank rows for unknown grants
malaria_extension = data.table(country='Guatemala', grant_number='GTM-M-ext-MSPAS', grant_period='2018')
malaria_full = data.table(country='Guatemala', grant_number='GTM-M-MSPAS', grant_period='2019-2021')
data = rbind(data, malaria_extension, fill=TRUE)
data = rbind(data, malaria_full, fill=TRUE)

# order rows to match the spreadsheet
rowOrder = c('COD-T-MOH', 'COD-H-MOH', 'COD-C-CORDAID', 'COD-M-MOH', 'COD-M-SANRU', 
		'GTM-H-HIVOS', 'FR100-GTM-H', 'GTM-T-MSPAS', 'FR417-GTM-T', 'GTM-M-ext-MSPAS', 'GTM-M-MSPAS', 
		'UGA-T-MoFPED', 'UGA-H-MoFPED', 'UGA-C-TASO', 'UGA-M-MoFPED', 'UGA-M-TASO')
data = data[match(rowOrder, grant_number)]
# ---------------------------------------------------------------


# ------------------------------------
# Save
write.csv(data, outFile, row.names=F)
# ------------------------------------
