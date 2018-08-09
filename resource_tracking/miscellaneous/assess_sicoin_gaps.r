# ---------------------------------------------
# David Phillips
#
# 8/8/2018
# Assess the gaps in our available sicoin data
# ---------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
# ------------------


# ------------------------------------------------------------------------
# Files/directories

# root directory
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/'

# input file
inFile = paste0(dir, 'total_resource_tracking_data.csv')

# output file

# ------------------------------------------------------------------------


# ---------------------------------
# Load/prep data

# load
data = fread(inFile)

# subset to only sicoin
data = data[data_source=='sicoin']
# ---------------------------------


# ----------------------------------------------------------------------------------------------------
# Make identifiers to figure out what level of detail is available in each disease-year

# identify which disease-years have subnational
data[adm2==100, loc_name:='gtm']
data[, n_locs:=length(unique(loc_name)), by=c('disease','year')]
data[, has_subnational:=ifelse(n_locs>1,1,0)]

# identify which disease-years have monthly
data[, has_monthly:=ifelse(period<365,1,0)]
data[, has_monthly:=max(has_monthly), by=c('disease','year')]

# identify which disease-years have modules
data[module!='all', n_modules:=length(unique(module)), by=c('disease','year')]
data[, n_modules:=max(n_modules,na.rm=TRUE), by=c('disease','year')]
data[, has_modules:=ifelse(n_modules>1,1,0)]

# reduce to unique source-disease-years
data = unique(data[,c('financing_source','disease','year','has_subnational',
												'has_monthly','has_modules')])

# identify whether GF is available in each disease-year
data[, has_gf:=ifelse(financing_source=='gf',1,0)]
data[,has_gf:=max(has_gf), by=c('disease','year')]

# identify whether other DAH is available in each disease-year
data[, has_dah:=ifelse(financing_source=='donacions',1,0)]
data[,has_dah:=max(has_dah), by=c('disease','year')]

# identify whether GHE is available in each disease-year
data[, has_ghe:=ifelse(financing_source=='ghe',1,0)]
data[,has_ghe:=max(has_ghe), by=c('disease','year')]

# reduce to unique disease-years
data = unique(data[,c('disease','year','has_subnational','has_monthly',
					'has_modules','has_gf','has_dah','has_ghe')])

# summarize
data[order(disease,year)]
# ----------------------------------------------------------------------------------------------------
