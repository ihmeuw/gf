# ------------------------------------------------------------
# David Phillips
# 
# 2/4/2019
# Script that adjusts SNIS to match PNLP for select indicators
# Decisions:
# 1. Adjusting SNIS is preferable because a) data quality, b) ...
# 2. Goal is to have a minimum viable product, not strive for ideal rigor
# 3. AL has problems that are unfixable through a general solution
# ------------------------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(lme4)
# -------------------


# ----------------------------------------------------------------------------
# Files and directories

# root directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/')

# input file
inFile = paste0(dir, 'snis_pnlp_malaria_hz_level.rds')

# output file
outFile = paste0(dir, 'snis_pnlp_malaria_adjusted.rds')
# ----------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(inFile)

# clean up dataset names
data = droplevels(data)
data[grepl('SIGL1',data_set), data_set:='sigl1']
data[grepl('SIGL2',data_set), data_set:='sigl2']
data[grepl('base',data_set), data_set:='base_services']

# identify indicators that are in both
datasets = unique(data[,c('indicator','data_set')])
datasets = dcast.data.table(datasets, indicator~data_set)
overlapping_indicators = datasets[!is.na(pnlp) & 
	(!is.na(sigl1) | !is.na(sigl2) | !is.na(base_services))]
	
# keep only indicators that are in both
data = data[indicator %in% overlapping_indicators$indicator]

# collapse subpopulations where necessary to get both to match
data[data_set=='base_services' & indicator=='LLIN' & 
	subpopulation %in% c('distAtANC1', 'distAtANC2'), subpopulation:='distAtANC'] # note this comparison is comparing LLIN distAtANC in both, but in the past we have used the sum of PNLP LLIN vars compared to SIGL LLIN consumed 
data[data_set=='sigl1' & indicator=='ArtLum' & 
	subpopulation %in% c('consumed(240+40)', 'consumed(480+80)'), subpopulation:='used']
idVars = names(data)[names(data)!='value']
data = data[, .(value=sum(value)), by=idVars]
	
# drop subpopulations that don't have an equivalent
idVars = idVars[idVars!='data_set']
form = as.formula(paste(paste(idVars, collapse='+'),'~data_set'))
data = dcast.data.table(data, form)
data = data[!is.na(pnlp) & (!is.na(sigl1) | !is.na(base_services))]

# combine sigl and base (test first)
if (nrow(data[!is.na(sigl1) & !is.na(base_services)])>0) { 
	stop('SIGL and Base Services overlap still, decide which to use') 
}
data[, snis:=ifelse(is.na(sigl1), base_services, sigl1)]
data$sigl1 = NULL
data$base_services = NULL

# reshape long again
long = melt(data, id.vars=idVars, variable.name='data_set')
# ---------------------------------------------------------------------------------------


