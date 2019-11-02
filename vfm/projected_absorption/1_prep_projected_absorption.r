# -------------------------------------------
# David Phillips
#
# 12/11/2017
# Analyze correlates of absorption
# -------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(boot)
library(readxl)
library(data.table)
library(stringr)
library(ggplot2)
# ------------------


# -----------------------------------------------------------------
# Files and directories

# root directory for input/output
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/'

# input data
inFile = paste0(dir, 'mapping/prepped_gos_data.csv')

# place to store the regression output
regOutFile = paste0(dir, '../../vfm/outputs/absorption_correlates_model_fit.rdata')

# output graphs
outFile = paste0(dir, '../../vfm/visualizations/absorption_correlates.pdf')
# -----------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep data

# load
allData = fread(inFile)

# identify quarters
allData[, quarter:=quarter(start_date)]

# collapse to module-quarter level
byVars = c('disease','country','grant_number','year','quarter','abbrev_module')
data = allData[, list('budget'=sum(budget,na.rm=TRUE), 
			'expenditure'=sum(expenditure,na.rm=TRUE)), by=byVars]

# compute absorption
data[, absorption:=expenditure/budget]

# define lemon squeeze function (store N to global environment for later use)
# citation: Smithson M, Verkuilen J. A better lemon squeezer? Maximum-likelihood regression with beta-distributed dependent variables. Psychological methods. 2006 Mar;11(1):54.
lemonSqueeze = function(x) { 
	N <<- length(x[!is.na(x)])
	return(logit(((x*(N-1))+0.5)/N))
}
reverseLemonSqueeze = function(x) { 
	N = length(x[!is.na(x)])
	return(((inv.logit(x)*N)-0.5)/(N-1))
}

# handle 1's and 0's so logit doesn't drop them
data = data[is.finite(absorption) & absorption>=0]
data[absorption>1, absorption:=1] 
data[, absorption:=lemonSqueeze(absorption)]
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Generate extra predictor variables

# year within grant and years from end of grant
data[, grant_year:=as.numeric(as.factor(year)), by='grant_number']
data[, years_from_end:=max(grant_year)-grant_year+1, by='grant_number']
data[, yearid:=as.numeric(as.factor(year)), by='grant_number']
data[, quarterid:=(((yearid-1)*4))+quarter]
data[, quarters_from_end:=max(quarterid)-quarterid+1, by='grant_number']

# total budget of the grant
data[, total_budget:=sum(budget,na.rm=TRUE), by='grant_number']

# number of modules within grant
data[, num_modules:=length(unique(abbrev_module)), by='grant_number']
# ----------------------------------------------------------------------

