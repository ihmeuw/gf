# -----------------------------------------------------------
# David Phillips
#
# 11/2/2019
# Set up data to estimate projected absorption
# Intended to be called by 2_estimate_projected_absorption.r 
# -----------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(boot)
library(stringr)
# ------------------


# -----------------------------------------------------------------
# Files and directories

# root directory for input/output
dir = 'J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data'

# input data
inFile = paste0(dir, '/final_expenditures.rds')

# output file
outFile = paste0(dir, '/projected_absorption_input.rds')
# -----------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(inFile)

# subset to previous grants only
# data = data[is.na(grant_status) | grant_status!='active']

# collapse to module-quarter level
byVars = c('disease','grant','grant_period','start_date','end_date', 'gf_module')
data = data[, .('budget'=sum(budget,na.rm=TRUE), 
			'expenditure'=sum(expenditure,na.rm=TRUE)), by=byVars]

# compute absorption
data[, absorption:=expenditure/budget]

# identify the midpoint of the reporting period (assume quarterly if no end date)
data[, report_midpoint:=start_date+((end_date-start_date)/2)]
data[is.na(report_midpoint), report_midpoint:=start_date + floor(365/8)]

# compute days until end of grant
data[, end_of_grant:=as.Date(paste0(str_sub(grant_period, -4, -1), '-12-31'))]
data[, days_until_end:=as.numeric(end_of_grant-report_midpoint)]

# compute days since start of grant
data[, start_of_grant:=as.Date(paste0(str_sub(grant_period, 1, 4), '-01-01'))]
data[, days_since_start:=as.numeric(report_midpoint-start_of_grant)]

# number of modules within grant
data[, num_modules:=length(unique(gf_module)), by='grant']
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Clean variables for analysis

# absorption of Inf won't work
data = data[is.finite(absorption)]

# capture original values
data[, raw_absorption:=absorption]

# replace absorption outliers
p95 = quantile(data$absorption, .95)
p96 = quantile(data$absorption, .96)
data[absorption>p95, absorption:=p96]

# disallow reports past the end of grant
# data[days_until_end<0, days_until_end:=1]

# define lemon squeeze function (store N to global environment for later use)
# citation: Smithson M, Verkuilen J. A better lemon squeezer? Maximum-likelihood regression with beta-distributed dependent variables. Psychological methods. 2006 Mar;11(1):54.
lemonSqueeze = function(x) { 
	N <<- length(x[!is.na(x)])
	return(logit(((x*(N-1))+0.5)/N))
}

# handle 1's and 0's so logit doesn't drop them
data = data[is.finite(absorption) & absorption>=0]
data[absorption>1, absorption:=1] 
data[, absorption:=lemonSqueeze(absorption)]
# ----------------------------------------------------------------------


# --------------------
# Save
saveRDS(data, outFile)
# --------------------
