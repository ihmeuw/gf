# ----------------------------------------------
# David Phillips
#
# 8/1/2017
# Prep Uganda and DRC dah data from aiddata.org
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
# --------------------


# -----------------------------------------------------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/resource_tracking/')

# country directories
drcDir = paste0(dir, 'cod/aiddata/DRC-AIMS_GeocodedResearchRelease_Level1_v1.3.1/data/')
ugaDir = paste0(dir, 'uga/aiddata/UgandaAIMS_GeocodedResearchRelease_Level1_v1.4.1/data/')

# input files
drcProjFile = paste0(drcDir, 'projects.csv')
drcLocFile = paste0(drcDir, 'locations.csv')
ugaProjFile = paste0(ugaDir, 'projects.csv')
ugaLocFile = paste0(ugaDir, 'locations.csv')

# output files
drcOutFile = paste0(dir, 'cod/prepped/cod_aiddata.csv')
ugaOutFile = paste0(dir, 'uga/prepped/uga_aiddata.csv')
# -----------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------
# Load/prep data

# load
drcProjData = fread(drcProjFile)
drcLocData = fread(drcLocFile)
ugaProjData = fread(ugaProjFile)
ugaLocData = fread(ugaLocFile)

# merge
drcData = merge(drcProjData, drcLocData, 'project_id', all.x=TRUE)
ugaData = merge(ugaProjData, ugaLocData, 'project_id', all.x=TRUE)
drcData = drcData[!is.na(total_disbursements)]
ugaData = ugaData[!is.na(total_disbursements)]

# subset observations to anything in the health sector
drcData = drcData[grepl('Health', ad_sector_names)]
ugaData = ugaData[grepl('Health', ad_sector_names)]

# subset observations to anything related to HIV/TB/Malaria
# NOTE: Uganda has a program called "aidstar" which might not be related to AIDS. 
# Grepping the string "aids" is necessary though
drcData[, project_title:=tolower(project_title)]
ugaData[, project_title:=tolower(project_title)]
# the spaces before/after "tb" are to avoid grepping words like "outbreak"
patterns = 'vih|hiv|aids|\\stb|tb\\s|tuberculosis|tuberculeux|tuberculose|malaria|paludisme' 
drcData = drcData[grepl(patterns, project_title)] 
ugaData = ugaData[grepl(patterns, project_title)]

# format variables
drcData[, latitude:=as.numeric(latitude)]
drcData[, longitude:=as.numeric(longitude)]
ugaData[, latitude:=as.numeric(latitude)]
ugaData[, longitude:=as.numeric(longitude)]
drcData[, transactions_start_year:=as.numeric(transactions_start_year)]
drcData[, transactions_end_year:=as.numeric(transactions_end_year)]
ugaData[, transactions_start_year:=as.numeric(transactions_start_year)]
ugaData[, transactions_end_year:=as.numeric(transactions_end_year)]
drcData[, total_disbursements:=as.numeric(total_disbursements)]
ugaData[, total_disbursements:=as.numeric(total_disbursements)]
drcData[, total_commitments:=as.numeric(total_commitments)]
ugaData[, total_commitments:=as.numeric(total_commitments)]

# split evenly over years
drcData[, nyears:=transactions_end_year-transactions_start_year+1]
ugaData[, nyears:=transactions_end_year-transactions_start_year+1]
drcData[, annual_commitments:=total_commitments/nyears]
ugaData[, annual_commitments:=total_commitments/nyears]
# ---------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Assess validity

# maps
ggplot(drcData, aes(x=longitude, y=latitude)) + 
	geom_point()
ggplot(ugaData, aes(x=longitude, y=latitude)) + 
	geom_point()
	
# totals by year
byvars = c('transactions_start_year', 'transactions_end_year', 'nyears')
drcAgg = drcData[!is.na(nyears), sum(annual_commitments, na.rm=TRUE), by=byvars]
ugaAgg = ugaData[!is.na(nyears), sum(annual_commitments, na.rm=TRUE), by=byvars]
drcAgg = drcAgg[rep(seq(.N), nyears)]
ugaAgg = ugaAgg[rep(seq(.N), nyears)]
drcAgg[, year:=seq(.N), by=byvars]
drcAgg[, year:=transactions_start_year+year-1]
ugaAgg[, year:=seq(.N), by=byvars]
ugaAgg[, year:=transactions_start_year+year-1]
drcAgg = drcAgg[, sum(V1), by='year']
ugaAgg = ugaAgg[, sum(V1), by='year']
drcAgg[, V1:=V1/1000000]
ugaAgg[, V1:=V1/1000000]

# graph totals
ggplot(drcAgg, aes(y=V1, x=year)) + geom_line() + labs(y='Commmitments (Millions)')
ggplot(ugaAgg, aes(y=V1, x=year)) + geom_line() + labs(y='Commmitments (Millions)')
# ---------------------------------------------------------------------------------


# ---------------------------------------------
# Save
write.csv(drcData, drcOutFile, row.names=FALSE)
write.csv(ugaData, ugaOutFile, row.names=FALSE)
# ---------------------------------------------
