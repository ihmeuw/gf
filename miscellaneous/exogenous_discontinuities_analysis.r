# -----------------------------------------
# David Phillips
#
# 2/5/2020
# Check if exogenous discontinuities in GF funding result in subsequent changes in reported output
# -----------------------------------------


# -----------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(gridExtra)
# -----------------------------------------


# -----------------------------------------
# Files

# dah (from http://internal-ghdx.healthdata.org/record/ihme-data/development-assistance-health-database-1990-2018)
dahFile = "C:/Users/davidp6/Downloads/IHME_DAH_DATABASE_1990_2018_CSV/IHME_DAH_DATABASE_1990_2018_Y2019M04D24.CSV"

# TB outputs (from https://www.who.int/tb/country/data/download/en/)
tbOutcomeFile = "C:/Users/davidp6/Downloads/TB_outcomes_2020-02-05.csv"
# -----------------------------------------


# -----------------------------------------
# Load/prep

# load
dahData = fread(dahFile)
tbOcData = fread(tbOutcomeFile)

# collapse to channel level
numVars = names(dahData)[grepl('dah', names(dahData))]
byVars = c('channel', 'recipient_isocode', 'recipient_country', 'year')
for(v in numVars) dahData[, (v):=as.numeric(get(v))]
dahData = dahData[, lapply(.SD, sum, na.rm=TRUE), by=byVars, .SDcols=numVars]

# check if there even are discontinuities (yes there are e.g. COD)
sample = sample(dahData$recipient_isocode, 9)
p1 = ggplot(dahData[recipient_isocode %in% sample & channel=='GFATM'], aes(y=tb_treat_dah_18, x=year)) + 
	geom_point() + 
	facet_wrap(~recipient_isocode, scales='free')
p2 = ggplot(tbOcData[iso3 %in% sample], aes(y=c_new_tsr, x=year)) + 
	geom_point() + 
	facet_wrap(~iso3, scales='free')
grid.arrange(p2,p1)

# merge
data = merge(dahData, tbOcData, by.x=c('recipient_isocode','year'), by.y=c('iso3','year'), all=TRUE)

# grand window variables
data[, nfm:=year>=2015]

# lag of spending
data[, lag_tb_treat_dah_18:=shift(tb_treat_dah_18), by=recipient_isocode]
# -----------------------------------------


# -----------------------------------------
# Analysis

# check if there's an effect of grant window alone
summary(lm(c_new_tsr~nfm + lag_tb_treat_dah_18*channel, data))

# -----------------------------------------
