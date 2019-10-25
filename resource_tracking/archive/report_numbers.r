# ----------------------------------------------
# David Phillips
#
# 1/22/2017
# Look up numbers for country/synthesis reports
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
# --------------------


# -------------------------------------------------------------------------
# Files and directories

# data directory (temporary location)
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/'

# input file
inFile = paste0(dir, 'cleaned_total_data.csv')
# -------------------------------------------------------------------------


# --------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# identify 3-year periods
ends = c(seq(from=2020, to=2000, by=-3))
starts = c(seq(from=2018, to=2000, by=-3))
for(i in seq_along(starts)) data[year>=starts[i] & year<=ends[i], window:=paste(starts[i], '-', ends[i])]

# identify funds explicitly allocated toward key populations
data[, kp:=ifelse(program_activity=='Key and vulnerable populations','kp', 'other')]

# sum KPs
kps = data[, sum(budget, na.rm=TRUE), by=c('country','disease','kp','window','data_source')]
kps = dcast(kps, country+disease+window+data_source~kp)

# compute percentages
kps[, pct:=kp/other]
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Display numbers

# display KP totals before and after (in millions)
kps[window %in% c('2015 - 2017','2018 - 2020'), 
	round(sum(kp/100000, na.rm=TRUE))/10, 
	by=c('country','window','data_source')]


# display KP percentages before and after (in millions)
tmp = kps[window %in% c('2015 - 2017','2018 - 2020'), 
	list(kp=sum(kp, na.rm=TRUE), other=sum(other,na.rm=T)), 
	by=c('country','window')]
tmp[, pct:=kp/(kp+other)]
tmp

# display KP totals before and after (in millions)
kps[window %in% c('2018 - 2020'), 
	round(sum(kp/100000, na.rm=TRUE))/10, 
	by=c('country','window','data_source')]

# --------------------------------------------------------------------
