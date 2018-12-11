# -----------------------------------------------------------------------------
# David Phillips
# 
# 10/8/2018
# Explore whether the stockout data appears to be cumulative
# Which would explain why we sometimes get values >31
# Possible reasons for >31:
# 1. Forgot how many days there are in the month
# 2. Cumulative reporting 
# 		a. value - lag(value) <= 32 (32 because they may have committed #1 also)
# 		b. previous N values are missing, but value - (31*N) <= 31
# 3. Repeated value from previous month (value==lag(value))
# 4. Clear typo (value > 31 * N months by a lot)
# 5. Unexplained (value - lag(value) > 31, but not 1, 2 or 4)
# 
# Big problem: impossible to determine if cumulative reporting is used when values are < 31
# -----------------------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(scales)
library(RColorBrewer)
# ------------------


# ------------------------------------------------------------------------
# Files and directories

# switch for the cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')

# data directory
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# codebook file
codebookFile = paste0(dir, 'catalogues/data_elements_cod.csv')

# input file where the given variable can be found
inFile = paste0(dir, 'prepped/sigl_drc_01_2015_07_2018_prepped.rds')

# output file
outFile = paste0(dir, '../visualizations/snis_stockout_outliers.pdf')
# ------------------------------------------------------------------------


# --------------------------------------------------
# Load/prep data

# load codebook
codebook = fread(codebookFile)

# identify stockout variables
stockoutCodebook = codebook[grepl('stock', element) & 
					grepl('out', element) & type!='']
variables = stockoutCodebook[order(-type, -drug, element)]$element

# load LMIS data
data = readRDS(inFile)

# subset to the specified variable(s) post 2016
data = data[element_eng %in% variables & year>=2017]
v = 'Artesunate-amodiaquine C1 12.1 (2-11 months) + 25mg tablet 67,5mg - days out of stock'
data = data[element_eng==v]

# ensure order for lagging
data = data[order(org_unit_id, element_eng, date)]

# rectangularize
frame = expand.grid(org_unit_id=unique(data$org_unit_id), 
					date=unique(data$date), 
					element_eng=unique(data$element_eng))
data = merge(data, frame, by=c('org_unit_id','date','element_eng'), all=TRUE)
# --------------------------------------------------


# --------------------------------------------------
# Identify different explanations for invalid numbers
# 1. Forgot how many days there are in the month
# 2. Cumulative reporting 
# 		a. value - lag(value) <= 32 (32 because they may have committed #1 also)
# 		b. previous N values are missing, but value - (31*N) <= 31
# 3. Repeated value from previous month (value==lag(value))
# 4. Clear typo (value > 31 * N months by a lot)
# 5. Unexplained (value - lag(value) > 31, but not 1, 2 or 4)

# identify type 1
data[, oob_type:=as.character(NA)]
data[, days_this_month:=monthDays(date)]
data[value>days_this_month & value %in% c(29,30,31), oob_type:='1']

# identify type 2a
data[, lag:=shift(value), by=c('org_unit_id','element_eng')]
data[, diff:=value-lag]
data[value>days_this_month & diff<=32 & is.na(oob_type), oob_type:='2a']

# identify type 2b
data[, consecutive_nas:=sequence(rle(is.na(data$value))$lengths)]
data[!is.na(value), consecutive_nas:=NA]
data[!is.na(shift(consecutive_nas)) & value>days_this_month & 
	(value/(31*shift(consecutive_nas))<31) & is.na(oob_type), oob_type:='2b']

# identify type 3
data[value>days_this_month & diff==0 & is.na(oob_type), oob_type:='3']

# identify type 4
data[value>31*12*4 & is.na(oob_type), oob_type:='4']

# identify type 5
data[value>days_this_month & is.na(oob_type), oob_type:='5']

# label types
data[, type_description:=as.character(NA)]
data[oob_type=='1', type_description:='Type 1: Wrong month length']
data[oob_type=='2a', type_description:='Type 2: Cumulative reporting']
data[oob_type=='2b', type_description:='Type 3: Probably cumulative reporting with missing values']
# data[oob_type=='3', type_description:='Type 3: Repeated value from previous month']
data[oob_type=='4', type_description:='Type 4: Clear data entry error']
data[oob_type=='5', type_description:='Type 5: Unexplained']
# --------------------------------------------------


# --------------------------------------------------
# Do something about each type

# save original value
data[, orig_value:=value]

# assume 1 is just an error, replace with length of month
data[oob_type=='1',  value:=days_this_month]

# assume 2a is cumulative: subtract previous month
data[oob_type=='2a', value:=diff]
data[oob_type=='2a' & value>days_this_month, value:=days_this_month]

# assume 2b is cumulative: subtract out what the previous month must have been to make it cumulative
data[oob_type=='2b', value:=orig_value%%30] 

# assume 3 is mistaken cumulative reporting: replace with length of month
data[oob_type=='3',  value:=days_this_month]

# assume type 4 is unknown: replace with NA
data[oob_type=='4',  value:=NA]

# assume type 5 is unknown: replace with NA
data[oob_type=='5',  value:=NA]
# --------------------------------------------------


# --------------------------------------------------
# loop over facilities with OOB values
plots = list()
facs = unique(data[!is.na(oob_type), c('org_unit','org_unit_id','oob_type'), with=F])
facs = facs[order(oob_type, org_unit, org_unit_id)]
facs[, n:=seq_len(.N), by='oob_type']
facs = facs[n<6]
for(i in seq_along(facs$org_unit_id)) { 
	o = facs$org_unit_id[i]
	f = facs$org_unit[i]
	plots[[i]] = ggplot(data[org_unit_id==o], aes(y=value,x=date)) + 
		geom_line() + 
		geom_point(aes(shape='New Value')) + 
		geom_point(data=data[org_unit_id==o & !is.na(oob_type)], 
			aes(y=orig_value, color=type_description, shape='Original Value')) + 
		geom_text(aes(label=value), vjust=-.5, hjust=1) + 
		geom_text(data=data[org_unit_id==o & !is.na(oob_type)], 
			aes(y=orig_value, label=orig_value, color=type_description), vjust=-.5, hjust=1) + 
		scale_shape_manual(values=c('Original Value'=1, 'New Value'=19)) + 
		labs(title=f, subtitle=v, y='Number of Days of Stockout',x='',color='Issue') + 
		theme_bw()
}

# print
pdf(outFile, height=5.5, width=9)
for(p in seq(length(plots))) print(plots[[p]])
dev.off()
# --------------------------------------------------
