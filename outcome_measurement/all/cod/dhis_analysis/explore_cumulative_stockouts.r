# -----------------------------------
# David Phillips
# 
# 10/8/2018
# Explore whether the stockout data appears to be cumulative
# Which would explain why we sometimes get values >31
# -----------------------------------


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

# sort
data = data[order(element_eng, org_unit_id, date)]

# identify OOB
data[, oob:=value>31]
# --------------------------------------------------


# --------------------------------------------------
# loop over facilities with >31
plots = list()
facs = unique(data[value>31, c('org_unit','org_unit_id')])
for(i in seq_along(facs$org_unit)) { 
	o = facs$org_unit_id[i]
	f = facs$org_unit[i]
	plots[[i]] = ggplot(data[org_unit_id==o], aes(y=value,x=date)) + 
		geom_line() + 
		geom_point(aes(color=oob)) + 
		geom_text(aes(label=value), vjust=-.5, hjust=1) + 
		labs(title=v, subtitle=f,y='Number of Days of Stockout',x='',color='>31') + 
		theme_bw()
}

# print
pdf(outFile, height=5.5, width=9)
for(p in seq(length(plots))) print(plots[[p]])
dev.off()
# --------------------------------------------------
