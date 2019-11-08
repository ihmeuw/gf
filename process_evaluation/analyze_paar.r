# -----------------------------------------
# David Phillips
#
# 11/8/2019
# Analyze the PAAR
# Data downloaded from https://www.theglobalfund.org/media/6578/core_2017-2019registerunfundedqualitydemand_tool_en.xlsx?u=636679305150000000 
# -----------------------------------------


# -----------------------------------------
# Set up R
rm(list=ls())
library(readxl)
library(data.table)
library(stringr)
library(ggplot2)
# -----------------------------------------

# -----------------------------------------
# Files and directories
dir = 'C:/Users/davidp6/Downloads'
inFile = paste0(dir, "/core_2017-2019registerunfundedqualitydemand_tool_en.xlsx")
outFile = paste0(dir, '/paar_graphs.pdf')
# -----------------------------------------

# --------------------------------------------------------------------------
# Load/prep data

# load
data = data.table(read_excel(inFile, sheet='Data Set'))

# subset to pce countries
countries = c('Congo (Democratic Republic)','Guatemala','Senegal','Uganda')
data = data[Applicant %in% countries]

# subset to active UQD only
data = data[get('UQD status')=='Active UQD']
# --------------------------------------------------------------------------

# -----------------------------------------
# Summarize UQD by module

# amount of money by module
data[, sum(get('UQD amount US$')), by=c('Disease Component','Module')][order(-V1)]
agg1 = data[, sum(get('UQD amount US$')), by=c('Applicant','Disease Component','Module')][order(Applicant, -V1)]

# number of line items by module
data[, .N, by=c('Disease Component','Module')][order(-N)]
agg2 = data[, .N, by=c('Applicant','Disease Component','Module')][order(Applicant, -N)]
# -----------------------------------------


# -----------------------------------------
# Graph

# set up to graph
setnames(agg1, 'Disease Component', 'Disease')
agg1[, Module:=paste(Disease, Module)]
agg1[, Module:=str_wrap(Module,35)]

# loop over countries
plots = list()
for(c in seq_along(countries)) {
	plots[[c]] = ggplot(agg1[Applicant==countries[c]], aes(y=V1/1000000, x=reorder(Module, -V1), fill=Disease)) + 
		geom_bar(stat='identity') + 
		labs(title='Total Value of PAAR by Module', subtitle=countries[c], 
			y='Sum of PAAR (in millions)', x='') +
		theme_bw() + 
		theme(axis.text.x=element_text(angle=315, hjust=0))
}

# save pdf
pdf(outFile, height=5.5, width=9)
for(p in seq_along(plots)) print(plots[[p]])
dev.off()
# -----------------------------------------
