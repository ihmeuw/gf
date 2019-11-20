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
inFile1 = paste0(dir, '/core_2017-2019registerunfundedqualitydemand_tool_en_30Nov2018.xlsx')
inFile2 = paste0(dir, '/core_2017-2019registerunfundedqualitydemand_tool_en_21Dec2018.xlsx')
inFile3 = paste0(dir, '/core_2017-2019registerunfundedqualitydemand_tool_en_12Jun2019.xlsx')
inFile4 = paste0(dir, '/core_2017-2019registerunfundedqualitydemand_tool_en.xlsx')
outFile = paste0(dir, '/paar_graphs.pdf')
# -----------------------------------------


# --------------------------------------------------------------------------
# Load/prep data

# load all four versions
data1 = data.table(read_excel(inFile1, sheet='Data Set'))
data2 = data.table(read_excel(inFile2, sheet='Data Set'))
data3 = data.table(read_excel(inFile3, sheet='Data Set'))
data4 = data.table(read_excel(inFile4, sheet='Data Set'))

# label versions
data1[, version:=as.Date('2018-11-30')]
data2[, version:=as.Date('2018-12-21')]
data3[, version:=as.Date('2019-06-12')]
data4[, version:=as.Date('2019-10-31')]

# append versions
data = rbind(data1, data2, fill=TRUE)
data = rbind(data, data3, fill=TRUE)
data = rbind(data, data4, fill=TRUE)

# subset to pce countries
countries = c('Congo (Democratic Republic)','Guatemala','Senegal','Uganda')
countries = c(countries, 'Sudan','Cambodia','Myanmar','Mozambique')
data = data[Applicant %in% countries]

# subset to active UQD only
data = data[get('UQD status')=='Active UQD']
# --------------------------------------------------------------------------


# -----------------------------------------
# Summarize UQD by module and version

# amount of money by module
data[, sum(get('UQD amount US$')), by=c('Disease Component','Module', 'version')][order(-V1)]
agg1 = data[, sum(get('UQD amount US$')), by=c('Applicant','Disease Component','Module', 'version')][order(Applicant, -V1)]

# number of line items by module
data[, .N, by=c('Disease Component','Module', 'version')][order(-N)]
agg2 = data[, .N, by=c('Applicant','Disease Component','Module', 'version')][order(Applicant, -N)]
# -----------------------------------------


# -----------------------------------------
# Graph

# set up to graph
setnames(agg1, 'Disease Component', 'Disease')
agg1[, Module:=paste(Disease, Module)]
agg1[, Module:=str_wrap(Module,35)]

# graph the most recent version by countr
plots = list()
for(c in seq_along(countries)) {
	plots[[c]] = ggplot(agg1[Applicant==countries[c] & version=='2019-10-31'], aes(y=V1/1000000, x=reorder(Module, -V1), fill=Disease)) + 
		geom_bar(stat='identity') + 
		labs(title='Total Value of PAAR by Module', subtitle=countries[c], 
			y='Sum of PAAR (in millions)', x='', caption='UQD Dated 31Oct2019') +
		theme_bw() + 
		theme(axis.text.x=element_text(angle=315, hjust=0))
}

# graph different versions by country
plots2 = list()
for(c in seq_along(countries)) {
	plots2[[c]] = ggplot(agg1[Applicant==countries[c]], aes(y=V1/1000000, x=reorder(Module, -V1), fill=factor(version))) + 
		geom_bar(stat='identity', position='dodge') + 
		labs(title='Total Value of PAAR by Module and UQD Version', subtitle=countries[c], 
			y='Sum of PAAR (in millions)', x='', fill='Version of UQD') +
		theme_bw() + 
		theme(axis.text.x=element_text(angle=315, hjust=0))
}

# save pdf
pdf(outFile, height=5.5, width=9)
for(p in seq_along(plots)) print(plots[[p]])
for(p in seq_along(plots2)) print(plots2[[p]])
dev.off()
# -----------------------------------------
