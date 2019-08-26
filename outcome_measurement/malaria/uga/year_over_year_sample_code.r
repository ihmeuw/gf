# ---------------------------------------------------------------------------------------------
# David Phillips
# 
# 8/22/2019
# Sample code to help with a "year-over-year" analysis of the recent malaria upsurge in Uganda
# 
#	Note: this assumes the data are formatted with four columns:
#	district (character) - name of district
#	year (number) - year in which cases occurred (YYYY)
#	month (number) - month in which cases occurred (MM)
#	number_of_cases (number) - number of cases of malaria in this district-year-month
# ---------------------------------------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# ------------------


# --------------------------------------------------------------------
# Files and directories

# root directory CHANGE THIS TO THE LOCATION OF YOUR DATA
dir = 'C:/local/examples/'

# name of input data file CHANGE THIS TO THE NAME OF YOUR DATA FILE
inFile = paste0(dir, 'input_data.csv')

# output file
outFile = paste0(dir, 'year_over_year_cases.pdf')
# --------------------------------------------------------------------


# --------------------------------------------------------------------------
# Load/prep data

# load data
data = fread(inFile)

# format month and year like a date (assign to 15th day of month)
data[, date:=as.Date(paste(year, month, '15', sep='-'))]

# ensure the data are sorted
data = data[order(district, year, month)]

# make a national total
national = data[, .(number_of_cases=sum(number_of_cases)), by=c('year','month','date')]

# compute "year over year", i.e. difference from last year in the same month
data[, yoy:=number_of_cases-shift(number_of_cases, 12), by='district']

# compute "year over year" for national numbers too
national[, yoy:=number_of_cases-shift(number_of_cases, 12)]
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Graph year-over-year at national and district level

# national level
p1 = ggplot(national, aes(y=yoy, x=date)) + 
	geom_point() + 
	geom_line() + 
	geom_hline(yintercept=0) + 
	labs(title='Year-Over-Year Malaria Cases - National Level', 
		subtitle='(Difference from Previous Year in the Same Month)', 
		y='Change in Malaria Cases from Previous Year', x='') + 
	theme_bw()

# district level
plots = lapply(unique(data$district), function(d) {
	ggplot(data[district==d], aes(y=yoy, x=date)) + 
		geom_point() + 
		geom_line() + 
		geom_hline(yintercept=0) + 
		labs(title=paste('Year-Over-Year Malaria Cases -', d, 'District'), 
			subtitle='(Difference from Previous Year in the Same Month)', 
			y='Change in Malaria Cases from Previous Year', x='') + 
		theme_bw()
})
# --------------------------------------------------------------------------


# ---------------------------------------------
# Save graphs
pdf(outFile, height=6, width=8)
p1
for(i in seq(length(plots))) print(plots[[i]])
dev.off()
# ---------------------------------------------
