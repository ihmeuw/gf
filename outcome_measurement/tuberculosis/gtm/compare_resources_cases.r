# ---------------------------------------------------
# David Phillips
# 
# 8/14/2018
# Compute ratios like $/case cured
# ---------------------------------------------------


# ---------------------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# ---------------------------------------------------


# ---------------------------------------------------
# Directories

# # root directory - change this
# dir = 'C:/Users/davidp6/Google Drive/Work/IHME Work/GF/Workshops/gtm_aug2018/'
# 
# # resource tracking data
# rtFile = paste0(dir, 'Day 1/total_resource_tracking_data_tb.csv')
# 
# # drug distribution data
# distrFile = paste0(dir, 'Day 2/GTM-TB-distribution-2013-2018.csv')
# 
# # cohort data
# cohortFile = paste0(dir, 'Day 2/GTM - Tx cohort data 2012-2016.csv')
# 
# # output file
# outFile = paste0(dir, 'Day 2/Resource Comparison.pdf')

outcome_dir = "J:/Project/Evaluation/GF/outcome_measurement/gtm/"
rtFile = "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv"
distrFile = paste0(outcome_dir, "prepped_data/GTM-TB-distribution-2013-2018_with_Unit_Cost.csv")
cohortFile = paste0(outcome_dir, "prepped_data/GTM - Tx cohort data 2012-2016.csv")
outFile = paste0(outcome_dir, "Resource Comparison.pdf")

# ---------------------------------------------------


# ---------------------------------------------------
# Load prep RT data

# load
rtData = fread(rtFile)

# subset to TB only
rtData = rtData[disease=='tb']

# subset to GHE/external from sicoin and GF from budgets
rtData = rtData[country=='Guatemala' & 
                  ((data_source=='sicoin' & financing_source %in% c('ghe','other_dah')) | 
                     (data_source%in%c('fgh','fpm') & financing_source=='gf')) ]

# 
# # convert to usd
# rates = c(7.77226,7.82188,7.84517,7.72041,7.63987,7.58393,7.33155,7.38807)
# conv = data.table(year=seq(2011,2018), rate=rates)
# rtData = merge(rtData, conv, by='year',all.x=TRUE)
# rtData[data_source=='sicoin', budget:=budget/rate]
# rtData[data_source=='sicoin', disbursement:=disbursement/rate]

# aggregate by year
rtData = rtData[, .(budget=sum(budget), disbursement=sum(disbursement)), by=c('financing_source','year')]

# make total aggregate
rtAgg = rtData[, .(budget=sum(budget), disbursement=sum(disbursement)), by='year']
# ---------------------------------------------------


# ---------------------------------------------------
# Load/prep distribution data
# load
distrData = fread(distrFile)
setnames(distrData, old = "Unit Cost (USD)", new = "unitCost")

# aggregate
distrAgg = distrData[Medicine=='RIFAMPICINA', 
                     .(rifampicin=sum(Amount * unitCost, na.rm=TRUE)), by=c("Year")]
# ---------------------------------------------------


# ---------------------------------------------------
# Load/prep cohort data

# load
cohortDataAll = fread(cohortFile)

# subset to totals
cohortData = cohortDataAll[col_name=='TOTAL' & deptocode==0]

# subset to avoid overlapping types
cohortData = cohortData[table %in% c('Nuevos Pulmonares BK+', 
	'Nuevos Pulmonares BK-', 'Nuevos Pediatricos', 
    'Nuevos Extrapulmonares', 'Nuevos TB/VIH', 'Retratamiento')]

# aggregate
cohortAgg = cohortData[row_name %in% c('CURADOS','TRATAMIENTOS COMPLETOS'), 
						.(cured=sum(value, na.rm=TRUE)), by='year']
cohortAgg = merge(cohortAgg, cohortData[, .(total=sum(value, na.rm=TRUE)), by='year'], by='year')

# aggregate including MDR
cohortAgg = cohortData[row_name %in% c('CURADOS','TRATAMIENTOS COMPLETOS'), 
						.(cured=sum(value, na.rm=TRUE)), by='year']
cohortAgg = merge(cohortAgg, cohortData[, .(total=sum(value, na.rm=TRUE)), by='year'], by='year')

# aggregate MDR started treatment only
cohortAggMDR = cohortDataAll[deptocode==0 & grepl('2nd line treatment begun', table), 
							.(second_line_begun=sum(value,na.rm=TRUE)), by='year']
							
# aggregate excluding MDR
cohortAggNoMDR = merge(cohortAgg, cohortAggMDR, by='year')
cohortAggNoMDR[, cured:=cured-second_line_begun]
cohortAggNoMDR[, total:=total-second_line_begun]
cohortAggNoMDR$second_line_begun = NULL
setnames(cohortAggNoMDR, old = c('cured','total'), new = c('cured_first_line','total_first_line'))
# ---------------------------------------------------


# ---------------------------------------------------
# Ratios

# merge data
data = merge(rtAgg, cohortAgg, by='year')
data = merge(data, distrAgg, by.x='year', by.y='Year', all.x=TRUE)
data = merge(data, cohortAggMDR, by='year', all.x=TRUE)
data = merge(data, cohortAggNoMDR, by='year', all.x=TRUE)

# dollars per case cured over time
data[, budget_cured:=budget/cured]
data[, disbursement_cured:=disbursement/cured]

# dollars per case treated with 2nd line over time
data[, budget_second_line:=budget/second_line_begun]
data[, disbursement_second_line:=disbursement/second_line_begun]

# dollars per case treated with 1st line over time
data[, budget_cured_first:=budget/cured_first_line]
data[, disbursement_cured_first:=disbursement/cured_first_line]

# drugs distributed per case cured over time
data[, distr_cured:=rifampicin/cured]

# set up to graph
graphData = melt(data, id='year')
graphData[variable=='budget_cured', label:='Budget/Cured']
graphData[variable=='disbursement_cured', label:='Disbursement/Cured']
graphData[variable=='budget_second_line', label:='Budget/Second-Line']
graphData[variable=='disbursement_second_line', label:='Disbursement/Second-Line']
graphData[variable=='budget_cured_first', label:='Budget/Cured']
graphData[variable=='disbursement_cured_first', label:='Disbursement/Cured']
graphData[!is.finite(value), value:=NA]
# ---------------------------------------------------


# ---------------------------------------------------
# Graph
p1 = ggplot(graphData[variable %in% c('budget_cured','disbursement_cured')], 
	aes(y=value, x=year, color=label)) + 
	geom_line(size=1.5) +
	geom_point(size=3.5, color='grey45') + 
	labs(title='Dollars Budgeted and Disbursed Compared to Cases Cured', 
		y='USD per Case Cured', x='Year',color='',
		caption='Resources from Government and Global Fund Combined') + 
	theme_bw()

p2 = ggplot(graphData[variable %in% c('budget_second_line','disbursement_second_line')], 
	aes(y=value, x=year, color=label)) + 
	geom_line(size=1.5) +
	geom_point(size=3.5, color='grey45') + 
	labs(title='Dollars Budgeted and Disbursed Compared to Cases Started Second-Line Treatment', 
		y='USD per Case Started Second-Line Treatment', x='Year',color='',
		caption='Resources from Government and Global Fund Combined') + 
	theme_bw()

p3 = ggplot(graphData[variable %in% c('budget_cured_first','disbursement_cured_first')], 
	aes(y=value, x=year, color=label)) + 
	geom_line(size=1.5) +
	geom_point(size=3.5, color='grey45') + 
	labs(title='Dollars Budgeted and Disbursed Compared to Cases Cured (Excluding MDR)', 
		y='USD per Case Cured (Excluding MDR)', x='Year',color='',
		caption='Resources from Government and Global Fund Combined') + 
	theme_bw()

p4 = ggplot(data, aes(y=distr_cured, x=year, label=rifampicin)) + 
	geom_line(size=1.5) +
	geom_point(size=3.5, color='grey45') + 
	geom_text(hjust=1, vjust=0) + 
	labs(title='Rifampicin Distributed Compared to Cases Cured', 
		y='Units Distributed per Case Cured', x='Year', 
		caption='Labels: Total Units Distributed') + 
	theme_bw()
# ---------------------------------------------------


# ---------------------------------------------------
# save
pdf(outFile, height=5.5, width=7)
p1
p2
p3
p4
dev.off()
# ---------------------------------------------------
