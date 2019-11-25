# ---------------------------------------------
# David Phillips
# 
# 11/11/2019
# Make one big graph of unit costs
# ---------------------------------------------


# ---------------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# ---------------------------------------------


# -----------------------------------------------------------------------------------------------
# Files and directories
dir = 'J:/Project/Evaluation/GF/vfm/'
inFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr_with_sept_download_data.rds')
outFile = paste0(dir, 'visualizations/pqr_aggregate_ts_graph.pdf')
# -----------------------------------------------------------------------------------------------


# --------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(inFile)

# drop outliers
data = data[unit_cost_usd<100]

# subset to interesting categories
categories = c('anti_malaria_medicine','anti_retroviral', 
		'anti_tb_medicine_first_line', 'anti_tb_medicine_second_line', 
		'bednet', 'diagnostic_test_hiv', 'diagnostic_test_malaria',
		'diagnostic_test_tb')
data = data[product_category %in% categories]

# label categories better
data[product_category=='anti_malaria_medicine', product_category:='Anti-Malarials']
data[product_category=='anti_retroviral', product_category:='ARVs']
data[product_category=='anti_tb_medicine_first_line', product_category:='TB Medications (First Line)']
data[product_category=='anti_tb_medicine_second_line', product_category:='TB Medications (Second Line)']
data[product_category=='bednet', product_category:='LLINs']
data[product_category=='diagnostic_test_hiv', product_category:='HIV Tests']
data[product_category=='diagnostic_test_malaria', product_category:='Malaria Tests']
data[product_category=='diagnostic_test_tb', product_category:='TB Tests']

# make combined drug name/size
data[, commodity:=paste0(product_name_en,'-',description)]

# drop NA
data = data[!is.na(unit_cost_usd) & is.finite(unit_cost_usd)]

# look up sample size by commodity
data[, N:=.N, by=commodity]
# --------------------------------------------------------------------


# --------------------------------------------------------------------------------------------------
# Fit trends
lmFit = glm(log(unit_cost_usd)~purchase_order_date*commodity, data=data, weight=total_units_in_order)
lmFit = glm(log(unit_cost_usd)~purchase_order_date, data=data[product_category=='Anti-Malarials'], weight=total_units_in_order)

# get fitted values by product category and date
fits = data.table(expand.grid(purchase_order_date=seq(min(data$purchase_order_date), max(data$purchase_order_date), by='month'), 
	commodity=unique(data[N>10]$commodity)))
fits[, fit:=exp(predict(lmFit, newdata=fits))]

# display how the "average-of-averages" has changed by product category (to account for different trends by commodity)
fits = merge(fits, unique(data[,c('product_category','commodity'),with=F]))
display = fits[purchase_order_date=='2009-04-26' | purchase_order_date=='2019-04-26', mean(fit), by=c('purchase_order_date','product_category')]

# display
dcast(display, product_category~purchase_order_date)
# --------------------------------------------------------------------------------------------------


# ---------------------------------------------
# Graph
# pdf(outFile, height=5.5, width=8

# graph drugs
ggplot(data[grepl('ARV|Anti|Med', product_category)], aes(y=unit_cost_usd, x=purchase_order_date, 
		size=total_units_in_order, color=commodity)) + 
	geom_point(alpha=.5) + 
	facet_wrap(~product_category, scales='free') + 
	theme_minimal()
	
# graph tests and bednets
ggplot(data[grepl('Test|LLIN', product_category)], aes(y=unit_cost_usd, x=purchase_order_date, 
		weight=total_units_in_order, size=total_units_in_order)) + 
	geom_point(alpha=.5) + 
	facet_wrap(~product_category, scales='free') + 
	theme_minimal()
# dev.off()
# ---------------------------------------------
	