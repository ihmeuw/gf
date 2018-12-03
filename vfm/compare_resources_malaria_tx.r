# ---------------------------------------------------
# David Phillips
# 
# 8/24/2018
# Compute ratios like $/case treated for malaria in Uganda
# ---------------------------------------------------


# ---------------------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# ---------------------------------------------------


# ----------------------------------------------------------------------------------------------
# Directories

# root directory - change this
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')

# resource tracking data
rtFile = paste0(dir, 'resource_tracking/multi_country/mapping/total_resource_tracking_data.csv')

# treatment data extracted from a graph
txFile = paste0(dir, 'outcome_measurement/uga/hmis/confirmed_malaria_treated.csv')

# output file
outFile = paste0(dir, 'outcome_measurement/uga/visualizations/Resource Comparison.pdf')
# ----------------------------------------------------------------------------------------------


# ---------------------------------------------------
# Load prep RT data

# load
rtData = fread(rtFile)

# subset to UGA malaria
rtData = rtData[disease=='malaria' & country=='Uganda' & year>2005]

# subset to FGH disbursement actuals
rtDataActuals = rtData[data_source=='fgh' & fin_data_type=='actual']
rtDataActuals = dcast.data.table(rtDataActuals, year+financing_source~module, value.var='disbursement')

# get an estimate of the pct treatment from non-GF
pctTx = rtDataActuals[financing_source!='gf', .(tx=sum(mal_treat_dah_17), total=sum(total_mal_17)), by='year']
pctTx[, pct_tx:=tx/total]
fit = lm(pct_tx~year, pctTx)

# predict the percentage in the future
year = data.table(year=seq(2017, 2020))
pctTx = rbind(pctTx, year, fill=TRUE)
pctTx[, pred:=predict(fit, newdata=pctTx)]
pctTx[is.na(pct_tx), pct_tx:=pred]

# compute the non-gf malaria percentage
nongf = rtDataActuals[financing_source!='gf', .(nongf=sum(total_mal_17)), by='year']
total = rtDataActuals[, .(total=sum(total_mal_17)), by='year']
nongf = merge(nongf, total, by='year')
nongf[, pct:=nongf/total]

# extend the 2016 non-gf pct forward
non_gf_pct_2016 = nongf[year==2016]$pct
nongf = rbind(nongf, year, fill=TRUE)
nongf[is.na(pct), pct:=non_gf_pct_2016]

# compute the non-GF treatment total in the forecasts
forecasts = rtData[data_source=='fgh' & financing_source=='dah' & fin_data_type=='model_estimates' & year<=2020]
forecasts = merge(forecasts, nongf[,c('year','pct'), with=FALSE], by='year')
forecasts = merge(forecasts, pctTx[,c('year','pct_tx'), with=FALSE], by='year')
forecasts[, nongf_tx:=disbursement*pct*pct_tx]

# compute the GF treatment total from the budgets
txInts = c('Facility-based treatment', 'iCCM', 'Severe malaria', 'Private sector case management')
gf = rtData[data_source=='fpm' & abbrev_intervention %in% txInts, .(gf_tx=sum(budget)), by='year']

# merge gf and nongf
tx_spend = merge(gf, forecasts[, c('year','nongf_tx'), with=FALSE], by='year')
tx_spend[, tx_spend:=gf_tx+nongf_tx]

# amortize
frame = expand.grid(year=seq(2015,2020), month=seq(12))
tx_spend = merge(tx_spend, frame, by='year')
tx_spend[, tx_spend:=tx_spend/12]
# ---------------------------------------------------


# ---------------------------------------------------
# Load/prep treatment data

# load
tx_counts = fread(txFile)
# ---------------------------------------------------


# ---------------------------------------------------
# Ratios

# merge data
data = merge(tx_spend, tx_counts, by=c('year','month'))

# compute ratio
data[, dah_case:=tx_spend/cases_treated]

# format dates
data[, date:=as.Date(paste0('01-',month,'-',year), '%d-%m-%Y')]

# identify which is based on budget and which is based on disbursement
data[year<=2016, fin_type:='Reported Disbursements']
data[year>2016, fin_type:='Expected Budgets']
# ---------------------------------------------------


# ---------------------------------------------------
# Graph
p1 = ggplot(data, aes(y=dah_case, x=date, color=fin_type)) + 
	geom_smooth(aes(y=dah_case, x=date), inherit.aes=FALSE) + 
	geom_line(size=1.5) +
	geom_point(aes(size=num_cases/1000000), color='#08519c') + 
	scale_color_manual('', values=c('grey45','grey15')) + 
	scale_size_continuous('N Cases Reported\n(millions)', range=c(1,3)) + 
	labs(title='Development Assistance for Malaria Treatment\nCompared to Confirmed Cases Treated', 
		subtitle='Budgeted or Disbursed', y='USD from Donors per Case Treated', x='Year', color='',
		caption='Annually-Reported Resources Amortized Over 12 Months') + 
	theme_bw(base_size=16)

# ---------------------------------------------------


# ---------------------------------------------------
# save
pdf(outFile, height=5.5, width=7)
p1
dev.off()
# ---------------------------------------------------
