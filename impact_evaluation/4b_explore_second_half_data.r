# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# Exploratory visualizations to get a good sense of the impact evaluation data
# The current working directory should be the root of this repo (set manually by user)
# ------------------------------------------------

source('./impact_evaluation/_common/set_up_r.r')

# ----------------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(outputFile2c)

# test unique identifiers
test = nrow(data)==nrow(unique(data[,c('health_zone','date'), with=F]))
if (test==FALSE) stop(paste('Something is wrong. health_zone and date do not uniquely identify rows.'))

# last-minute prep that shouldn't be necessary after bugs are fixed
# data[mildMalariaTreated>=2*newCasesMalariaMild, mildMalariaTreated:=NA]
# data[severeMalariaTreated>=2*newCasesMalariaSevere, severeMalariaTreated:=NA]

# extrapolate population and incidence
for(h in unique(data$health_zone)) {
	glmFit = glm(population~date, 'poisson', data[health_zone==h])
	preds = exp(predict(glmFit, newdata=data[health_zone==h]))
	data[health_zone==h, pred:=preds]
	data[health_zone==h & is.na(population), population:=pred]
	data$pred=NULL
	glmFit = glm(incidence~date, 'poisson', data[health_zone==h])
	preds = exp(predict(glmFit, newdata=data[health_zone==h]))
	data[health_zone==h, pred:=preds]
	data[health_zone==h & is.na(incidence), incidence:=pred]
	data$pred=NULL
}
# ----------------------------------------------------------------------------


# ----------------------------------------------
# Set up to graph

# aggregate to national level
aggVars = c('act_coverage', 'mildMalariaTreated', 
	'severeMalariaTreated', 'itn_coverage', 'incidence', 
	'prevalence', 'mortality', 'malariaDeaths', 
	'newCasesMalariaMild', 'newCasesMalariaSevere', 'population')
data_agg = data[, lapply(.SD, sum, na.rm=T), by=c('date','year'), .SDcols=aggVars]
data_agg[year>=2016, mortality:=NA]
data_agg[year>=2017, act_coverage:=NA]
data_agg[year>=2017, itn_coverage:=NA]
data_agg[year>=2018, prevalence:=NA]
data_agg[year>=2018, malariaDeaths:=NA]

# make rates at HZ and national level
data[, act_coverage_rate:=act_coverage/incidence]
data[, mildMalariaTreated_rate:=mildMalariaTreated/newCasesMalariaMild]
data[, severeMalariaTreated_rate:=severeMalariaTreated/newCasesMalariaSevere]
data[, itn_coverage_rate:=itn_coverage/population]
data[, incidence_rate:=(incidence/12)/population*100000]
data[, prevalence_rate:=(prevalence/12)/population]
data[, mortality_rate:=(mortality/12)/population*100000]
data[, malariaDeaths_rate:=malariaDeaths/population*100000]
data[, newCasesMalariaMild_rate:=newCasesMalariaMild/population*100000]
data[, newCasesMalariaSevere_rate:=newCasesMalariaSevere/population*100000]
data_agg[, act_coverage_rate:=act_coverage/incidence]
data_agg[, mildMalariaTreated_rate:=mildMalariaTreated/newCasesMalariaMild]
data_agg[, severeMalariaTreated_rate:=severeMalariaTreated/newCasesMalariaSevere]
data_agg[, itn_coverage_rate:=itn_coverage/population]
data_agg[, incidence_rate:=(incidence/12)/population*100000]
data_agg[, prevalence_rate:=(prevalence/12)/population]
data_agg[, mortality_rate:=(mortality/12)/population*100000]
data_agg[, malariaDeaths_rate:=malariaDeaths/population*100000]
data_agg[, newCasesMalariaMild_rate:=newCasesMalariaMild/population*100000]
data_agg[, newCasesMalariaSevere_rate:=newCasesMalariaSevere/population*100000]

# make lags
data[, lag_mildMalariaTreated_rate:=
	data.table::shift(mildMalariaTreated_rate), by='health_zone']
data[, lag_severeMalariaTreated_rate:=
	data.table::shift(severeMalariaTreated_rate), by='health_zone']
data_agg[, lag_mildMalariaTreated_rate:=
	data.table::shift(mildMalariaTreated_rate)]
data_agg[, lag_severeMalariaTreated_rate:=
	data.table::shift(severeMalariaTreated_rate)]

# melt long
long = melt(data, id.vars=c('health_zone','date','year'))
long_agg = melt(data_agg, id.vars=c('date','year'))

# identify variables
progVars = c('mildMalariaTreated', 'severeMalariaTreated', 
	'malariaDeaths', 'newCasesMalariaMild', 'newCasesMalariaSevere')
long[, source:='Model Estimates']
long_agg[, source:='Model Estimates']
for(v in progVars) long[grepl(v,variable), source:='Program Data']
for(v in progVars) long_agg[grepl(v,variable), source:='Program Data']
outcomeVars = c('act_coverage', 'mildMalariaTreated', 
	'severeMalariaTreated', 'itn_coverage')
long[, section:='Impact']
long_agg[, section:='Impact']
for(v in outcomeVars) long[grepl(v,variable), section:='Outcomes']
for(v in outcomeVars) long_agg[grepl(v,variable), section:='Outcomes']
long[, mortality:=ifelse(grepl('mortality|Deaths',variable),'Mortality','')]
long_agg[, mortality:=ifelse(grepl('mortality|Deaths',variable),'Mortality','')]

# ensure missing isn't treated line zero
long_agg[year>=2017 & source=='Model Estimates', value:=NA]

# apply limits
data[lag_mildMalariaTreated_rate>2, lag_mildMalariaTreated_rate:=NA]
data[lag_severeMalariaTreated_rate>2.5, lag_severeMalariaTreated_rate:=NA]
data[mildMalariaTreated_rate>2, mildMalariaTreated_rate:=NA]
data[severeMalariaTreated_rate>2.5, severeMalariaTreated_rate:=NA]
data[newCasesMalariaMild_rate>100000, newCasesMalariaMild_rate:=NA]
data[newCasesMalariaSevere_rate>100000, newCasesMalariaSevere_rate:=NA]
data[malariaDeaths_rate>1000, malariaDeaths_rate:=NA]
long[variable=='mildMalariaTreated_rate' & value>2, value:=NA]
long[variable=='severeMalariaTreated_rate' & value>2.5, value:=NA]
long[variable=='lag_mildMalariaTreated_rate' & value>2, value:=NA]
long[variable=='lag_severeMalariaTreated_rate' & value>2.5, value:=NA]
long[variable=='newCasesMalariaMild_rate' & value>100000, value:=NA]
long[variable=='newCasesMalariaSevere_rate' & value>100000, value:=NA]
long[variable=='malariaDeaths_rate' & value>1000, value:=NA]

# load "node table" for convenient labels FIX THIS FILE PATH
# nodeTable = fread('C:/local/gf/impact_evaluation/visualizations/vartable2.csv')
# ----------------------------------------------


# ----------------------------------------------
# Make time series graphs

# national
p1 = ggplot(long_agg[grepl('rate',variable) & !grepl('lag_',variable) & 
		section=='Outcomes'], 
		aes(y=value, x=date, color=variable, linetype=source)) + 
	geom_line(size=1) + 
	labs(title='Time Series - Outcomes', y='Coverage', 
		x='Quarter', color='Intervention') + 
	theme_bw(base_size=16)

# health zone
actVars = c('act_coverage_rate','mildMalariaTreated_rate')
p2 = ggplot(long[variable %in% actVars], 
		aes(y=value, x=date, group=health_zone)) + 
	geom_line(alpha=.25, linetype='dashed') + 
	facet_wrap(~source) + 
	labs(title='Time Series - ACT Coverage', subtitle='By Health Zone', 
		y='Coverage', x='Month', color='Intervention') + 
	theme_bw(base_size=16)

# national
p3 = ggplot(long_agg[grepl('rate',variable) & section=='Impact'], 
		aes(y=value, x=date, color=variable, linetype=source)) + 
	geom_line(size=1) + 
	facet_wrap(~mortality, scales='free_y') + 
	labs(title='Time Series - Impact', y='Coverage', 
		x='Quarter', color='Intervention') + 
	theme_bw(base_size=16)

# health zone
incVars = c('incidence_rate','newCasesMalariaMild_rate')
p4 = ggplot(long[variable %in% incVars], 
		aes(y=value, x=date, group=health_zone)) + 
	geom_line(alpha=.25, linetype='dashed') + 
	facet_wrap(~source) + 
	labs(title='Time Series - Incidence Rates', subtitle='By Health Zone', 
		y='Incidence per 100,000 Population', x='Month', color='Intervention') + 
	theme_bw(base_size=16)
# ----------------------------------------------


# ----------------------------------------------
# Make distribution graphs

# national
p5 = ggplot(long_agg[grepl('rate',variable) & !grepl('lag_',variable) & 
		section=='Outcomes'], 
		aes(x=value)) + 
	geom_histogram() + 
	facet_wrap(~variable, scales='free') + 
	labs(title='Histograms - Outcomes', y='Frequency', x='Coverage') + 
	theme_bw(base_size=16)	
	
# health zone
p6 = ggplot(long[grepl('rate',variable) & section=='Outcomes' & value<2], 
		aes(x=value)) + 
	geom_histogram() + 
	facet_wrap(~variable, scales='free') + 
	labs(title='Histograms - Outcomes', subtitle='By Health Zone', 
		y='Frequency', x='Coverage') + 
	theme_bw(base_size=16)

# national
p7 = ggplot(long_agg[grepl('rate',variable) & !grepl('lag_',variable) & 
		section=='Impact'], 
		aes(x=value)) + 
	geom_histogram() + 
	facet_wrap(~variable, scales='free') + 
	labs(title='Histograms - Impact', y='Frequency', x='Coverage') + 
	theme_bw(base_size=16)	
	
# health zone
p8 = ggplot(long[grepl('rate',variable) & section=='Impact'], 
		aes(x=value)) + 
	geom_histogram() + 
	facet_wrap(~variable, scales='free') + 
	labs(title='Histograms - Impact', subtitle='By Health Zone', 
		y='Frequency', x='Coverage') + 
	theme_bw(base_size=16)
# ----------------------------------------------


# ----------------------------------------------
# Make correlation graphs

# log transform
logVars = c('newCasesMalariaMild_rate','newCasesMalariaSevere_rate', 'malariaDeaths_rate')
for(v in logVars) data[, (v):=log(get(v))]
for(v in logVars) data[!is.finite(get(v)), (v):=quantile(data[is.finite(get(v))][[v]],.01,na.rm=T)]

# lag-acts and incidence
p9 = list()
p9[[1]] = ggplot(data_agg, 
		aes(y=newCasesMalariaMild_rate, x=lag_mildMalariaTreated_rate)) + 
	geom_point() + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Observed Incidence Rate', x='Lag of Observed Treatment Coverage') + 
	theme_bw(base_size=16)
p9[[2]] = ggplot(data_agg, 
		aes(y=newCasesMalariaSevere_rate, x=lag_severeMalariaTreated_rate)) + 
	geom_point() + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Observed Severe Incidence Rate', x='Lag of Observed Severe Treatment Coverage') + 
	theme_bw(base_size=16)

# acts and mortality
p10 = list()
i=1
for(v in c('lag_mildMalariaTreated_rate', 'lag_severeMalariaTreated_rate',
	'newCasesMalariaMild_rate','newCasesMalariaSevere_rate')) { 
	# l = nodeTable[variable==v]$label
	p10[[i]] = ggplot(data_agg[!is.na(malariaDeaths_rate) & !is.na(get(v))], 
			aes_string(y='malariaDeaths_rate', x=v)) + 
		geom_point() + 
		geom_smooth(method='lm', se=FALSE) + 
		labs(y='Observed Mortality Rate', x=v) + 
		theme_bw()
	i=i+1
}

# lag-acts and incidence at health zone level (sampled because this graph is huge)
sampled_hzs = unique(data$health_zone)
sampled_hzs = sampled_hzs[sample(length(sampled_hzs),.1*length(sampled_hzs))]
sample = data[health_zone %in% sampled_hzs]
p11 = list()
p11[[1]] = ggplot(sample, 
		aes(y=newCasesMalariaMild_rate, x=lag_mildMalariaTreated_rate)) + 
	geom_point() + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Log of Observed Incidence Rate', x='Lag of Observed Treatment Coverage') + 
	theme_bw(base_size=16)
p11[[2]] = ggplot(sample, 
		aes(y=newCasesMalariaSevere_rate, x=lag_severeMalariaTreated_rate, caption=' ')) + 
	geom_point() + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Log of Observed Severe Incidence Rate', x='Lag of Observed Severe Treatment Coverage', 
		caption='10% sample of observations') + 
	theme_bw(base_size=16)

# acts and mortality at health zone level
p12 = list()
i=1
for(v in c('lag_mildMalariaTreated_rate', 'lag_severeMalariaTreated_rate',
	'newCasesMalariaMild_rate','newCasesMalariaSevere_rate')) { 
	# l = nodeTable[variable==v]$label
	p12[[i]] = ggplot(sample[!is.na(malariaDeaths_rate) & !is.na(get(v))], 
			aes_string(y='malariaDeaths_rate', x=v)) + 
		geom_point() + 
		geom_smooth(method='lm', se=FALSE) + 
		labs(y='Log of Observed Mortality Rate', x=v, caption=' ') + 
		theme_bw()
	if (i==4) p12[[i]] = p12[[i]] + labs(caption=paste0('Variables log-transformed:', paste(logVars, collapse=',')))
	i=i+1
}

# acts and incidence exploring ecological fallacy
p13 = list()
p13 = ggplot(sample, 
		aes(y=newCasesMalariaMild_rate, x=lag_mildMalariaTreated_rate, group=health_zone)) + 
	geom_point() + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Log of Observed Incidence Rate', x='Lag of Observed Treatment Coverage') + 
	theme_bw(base_size=16)
# p13[[2]] = ggplot(sample, 
		# aes(y=newCasesMalariaSevere_rate, x=lag_severeMalariaTreated_rate, caption=' ')) + 
	# geom_point() + 
	# geom_smooth(method='lm', se=FALSE) + 
	# labs(y='Log of Observed Severe Incidence Rate', x='Lag of Observed Severe Treatment Coverage', 
		# caption='10% sample of observations') + 
	# theme_bw(base_size=16)

# ----------------------------------------------


# --------------------------------
# Save file
pdf(outputFile4b, height=5.5, width=9)
p1
p2
p3
p4
p5
p6
p7
p8
do.call('grid.arrange',c(p9,list(ncol=2, top=textGrob('Correlations - Incidence and Lag of Treatment Coverage', gp=gpar(fontsize=16)))))
do.call('grid.arrange',c(p10,list(ncol=2, top=textGrob('Correlations - Mortality and Lag of Treatment Coverage', gp=gpar(fontsize=16)))))
do.call('grid.arrange',c(p11,list(ncol=2, top=textGrob('Correlations - Incidence and Lag of Treatment Coverage (Health Zone level)', gp=gpar(fontsize=16)))))
do.call('grid.arrange',c(p12,list(ncol=2, top=textGrob('Correlations - Mortality and Lag of Treatment Coverage (Health Zone level)', gp=gpar(fontsize=16)))))
dev.off()

# save a time-stamped version for reproducibility
date_time = gsub('-|:| ', '_', Sys.time())
outputFile4bArchive = gsub('visualizations/', 'visualizations/archive/', outputFile4b)
outputFile4bArchive = gsub('.pdf', paste0('_', date_time, '.pdf'), outputFile4bArchive)
file.copy(outputFile4b, outputFile4bArchive)
# --------------------------------
