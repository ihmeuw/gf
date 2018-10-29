# -------------------------------------------
# David Phillips
#
# 1-/22/2018
# Sensitivity analysis for different models to find absorption correlates
# Options:
# 1. module vs intervention level aggregation of data
# 2. year vs quarter level aggregation of data
# 3. exclude, replace or lemon-squeeze  0's and 1's
# 4. fixed effect on quarter or no
# 5. fixed effect on intervention or no
# -------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(boot)
library(readxl)
library(data.table)
library(stringr)
library(ggplot2)
# ------------------


# -----------------------------------------------------------------
# Files and directories

# root directory for input/output
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/'

# input data
inFile = paste0(dir, 'mapping/prepped_gos_data.csv')

# output graphs
outFile = paste0(dir, '../../vfm/visualizations/absorption_correlates_sensitivity_analysis.pdf')
# -----------------------------------------------------------------


# --------------------------------------
# Load/prep data

# load
allData = fread(inFile)

# identify quarters
allData[, quarter:=quarter(start_date)]
# --------------------------------------


# ----------------------------------------------------------------------
# Different levels of aggregation

# collapse to module-year level
byVars = c('disease','country','grant_number','year','abbrev_module')
mod_year_data = allData[, list('budget'=sum(budget,na.rm=TRUE), 
			'expenditure'=sum(expenditure,na.rm=TRUE)), by=byVars]

# collapse to module-quarter level
byVars = c('disease','country','grant_number','year','quarter','abbrev_module')
mod_quarter_data = allData[, list('budget'=sum(budget,na.rm=TRUE), 
			'expenditure'=sum(expenditure,na.rm=TRUE)), by=byVars]

# collapse to intervention-year level
byVars = c('disease','country','grant_number','year','abbrev_module','abbrev_intervention')
int_year_data = allData[, list('budget'=sum(budget,na.rm=TRUE), 
			'expenditure'=sum(expenditure,na.rm=TRUE)), by=byVars]

# collapse to intervention-quarter level
byVars = c('disease','country','grant_number','year','quarter','abbrev_module','abbrev_intervention')
int_quarter_data = allData[, list('budget'=sum(budget,na.rm=TRUE), 
			'expenditure'=sum(expenditure,na.rm=TRUE)), by=byVars]

# store list of names
aggregates = c('mod_year_data','mod_quarter_data','int_year_data','int_quarter_data')
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Compute absorption among the four aggregations

# define lemon squeeze function
lemonSqueeze = function(x) { 
	N = length(x[!is.na(x)])
	return(logit(((x*(N-1))+0.5)/N))
}
reverseLemonSqueeze = function(x) { 
	N = length(x[!is.na(x)])
	return(((inv.logit(x)*N)-0.5)/(N-1))
}

# loop over different aggregates
for(dt in aggregates) { 
	# compute cumulative budget/expenditure by grant-module
	byVars = c('grant_number','abbrev_module')
	if ('abbrev_intervention' %in% names(get(dt))) byVars = c(byVars,'abbrev_intervention')
	assign(dt, get(dt)[order(country, disease, year, grant_number, abbrev_module)])
	if ('quarter' %in% names(get(dt))) { 
		assign(dt, get(dt)[order(country, disease, year, quarter, grant_number, abbrev_module)])
	}
	get(dt)[, cumulative_budget:=cumsum(budget), by=byVars]
	get(dt)[, cumulative_expenditure:=cumsum(expenditure), by=byVars]

	# compute absorption
	get(dt)[, absorption:=cumulative_expenditure/cumulative_budget]
	get(dt)[, absorption:=expenditure/budget]

	# handle 1's and 0's so logit doesn't drop them
	assign(dt, get(dt)[is.finite(absorption) & absorption>=0])

	# replace OOB absorption with max and logit transform
	get(dt)[, absorption_minmax:=absorption]
	get(dt)[absorption>=1, absorption_minmax:=max(get(dt)[absorption<1]$absorption)] 
	get(dt)[absorption<=0, absorption_minmax:=min(get(dt)[absorption>0]$absorption)] 
	get(dt)[, absorption_minmax:=logit(absorption_minmax)]

	# lemon-squeeze OOB absorption
	get(dt)[, absorption_ls:=absorption]
	get(dt)[absorption>1, absorption_ls:=1] 
	get(dt)[, absorption_ls:=lemonSqueeze(absorption_ls)]
}
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Generate extra predictor variables
for(dt in aggregates) { 
	# year within grant and years from end of grant
	get(dt)[, grant_year:=as.numeric(as.factor(year)), by='grant_number']
	get(dt)[, years_from_end:=max(grant_year)-grant_year+1, by='grant_number']
	get(dt)[, yearid:=as.numeric(as.factor(year)), by='grant_number']
	if (grepl('quarter',dt)) { 
		get(dt)[, quarterid:=(((yearid-1)*4))+quarter]
		get(dt)[, quarters_from_end:=max(quarterid)-quarterid+1, by='grant_number']
	}
	
	# number of modules within grant
	get(dt)[, num_modules:=length(unique(abbrev_module)), by='grant_number']
}
# ----------------------------------------------------------------------


# ----------------------------------------------------------
# Run regressions

# logit year-module level
form = as.formula('absorption_minmax ~ abbrev_module + 
					years_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFitYMLo = lm(form, data=mod_year_data)

# lemon-squeeze year-module level
form = as.formula('absorption_ls ~ abbrev_module + 
					years_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFitYMLs = lm(form, data=mod_year_data)

# logit quarter-module level
form = as.formula('absorption_minmax ~ abbrev_module + 
					years_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFitQMLo = lm(form, data=mod_quarter_data)

# lemon-squeeze quarter-module level
form = as.formula('absorption_ls ~ abbrev_module + 
					years_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFitQMLs = lm(form, data=mod_quarter_data)

# logit quarter-module level with quarter feff
form = as.formula('absorption_minmax ~ abbrev_module + 
					quarters_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFitQMLoQff = lm(form, data=mod_quarter_data)

# lemon-squeeze quarter-module level with quarter feff
form = as.formula('absorption_ls ~ abbrev_module + 
					quarters_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFitQMLsQff = lm(form, data=mod_quarter_data)

# logit quarter-intervention level
form = as.formula('absorption_minmax ~ abbrev_module + 
					years_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFitQILo = lm(form, data=int_quarter_data)

# lemon-squeeze quarter-intervention level
form = as.formula('absorption_ls ~ abbrev_module + 
					years_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFitQILs = lm(form, data=int_quarter_data)

# logit quarter-intervention level with quarter feff
form = as.formula('absorption_minmax ~ abbrev_module + 
					quarters_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFitQILoQff = lm(form, data=int_quarter_data)

# lemon-squeeze quarter-intervention level with quarter feff
form = as.formula('absorption_ls ~ abbrev_module + 
					quarters_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFitQILsQff = lm(form, data=int_quarter_data)

# # logit quarter-intervention level with intervention feff
# form = as.formula('absorption_minmax ~ abbrev_module + abbrev_intervention + 
					# years_from_end + disease + country + 
					# log(cumulative_budget) + num_modules')
# lmFitQILoIff = lm(form, data=int_quarter_data)

# # lemon-squeeze quarter-intervention level with intervention feff
# form = as.formula('absorption_ls ~ abbrev_module + abbrev_intervention + 
					# years_from_end + disease + country + 
					# log(cumulative_budget) + num_modules')
# lmFitQILsIff = lm(form, data=int_quarter_data)

# # logit quarter-intervention level with quarter feff and intervention feff
# form = as.formula('absorption_minmax ~ abbrev_module + abbrev_intervention + 
					# quarters_from_end + disease + country + 
					# log(cumulative_budget) + num_modules')
# lmFitQILoQffIff = lm(form, data=int_quarter_data)

# # lemon-squeeze quarter-intervention level with quarter feff and intervention feff
# form = as.formula('absorption_ls ~ abbrev_module + abbrev_intervention + 
					# quarters_from_end + disease + country + 
					# log(cumulative_budget) + num_modules')
# lmFitQILsQffIff = lm(form, data=int_quarter_data)
# ----------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Store regression results

# store models and labels for them
models = c('lmFitYMLo',
			'lmFitYMLs',
			'lmFitQMLo',
			'lmFitQMLs',
			'lmFitQMLoQff',
			'lmFitQMLsQff',
			'lmFitQILo',
			'lmFitQILs',
			'lmFitQILoQff',
			'lmFitQILsQff')
			# 'lmFitQILoIff',
			# 'lmFitQILsIff',
			# 'lmFitQILoQffIff',
			# 'lmFitQILsQffIff')
modLabels = c('01. Level: Year-Module, Transformation: Logit',
			'02. Level: Year-Module, Transformation: Lemon-Squeeze',
			'03. Level: Quarter-Module, Transformation: Logit',
			'04. Level: Quarter-Module, Transformation: Lemon-Squeeze',
			'05. Level: Quarter-Module, Transformation: Logit (with Quarter Fixed Effect)',
			'06. Level: Quarter-Module, Transformation: Lemon-Squeeze (with Quarter Fixed Effect)',
			'07. Level: Quarter-Intervention, Transformation: Logit',
			'08. Level: Quarter-Intervention, Transformation: Lemon-Squeeze',
			'09. Level: Quarter-Intervention, Transformation: Logit (with Quarter Fixed Effect)',
			'10. Level: Quarter-Intervention, Transformation: Lemon-Squeeze (with Quarter Fixed Effect)')
			# 'Level: Quarter-Intervention, Transformation: Logit (with Intervention Fixed Effect)',
			# 'Level: Quarter-Intervention, Transformation: Lemon-Squeeze (with Intervention Fixed Effect)',
			# 'Level: Quarter-Intervention, Transformation: Logit (with Quarter Fixed Effect and Intervention Fixed Effect)',
			# 'Level: Quarter-Intervention, Transformation: Lemon-Squeeze (with Quarter Fixed Effect and Intervention Fixed Effect)')

# predictions from each model
# set to the most central categories for all other variables
frame = data.table(unique(mod_year_data$abbrev_module))
setnames(frame, 'abbrev_module')
frame[, years_from_end:=1]
frame[, quarters_from_end:=1]
frame[, disease:='hiv']
frame[, country:='Congo (Democratic Republic)']
frame[, cumulative_budget:=median(mod_year_data$cumulative_budget)]
frame[, num_modules:=median(mod_year_data$num_modules)]

# predict from models
for(model in models) { 
	preds = predict(get(model), newdata=frame, interval='confidence')
	if (grepl('Lo', model)) preds = inv.logit(preds) 
	if (grepl('Ls', model)) preds = reverseLemonSqueeze(preds) 
	preds = data.table(preds)
	setnames(preds, paste0(model, names(preds)))
	frame = cbind(frame, preds)
}
# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Set up to graph

# labels
frame[, label:=abbrev_module]

# identify highly-comoditized program areas
commodities = c('Treatment, care & support', 'Vector control', 
			'Case management', 'Care & prevention', 'MDR-TB', 'HIV Testing Services')
frame[, commoditized:=ifelse(abbrev_module %in% commodities, 'Commoditized', 'Programmatic')]

# store aggregate absorption
agg = sum(allData$expenditure)/sum(allData$budget)

# colors
cols = c('#008080','#70a494','#b4c8a8','#f6edbd','#edbb8a','#de8a5a','#ca562c')

# other settings
b = 14

# melted version
idVars = c('abbrev_module', 'years_from_end', 'quarters_from_end', 
		'disease', 'country', 'cumulative_budget', 'num_modules', 'label', 'commoditized')
melt = melt(frame, id.vars=idVars)
melt[grepl('fit',variable), est:='fit']
melt[grepl('lwr',variable), est:='lwr']
melt[grepl('upr',variable), est:='upr']
melt[, variable:=gsub('fit','',variable)]
melt[, variable:=gsub('lwr','',variable)]
melt[, variable:=gsub('upr','',variable)]
for(m in models) { 
	l = modLabels[models==m]
	melt[grepl(m, variable), model_label:=str_wrap(l,35)]
}
# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Graphs
plots = list() 

# graph model predictions
i=1
for(m in models) { 
	l = modLabels[models==m]
	tmp = frame[,c('label',paste0(m,c('fit','lwr','upr')),'commoditized'), with=FALSE]
	setnames(tmp, c('label','fit','lwr','upr','commoditized'))
	plots[[i]] = ggplot(tmp, aes(y=fit, ymin=lwr, ymax=upr, x=reorder(label,fit))) + 
		geom_bar(stat='identity', aes(fill=commoditized)) + 
		geom_errorbar(width=.25, size=1.1, color='gray25') + 
		geom_hline(yintercept=agg, color='red', lty='longdash') + 
		scale_fill_manual('', values=c('#55967e','#6d819c')) + 
		annotate('text', x=tmp[fit==max(fit)]$label, y=agg, 
				label='Overall Absorption', hjust=.9, vjust=1.2, size=5) + 
		labs(title='Mean Absorption by Service Delivery Area', subtitle=l,
				caption='Estimates controlling for all variables in model 1', y='Mean Absorption', x='') + 
		theme_bw(base_size=b) + 
		theme(axis.text.x = element_text(angle=45, hjust=1))
	i=i+1
}

p1 = ggplot(melt[est=='fit'], aes(y=value, x=model_label, group=label, color=reorder(label, -value))) + 
	geom_point() + 
	geom_line() + 
	labs(title='Model Comparison', y='', x='', color='Module') + 
	theme_bw() + 
	theme(axis.text.x=element_text(size=8, angle=45, hjust=1))
# -------------------------------------------------------------------------------------------------


# -----------------------------
# Save graphs
pdf(outFile, height=6, width=10.5)
p1
for(p in seq(length(plots))) print(plots[[p]])
dev.off()
# -----------------------------

