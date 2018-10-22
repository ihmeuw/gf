# -------------------------------------------
# David Phillips
#
# 12/11/2017
# Analyze correlates of absorption
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

# place to store the regression output
regOutFile = paste0(dir, '../../vfm/outputs/absorption_correlates_model_fit.rdata')

# output graphs
outFile = paste0(dir, '../../vfm/visualizations/absorption_correlates.pdf')
# -----------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep data

# load
allData = fread(inFile)

# identify quarters
allData[, quarter:=quarter(start_date)]

# collapse to module-quarter level
byVars = c('disease','country','grant_number','year','quarter','abbrev_module')
data = allData[, list('budget'=sum(budget,na.rm=TRUE), 
			'expenditure'=sum(expenditure,na.rm=TRUE)), by=byVars]

# compute absorption
data[, absorption:=expenditure/budget]

# define lemon squeeze function
lemonSqueeze = function(x) { 
	N = length(x[!is.na(x)])
	return(logit(((x*(N-1))+0.5)/N))
}
reverseLemonSqueeze = function(x) { 
	N = length(x[!is.na(x)])
	return(((inv.logit(x)*N)-0.5)/(N-1))
}

# handle 1's and 0's so logit doesn't drop them
data = data[is.finite(absorption) & absorption>=0]
data[absorption>1, absorption:=1] 
data[, absorption:=lemonSqueeze(absorption)]
# data[absorption>=1, absorption:=max(data[absorption<1]$absorption)] 
# data[absorption<=0, absorption:=min(data[absorption>0]$absorption)] 
# data[, absorption:=logit(absorption)]
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Generate extra predictor variables

# year within grant and years from end of grant
data[, grant_year:=as.numeric(as.factor(year)), by='grant_number']
data[, years_from_end:=max(grant_year)-grant_year+1, by='grant_number']
data[, yearid:=as.numeric(as.factor(year)), by='grant_number']
data[, quarterid:=(((yearid-1)*4))+quarter]
data[, quarters_from_end:=max(quarterid)-quarterid+1, by='grant_number']

# number of modules within grant
data[, num_modules:=length(unique(abbrev_module)), by='grant_number']
# ----------------------------------------------------------------------


# ----------------------------------------------------------
# Run regressions

# all confounding variables to SDA
form1 = as.formula('logit(absorption) ~ 
					years_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFit1 = lm(form1, data=data)
summary(lmFit1)

# program activity controlling for all confounders
form2 = as.formula('absorption ~ abbrev_module + 
					quarters_from_end + disease + country + 
					log(cumulative_budget) + num_modules')
lmFit2 = lm(form2, data=data)
# ----------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Store regression results

# regression coefficients from model 1 (all but SDA)
coefs1 = data.table(cbind(names(coef(lmFit1)), summary(lmFit1)$coefficients, confint(lmFit1)))
setnames(coefs1, c('variable','est','se','t','p','lower','upper'))
coefs1 = coefs1[, lapply(.SD, as.numeric), .SDcols=c('est','p','lower','upper'), by='variable']
# coefs1 = coefs1[, lapply(.SD, inv.logit), .SDcols=c('est','lower','upper'), by=c('variable','p')]

# predictions from model 2 (full model) 
# set to the most central categories for all other variables
coefs2 = data.table(unique(data$abbrev_module))
setnames(coefs2, 'abbrev_module')
coefs2[, years_from_end:=1]
coefs2[, quarters_from_end:=1]
coefs2[, disease:='hiv']
coefs2[, country:='Congo (Democratic Republic)']
coefs2[, cumulative_budget:=median(data$cumulative_budget)]
coefs2[, num_modules:=median(data$num_modules)]
coefs2 = cbind(coefs2, reverseLemonSqueeze(predict(lmFit2, newdata=coefs2, interval='confidence')))
# coefs2 = cbind(coefs2, inv.logit(predict(lmFit2, newdata=coefs2, interval='confidence')))
# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Set up to graph

# labels
coefs1[variable=='(Intercept)', label:='Intercept']
coefs1[variable=='years_from_end', label:='Years from Grant End']
coefs1[variable=='diseasemalaria', label:='Component: Malaria']
coefs1[variable=='diseasetb', label:='Component: TB']
coefs1[variable=='countryGuatemala', label:='Country: Guatemala']
coefs1[variable=='countryUganda', label:='Country: Uganda']
coefs1[variable=='log(cumulative_budget)', label:='Log-Cumulative Budget']
coefs1[variable=='num_modules', label:='Number of Modules']
# coefs2[, label:=str_wrap(abbrev_module, 32)]
coefs2[, label:=abbrev_module]
data[, label:=str_wrap(abbrev_module, 22)]

# identify highly-comoditized program areas
commodities = c('Treatment, care & support', 'Vector control', 'Case management', 'Care & prevention', 'MDR-TB', 'HIV Testing Services', 'PSM')

coefs2[, commoditized:=ifelse(abbrev_module %in% commodities, 'Commoditized', 'Programmatic')]

# store aggregate absorption
agg = sum(data$expenditure)/sum(data$budget)

# store average absorption by sda
means = data[, list(absorption=mean(absorption)), by=label]

# colors
cols = c('#008080','#70a494','#b4c8a8','#f6edbd','#edbb8a','#de8a5a','#ca562c')

# other settings
b = 14
# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Graphs

# graph data
p1 = ggplot(data, aes(y=absorption*100, x=grant_year, group=grant_number, color=year)) + 
	geom_line(alpha=.85) + 
	geom_point(alpha=.85, aes(size=budget/1000000)) + 
	geom_hline(data=means, aes(yintercept=absorption*100, linetype='Mean')) + 
	facet_wrap(~label, ncol=7) + 
	scale_color_gradientn(colors=cols) + 
	scale_linetype_manual('', values=c('Mean'='solid')) + 
	labs(title='Absorption by Grant and Service Delivery Area', 
			y='Absorption %', x='Year within Grant', size='Budget $\n(Millions)') + 
	theme_bw(base_size=b) + 
	theme(plot.title=element_text(hjust=.5), strip.text=element_text(size=9))

# graph model 1 coefficients
p2 = ggplot(coefs1[label!='Intercept'], aes(y=est, ymin=lower, ymax=upper, x=label)) + 
	geom_bar(stat='identity', fill='#6d819c') + 
	geom_errorbar(width=.2, size=1.1, color='gray25') + 
	labs(title='Regression Coefficients', subtitle='Model 1', caption='Model Intercept not Shown', 
			y='Correlation with Absorption (logit)', x='') + 
	theme_bw(base_size=b) + 
	theme(plot.title=element_text(hjust=.5), plot.subtitle=element_text(hjust=.5), 
			axis.text.x = element_text(angle=45, hjust=1), plot.margin=unit(c(.5,5,.1,5),'cm'))

# graph model 2 predictions
p3 = ggplot(coefs2, aes(y=fit, ymin=lwr, ymax=upr, x=reorder(label,fit))) + 
	geom_bar(stat='identity', aes(fill=commoditized)) + 
	geom_errorbar(width=.25, size=1.1, color='gray25') + 
	geom_hline(yintercept=agg, color='red', lty='longdash') + 
	scale_fill_manual('', values=c('#55967e','#6d819c')) + 
	annotate('text', x=coefs2[fit==max(fit)]$label, y=agg, 
			label='Overall Absorption', hjust=.9, vjust=1.2, size=5) + 
	labs(title='Mean Absorption by Service Delivery Area', 
			caption='Estimates controlling for all variables in model 1', y='Mean Absorption', x='') + 
	theme_bw(base_size=b) + 
	theme(plot.title=element_text(hjust=.5), axis.text.x = element_text(angle=45, hjust=1))
# -------------------------------------------------------------------------------------------------


# -----------------------------
# Save graphs
pdf(outFile, height=6, width=10.5)
p1
p2
p3
dev.off()
# -----------------------------


# -----------------------------
# Save model output
save('lmFit2', file=regOutFile)
# -----------------------------
