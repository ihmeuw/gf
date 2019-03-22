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
load(outputFile5c)

# unrescale
transformedData = copy(data)
for(v in names(scaling_factors)) data[,(v):=get(v)*scaling_factors[[v]]]

# exponentiate
logVars = c('ITN','RDT','SP','SSCACT','mildMalariaTreated','severeMalariaTreated',
	'RDT_rate','SP_rate','ACTs_CHWs_rate','ITN_rate',
	'newCasesMalariaMild_rate','newCasesMalariaSevere_rate','malariaDeaths_rate')
for(v in logVars) data[, (v):=exp(get(v))]
# ----------------------------------------------------------------------------


# ----------------------------------------------
# Set up to graph

# variable lists
byVars = c('date','health_zone')
outcomeVars = c('ACTs_CHWs_rate', 'mildMalariaTreated_rate', 
	'RDT_rate', 'severeMalariaTreated_rate', 'ITN_rate', 
	'SP_rate','itn_coverage_rate', 'act_coverage_rate')
impactVars = c('newCasesMalariaMild_rate','newCasesMalariaSevere_rate',
	'malariaDeaths_rate', 'incidence_rate', 'prevalence_rate', 
	'mortality_rate')
modelVars = c('itn_coverage_rate', 'act_coverage_rate', 
	'incidence_rate', 'prevalence_rate', 'mortality_rate')
	
# melt un-transformed data long
long=melt(data, id.vars=byVars)
long[variable %in% outcomeVars, section:='outcomes']
long[variable %in% impactVars, section:='impact']
long[, model_estimate:=ifelse(variable %in% modelVars, TRUE, FALSE)]
	
# melt transformed data long
longT=melt(transformedData, id.vars=byVars)
longT[variable %in% outcomeVars, section:='outcomes']
longT[variable %in% impactVars, section:='impact']
longT[, model_estimate:=ifelse(variable %in% modelVars, TRUE, FALSE)]
# ----------------------------------------------


# ----------------------------------------------
# Make time series graphs

# outcomes by health zone
itnCoverage = long[variable=='itn_coverage_rate']
actCoverage = long[variable=='act_coverage_rate']
dropVars = c('variable','section','model_estimate')
itnCoverage = itnCoverage[, -dropVars, with=FALSE]
actCoverage = actCoverage[, -dropVars, with=FALSE]
setnames(itnCoverage, 'value', 'itn_coverage_rate')
setnames(actCoverage, 'value', 'act_coverage_rate')
tsWide1 = merge(long[!variable %in% modelVars], itnCoverage, by=byVars)
tsWide1 = merge(tsWide1, actCoverage, by=byVars)
tsWide1[variable=='ITN_rate', model_value:=itn_coverage_rate]
tsWide1[!variable %in% c('ITN_rate', 'RDT_rate') & section=='outcomes', 
	model_value:=act_coverage_rate]

hzOutcomePlotsTs = lapply(sample(unique(data$health_zone), 15), function(h) { 
	ggplot(tsWide1[health_zone==h & section=='outcomes'], aes(y=value, x=date)) + 
		geom_point(aes(color='Observed Program Data')) + 
		geom_line(aes(y=model_value, color='LBD/MAP Model Estimate')) + 
		scale_color_manual('', values=c('Observed Program Data'='black', 
			'LBD/MAP Model Estimate'='blue')) + 
		facet_wrap(~variable, scales='free') + 
		labs(title=h) + 
		theme_bw()
})

# incidence/mortality by health zone
incidence = long[variable=='incidence_rate']
mortality = long[variable=='mortality_rate']
incidence = incidence[, -dropVars, with=FALSE]
mortality = mortality[, -dropVars, with=FALSE]
setnames(incidence, 'value', 'incidence_rate')
setnames(mortality, 'value', 'mortality_rate')
tsWide2 = merge(long[!variable %in% modelVars], incidence, by=byVars)
tsWide2 = merge(tsWide2, mortality, by=byVars)
tsWide2[variable=='malariaDeaths_rate', model_value:=mortality_rate]
tsWide2[variable!='malariaDeaths_rate' & section=='impact', 
	model_value:=incidence_rate]

hzImpactPlotsTs = lapply(sample(unique(data$health_zone), 15), function(h) { 
	ggplot(tsWide2[health_zone==h & section=='impact'], aes(y=value, x=date)) + 
		geom_point(aes(color='Observed Program Data')) + 
		geom_line(aes(y=model_value, color='LBD/MAP Model Estimate')) + 
		scale_color_manual('', values=c('Observed Program Data'='black', 
			'LBD/MAP Model Estimate'='blue')) + 
		facet_wrap(~variable, scales='free') + 
		labs(title=h) + 
		theme_bw()
})
# ----------------------------------------------


# ----------------------------------------------
# Make distribution graphs


# ----------------------------------------------


# ----------------------------------------------
# Make correlation graphs


# outcomes and incidence by health zone
incidenceMild = longT[variable=='lead_newCasesMalariaMild_rate']
setnames(incidenceMild, 'value', 'lead_newCasesMalariaMild_rate')
incidenceMild = incidenceMild[, -dropVars, with=FALSE]
wide = merge(longT[variable!='lead_newCasesMalariaMild_rate'], incidenceMild, by=byVars)

i=1
hzOutcomePlotsMild=list()
for(h in sample(unique(data$health_zone), 15)) {
	hzOutcomePlotsMild[[i]] = ggplot(wide[health_zone==h & section=='outcomes'], 
		aes(y=lead_newCasesMalariaMild_rate, x=value)) + 
	geom_point() + 
	geom_smooth() + 
	facet_wrap(~variable, scales='free') + 
	labs(title=h) + 
	theme_bw()
	i=i+1
}


# outcomes and severe incidence by health zone
incidenceSevere = longT[variable=='lead_newCasesMalariaSevere_rate']
setnames(incidenceSevere, 'value', 'lead_newCasesMalariaSevere_rate')
incidenceSevere = incidenceSevere[, -dropVars, with=FALSE]
wide = merge(wide[variable!='lead_newCasesMalariaSevere_rate'], incidenceSevere, by=byVars)

i=1
hzOutcomePlotsSev=list()
for(h in sample(unique(data$health_zone), 15)) {
	hzOutcomePlotsSev[[i]] = ggplot(wide[health_zone==h & section=='outcomes'], 
		aes(y=lead_newCasesMalariaSevere_rate, x=value)) + 
	geom_point() + 
	geom_smooth() + 
	facet_wrap(~variable, scales='free') + 
	labs(title=h) + 
	theme_bw()
	i=i+1
}

# outcomes and mortality by health zone
mortality = longT[variable=='lead_malariaDeaths_rate']
setnames(mortality, 'value', 'lead_malariaDeaths_rate')
mortality = mortality[, -dropVars, with=FALSE]
wide = merge(wide[variable!='lead_malariaDeaths_rate'], mortality, by=byVars)

i=1
hzOutcomePlotsMort=list()
for(h in sample(unique(data$health_zone), 15)) {
	hzOutcomePlotsMort[[i]] = ggplot(wide[health_zone==h & section=='outcomes'], 
		aes(y=lead_malariaDeaths_rate, x=value)) + 
	geom_point() + 
	geom_smooth() + 
	facet_wrap(~variable, scales='free') + 
	labs(title=h) + 
	theme_bw()
	i=i+1
}


# ----------------------------------------------


# --------------------------------
# Save file
pdf(outputFile4b, height=5.5, width=9)
for(i in seq(length(hzOutcomePlotsTs))) print(hzOutcomePlotsTs[[i]])
for(i in seq(length(hzImpactPlotsTs))) print(hzImpactPlotsTs[[i]])
for(i in seq(length(hzOutcomePlotsMild))) print(hzOutcomePlotsMild[[i]])
for(i in seq(length(hzOutcomePlotsSev))) print(hzOutcomePlotsSev[[i]])
for(i in seq(length(hzOutcomePlotsMort))) print(hzOutcomePlotsMort[[i]])
dev.off()

# save a time-stamped version for reproducibility
date_time = gsub('-|:| ', '_', Sys.time())
outputFile4bArchive = gsub('visualizations/', 'visualizations/archive/', outputFile4b)
outputFile4bArchive = gsub('.pdf', paste0('_', date_time, '.pdf'), outputFile4bArchive)
file.copy(outputFile4b, outputFile4bArchive)
# --------------------------------
