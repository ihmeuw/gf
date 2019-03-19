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

# # unrescale
# for(v in names(scaling_factors)) data[,(v):=get(v)*scaling_factors[[v]]]

# # exponentiate
# logVars = c('ITN','RDT','SP','SSCACT','mildMalariaTreated','severeMalariaTreated',
	# 'RDT_rate','SP_rate','ACTs_CHWs_rate','ITN_rate',
	# 'newCasesMalariaMild_rate','newCasesMalariaSevere_rate','malariaDeaths_rate')
# for(v in logVars) data[, (v):=exp(get(v))]
# ----------------------------------------------------------------------------


# ----------------------------------------------
# Set up to graph
byVars = c('date','health_zone')
long=melt(data, id.vars=byVars)
long[variable %in% c('ACTs_CHWs_rate', 'mildMalariaTreated_rate', 'RDT_rate', 'severeMalariaTreated_rate', 'ITN_rate', 'SP_rate'), section:='outcomes']
# ----------------------------------------------


# ----------------------------------------------
# Make time series graphs

# outcomes by health zone
ggplot(long[health_zone=='aba' & section=='outcomes'], aes(y=value, x=date, color=variable)) + 
	geom_point() + 
	geom_line()

# ----------------------------------------------


# ----------------------------------------------
# Make distribution graphs


# ----------------------------------------------


# ----------------------------------------------
# Make correlation graphs


# outcomes and incidence by health zone
incidenceMild = long[variable=='lead_newCasesMalariaMild_rate']
setnames(incidenceMild, 'value', 'lead_newCasesMalariaMild_rate')
incidenceMild$variable = NULL
incidenceMild$section = NULL
wide = merge(long[variable!='lead_newCasesMalariaMild_rate'], incidenceMild, by=byVars)

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
incidenceSevere = long[variable=='lead_newCasesMalariaSevere_rate']
setnames(incidenceSevere, 'value', 'lead_newCasesMalariaSevere_rate')
incidenceSevere$variable = NULL
incidenceSevere$section = NULL
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
mortality = long[variable=='lead_malariaDeaths_rate']
setnames(mortality, 'value', 'lead_malariaDeaths_rate')
mortality$variable = NULL
mortality$section = NULL
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
