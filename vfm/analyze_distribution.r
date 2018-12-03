# --------------------------------------------------
# David Phillips
#
# 9/11/2018
# Compare resource allocation (commodities) to need 
# This loads data created by ./vfm/prep_distribution.r
# --------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(quantreg)
library(RColorBrewer)
library(ggplot2)
library(ggrepel)
# --------------------


# ----------------------------------------------
# Parameters and settings

# whether to operate at "HZ" or "DPS" level
analysisLevel = 'DPS'
# ----------------------------------------------


# --------------------------------------------------------------------------------------------
# Files and directories

# switch for cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')

# PNLP data directory
dataDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/')

# output directory
outDir = paste0(j, '/Project/Evaluation/GF/vfm/visualizations/')

# shapefile
shapeFile = paste0(j, '/Project/Evaluation/GF/mapping/cod/health_zones_who/health2.shp')

# shapefile of lakes
shapeFileLakes = paste0(j, '/WORK/11_geospatial/06_original shapefiles/GLWD_lakes/glwd_1.shp')

# input data (from prep_distribution.r)
inFile = paste0(dataDir, './pnlp_map_', tolower(analysisLevel), '_year_level.rds')

# output files
outFile = paste0(outDir, 'commodity_distribution_and_need.pdf')
# --------------------------------------------------------------------------------------------


# --------------------------------------------------------------------
# Load data

# load
data = readRDS(inFile)

# melt y-variables
idVars = c('province','dps', 'health_zone', 'year',
			'mean_newCasesMalariaMild','mean_newCasesMalariaSevere', 
			'lower_newCasesMalariaMild','lower_newCasesMalariaSevere', 
			'upper_newCasesMalariaMild','upper_newCasesMalariaSevere', 
			'pf_incidence','mean_suspectedMalaria',
			'lower_suspectedMalaria','upper_suspectedMalaria')
if (analysisLevel!='HZ') idVars = idVars[idVars!='health_zone']
data = melt(data, id.vars=idVars)

# drop imputation uncertainty for now
data = data[grepl('mean',variable)]

# facet labels
data[variable=='mean_ASAQreceived', label:='ASAQ Doses']
data[variable=='mean_ArtLum', label:='AL Doses']
data[variable=='mean_ITN', label:='ITNs']
data[variable=='mean_RDT', label:='RDTs']

# make lag of incidence and reporting
data = data[order(dps, variable, year)]
cols = c('pf_incidence','mean_newCasesMalariaMild',
		'mean_newCasesMalariaSevere','mean_suspectedMalaria')
names = paste0('lag_',cols)
data[, (names):=shift(.SD), by=c('dps','variable'), .SDcols=cols]
# --------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# Set up to graph

# colors
colors = brewer.pal(length(unique(data$year)), 'YlGnBu')
paired = brewer.pal(6, 'Paired')
manColors = c('Distributed Number'=paired[4],'Estimated Incidence'=paired[2], 'Reported Cases'=paired[1])

# aggregate to DPS
byVars = c('province','dps','year','variable','label')
data[, variable:=paste0(variable, '_received')]
data[, variable:=gsub('received_received', 'received', variable)]
dps = data[, lapply(.SD, sum, na.rm=TRUE), by=byVars, 
		.SDcols=names(data)[!names(data) %in% c(byVars,'health_zone')]]

# identify interesting DPS's to label for MAP estimates vs ITNs
fit = lm(value~pf_incidence*factor(year), dps[label=='RDTs'])
dps[label=='RDTs', resid:=value-predict(fit)]
dps[label=='RDTs', resid_upper:=quantile(resid,.85), by=c('year','variable')]
dps[label=='RDTs', resid_lower:=quantile(resid,.25), by=c('year','variable')]
dps[label=='RDTs' & (resid>resid_upper | resid<resid_lower), dps_label0:=dps]

# identify interesting DPS's to label for MAP estimates vs distribution
fit = lm(value~pf_incidence*factor(year)*factor(variable), dps)
dps[, resid:=value-predict(fit)]
dps[, resid_upper:=quantile(resid,.85), by=c('year','variable')]
dps[, resid_lower:=quantile(resid,.15), by=c('year','variable')]
dps[resid>resid_upper | resid<resid_lower, dps_label1:=dps]

# identify interesting DPS's to label for PNLP vs distribution
fit = lm(value~mean_newCasesMalariaSevere*factor(year)*factor(variable), dps)
dps[, resid:=value-predict(fit)]
dps[, resid_upper:=quantile(resid,.85), by=c('year','variable')]
dps[, resid_lower:=quantile(resid,.15), by=c('year','variable')]
dps[resid>resid_upper | resid<resid_lower, dps_label2:=dps]

# identify interesting DPS's to label for PNLP vs MAP
fit = lm(pf_incidence~mean_newCasesMalariaSevere*factor(year), dps)
dps[, resid:=value-predict(fit)]
dps[, resid_upper:=quantile(resid,.8), by=c('year','variable')]
dps[, resid_lower:=quantile(resid,.2), by=c('year','variable')]
dps[resid>resid_upper | resid<resid_lower, dps_label3:=dps]

# fit a quantile regression for p1 instead of OLS
qrFit = rq(value~lag_pf_incidence*factor(label), data=dps[year==2017])
dps[year==2017, qr_fitted:=predict(qrFit)]
# --------------------------------------------------------------------------------------


# ----------------------------------------------
# Graph

# comparison in 2017 with lag-x ITN only
p0 = ggplot(dps[year==2017 & label=='RDTs'], aes(y=value/1000000, 
		x=lag_pf_incidence/1000000, label=dps_label0)) + 
	geom_point() + 
	geom_line(aes(y=qr_fitted/1000000), color='blue') + 
	geom_text_repel(color='grey25', box.padding=1.5, 
		segment.color='grey75', min.segment.length=0) + 
	scale_color_manual(values=colors) + 
	labs(y='RDTs Distributed in 2017 (in Millions)', 
		x='Model-Estimated Incidence in 2016 (Millions of Cases among Children)', 
		color='Year') + 
	theme_bw(base_size=18)

# comparison in 2017 with lag-x
p1 = ggplot(dps[year==2017], aes(y=value/1000000, 
		x=lag_pf_incidence/1000000, label=dps_label1)) + 
	geom_point() + 
	geom_line(aes(y=qr_fitted/1000000), color='blue') + 
	geom_text_repel(color='grey25', box.padding=1.5, 
		segment.color='grey75', min.segment.length=0) + 
	facet_wrap(~label, scales='free_y') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Estimated Number of Cases - 2017', 
		subtitle='Aggregated by DPS', 
		y='Number Distributed in 2017 (in Millions)', 
		x='Model-Estimated Incidence in 2016 (Millions of Cases)', 
		color='Year') + 
	theme_bw()
	
# comparison using reported cases
p2 = ggplot(dps[year==2017], aes(y=value/1000000, 
		x=(lag_mean_newCasesMalariaSevere+lag_mean_newCasesMalariaMild)/1000000, label=dps_label2)) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	geom_text_repel(color='grey25', box.padding=1.5, 
		segment.color='grey75', min.segment.length=0) + 
	facet_wrap(~label, scales='free_y') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Reported Number of Cases - 2017', 
		subtitle='Aggregated by DPS', 
		y='Number Distributed (in Millions)', x='Reported Cases (Mild + Severe in Millions)', color='Year') + 
	theme_bw()
	
# comparison using suspected cases
p3 = ggplot(dps[year==2017], aes(y=value/1000000, 
		x=(lag_mean_suspectedMalaria)/1000000, label=dps_label2)) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	geom_text_repel(color='grey25', box.padding=1.5, 
		segment.color='grey75', min.segment.length=0) + 
	facet_wrap(~label, scales='free_y') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Reported Number of Suspected Cases - 2017', 
		subtitle='Aggregated by DPS', 
		y='Number Distributed (in Millions)', x='Suspected Cases Reported', color='Year') + 
	theme_bw()
	
# comparison of MAP vs PNLP cases (selecting one variable arbitrarily to avoid duplication)
p4 = ggplot(dps[variable=='mean_RDT_received' & year==2017], 
		aes(y=(lag_mean_newCasesMalariaSevere+lag_mean_newCasesMalariaMild)/100000, 
		x=pf_incidence/1000000, label=dps_label3)) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	geom_text_repel(color='grey25', box.padding=1.5, 
		segment.color='grey75', min.segment.length=0) + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Reported Number of Cases - 2017', 
		subtitle='Aggregated by DPS', 
		y='Reported Cases (Mild + Severe in 100,000\'s)', 
		x='Model-Estimated Incidence (Millions of Cases)', 
		color='Year') + 
	theme_bw()

# comparison across all years
p5 = ggplot(dps, aes(y=value, 
		x=lag_pf_incidence)) + 
	geom_point() + 
	geom_smooth(method='lm', se=FALSE) + 
	facet_grid(year~label, scales='free') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Number of Cases', 
		subtitle='Aggregated by DPS and Year', 
		y='Number Distributed', x='Under-5 Incidence', color='Year') + 
	theme_bw()
	
# comparison across all years
p6 = ggplot(dps, aes(y=value, 
		x=(lag_mean_newCasesMalariaSevere+lag_mean_newCasesMalariaMild))) + 
	geom_point() + 
	geom_smooth(method='lm', se=FALSE) + 
	facet_grid(year~label, scales='free') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Number of Cases', 
		subtitle='Aggregated by DPS and Year', 
		y='Number Distributed', x='Under-5 Incidence', color='Year') + 
	theme_bw()
	
# rolling comparison
p7 = list()
i=1
for (d in unique(dps$dps)) { 
	p7[[i]] = ggplot(dps[dps==d], aes(y=value, x=year)) + 
		geom_line(aes(color='Distributed Number'), size=1.5) + 
		geom_line(aes(y=pf_incidence, color='Estimated Incidence'), size=1.5) +
		geom_line(aes(y=mean_newCasesMalariaSevere+mean_newCasesMalariaMild, color='Reported Cases'), size=1.5) +
		scale_color_manual(values=manColors) + 
		facet_wrap(~label) + 
		labs(title='Time Series of Commodity Distribution vs Incidence (Reported and Estimated)', 
			subtitle=d, y='Number', x='') + 
		theme_bw()
	i=i+1
}
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(outFile, height=6, width=9)
p0
p1
p2
p3
p4
p5
p6
# for(i in seq(length(p7))) print(p7[[i]])
dev.off()
# --------------------------------
