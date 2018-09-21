# --------------------------------------------------
# David Phillips
#
# 9/11/2018
# Compare resource allocation (commodities) to need 
# --------------------------------------------------


# TO DO
# identify age groups in incidence rasters and do an age-specific merge
# use PNLP-to-shapefile HZ names to connect (for now just using PNLP incidence)

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
library(rgeos)
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
idVars = c('province','dps', 'health_zone', 'year', 'subpopulation',
			'mean_newCasesMalariaMild','mean_newCasesMalariaSevere', 
			'lower_newCasesMalariaMild','lower_newCasesMalariaSevere', 
			'upper_newCasesMalariaMild','upper_newCasesMalariaSevere', 
			'pf_incidence')
if (analysisLevel!='HZ') idVars = idVars[idVars!='health_zone']
data = melt(data, id.vars=idVars)

# combine ASAQ subpopulations to match incidence age groups
data[subpopulation %in% c('2to11mos','1to5yrs'), subpopulation:='under5']
data[subpopulation %in% c('6to13yrs','14yrsAndOlder'), subpopulation:='5andOlder']
data[subpopulation=='pregnantWomen', subpopulation:='5andOlder']
byVars = c('province','dps','health_zone','year', 'subpopulation','variable')
if (analysisLevel!='HZ') byVars = byVars[byVars!='health_zone']
data = data[, lapply(.SD, sum, na.rm=TRUE), by=byVars, .SDcols=names(data)[!names(data) %in% byVars]]

# facet labels
data[variable=='mean_ASAQreceived', label:='ASAQ Doses']
data[variable=='mean_ArtLum', label:='AL Doses']
data[variable=='mean_ITN', label:='ITNs']
data[variable=='mean_RDT', label:='RDTs']
# --------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# Set up to graph

# colors
colors = brewer.pal(length(unique(data$year)), 'YlGnBu')

# aggregate to DPS
byVars = c('province','dps','year','variable','label')
dps = data[, lapply(.SD, sum, na.rm=TRUE), by=byVars, 
		.SDcols=names(data)[!names(data) %in% c(byVars,'health_zone','subpopulation')]]
		
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
fit = lm(pf_incidence~mean_newCasesMalariaSevere*factor(year)*, dps)
dps[, resid:=value-predict(fit)]
dps[, resid_upper:=quantile(resid,.8), by=c('year','variable')]
dps[, resid_lower:=quantile(resid,.2), by=c('year','variable')]
dps[resid>resid_upper | resid<resid_lower, dps_label3:=dps]

# aggregate to DPS-subpopulation for ASAQ (the only one with subpopulations)
byVars = c('province','dps','year','subpopulation','variable','label')
dps_subpop = data[variable=='mean_ASAQreceived' & subpopulation!='received', 
		lapply(.SD, sum, na.rm=TRUE), by=byVars, 
		.SDcols=names(data)[!names(data) %in% c(byVars,'health_zone')]]

# identify interesting DPS's to label by subpop
fit = lm(value~mean_newCasesMalariaSevere*factor(year)*factor(subpopulation), dps_subpop)
dps_subpop[, resid:=value-predict(fit)]
dps_subpop[, resid_upper:=quantile(resid,.85), by=c('year','variable','subpopulation')]
dps_subpop[, resid_lower:=quantile(resid,.15), by=c('year','variable','subpopulation')]
dps_subpop[resid>resid_upper | resid<resid_lower, dps_label:=dps]
# --------------------------------------------------------------------------------------


# ----------------------------------------------
# Graph

# comparison in 2017
p1 = ggplot(dps[grepl('mean',variable) & year==2017], aes(y=value/100000, 
		x=pf_incidence/1000000, label=dps_label1)) + 
	geom_point() + 
	geom_smooth(method='lm', se=FALSE) + 
	geom_text_repel(color='grey25', box.padding=1.5, 
		segment.color='grey75', min.segment.length=0) + 
	facet_wrap(~label, scales='free_y') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Estimated Number of Cases - 2017', 
		subtitle='Aggregated by DPS', 
		y='Number Distributed (in 100,000\'s)', 
		x='Model-Estimated Incidence (Millions of Cases)', 
		color='Year') + 
	theme_bw()
	
# comparison using reported cases
p2 = ggplot(dps[grepl('mean',variable) & year==2017], aes(y=value/100000, 
		x=(mean_newCasesMalariaSevere+mean_newCasesMalariaMild)/100000, label=dps_label2)) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	geom_text_repel(color='grey25', box.padding=1.5, 
		segment.color='grey75', min.segment.length=0) + 
	facet_wrap(~label, scales='free_y') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Reported Number of Cases - 2017', 
		subtitle='Aggregated by DPS', 
		y='Number Distributed (in 100,000\'s)', x='Reported Cases (Mild + Severe in 100,000\'s)', color='Year') + 
	theme_bw()
	
# comparison of MAP vs PNLP cases (selecting one variable arbitrarily to avoid duplication)
p3 = ggplot(dps[variable=='mean_RDT' & year==2017], 
		aes(y=(mean_newCasesMalariaSevere+mean_newCasesMalariaMild)/100000, 
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

# comparison in 2017 among subpopulations for ASAQ (the only one with subpopulations)
p4 = ggplot(dps_subpop[label=='ASAQ Doses' & year==2017 & subpopulation!='received'], aes(y=value/1000000, 
		x=pf_incidence/1000000, label=dps_label)) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	geom_text_repel(color='grey25', box.padding=1.5, 
		segment.color='grey75', min.segment.length=0) + 
	facet_wrap(~subpopulation, scales='free') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Estimated Number of Cases - 2017', 
		subtitle='Aggregated by DPS', 
		y='Number Distributed (in Millions)', 
		x='Model-Estimated Incidence (Millions of Cases)', 
		color='Year') + 
	theme_bw()

# comparison across all years
p5 = ggplot(dps[grepl('mean',variable)], aes(y=value, 
		x=pf_incidence)) + 
	geom_point() + 
	geom_smooth(method='lm', se=FALSE) + 
	facet_grid(year~label, scales='free') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Number of Cases', 
		subtitle='Aggregated by DPS and Year', 
		y='Number Distributed', x='Under-5 Incidence', color='Year') + 
	theme_bw()
	
# comparison across all years
p6 = ggplot(dps[grepl('mean',variable)], aes(y=value, 
		x=(mean_newCasesMalariaSevere+mean_newCasesMalariaMild))) + 
	geom_point() + 
	geom_smooth(method='lm', se=FALSE) + 
	facet_grid(year~label, scales='free') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Number of Cases', 
		subtitle='Aggregated by DPS and Year', 
		y='Number Distributed', x='Under-5 Incidence', color='Year') + 
	theme_bw()
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(outFile, height=6, width=9)
p1
p2
p3
p4
p5
p6
dev.off()
# --------------------------------
