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
inFile = paste0(dataDir, 'pnlp_map_hz_year_level.rds')

# output files
outFile = paste0(outDir, 'commodity_distribution_and_need.pdf')
# --------------------------------------------------------------------------------------------


# --------------------------------------------------------------------
# Load data

# load
data = readRDS(inFile)

# melt y-variables
idVars = c('province','dps','health_zone','year', 'subpopulation',
			'mean_newCasesMalariaMild','mean_newCasesMalariaSevere', 
			'lower_newCasesMalariaMild','lower_newCasesMalariaSevere', 
			'upper_newCasesMalariaMild','upper_newCasesMalariaSevere')
data = melt(data, id.vars=idVars)

# combine ASAQ subpopulations to match incidence age groups
data[subpopulation %in% c('2to11mos','1to5yrs'), subpopulation:='under5']
data[subpopulation %in% c('6to13yrs','14yrsAndOlder'), subpopulation:='5andOlder']
data[subpopulation=='pregnantWomen', subpopulation:='5andOlder']
byVars = c('province','dps','health_zone','year', 'subpopulation','variable')
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
		
# identify interesting DPS's to label
fit = lm(value~mean_newCasesMalariaSevere*factor(year)*factor(variable), dps)
dps[, resid:=value-predict(fit)]
dps[, resid_upper:=quantile(resid,.85), by=c('year','variable')]
dps[, resid_lower:=quantile(resid,.15), by=c('year','variable')]
dps[resid>resid_upper | resid<resid_lower, dps_label:=dps]
	
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

# comparison
p1 = ggplot(dps[grepl('mean',variable)], aes(y=value, 
		x=mean_newCasesMalariaSevere, color=factor(year))) + 
	geom_point() + 
	geom_smooth(method='lm', aes(y=value, 
		x=mean_newCasesMalariaSevere), inherit.aes=FALSE) + 
	facet_wrap(~label, scales='free') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Number of Cases', 
		subtitle='Aggregated by DPS and Year', 
		y='Number Distributed', x='Number of New Cases (Severe Malaria)', color='Year') + 
	theme_bw()

# comparison in 2017
p2 = ggplot(dps[grepl('mean',variable) & year==2017], aes(y=value, 
		x=mean_newCasesMalariaSevere, label=dps_label)) + 
	geom_point() + 
	geom_smooth(method='lm', aes(y=value, 
		x=mean_newCasesMalariaSevere), inherit.aes=FALSE) + 
	geom_text_repel(color='grey25', box.padding=1.5, 
		segment.color='grey75', min.segment.length=0) + 
	facet_wrap(~label, scales='free') + 
	scale_color_manual(values=colors) + 
	labs(title='Commodity Distribution Compared to Number of Cases - 2017', 
		subtitle='Aggregated by DPS', 
		y='Number Distributed', x='Number of New Cases (Severe Malaria)', color='Year') + 
	theme_bw()

# comparison in 2017 among subpopulations for ASAQ (the only one with subpopulations)
p3 = ggplot(dps_subpop[label=='ASAQ Doses' & year==2017 & subpopulation!='received'], aes(y=value, 
		x=mean_newCasesMalariaSevere, label=dps_label)) + 
	geom_point() + 
	geom_smooth(method='lm', aes(y=value, 
		x=mean_newCasesMalariaSevere), inherit.aes=FALSE) + 
	geom_text_repel(color='grey25', box.padding=1.5, 
		segment.color='grey75', min.segment.length=0) + 
	facet_wrap(~subpopulation, scales='free') + 
	scale_color_manual(values=colors) + 
	labs(title='ASAQ Distribution Compared to Number of Cases - 2017', 
		subtitle='Aggregated by DPS and Age Group', 
		y='ASAQ Doses Distributed', x='Number of New Cases (Severe Malaria)', color='Year') + 
	theme_bw()
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(outFile, height=6, width=9)
p1
p2
p3
dev.off()
# --------------------------------
