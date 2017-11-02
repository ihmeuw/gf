# ----------------------------------------------
# David Phillips
#
# 10/31/2017
# Various comparisons between aggregate PHIA VL suppression 
# estimates and numbers from the Uganda VL dashboard
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(readstata13)
library(tools)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
# --------------------


# -------------------------------------------------------------------------------------------
# Files and directories

# data directory
dir = 'J:/Project/Evaluation/GF/special_assessments/uga/'

# input files
inFilePHIA = paste0(dir, 'phia_2016/vl_suppression_by_region.csv')
inFileVLD = paste0(dir, 'vl_dashboard/facilities_suppression_201710311708_aug16_mar17.csv')
inFileAIS = "J:/DATA/MACRO_AIS/UGA/2011/UGA_AIS6_2011_IND_Y2012M10D11.DTA"

# district/region maps
distMapFile = paste0(dir, '../../mapping/uga/uga_geographies_map.csv')
distAltMapFile = paste0(dir, '../../mapping/uga/uga_alternate_dist_names.csv')
regAltMapFile = paste0(dir, '../../mapping/uga/uga_alternate_region_names.csv')

# uganda shapefile
shapeFile = paste0(dir, '../../mapping/uga/uga_region10_map.rdata')

# output files
outFile = paste0(dir, 'phia_vl_dashboard.pdf')
# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# Load/prep both datasets

# load
phiaData = fread(inFilePHIA)
vldData = fread(inFileVLD)

# map phia to standard regions
regAltMap = fread(regAltMapFile)
phiaData = merge(phiaData, regAltMap, by.x='Region', by.y='region10_alt_name', all.x=TRUE)
phiaData[is.na(region10_name), region10_name:=Region]
phiaData$Region = NULL

# correct non-standard district names
distAltMap = fread(distAltMapFile)
vldData[, District:=gsub(' District', '', District)]
vldData = merge(vldData, distAltMap, by.x='District', by.y='dist_alt_name', all.x=TRUE)
vldData[is.na(dist_name), dist_name:=District]

# test for matching district names
distMap = fread(distMapFile)
t1 = unique(vldData$dist_name)[!unique(vldData$dist_name) %in% unique(distMap$dist112_name)]
t2 = unique(distMap$dist112_name)[!unique(distMap$dist112_name) %in% unique(vldData$dist_name)]
if (length(t1)>0) stop('Warning! There are some districts in the VLD data that aren\'t in the standard 112 list!')
if (length(t2)>0) stop('Warning! There are some districts the standard 112 list that aren\'t in in the VLD data!')

# map vld data to standard regions
distMap = distMap[, c('region10_name', 'region10', 'dist112_name', 'dist112'), with=FALSE]
vldData = merge(vldData, distMap, by.x='dist_name', by.y='dist112_name', all.x=TRUE)
# -------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------
# Load/prep AIS dataset

# load
aisData = data.table(read.dta13(inFileAIS))

# collapse to estimate art coverage at the region level
aisData = aisData[, list('art_coverage'=mean(s535=='yes', na.rm=TRUE)), by='v024']

# map to standard regions
aisData[,v024:=toTitleCase(as.character(v024))]
aisData[,v024:=gsub(' ', '_', v024)]
aisData = merge(aisData, regAltMap, by.x='v024', by.y='region10_alt_name', all.x=TRUE)
aisData[is.na(region10_name), region10_name:=v024]
aisData$v024 = NULL
# ------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# Merge datasets and format for analysis

# merge
facLevelData = merge(phiaData, vldData, by='region10_name')

# clean up variable names
setnames(facLevelData, c('VLS Prevalence (%)', '95% CI', 'Valid Results', 'Suppressed Results'), c('phia_vls', 'phia_vls_ci', 'samples', 'vl_suppressed_samples'))

# split confidence intervals
facLevelData[, c('phia_vls_lower', 'phia_vls_upper'):=tstrsplit(phia_vls_ci, '-', fixed=TRUE)]
facLevelData[, phia_vls_lower:=as.numeric(phia_vls_lower)]
facLevelData[, phia_vls_upper:=as.numeric(phia_vls_upper)]

# collapse to region level
data = facLevelData[, list(phia_vls=mean(phia_vls), 
					phia_vls_lower=mean(phia_vls_lower), 
					phia_vls_upper=mean(phia_vls_upper), 
					samples=sum(samples), 
					vl_suppressed_samples=sum(vl_suppressed_samples)), 
					by=c('region10_name','region10')]
					
# recompute suppression from the dashboard data
data[, vld_suppression:=vl_suppressed_samples/samples*100]

# bring in coverage estimates
data = merge(data, aisData, 'region10_name')

# compute vld suppression adjusted for coverage
data[, vld_suppression_adj:=vld_suppression*art_coverage]
# -------------------------------------------------------------------------------------------


# ----------------------------------------
# Store linear fit
lmFit = lm(vld_suppression~phia_vls, data)
coefs = lmFit$coefficients
# ----------------------------------------


# -------------------------------------------------------------------------------------------
# Set up to graph

# clean names
data[, region10_name:=gsub('_', ' ', region10_name)]

# load/fortify shape data
load(shapeFile)
mapData = data.table(fortify(map))

# reshape long
long = melt(data, id.vars='region10_name')
long[, value:=as.numeric(value)]

# merge to map and melt data
data[, region10:=as.character(region10)]
mapData = merge(mapData, data, by.x='id', by.y='region10', all.x=TRUE)
mapData = melt(mapData, id.vars=c('long','lat','region10_name','id','group','order','hole','piece'))

# clean up variable labels
long[variable=='phia_vls', variable:='PHIA']
long[variable=='vld_suppression', variable:='National Dashboard']
mapData[variable=='phia_vls', variable:='Viral Load Suppression\nPHIA']
mapData[variable=='vld_suppression', variable:='Reported Viral Load Suppression\nNational Dashboard']
mapData[variable=='phia_vls_lower', variable:='Lower']
mapData[variable=='phia_vls_upper', variable:='Upper']
mapData[variable=='samples', variable:='N Samples']
mapData[variable=='vl_suppressed_samples', variable:='N Samples Suppressed']

# colors
colors = c('#CAF270', '#73D487', '#30B097', '#288993', '#41607A', '#453B52')
mapColors = colorRampPalette(colors)
mapColors = mapColors(10)
# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# Make graphs

# map side-by-side
vars1 = c('Reported Viral Load Suppression\nNational Dashboard', 'Viral Load Suppression\nPHIA')
ggplot(mapData[variable %in% vars1], aes(x=long, y=lat, group=group, fill=value)) + 
	geom_polygon() + 
	geom_path(color='grey95', size=.05) + 
	facet_wrap(~variable) + 
	scale_fill_gradientn('%', colours=mapColors) + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	theme_minimal(base_size=16)

# bar graphs
vars2 = c('National Dashboard', 'PHIA')
ggplot(long[variable %in% vars2], aes(x=region10_name, y=value, fill=variable)) + 
	geom_bar(stat='identity', position='dodge') + 
	scale_fill_manual('Data Source', values=colors[c(6,4)]) + 
	labs(y='Viral Load Suppression (%)', x='') + 
	theme_bw(base_size=14)

# scatterplot
min = min(data$phia_vls,data$vld_suppression)
max = max(data$phia_vls,data$vld_suppression)
ggplot(data, aes(x=phia_vls, y=vld_suppression)) + 
	geom_abline(color='red', slope=coefs[2], intercept=coefs[1], linetype='longdash', size=1.25) + 
	geom_abline(slope=1, intercept=0) + 
	geom_point(color=colors[4], size=4.5, alpha=.7, stroke=0) + 
	scale_fill_manual('Data Source', values=colors[c(6,4)]) + 
	labs(title='Viral Load Suppression', subtitle='Comparison of Sources', y='National Dashboard', x='PHIA') + 
	scale_x_continuous(limits=c(min,max)) +
	scale_y_continuous(limits=c(min,max)) +
	theme_bw(base_size=14) + 
	theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
	
# maps showing uncertainty


# scatterplot incorporating uncertainty

# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# Save

# -------------------------------------------------------------------------------------------
