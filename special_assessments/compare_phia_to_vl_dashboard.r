# ---------------------------------------------------------
# David Phillips
#
# 10/31/2017
# Various comparisons between aggregate PHIA VL suppression 
# estimates and numbers from the Uganda VL dashboard
# The working directory should be the root of this repo
# ---------------------------------------------------------


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
library(gridExtra)
# --------------------


# ----------------------------------------------------------------------
# Files and directories

# prep function
source('./special_assessments/prep_phia_vl_dashboard.r')

# data directory
dir = 'J:/Project/Evaluation/GF/special_assessments/uga/'

# uganda shapefile
shapeFileReg = paste0(dir, '../../mapping/uga/uga_region10_map.rdata')
shapeFileDist = paste0(dir, '../../mapping/uga/uga_dist112_map.rdata')

# output files
outFile = paste0(dir, 'visualizations/phia_vl_dashboard.pdf')
# ----------------------------------------------------------------------


# --------------------------------------
# Prep data at different levels
regData = prepVL(dir, level='region')
distData = prepVL(dir, level='district')
# --------------------------------------


# -----------------------------------------------
# Analysis

# linear fit
lmFit = lm(vld_suppression_adj~phia_vls, regData)
coefs = lmFit$coefficients

# fixed effects for bias correction? would need to logit()
# fe = lm(phia_vls~factor(region10_name):vld_suppression_adj, regData)

# region-specific adjustment factors
regData[, ratio:=vld_suppression_adj/phia_vls]
ratios = regData[,c('region10_name','ratio'),with=F]
distData = merge(distData, ratios, by='region10_name')
distData[, vld_suppression_hat:=vld_suppression_adj/ratio]
# distData[, vld_suppression_pred:=predict(fe, newdata=distData)]
# -----------------------------------------------


# -------------------------------------------------------------------------------------------
# Set up to graph

# clean names
regData[, region10_name:=gsub('_', ' ', region10_name)]

# load/fortify shape data
load(shapeFileReg)
mapDataReg = data.table(fortify(map))
load(shapeFileDist)
mapDataDist = data.table(fortify(map))

# reshape long
long = melt(regData, id.vars='region10_name')
long[, value:=as.numeric(value)]

# wrap text
long[, region10_name:=str_wrap(region10_name, 6)]

# merge to map and melt data
regData[, region10:=as.character(region10)]
mapDataReg = merge(mapDataReg, regData, by.x='id', by.y='region10', all.x=TRUE)
mapDataReg = melt(mapDataReg, id.vars=c('long','lat','region10_name','id','group','order','hole','piece'))

# clean up variable labels
long[variable=='phia_vls', variable:='PHIA']
long[variable=='vld_suppression_adj', variable:='National Dashboard*']
mapDataReg[variable=='phia_vls', variable:='Viral Load Suppression\nPHIA']
mapDataReg[variable=='vld_suppression_adj', variable:='Reported Viral Load Suppression\nNational Dashboard*']
mapDataReg[variable=='phia_vls_lower', variable:='Lower']
mapDataReg[variable=='phia_vls_upper', variable:='Upper']
mapDataReg[variable=='samples', variable:='N Samples']
mapDataReg[variable=='vl_suppressed_samples', variable:='N Samples Suppressed']

# district map
tmp = distData[,c('dist112','vld_suppression_adj','vld_suppression_hat'),with=FALSE]
mapDataDist = merge(mapDataDist, tmp, by.x='id', by.y='dist112', all.x=TRUE)
mapDataDist = melt(mapDataDist, id.vars=c('long','lat','id','group','order','hole','piece'))
mapDataDist[variable=='vld_suppression_adj', variable:='Original']
mapDataDist[variable=='vld_suppression_hat', variable:='Corrected']

# colors
colors = c('#CAF270', '#73D487', '#30B097', '#288993', '#41607A', '#453B52')
mapColors = colorRampPalette(colors)
mapColors = mapColors(10)
# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# Make graphs

# map side-by-side
vars1 = c('Reported Viral Load Suppression\nNational Dashboard*', 'Viral Load Suppression\nPHIA')
p1 = ggplot(mapData[variable %in% vars1], aes(x=long, y=lat, group=group, fill=value)) + 
	geom_polygon() + 
	geom_path(color='grey95', size=.05) + 
	facet_wrap(~variable) + 
	scale_fill_gradientn('%', colours=mapColors) + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(caption='*Adjusted for ART coverage') + 
	theme_minimal(base_size=16) + 
	theme(plot.caption=element_text(size=10)) 

# bar graphs
vars2 = c('National Dashboard*', 'PHIA')
p2 = ggplot(long[variable %in% vars2], aes(x=region10_name, y=value, fill=variable)) + 
	geom_bar(stat='identity', position='dodge') + 
	scale_fill_manual('Data Source', values=colors[c(6,4)]) + 
	labs(y='Viral Load Suppression (%)', x='', caption='*Adjusted for ART coverage') + 
	theme_bw(base_size=14) + 
	theme(plot.caption=element_text(size=10)) 

# scatterplot
min = min(regData$phia_vls,regData$vld_suppression_adj)
max = max(regData$phia_vls,regData$vld_suppression_adj)
p3 = ggplot(regData, aes(x=phia_vls, y=vld_suppression_adj)) + 
	geom_abline(color='red', slope=coefs[2], intercept=coefs[1], linetype='longdash', size=1.25) + 
	geom_abline(slope=1, intercept=0) + 
	geom_point(color=colors[4], size=4.5, alpha=.7, stroke=0) + 
	scale_fill_manual('Data Source', values=colors[c(6,4)]) + 
	labs(title='Viral Load Suppression', subtitle='Comparison of Sources', 
		y='National Dashboard*', x='PHIA', caption='*Adjusted for ART coverage') + 
	scale_x_continuous(limits=c(min,max)) +
	scale_y_continuous(limits=c(min,max)) +
	theme_bw(base_size=14) + 
	theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), plot.caption=element_text(size=10))
	
# maps showing uncertainty


# scatterplot incorporating uncertainty

# -------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# Save
pdf(outFile, height=6, width=9)
p1
p2
p3
dev.off()
# -------------------------------------------------------------------------------------------
