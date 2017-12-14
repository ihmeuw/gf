# ---------------------------------------------------
# David Phillips
#
# 11/29/2017
# Various graphs showing the distribution of 
# VL suppression and malaria indicators over poverty
# The working directory should be the root of this repo
# ---------------------------------------------------


# to do
# make district aggregation population-weighted

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(raster)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(rgdal)
library(rgeos)
library(grid)
library(gridExtra)
library(boot)
# --------------------


# ---------------------------------
# Parameters and settings

# inputs for load_map_data.r
years = 2014
iso3s = 'UGA'
inds = c('prev','pop_total','antmal','itn')
crop = FALSE
# ---------------------------------


# ----------------------------------------------
# Files and directories

# MAP data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/map/')

# poverty files
povertyFile = paste0(dir, '../../../covariates/uga/Uganda 1km Poverty/uga11povmpi.tif')

# viral load suppression
inFilevls = paste0(dir, '../../../special_assessments/uga/output/district_year_vls.csv')

# output files
graphFile = paste0(dir, '../../vfm/', iso3s, '/visualizations/unmet_need_graphs.pdf')

# prep function
source('./outcome_measurement/malaria/load_map_data.r')
# ----------------------------------------------


# -----------------------------------------------------
# Load/prep MAP data

# load
out = loadMapData(iso3s, years, inds, crop=FALSE)
data = out$data
map = out$maps$UGA
shapeData = out$shapeData
baseRaster = out$baseRasters$UGA
# -----------------------------------------------------


# -----------------------------------------------------
# Load/prep VL data
vlData = fread(inFilevls)
# -----------------------------------------------------


# -----------------------------------------------------
# Load/prep poverty data

# load
povertyHighRes = raster(povertyFile)

# store data table of high res for mapping
povertyHighResdt = data.table(as.data.frame(povertyHighRes, xy=TRUE))

# aggregate to the same resolution as MAP
poverty = projectRaster(povertyHighRes, baseRaster)

# format as data table
poverty = data.table(as.data.frame(poverty, xy=TRUE))
setnames(poverty, c('x','y','mpi'))
# -----------------------------------------------------


# -----------------------------------------------------
# Merge

# the projectRaster function should enable perfect merging
data = merge(poverty, data, by=c('x','y'), all=TRUE)

# aggregate to ditrict level
dataDist = extract(rasterFromXYZ(data[,c('x','y','mpi',inds),with=FALSE]), map)
dataDist = data.table(do.call('rbind',lapply(dataDist, colMeans, na.rm=TRUE)))
dataDist[,dist112:=as.character(map@data[,'dist112'])]

# merge to VL data
dataDist = merge(vlData, dataDist, by='dist112',all=TRUE)
# -----------------------------------------------------


# ---------------------------------
# Run analysis

# ITN controlling for prevalence 
summary(lm(logit(vld_suppression_hat/100)~logit(mpi)+logit(prev)+log(pop_total),dataDist))

# VLS 
summary(lm(logit(vld_suppression_hat/100)~logit(mpi),dataDist))
# ---------------------------------


# ----------------------------------------------
# Set up to graph

# colors
cols = rev(c('#009392','#39b185','#9ccb86','#e9e29c','#eeb479','#e88471','#cf597e'))
border = 'grey65'

# identify population quantiles
l=3
qs = quantile(data[pop_total>1]$pop_total, seq(0,1,length=l+1), na.rm=TRUE)
qs[1] = -1
qs[length(qs)] = max(data$pop_total,na.rm=TRUE)+1
labs = paste('Population Quantile', seq(l))
data[pop_total>1, pop_quantile:=cut(pop_total, qs, labels=labs)]

# identify prevalence quantiles
l=6
qs = quantile(data$prev, seq(0,1,length=l+1), na.rm=TRUE)
qs[1] = -1
qs[length(qs)] = 2
labs = paste('Prevalence Quantile', seq(l))
data[, prev_quantile:=cut(prev, qs, labels=labs)]

# merge district level data to shapedata
shapeData = shapeData[, 1:8, with=FALSE]
dataDist[, dist112:=as.numeric(dist112)]
shapeData = merge(shapeData, dataDist[year==2016, c('dist112','mpi'), with=FALSE], by.x='id', by.y='dist112')
# ----------------------------------------------


# ----------------------------------------------
# Graph

# map of poverty
p1 = ggplot(povertyHighResdt, aes(y=y, x=x, fill=uga11povmpi)) + 
	geom_tile() + 
	geom_path(data=shapeData, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('% Impoverished', colors=cols, na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Multidimensional Poverty Index') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 

# map of district-level poverty
ggplot(shapeData, aes(x=long, y=lat, group=group, fill=mpi)) + 
	geom_polygon() + 
	geom_path(color=border, size=.05) + 
	scale_fill_gradientn('% Impoverished', colors=cols, na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Multidimensional Poverty Index') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 

# map of itn
p2 = ggplot(data, aes(y=y, x=x, fill=itn)) + 
	geom_tile() + 
	geom_path(data=shapeData, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('Log-Population', colors=cols, na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Population') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 

# poverty over population density
ggplot(data[pop_total>1], aes(x=mpi, y=log(pop_total))) + 
	geom_point(alpha=.5, color=cols[6]) + 
	geom_smooth(method='lm') + 
	labs(title='Poverty Compared to Population Density', y='Log-Population', x='Poverty') + 
	theme_bw(base_size=16)
	
# prevalence over poverty
ggplot(data, aes(x=mpi, y=logit(prev))) + 
	geom_point(alpha=.5, color=cols[2]) + 
	geom_smooth(method='lm') + 
	theme_bw(base_size=16)
	
# itn over poverty
ggplot(data, aes(x=mpi, y=logit(itn))) + 
	geom_point(alpha=.5, color=cols[2]) + 
	geom_smooth(method='lm') + 
	labs(title='ITN Coverage Compared to Poverty', y='Logit-ITN Coverage', x='Poverty') + 
	theme_bw(base_size=16)
	
# antimalarials over poverty
ggplot(data, aes(x=mpi, y=logit(antmal))) + 
	geom_point(alpha=.5, color=cols[3]) + 
	geom_smooth(method='lm') + 
	labs(title='Antimalarial Coverage Compared to Poverty', y='Logit-Antimalarial Coverage', x='Poverty') + 
	theme_bw(base_size=16)
	
# prevalence over poverty by population quantiles
ggplot(data[pop_total>1], aes(x=mpi, y=logit(prev), color=log(pop_total))) + 
	geom_point(alpha=.5) + 
	facet_wrap(~pop_quantile, scales='free') + 
	geom_smooth(method='lm') + 
	scale_color_gradientn(colors=cols) + 
	theme_bw(base_size=16)
	
# itn over poverty by prevalence quantiles
ggplot(data[!is.na(prev)], aes(x=mpi, y=logit(itn), color=prev)) + 
	geom_point(alpha=.5) + 
	facet_wrap(~prev_quantile, scales='free') + 
	geom_smooth(method='lm') + 
	scale_color_gradientn(colors=cols) + 
	labs(title='ITN Coverage Compared to Poverty', y='Logit-ITN Coverage', x='Poverty') + 
	theme_bw(base_size=16)
	
# antimalarials over poverty by prevalence quantiles
ggplot(data[!is.na(prev)], aes(x=mpi, y=logit(antmal), color=prev)) + 
	geom_point(alpha=.5) + 
	facet_wrap(~prev_quantile, scales='free') + 
	geom_smooth(method='lm') + 
	scale_color_gradientn(colors=cols) + 
	labs(title='Antimalarial Coverage Compared to Poverty', y='Logit-Antimalarial Coverage', x='Poverty') + 
	theme_bw(base_size=16)

# vls over poverty
ggplot(dataDist, aes(x=mpi, y=logit(vld_suppression_hat/100))) + 
	geom_point(alpha=.5, color=cols[1]) + 
	geom_smooth(method='lm') + 
	labs(title='Viral Load Suppression Compared to Poverty', y='Logit-VL Suppression', x='Poverty') + 
	facet_wrap(~year) + 
	theme_bw(base_size=16)
# ----------------------------------------------


# --------------------------------
# Save graphs
# pdf(graphFile, height=6, width=9)
grid.arrange(p1, p2, ncol=2)
# dev.off()
# --------------------------------
