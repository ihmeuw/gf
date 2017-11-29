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
# --------------------


# ---------------------------
# Parameters and settings

# inputs for load_map_data.r
year = 2014
iso3s = 'UGA'
inds = c('itn','prev')
crop = FALSE
# ---------------------------


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
# ----------------------------------------------


# -----------------------------------------------------
# Load/prep MAP data

# load
source('./outcome_measurement/malaria/load_map_data.r')

# append itn and prevalence
dataUGAprev = dataUGAprev[order(x,y)]
dataUGAitn = dataUGAitn[order(x,y)]
if (nrow(dataUGAprev)!=nrow(dataUGAitn)) stop('Error: cbinding rasters with different lengths is bad!')
dataUGA = cbind(dataUGAprev, dataUGAitn$value)
setnames(dataUGA, c('x','y','prev','itn'))
# -----------------------------------------------------


# -----------------------------------------------------
# Load/prep VL data
vlData = fread(inFilevls)
# -----------------------------------------------------


# -----------------------------------------------------
# Load/prep poverty data

# load
povertyHighRes = raster(povertyFile)

# aggregate to the same resolution as MAP
poverty = projectRaster(povertyHighRes, rasterDataUGA)

# format as data table
poverty = data.table(as.data.frame(poverty, xy=TRUE))
setnames(poverty, c('x','y','mpi'))
# -----------------------------------------------------


# -----------------------------------------------------
# Merge

# the projectRaster function should enable perfect merging
data = merge(poverty, dataUGA, by=c('x','y'), all=TRUE)

# aggregate to ditrict level
dataDist = extract(rasterFromXYZ(data[,c('x','y','mpi',inds),with=FALSE]), mapUGA)
dataDist = data.table(do.call('rbind',lapply(dataDist, colMeans, na.rm=TRUE)))
dataDist[,dist112:=as.character(mapUGA@data[,'dist112'])]

# merge to VL data
dataDist = merge(vlData, dataDist, by='dist112',all=TRUE)
# -----------------------------------------------------


# ---------------------------------
# Run analysis

# ITN controlling for prevalence 
summary(lm(logit(vld_suppression_hat/100)~logit(mpi)+logit(prev),dataDist))

# VLS 
summary(lm(logit(vld_suppression_hat/100)~logit(mpi),dataDist))
# ---------------------------------


# ----------------------------------------------
# Set up to graph

cols = c('#009392','#39b185','#9ccb86','#e9e29c','#eeb479','#e88471','#cf597e')

# identify prevalence quantiles
l=7
qs = quantile(data$prev, seq(0,1,length=l), na.rm=TRUE)
qs[1] = -1
qs[length(qs)] = 2
labs = paste('Prevalence Quantile', seq(l-1))
data[, prev_quantile:=cut(prev, qs, labels=labs)]
# ----------------------------------------------


# ----------------------------------------------
# Graph

# itn over poverty by prevalence quantiles
ggplot(data[!is.na(prev)], aes(x=mpi, y=logit(itn), color=prev)) + 
	geom_point() + 
	facet_wrap(~prev_quantile) + 
	geom_smooth(method='lm')

# itn over poverty
ggplot(dataDist, aes(x=logit(mpi), y=logit(itn))) + 
	geom_point()

# vls over poverty
ggplot(dataDist, aes(x=logit(mpi), y=logit(vld_suppression_hat/100))) + 
	geom_point() + 
	geom_smooth(method='lm') + 
	facet_wrap(~year)

# ----------------------------------------------


# --------------------------------
# Save graphs
# pdf(graphFile, height=6, width=9)
# p
# dev.off()
# --------------------------------
