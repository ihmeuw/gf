# --------------------------------------------------------
# David Phillips
#
# 11/17/2017
# Assess unmet need and visualize it
# The working directory should be the root of this repo
# --------------------------------------------------------

# to do:
# match up age groups better. currently poulation is 5-15 and prevalence is 2-10. everything else (i think) is all ages
# why does pixel-aggregated unmet antimalarial align so well with total unmet antimalarial (when itn does not)?

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(gridExtra)
# --------------------


# ----------------------------------------------
# Parameters and settings

# year
year = 2014

# indicators
inds = c('itn','antmal','prev','pop')

# countries
iso3s = 'COD'
# ----------------------------------------------


# ----------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/multi_country/map/')

# output files
graphFile = paste0(dir, '/visualizations/unmet_need_graphs.pdf')
# ----------------------------------------------


# ------------------------------------------------------
# Load/prep data
source('./outcome_measurement/malaria/load_map_data.r')
# ------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Assess unmet need

# compute prevalence numbers
dataCODprev = dataCODprev[order(x,y)]
dataCODpop = dataCODpop[order(x,y)]
dataCODitn = dataCODitn[order(x,y)]
dataCODantmal = dataCODantmal[order(x,y)]
if (nrow(dataCODprev)!=nrow(dataCODpop)) stop('Error: cbinding rasters with different lengths is bad!')
if (nrow(dataCODprev)!=nrow(dataCODitn)) stop('Error: cbinding rasters with different lengths is bad!')
if (nrow(dataCODprev)!=nrow(dataCODantmal)) stop('Error: cbinding rasters with different lengths is bad!')
dataCOD = cbind(dataCODprev, dataCODpop$value, dataCODitn$value, dataCODantmal$value)
setnames(dataCOD, c('x','y','prev','pop','itn','antmal'))

# test for problems
n1 = round(nrow(dataCOD[!is.na(prev) & is.na(pop)])/nrow(dataCOD)*100,3)
if (n1>0) print(paste('Warning:', n1, '% of pixels have prevalence but not population'))
n2 = round(nrow(dataCOD[is.na(prev) & !is.na(pop)])/nrow(dataCOD)*100,3)
if (n2>0) print(paste('Warning:', n2, '% of pixels have population but not prevalence'))
n1 = round(nrow(dataCOD[!is.na(prev) & is.na(itn)])/nrow(dataCOD)*100,3)
if (n1>0) print(paste('Warning:', n1, '% of pixels have prevalence but not ITN'))
n2 = round(nrow(dataCOD[is.na(prev) & !is.na(itn)])/nrow(dataCOD)*100,3)
if (n2>0) print(paste('Warning:', n2, '% of pixels have ITN but not prevalence'))
n1 = round(nrow(dataCOD[!is.na(prev) & is.na(antmal)])/nrow(dataCOD)*100,3)
if (n1>0) print(paste('Warning:', n1, '% of pixels have prevalence but not antimalarial coverage'))
n2 = round(nrow(dataCOD[is.na(prev) & !is.na(antmal)])/nrow(dataCOD)*100,3)
if (n2>0) print(paste('Warning:', n2, '% of pixels have antimalarial coverage but not prevalence'))

# compute prevalence number
dataCOD[, prev_num:=prev*pop]

# compute number of uncovered prevalent cases
dataCOD[, itn_num:=itn*prev_num]
dataCOD[, antmal_num:=antmal*prev_num]
dataCOD[, unmet_itn_num:=(1-itn)*prev_num]
dataCOD[, unmet_antmal_num:=(1-antmal)*prev_num]

# aggregate to national unmet need and "ecologically-fallacious" unmet need
cols = c('prev_num','pop','unmet_itn_num','unmet_antmal_num','itn_num','antmal_num')
natData = dataCOD[,lapply(.SD, sum, na.rm=TRUE), .SDcols=cols]
natData[, itn:=itn_num/pop]
natData[, unmet_itn_num_fal:=(1-itn)*prev_num]
natData[, antmal:=antmal_num/pop]
natData[, unmet_antmal_num_fal:=(1-antmal)*prev_num]
natData[, unmet_itn_rate:=unmet_itn_num/pop]
natData[, unmet_antmal_rate:=unmet_antmal_num/pop]
natData[, unmet_itn_rate_fal:=unmet_itn_num_fal/pop]
natData[, unmet_antmal_rate_fal:=unmet_antmal_num_fal/pop]

# aggregate to health zones and repeat
hzData = extract(rasterFromXYZ(dataCOD[,c('x','y',cols),with=FALSE]), mapCOD)
hzData = data.table(do.call('rbind',lapply(hzData, colSums, na.rm=TRUE)))
hzData[, itn:=itn_num/pop]
hzData[, unmet_itn_num_fal:=(1-itn)*prev_num] # coverage is among the population, but it makes sense to multiply by prevalence to count unmet. that's the inefficiency we're after...
hzData[, antmal:=antmal_num/pop]
hzData[, unmet_antmal_num_fal:=(1-antmal)*prev_num]
hzData[, unmet_itn_rate:=unmet_itn_num/pop]
hzData[, unmet_antmal_rate:=unmet_antmal_num/pop]
hzData[, unmet_itn_rate_fal:=unmet_itn_num_fal/pop]
hzData[, unmet_antmal_rate_fal:=unmet_antmal_num_fal/pop]
mapCOD@data = data.frame(mapCOD@data, hzSums[,c('pop','prev_num','unmet_itn','unmet_itn','unmet_itn_rate','unmet_antmal_rate'),with=FALSE])
# ---------------------------------------------------------------------------------------


# ----------------------------------------
# Set up to graph

# graph data
graphData = melt(natData)
graphData[!grepl('_fal', variable), calc:='National Coverage']
graphData[grepl('_fal', variable), calc:='Pixel-Level']
graphData[, variable:=gsub('_fal', '', variable)]
graphData = graphData[variable %in% c('unmet_itn_rate', 'unmet_antmal_rate')]
graphData[variable=='unmet_itn_rate', variable:='ITN']
graphData[variable=='unmet_antmal_rate', variable:='Antimalarial']

# colors
cols1 = brewer.pal(6, 'Spectral')
cols2 = brewer.pal(6, 'Paired')
border = 'grey65'

# limits 
lims = c(min(hzData$unmet_itn_rate,hzData$unmet_itn_rate_fal), max(hzData$unmet_itn_rate,hzData$unmet_itn_rate_fal))
# ----------------------------------------


# ----------------------------------------------------------------------
# Graph unmet need

# map of pixel-level unmet need
p1 = ggplot(dataCOD, aes(y=y, x=x, fill=unmet_itn*100)) + 
	geom_tile() + 
	geom_path(data=shapeDataCOD, aes(x=long, y=lat, group=group)
		, color=border, size=.05, inherit.aes=FALSE) + 
	scale_fill_gradientn('%', colors=cols1, na.value='white') + 
	coord_fixed(ratio=1) + 
	scale_x_continuous('', breaks = NULL) + 
	scale_y_continuous('', breaks = NULL) + 
	labs(title='Unmet ITN Need (% of cases)') + 
	theme_minimal(base_size=16) + 
	theme(plot.title=element_text(hjust=.5)) 
	
# national level
p2 = ggplot(graphData[calc=='Pixel-Level'], aes(x=variable, y=value, fill=calc)) + 
	geom_bar(stat='identity', position='dodge', fill=cols2[2]) + 
	labs(title='Unmet Need', y='Unmet Need (% of cases)', x='') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5), plot.subtitle=element_text(hjust=.5))

# national level comparing to ecological
p3 = ggplot(graphData, aes(x=variable, y=value, fill=calc)) + 
	geom_bar(stat='identity', position='dodge') + 
	scale_fill_manual('', values=cols2) + 
	labs(title='Unmet Need', y='Unmet Need (% of cases)', x='') + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5), plot.subtitle=element_text(hjust=.5))

# health zone level to show the ecological fallacy
p4 = ggplot(hzData, aes(y=unmet_itn_rate, x=unmet_itn_rate_fal)) + 
	geom_point(size=2, alpha=.5) + 
	geom_abline(slope=1, intercept=0) + 
	scale_x_continuous(limits=lims) +
	scale_y_continuous(limits=lims) +
	labs(title='Unmet ITN Need', subtitle='(% of cases)', y='Calculated Based on Health Zone Totals', x='Calculated at Pixel Level') + 
	theme_bw(base_size=16) + 
	theme(plot.title=element_text(hjust=.5), plot.subtitle=element_text(hjust=.5))
# ----------------------------------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
p1
p2
p3
p4
dev.off()
# --------------------------------
