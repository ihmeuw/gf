# --------------------------------------------------------
# David Phillips
#
# 11/17/2017
# Assess unmet need and visualize it
# The working directory should be the root of this repo
# --------------------------------------------------------

# to do:
# match up age groups better. currently poulation is 5-15 and prevalence is 2-10. everything else (i think) is all ages

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
graphFile = paste0(dir, '/visualizations/graphs.pdf')
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
dataCOD[, itn_num:=itn*pop]
dataCOD[, antmal_num:=antmal*pop]
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
# hzValues = extract(rasterFromXYZ(dataCOD), mapCOD)
# hzSums = data.table(do.call('rbind',lapply(hzValues, colSums, na.rm=TRUE)))
# hzSums[, unmet_itn_rate:=unmet_itn/pop]
# hzSums[, unmet_antmal_rate:=unmet_antmal/pop]
# mapCOD@data = data.frame(mapCOD@data, hzSums[,c('pop','prev_num','unmet_itn','unmet_itn','unmet_itn_rate','unmet_antmal_rate'),with=FALSE])
# ---------------------------------------------------------------------------------------


# ----------------------------------------------------------------------
# Set up to graph

# colors
cols = brewer.pal(6, 'Spectral')
border = 'grey65'
# ----------------------------------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)

dev.off()
# --------------------------------
