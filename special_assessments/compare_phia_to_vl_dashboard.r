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

# district/region maps
distMapFile = paste0(dir, '../../mapping/uga/uga_geographies_map.csv')
distAltMapFile = paste0(dir, '../../mapping/uga/uga_alternate_dist_names.csv')
regAltMapFile = paste0(dir, '../../mapping/uga/uga_alternate_region_names.csv')

# output files
outFile = paste0(dir, 'phia_vl_dashboard.pdf')
# -------------------------------------------------------------------------------------------


# ----------------------------------------------
# Load/prep data

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

# ----------------------------------------------
