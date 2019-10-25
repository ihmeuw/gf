# ----------------------------------------------
# David Phillips
# 
# 6/10/2019
# Exploration of absorption numbers from latest PUDRs
# Written for Uganda/malaria (but I tried to make it adaptable)
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
library(RColorBrewer)
library(ggplot2)
# --------------------


# ------------------
# Parameters

# country iso code
country = 'uga'

# disease of focus
disease = 'malaria'
# ------------------


# ----------------------------------------------
# ParaFiles and directories

# root directory
rtDir = 'J:/Project/Evaluation/GF/resource_tracking'

# input file
inFile = paste0(rtDir, '/_gf_files_gos/', country, '/prepped_data/absorption.csv')

# output files
graphFile = paste0(rtDir, 'visualizations/miscellaneous/', country, '_', disease, 'absorption_exploration.pdf')
# ----------------------------------------------


# ---------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# make extra variables
data[, module_disease:=str_sub(code,1,1)]
data[, grant_start:=as.numeric(str_sub(grant_period,1,4))]
data[, semester_id:=gsub('Semester ', '', semester)]
data[grepl('-', semester_id), pudr_duration:=12]
data[!grepl('-', semester_id), pudr_duration:=6]
data[, semester_id:=as.numeric(str_sub(semester_id,1,1))]
data[, pudr_start:=grant_start+(((semester_id-1)*6)/12)]

# subset observations
data = data[module_disease==str_sub(toupper(disease),1,1) | module_disease=='R']
# ---------------------------------------------------------------------


# ----------------------------------------------
# Graph

# colors
cols = brewer.pal(12, 'Paired')

# store graph
ggplot(data[module_disease=='M'], 
		aes(x=absorption, ymin=pudr_start, ymax=pudr_start+(pudr_duration/12), color=grant, size=grant)) + 
	geom_linerange() + 
	coord_flip() + 
	scale_size_manual('', values=c(1.5,2)) + 
	scale_color_manual('', values=cols) + 
	facet_wrap(~gf_module+gf_intervention, scales='free_y') + 
	theme_bw()
# ----------------------------------------------

