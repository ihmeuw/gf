# ----------------------------------------------
# David Phillips
#
# 11/14/2017
# Prep code for PNLS viral load data
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


# ---------------------------------------------
# Files and directories

# data directory
dir <- 'C:/local/Outcome Measurement/HIV/'

# input file
inFile <- paste0(dir, 'VL PNLS Labo Data.csv')
# ---------------------------------------------


# ----------------------------------------------------------------
# Load/prep data

# load
data <- fread(inFile)

# rename variables
names = c('lab', 'patient', 'dob', 'sex', 'partners', 'province', 
		'health_zone', 'health_facility', 'vl', 'vlog', 'notes')
setnames(data, names)

# format date variable
data[, dob:=as.numeric(dob)]
data[, dob:=as.Date(dob, origin='1900-01-01')]

# clean vl variable
data[tolower(vl)=='not detected', vl:=0]
data[, vl:=as.numeric(vl)]

# make new variables
data[, age:=as.numeric(floor((Sys.Date()-dob)/365.25))]
data[age<0 | age>100, age:=NA]
# ----------------------------------------------------------------


# ----------------------------------------------
# Graph

# colors
cols <- brewer.pal(6, 'Paired')

# store graph
p <- ggplot(data, aes(y=y, x=x, color=z)) + 
	geom_point() + 
	geom_line(aes(y=pred)) + 
	scale_color_gradientn('Z', colors=cols) + 
	theme_bw()
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
p
dev.off()
# --------------------------------
