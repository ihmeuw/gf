# ------------------------------
# David Phillips
# 
# 8/22/2018
# Use interrupted time series analysis to measure the 
# effect of rationalization on treatment outputs in DRC
# The current working directory should be the root of this repo
# ------------------------------


# ------------------------------
# set up R
rm(list=ls())
library(data.table)
library(readxl)
library(reshape2)
library(MASS)
library(stats4)
library(ggplot2)
library(RColorBrewer)
# ------------------------------


# ------------------------------
# Files and directories

# input file
dir = 'J:/Project/Evaluation/GF/impact_evaluation/cod/'
inFile = paste0(dir, 'prepped_data/acts_used_byFunderTransition_dpsLevel.csv')

# output file
outFile = paste0(dir, 'visualizations/itn_analysis.pdf')

# its function
source('./impact_evaluation/its.r')
source('./impact_evaluation/graph.r')
# ------------------------------


# ---------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# convert to date
data[, date:=as.Date(date)]

# subset to intervention
data = data[date>=as.Date('2011-06-01')]

# convert dates to numbers
# data[, moyr:=seq(nrow(data))]
setnames(data, 'date', 'moyr')
# ---------------------------------------------


# ------------------------------------------------------
# Set up/run regression

# run its function on intervention provinces
out1 = its(data=data[trans=='yes'], outcome='totACTs', 
		cutpoint=as.Date(c('2015-01-01', '2016-01-01')), 
		slope=TRUE, newEffectDate=NULL)

# run its function on control provinces
out2 = its(data=data[trans=='no'], outcome='totACTs', 
		cutpoint=as.Date(c('2015-01-01', '2016-01-01')), 
		slope=TRUE, newEffectDate=NULL)
		
# run graphing function
p1 = graph(out1, title='Provinces Undergoing Rationalization', ylab='Malaria Patients Treated (Millions)', yscale=1000000)
p2 = graph(out2, title='Other Provinces', ylab='Malaria Patients Treated (Millions)', yscale=1000000)
# ------------------------------------------------------


# -------------------------------
pdf(outFile, height=5.5, width=8)
p1
p2
dev.off()
# -------------------------------

