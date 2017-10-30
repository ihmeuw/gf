# ----------------------------------------------
# David Phillips
#
# 1/25/2017
# Template for a typical self-contained analysis
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
# --------------------


# ----------------------------------------------
# Files and directories

# data directory
dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/ghe_s'

# input file
inFile <- paste0(dir, 'data_file.csv')

# output files
modelOutputFile <- paste0(dir, 'output.rdata')
graphFile <- paste0(dir, 'graphs.pdf')
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data

# load
data <- fread(inFile)

# subset variables
data <- data[, c('var1','x','y','z'), with=FALSE]

# subset observations
data <- data[!is.na(var1)]

# format variables


# reshape data


# make new variables

# ----------------------------------------------


# ---------------------------------
# Run analysis

# model formula
form <- as.formula('y ~ x + z')

# run model
glmOut <- glm(form, 'gaussian', data)

# predict fitted values
data[, pred:=predict(glmOut)]
# ---------------------------------


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
