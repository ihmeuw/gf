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
library(readxl)
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
gf_data <- read_excel("C:/Users/irenac2/Documents/2013 MALARIA PRESUPUESTO POR ORGANISMO (departamento municipio).xls",
                         range='B247:AV302'
)
## remove empty columns 
gf_data<- Filter(function(x)!all(is.na(x)), gf_data)

# remove rows with "TOTAL" (they are redundant from looking at the original file),
gf_subset <- data.table(gf_data[ grep("TOTAL", gf_data$X__3, invert = TRUE) , ])

# remove rows where X__9 has a value (they are redundant),
gf_subset <- na.omit(gf_subset, cols="X__9")
# subset observations

## get region + budgeted expenses 

budget_dataset <- gf_subset[, c("X__9", "X__14"), with=FALSE]
names(budget_dataset) <- c("loc_id", "budgeted_amount")
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
