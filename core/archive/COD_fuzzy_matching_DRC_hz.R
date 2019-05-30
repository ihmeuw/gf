############################################################
# Adapted from Aaron O-Z's match_municipalities_v3
# to match the names of health zones in the 2016 DRC notifications
# to the WHO shapefile of health zones
# J. Ross / Audrey Batzel
# June 15, 2017 / October 24, 2018
#
# ----------------------------------------------
## Set up R / install packages 
# ----------------------------------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(lubridate)
library(raster)
library(rgeos)
library(ggplot2)
library(maptools)

setwd("C:/local/gf")
# ----------------------------------------------

# ----------------------------------------------
## Set up the directories to read/output files: 
# ----------------------------------------------
# Directories
j = ifelse(Sys.info()[1]=='Windows', 'J:/', '/home/j/')
data_dir <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/")
shape_dir <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/health2/")
out_dir <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLT_data/")

# input files
data <- "PNLT_totalCases_2016.rds"
hz_shapefile <-"health2.shp"

# snis <- readRDS("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis/all_units/master_facilities.rds")
# ----------------------------------------------

# ----------------------------------------------
## Load in prepped data & shapefile / set up
# ----------------------------------------------
dt <- readRDS(paste0(data_dir, data))
dt <- as.data.table(dt)

drcShape <- shapefile(paste0(shape_dir, hz_shapefile))
coordinates = as.data.table(fortify(drcShape, region='Name'))

shape_data <- drcShape@data

zones <- copy(dt)
shape <- copy(shape_data)
# ----------------------------------------------

###################################
## try some basic fuzzy matching ##
###################################

## make all chars lowercase
shape_low <- tolower(shape$Name)
zone_low <- tolower(zones$hz)

## look at 'edit' distances between strings. Function adist looks for string distances.
dists <- sapply(shape_low,
                FUN=function(x){
                  adist(x, zone_low)
                } )

min.dists <- apply(X=dists, MARGIN=2, FUN=min)

num.min <- sapply(X=1:length(min.dists),
                  FUN=function(x){
                    sum(dists[, x] == min.dists[x])
                  })

table(min.dists)
table(num.min)
table(min.dists, num.min)
#(351) that match exactly with just one. Another 69 that are only another 1 off.

sum(min.dists == 0 & num.min > 1)
#(4) cases where it looks like something matches perfectly in two regions

#########################
## assign good matches ##
#########################

## put row of matching gaul table entry into:
matched_row <- rep(NA, dim(shape)[1])
# rep function above puts NA into a vector with the length of shape (516)

## if there is 1 minimum, assume it's correct and match
one_matches <- which(num.min==1)
length(one_matches) ## 454
#which.min returns the index (aka position) of the minimmum value.
#So now these positions are hanging out in one.match, but how do they get bound together? See loop below.
for(i in one_matches){
  matched_row[i] <- which.min(dists[, i])
}

#############################################################################################
## manually assign remaining bad matches. 


#############################################################################################

m <- cbind(shape, zones[matched_row, ])
m <- as.data.table(m)

matches <- m[, .(Name, PROVNAME, hz, dps)]

data_after <- m$V2
shape_after <- m$id

data_before <- zones$hz
shape_before <- unique(shape$id)


unmatched_in_data <- data_before[!data_before %in% data_after]

unmatched_in_shape <- m[is.na(V2)]
unmatched_in_shape$id

unmatched_in_shape$id[order(unmatched_in_shape$id)]
unmatched_in_data[order(unmatched_in_data)]


