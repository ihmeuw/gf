# Audrey Batzel
# 10/23/2018
#
# 

# ----------------------------------------------
## Set up R / install packages 
# ----------------------------------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(rlang)
library(zoo)
library(lubridate)
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(maptools)
library(scales)
library(ggrepel)
library(naniar)
library(grid)
library(gridExtra)

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

source('./core/standardizeHZNames.R')

# input files
data <- "PNLT_totalCases_2016.rds"
hz_shapefile <-"health2.shp"

# output files
graphFile <- paste0(out_dir, "map_hz_level_2016_tot_cases.pdf")
graphFile2 <- paste0(out_dir, "map_hz_level_2016_tot_cases_by_pop.pdf")
# ----------------------------------------------


# ----------------------------------------------
## Load in prepped data & shapefile / set up
# ----------------------------------------------
dt <- readRDS(paste0(data_dir, data))
dt <- as.data.table(dt)
dt$tot_case <- as.numeric(dt$tot_case)
dt$pop_tot <- as.numeric(dt$pop_tot)
dt$hz_compare <- standardizeHZNames(dt$hz)

drcShape <- shapefile(paste0(shape_dir, hz_shapefile)) # shapefile with all DPS (use a different one for health zones)
coordinates = as.data.table(fortify(drcShape, region='Name'))
coordinates$id <- tolower(coordinates$id)
coordinates$id <- gsub(" ", "-", coordinates$id)
coordinates$id <- standardizeHZNames(coordinates$id)

# Make a variable for total cases by population
dt[is.na(pop_tot) | pop_tot=="0", pop_tot:=NA]
dt[, cases_per_pop := ((tot_case/pop_tot)*100000)]
  # there is one hz where it says the population is just 127 people... for now, removing it since it seems to really skew the values
  dt[hz=="loko", cases_per_pop := NA]
# ----------------------------------------------


# ----------------------------------------------
# Make maps at health zone level
# ----------------------------------------------
graphData_all <- merge(coordinates, dt, by.x='id', by.y='hz_compare', all=TRUE, allow.cartesian=TRUE)
graphData_kin <- graphData[dps == "kinshasa",]

graphData <- copy(graphData_all)
# note ------ # use these for standardizing scale
max = max(graphData$tot_case, na.rm=TRUE)
min = min(graphData$tot_case, na.rm=TRUE)
med = max/2

m <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=tot_case)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = med, ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  labs(title="Total TB cases for the first quarter of 2016", 
       fill='Number of cases') 
m

pdf(graphFile, height = 12, width = 12)
print(m)
dev.off()


# cases per 100,000 pop
max = max(graphData$cases_per_pop, na.rm=TRUE)
min = min(graphData$cases_per_pop, na.rm=TRUE)
med = max/2

m <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=cases_per_pop)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = med, ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  labs(title="TB cases for the first quarter of 2016 per 100,000 people", 
       fill='Number of cases (per 100,000 people)') 
m

pdf(graphFile2, height = 12, width = 12)
print(m)
dev.off()
