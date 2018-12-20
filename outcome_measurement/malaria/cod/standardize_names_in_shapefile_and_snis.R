# Audrey Batzel 
# 12-14-18
#
# Convert SNIS names and shapefile names to standardized ones 
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
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
# --------------------  

# ---------------------------------------------------
# Files and directories
# ---------------------------------------------------
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir <-paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/')
dir_pnlp <- paste0(dir, 'prepped_data/PNLP/')
dir_dhis = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')
dir_shape = paste0(root, "/Project/Evaluation/GF/mapping/cod/")

# input files
pnlp_natl <- "post_imputation/imputedData_run2_agg_country.rds"
dhis_base <- "base_services_drc_01_2017_09_2018_prepped.rds"
dhis_sigl <- "sigl_drc_01_2015_07_2018_prepped.rds"

dps_shp  = paste0(dir_shape, "gadm36_COD_shp/gadm36_COD_1.shp")
hz_shp = paste0(dir_shape, "health_zones_who/health2.shp")

old_dps_names = "./core/old_dps_names_to_new.csv"
dps_names = read.csv(old_dps_names, stringsAsFactors = FALSE)

# output files
out_dir = "C:/Users/abatzel/Documents/Audrey_PCE/edited data for Constant/"

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ---------------------------------------------------

# ---------------------------------------------------
# load data
# ---------------------------------------------------
base <- readRDS(paste0(dir_dhis, dhis_base)) 
sigl <- readRDS(paste0(dir_dhis, dhis_sigl))

shp_dps = shapefile(dps_shp)
shp_hz = shapefile(hz_shp)
# ---------------------------------------------------

# ---------------------------------------------------
# standardize names 
# ---------------------------------------------------
base$health_zone <- tolower(base$health_zone)
base$health_zone <- gsub(" ", "-", base$health_zone)
base$health_zone <- standardizeHZNames(base$health_zone)

base$dps <- tolower(base$dps)
base$dps <- gsub(" ", "-", base$dps)
base$dps <- standardizeDPSNames(base$dps)

sigl$health_zone <- tolower(sigl$health_zone)
sigl$health_zone <- gsub(" ", "-", sigl$health_zone)
sigl$health_zone <- standardizeHZNames(sigl$health_zone)

sigl$dps <- tolower(sigl$dps)
sigl$dps <- gsub(" ", "-", sigl$dps)
sigl$dps <- standardizeDPSNames(sigl$dps)

# fix duplicated names in shp_hz
shp_hz@data[shp_hz@data$Name=='Kikwit-sud' & shp_hz@data$Name_API=='Kikwit-Nord',]$Name <- 'Kikwit-Nord'
dups = duplicated(shp_hz@data[,c(1,7)])
shp_hz@data[shp_hz@data$Name=='Tshamilemba' & dups==TRUE,]$Name <- 'Tshamilemba_2'
shp_hz@data$Name <- standardizeHZNames(shp_hz@data$Name)
  # make a unique identifier
  shp_hz@data$index <- 1:nrow(shp_hz@data)

shp_dps@data$NAME_1 <- standardizeDPSNames(shp_dps@data$NAME_1)
shp_dps@data$NAME_1 <- tolower(shp_dps@data$NAME_1)
shp_dps@data$NAME_1 <- gsub(" ", "-", shp_dps@data$NAME_1)
# ----------------------------------------------

# ----------------------------------------------
# save updated sigl and base
# ----------------------------------------------
saveRDS(dt, paste0(out_dir, "snis_base_data.rds"))
saveRDS(dt_sigl, paste0(out_dir, "snis_sigl_data.rds"))

saveRDS(shp, paste0(out_dir, "hz_shapefile.rds"))
saveRDS(shp_dps, paste0(out_dir, "dps_shapefile.rds"))
# ----------------------------------------------

# ----------------------------------------------
# more changes to data 
# ----------------------------------------------
# add old dps names to data
dps_names <- as.data.table(dps_names)
dps_names <- dps_names[, .(old_name, new_name)]
setnames(dps_names, "old_name", "former_dps_name")
dt <- merge(dt, dps_names, by.x="dps", by.y="new_name", all.x=TRUE)
dt_sigl <- merge(dt_sigl, dps_names, by.x="dps", by.y="new_name", all.x=TRUE)

shp@data$PROVNAME <-  tolower(shp@data$PROVNAME)
shp@data$PROVNAME <- gsub(" ", "-", shp@data$PROVNAME)
shp@data$PROVNAME <- trimws(shp@data$PROVNAME)

shp@data[shp@data$PROVNAME=="haut-katanga" & !is.na(shp@data$PROVNAME),]$PROVNAME <- "katanga"
shp@data[shp@data$PROVNAME=="province-oriental" & !is.na(shp@data$PROVNAME),]$PROVNAME <- "oriental"
# ----------------------------------------------

# ----------------------------------------------
# test to make sure it works "out of the box" 
# ----------------------------------------------
dt = readRDS(paste0(out_dir, "snis_base_data.rds"))
dt_sigl = readRDS(paste0(out_dir, "snis_sigl_data.rds"))
shp = readRDS(paste0(out_dir, "hz_shapefile.rds"))

coordinates = as.data.table(fortify(shp, region='index'))
shp@data$index <- as.character(shp@data$index)
coordinates <- merge(coordinates, shp@data, by.x='id', by.y='index', all.x=TRUE)

dt_hz = dt[type=="malaria"& element=="A 1.4 Paludisme grave" & year=="2017", ]
dt_hz = dt_hz[, .(value = sum(value, na.rm=TRUE)), by= c("health_zone", "dps", "former_dps_name") ]

graphData <- merge(dt_hz, coordinates, by.x=c('health_zone', 'former_dps_name'), by.y=c('Name', 'PROVNAME'), all=TRUE)


max = max(graphData$value, na.rm=TRUE)
min = min(graphData$value, na.rm=TRUE)
med = max/2

m <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = med, ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  labs(title="Total number of severe malaria cases in 2017", 
       fill='Number of cases') 
m

pdf(paste0(out_dir, "severe_malaria_hz_map.pdf"), height = 9, width = 11)
m
dev.off()





