# --------------------
# Set up R / install packages
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(readxl)
library(stats)
library(rlang)
library(zoo)
library(tidyr)
library(dplyr)
library(raster)
library(rgeos)
library(maptools)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ---------------------------------------------- 
# data directory
dir_prepped <-"J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/archive/"
dir_shape = 'J:/Project/Evaluation/GF/mapping/cod/'

# input files
fullData_file <- "fullData_dps_standardized.csv"
dps_shape = "gadm36_COD_shp/gadm36_COD_1.shp"
hz_shape = "health_zones_who/health2.shp"

# functions
setwd('C:/local/gf/')
source('./core/standardizeDPSnames.R')
source('./core/standardizeHZnames.R')
# ----------------------------------------------   

# ----------------------------------------------     
# read in data
# ---------------------------------------------- 
fullData <- fread( paste0(dir_prepped, fullData_file), stringsAsFactors = FALSE) 
# ----------------------------------------------   

# ----------------------------------------------     
# get unique hz's by year
# ---------------------------------------------- 
fullData[, year:= year(date)]

hz_by_year = unique(fullData[, .(year, dps, health_zone)])
hz_by_year[, dummy_var := 1]
hz_by_year$year = as.character(hz_by_year$year)
hz_by_year = dcast.data.table( hz_by_year, dps + health_zone ~ year )

hz_by_year = melt.data.table(hz_by_year, id.vars = c("dps", "health_zone"))

missing = hz_by_year[ is.na(value) & dps != "0", ]
missing[, missing := "yes"]
missing = dcast.data.table( missing, dps + health_zone ~ variable, value.var = "missing" )

hk = fullData[dps == "haut-katanga",]

# TESTING - using healthFacilities_total, newCasesMalariaMild_under5, newCasesMalariaMild_5andOlder, totalCasesAllDiseases_5andOlder for testing

check_hk = unique(hk[, .(health_zone, year)])
# Found https://fr.wikipedia.org/wiki/Kashobwe that Kashobwe is part of Kasenga, and here http://www.world-guides.com/africa/central-africa/democratic-republic-congo/democratic_congo_districts.html 
# that Kashobwe has grown a lot in the last several years (from a village to a small town), so that might be why it's not in the shapefile, but it IS in PNLT/PNLP/SNIS
hk = hk[health_zone %in% c("kashobwe", "kasenga")]
# column with lowest NAs... 
sapply(hk, function(x) sum(is.na(x)))
hk[, date:=as.Date(date)]

g_hk <- ggplot(hk, aes(x=date, y=newCasesMalariaMild_under5, color = health_zone)) +
  geom_point() + geom_line() +
  ggtitle("Time series comparison between health zones") +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16),
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14), 
        strip.text = element_text(size = 14)) +
  scale_y_continuous( label= scales :: comma )
g_hk
# so, treat kashobwe as missing in 2010/2011 and impute it?

nkivu = fullData[dps == "nord kivu" | dps == "0",]
nkivu[, date:=as.Date(date)]
# all NK health zones ARE in the shapefile
# https://reliefweb.int/sites/reliefweb.int/files/resources/RDCongo%20Reference%20Map%20-%20Province%20du%20Nord%20Kivu%20-%20Carte%20administrative%20%28Mars%202012%29.pdf
# This makes it look like there are territories that have health zones within them.  Sometimes the health zone has the same name as the terrirtory

nkivu_subset = nkivu[health_zone %in% c("walikale", "itebero", "kibua", "pinga")]
g_nkivu <- ggplot(nkivu_subset, aes(x=date, y=newCasesMalariaMild_under5, color = health_zone)) +
  geom_point() + geom_line() +
  ggtitle("Time series comparison between health zones") +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16),
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14), 
        strip.text = element_text(size = 14)) +
  scale_y_continuous( label= scales :: comma )
g_nkivu

# HZ: (/territory)...
# Alimbongo: (/Lubero) looks like alimbongo could have been included in lubero prior to 2012?     nkivu[health_zone %in% c("alimbongo", "lubero")]
# Bambo: (/Rutshuru) likely wasn't included in rutshuru?                                          nkivu[health_zone %in% c("bambo", "bambu", "rutshuru", "kibirizi")]
# Kibirizi: (/Rutshuru) could maybe have been included in rutshuru?                               nkivu[health_zone %in% c("bambo", "bambu", "rutshuru", "kibirizi")]
# Itebero: (/Walikale) doesn't look included in walikale or pinga                                 nkivu[health_zone %in% c("walikale", "itebero", "kibua", "pinga")]  
# Kibua: (/Walikale)  doesn't look included in walikale or pinga                                  nkivu[health_zone %in% c("walikale", "itebero", "kibua", "pinga")]
# Nyiragongo: (/Nyiragongo)                                                                       nkivu[health_zone %in% c("nyiragongo", "karisimbi", "goma")]
# Mabalako: looks like it could be included in Beni Oicha 
# Kamango: looks like it could have been included in Beni or Oicha Katwa Mutwanga
# Kalungata: ??


# ----------------------------------------------  




