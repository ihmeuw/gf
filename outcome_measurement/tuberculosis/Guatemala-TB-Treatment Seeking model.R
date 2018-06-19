# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2018-06-12

# Treatment Seeking Model for Tuberculosis.

# ----------------------------------------------
# Dependencies

library(rgdal)
library(raster)
library(sp)
library(colorRamps)
library(ggmap)
library(ggplot2)
library(haven)
library(data.table)

# ----------------------------------------------
# Load worldpop population estimations for guatemala in 2015

AccessGT <- raster("PCE/Covariates and Other Data/Geospatial Covariates/Access/access2_mean_synoptic.tif")
AirQGT_2015 <- raster("PCE/Covariates and Other Data/Geospatial Covariates/Air Quality/ihmepm25_mean_1y_2015_00_00.tif")
WorldPopGT2015 <- raster("./DATOS/WorldPop/GTM_ppp_v2b_2015/GTM_ppp_v2b_2015.tif")

# The municipalities shape file obtained from SEGEPLAN website is most probably outdated. I've used a shapefile in geojson format obtained from IGN (national geographic institute of Guatemala) which seems to be more up to date. This file was retrieve in mid 2017 and has been uploaded to the internet archive so it is accesible.
# This file can be found in the archive: https://archive.org/download/IGNCartografiaBasicaDivisionPoliticaAdministrativaMunicipios/IGN-cartografia_basica-Division%20politica%20Administrativa%20%28Municipios%29.geojson
gtmMunisIGN = readOGR("./PCE/Outcome Measurement Data/GIS/GT-IGN-cartografia_basica-Division politica Administrativa (Municipios).geojson", encoding="utf-8")

AirQGT_2015_Muni = extract(AirQGT_2015, gtmMunisIGN[!(gtmMunisIGN$COD_DEPT__ %in% c(NA)) & !(gtmMunisIGN$COD_MUNI__ == 0),], fun = mean, na.rm = TRUE )

RoadAccess_Muni = extract(AccessGT, gtmMunisIGN[!(gtmMunisIGN$COD_DEPT__ %in% c(NA)) & !(gtmMunisIGN$COD_MUNI__ == 0),], fun = mean, na.rm = TRUE )

# Exploring the raster data files: 
# plot(AirQGT_2015, xlim = c(-92.7,-88), ylim= c(13.5,18), col=rainbow(255, start = 0.1, end=1))
# lAccessGT = log10(AccessGT)
# plot(AccessGT, xlim = c(-92.7,-88), ylim= c(13.5,18), col=rainbow(255, start = 0.10, end=0.75))
# plot(gtmMunisIGN, add = TRUE)
