# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2018-01-18
# Load demographic data

# ----Dependencies------------------------------------------
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
library(stringdist)
library(haven)

# ----Configure--------------------------------------------
dataPath = "CIESAR/PCE/"
extDataPath = "CIESAR/DATOS/"
codePath = "CIESAR/PCE/gf/"

# ----Load data--------------------------------------------

# Deaths from 2009 to 2015
# TODO: add 2016 and 2017.
defsData = c()
for (year in seq(2009, 2015, 1)) {
  defsData[[year]] = data.table(read_sav(paste0(extDataPath, "GTVitales y Censo/Defunciones ", year, ".sav")))
  defsData[[year]]$CaudefPRE = str_sub(defsData[[year]]$Caudef, 1, 3) 
  print(year)
}

# ----Load data--------------------------------------------

# Private hospital services from 2009 to 2015
# TODO: add 2016 and 2017.
privHospIData = c()
for (year in seq(2012, 2015, 1)) {
  privHospIData[[year]] = data.table(read_sav(paste0(extDataPath, "ServHospitalariosPrivados/ServHosptlPrivInternos-", year, ".sav")))
  privHospIData[[year]]$CAUFINPRE = str_sub(privHospIData[[year]]$CAUFIN, 1, 3) 
  print(year)
}

# Municipalities and departments GIS geometries
gtmMunisIGN = readOGR(paste0(dataPath, "Covariates and Other Data/GIS/GT-IGN-cartografia_basica-Division politica Administrativa (Municipios).geojson"))

gtmDeptosIGN = readOGR(paste0(dataPath, "Outcome Measurement Data/GIS/IGN-cartografia_basica-Departamentos.geojson", encoding = "UTF-8"))
gtmDeptosIGN@data$CODIGO = floor(as.numeric(as.character(gtmDeptosIGN@data$CODIGO))/100)

# Municipalities data with population estimates for 2015.
munisGT = read.csv(paste0(dataFilePrefix, "Covariates and Other Data/Demographics/Guatemala_Municipios_IGN2017_worldpop2015.csv"), encoding = "UTF-8")
dt.munisGT = data.table(munisGT)
