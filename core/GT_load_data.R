# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2018-01-18
# Load demographic data

# ----Dependencies------------------------------------------
library(data.table)
library(reshape2)
library(stringr)
library(rgdal)
library(ggplot2)
library(stringdist)
library(haven)

# ----Configure--------------------------------------------

#Changed this to run with a explicitly set boolean. EKL 3/18/19
#Boolean logic switch: 
# Set at_ciesar = 1 if you want CIESAR's local filepaths. 
at_ciesar = FALSE

if (at_ciesar==TRUE) { 
	dataPath = "PCE/Covariates and Other Data/GIS/"
	codePath = "PCE/gf/"
} else { 
	dataPath = "J:/Project/Evaluation/GF/mapping/gtm/"
	codePath = "C:/local/gf/"
}
extDataPath = "DATOS/"

# ----Load data--------------------------------------------

# Deaths from 2009 to 2015
# TODO: add 2016 and 2017.
loadDeathsData <- function() {
    defsData = c()
    for (year in seq(2009, 2016, 1)) {
      defsData[[year]] = data.table(read_sav(paste0(extDataPath, "GTVitales y Censo/Defunciones ", year, ".sav")))
      defsData[[year]]$CaudefPRE = str_sub(defsData[[year]]$Caudef, 1, 3) 
      print(year)
    }
    defsData
}
# ----Load data--------------------------------------------

# Private hospital internal services from 2009 to 2015
# TODO: add 2016 and 2017.
loadPrivateHospitalAdmsData <- function() {
    privHospIData = c()
    for (year in seq(2009, 2016, 1)) {
      privHospIData[[year]] = data.table(read_sav(paste0(extDataPath, "ServHospitalariosPrivados/ServHosptlPrivInternos-", year, ".sav")))
      privHospIData[[year]]$CAUFINPRE = str_sub(privHospIData[[year]]$CAUFIN, 1, 3) 
      print(year)
    }
    privHospIData
}


# Municipalities and departments GIS geometries
gtmMunisIGN = readOGR(paste0(dataPath, "GT-IGN-cartografia_basica-Division politica Administrativa (Municipios).geojson"))
gtmMunisIGN = gtmMunisIGN[!(gtmMunisIGN$COD_MUNI__ %in% c("2000")), ]

gtmDeptosIGN = readOGR(paste0(dataPath, "GT-IGN-cartografia_basica-Departamentos.geojson"), encoding = "UTF-8")
gtmDeptosIGN@data$CODIGO = floor(as.numeric(as.character(gtmDeptosIGN@data$CODIGO))/100)
gtmDeptosIGN = gtmDeptosIGN[gtmDeptosIGN$CODIGO != 23,]
# Municipalities data with population estimates for 2015.
if (at_ciesar == TRUE) munisGT = read.csv(paste0(dataPath, "../Demographics/Guatemala_Municipios_IGN2017_worldpop2010-2012-2015.csv"), encoding = "UTF-8")
if (at_ciesar== FALSE) munisGT = read.csv(paste0(dataPath, "/Guatemala_Municipios_IGN2017_worldpop2010-2012-2015.csv"), encoding = "UTF-8")
dt.munisGT = data.table(munisGT)[, 
                                 .(NOMBRE__ = first(NOMBRE__), COD_MUNI__, first(COD_DEPT__),
                                 AREA_KM__ = sum(AREA_KM__), Poblacion2010 = sum(Poblacion2010), 
                                 Poblacion2012 = sum(Poblacion2012), Poblacion2015 = sum(Poblacion2015)) ,
                                 by=COD_MUNI__]
