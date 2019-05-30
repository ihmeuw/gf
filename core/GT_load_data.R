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
<<<<<<< HEAD
if (!'at_ciesar' %in% ls()) at_ciesar = 1 # parameter for who's running the code
if (at_ciesar) { 
  gisDataPath = "PCE/Covariates and Other Data/GIS/"
  codePath = "PCE/gf/"
=======

#Changed this to run with a explicitly set boolean. EKL 3/18/19
#Boolean logic switch: 
# Set at_ciesar = 1 if you want CIESAR's local filepaths. 
at_ciesar = FALSE

if (at_ciesar==TRUE) { 
	dataPath = "PCE/Covariates and Other Data/GIS/"
	codePath = "PCE/gf/"
>>>>>>> 783e6be280284ae2336345e384f4b2fe2ddb4434
} else { 
  gisDataPath = "J:/Project/Evaluation/GF/mapping/gtm/"
  codePath = "C:/local/gf/"
  covariates = "J:/Project/Evaluation/GF/mapping/gtm/geospatial covariates/"
  #demographics = "
  
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
gtmMunisIGN = readOGR(paste0(gisDataPath, "GT-IGN-cartografia_basica-Division politica Administrativa (Municipios).geojson"))
gtmMunisIGN = gtmMunisIGN[!(gtmMunisIGN$COD_MUNI__ %in% c("2000")), ]

gtmDeptosIGN = readOGR(paste0(gisDataPath, "GT-IGN-cartografia_basica-Departamentos.geojson"), encoding = "UTF-8")
gtmDeptosIGN@data$CODIGO = floor(as.numeric(as.character(gtmDeptosIGN@data$CODIGO))/100)
gtmDeptosIGN = gtmDeptosIGN[gtmDeptosIGN$CODIGO != 23,]
# Municipalities data with population estimates for 2015.
<<<<<<< HEAD

if (at_ciesar) munisGT = read.csv(paste0(gisDataPath, "../Demographics/Guatemala_Municipios_IGN2017_worldpop2010-2012-2015.csv"), encoding = "UTF-8")
if (!at_ciesar) munisGT = read.csv(paste0(gisDataPath, "/Guatemala_Municipios_IGN2017_worldpop2010-2012-2015.csv"), encoding = "UTF-8")
# TODO: The following comes from the merge. Should we keep it?
=======
if (at_ciesar == TRUE) munisGT = read.csv(paste0(dataPath, "../Demographics/Guatemala_Municipios_IGN2017_worldpop2010-2012-2015.csv"), encoding = "UTF-8")
if (at_ciesar== FALSE) munisGT = read.csv(paste0(dataPath, "/Guatemala_Municipios_IGN2017_worldpop2010-2012-2015.csv"), encoding = "UTF-8")
>>>>>>> 783e6be280284ae2336345e384f4b2fe2ddb4434
dt.munisGT = data.table(munisGT)[, 
                                 .(NOMBRE__ = first(NOMBRE__), COD_MUNI__, first(COD_DEPT__),
                                   AREA_KM__ = sum(AREA_KM__), Poblacion2010 = sum(Poblacion2010), 
                                   Poblacion2012 = sum(Poblacion2012), Poblacion2015 = sum(Poblacion2015)) ,
                                 by=COD_MUNI__]
munisGT = data.table(munisGT)[, 
                              .(name = first(NOMBRE__), 
                                deptocode = first(COD_DEPT__),
                                deptoname = first(DEPTO__),
                                area = sum(AREA_KM__), Poblacion2010 = sum(Poblacion2010), 
                                Poblacion2012 = sum(Poblacion2012), Poblacion2015 = sum(Poblacion2015)) ,
                              by=.(municode = COD_MUNI__)] 

# Some municipalities have been created since 2009.
#    Huehuetenango (13) Petatán (1333) in 2015 was splitted from Concepción Huista (1322)
#    Escuintla (5) Sipacate (514) in 2015 was splitted from La Gomera (507)
#    Suchitepequez (10) San José La Máquina (1021) in 2014 was splitted from Cuyotenango (1002)
#    Petén (17) Las Cruces (1713) in 2011 was splitted from La Libertad (1705)
#    Petén (17) El Chal (1714) in 2014 was splitted from Dolores (1708)
#    Zacapa (19) San Jorge (1911) in 2014 was splitted from Zacapa (1901)
#    San Marcos (12) La Blanca (1230) in 2014 was splitted from Ocós (1218)
splitted_municipalities = data.table(
  parent_code = c(1322,507,1002,1705,1708,1901,1218),
  new_code =     c(1333,514,1021,1713,1714,1911,1230),
  year_of_split = c(2015,2015,2014,2011,2014,2014,2014)
)

munisGT = data.table(merge(munisGT, 
                           splitted_municipalities[,c("new_code", 
                                                      "parent_code")], 
                           by.x = "municode", by.y = "new_code", all=T) )

munisGT[is.na(parent_code), parent_code := municode]
munisGT.2009 = munisGT[,.(name = first(name), deptocode = first(deptocode),
                          area = sum(area), Poblacion2010 = sum(Poblacion2010), 
                          Poblacion2012 = sum(Poblacion2012), Poblacion2015 = sum(Poblacion2015)) ,
                       by= .(municode = parent_code)] 