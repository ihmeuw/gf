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
library(stringr)
library(ggplot2)
library(lme4)

AccessGT <- raster("PCE/Covariates and Other Data/Geospatial Covariates/Access/access2_mean_synoptic.tif")
AirQGT_2015 <- raster("PCE/Covariates and Other Data/Geospatial Covariates/Air Quality/ihmepm25_mean_1y_2015_00_00.tif")
WorldPopGT2015 <- raster("./DATOS/WorldPop/GTM_ppp_v2b_2015/GTM_ppp_v2b_2015.tif")

gtmMunisIGN = readOGR("./PCE/Outcome Measurement Data/GIS/GT-IGN-cartografia_basica-Division politica Administrativa (Municipios).geojson", encoding="utf-8")


# The following instructions have been run to generate the csv files with the indicators aggregated by municipality.
# Calculate the mean value per municipality from the raster files:
#
#       gtmMunisData = copy(gtmMunisIGN@data[!(gtmMunisIGN$COD_DEPT__ %in% c(NA)) & !(gtmMunisIGN$COD_MUNI__ == 0), c("COD_MUNI__")])
#
#       AirQGT_2015_Muni = extract(AirQGT_2015, gtmMunisIGN[!(gtmMunisIGN$COD_DEPT__ %in% c(NA)) & !(gtmMunisIGN$COD_MUNI__ == 0),], fun = mean, na.rm = TRUE )
#       RoadAccess_Muni = extract(AccessGT, gtmMunisIGN[!(gtmMunisIGN$COD_DEPT__ %in% c(NA)) & !(gtmMunisIGN$COD_MUNI__ == 0),], fun = mean, na.rm = TRUE )
#
# Exploring the raster data files: 
#       plot(AirQGT_2015, xlim = c(-92.7,-88), ylim= c(13.5,18), col=rainbow(255, start = 0.15, end=1))
#       lAccessGT = log10(AccessGT)
#       plot(AccessGT, xlim = c(-92.7,-88), ylim= c(13.5,18), col=rainbow(255, start = 0.10, end=0.75))
#       plot(gtmMunisIGN, add = TRUE)

# Attach data to municipalities dataset:
#       gtmMunisData$PM25Mean = AirQGT_2015_Muni
#       write.csv(gtmMunisData[, c("COD_MUNI__", "PM25Mean")], "./PCE/Covariates and Other Data/Geospatial Covariates/Air Quality/GTM_Municipality_PM25.csv")
#       gtmMunisData$Access = RoadAccess_Muni
#       write.csv(gtmMunisData[, c("COD_MUNI__", "Access")], "./PCE/Covariates and Other Data/Geospatial Covariates/Access/GTM_AccessMean.csv")

<<<<<<< HEAD
gtmMuniAccess   = data.table(read.csv("./PCE/Covariates and Other Data/Geospatial Covariates/Access/GTM_AccessMean.csv"))
gtmMuniPM25     = data.table(read.csv("./PCE/Covariates and Other Data/Geospatial Covariates/Air Quality/GTM_Municipality_PM25.csv"))
=======
gtmMuniAccess   = read.csv("./PCE/Covariates and Other Data/Geospatial Covariates/Access/GTM_AccessMean.csv")
gtmMuniPM25     = read.csv("./PCE/Covariates and Other Data/Geospatial Covariates/Air Quality/GTM_Municipality_PM25.csv")
>>>>>>> c663dc0437fa5faac512f1807389f0b6c43718fb
gtmTxSeeking    = read.csv("./PCE/Outcome Measurement Data/MULTI/ENSMI/GT - cough and fever tx seeking.csv")
gtmTBMuniMonthC = read.csv("./PCE/Outcome Measurement Data/TUBERCULOSIS/MunicipalityMonth.csv")

gtmMunisIGN = merge(gtmMunisIGN, gtmTxSeeking[, c("municode", "cough_wm", "seektx_wm")], by.x = "COD_MUNI__", by.y = "municode")

# Exploring the relations between treatment seeking and cases notifications.
# Group by department

TBCasesDept = aggregate(NInitiating ~ str_sub(COD_MUNI, 0, -3), gtmTBMuniMonthC[floor(gtmTBMuniMonthC$YearMonth/100) == 2015,], sum )
names(TBCasesDept) = c("dept", "ncases")
TxsDept = aggregate(gtmTxSeeking[,c("seektx_wm","cough_wm")], by = list(floor(gtmTxSeeking$municode/100)), sum)
TxsDept$txseek = TxsDept$seektx_wm/TxsDept$cough_wm
names(TxsDept) = c("dept", "seektx_wm", "cough_wm", "txseek")

<<<<<<< HEAD
plotDataDept = merge(TBCasesDept, TxsDept, by="dept")
ggplot(plotDataDept, aes(x = log(ncases), y = log(txseek/(1-txseek)) )) + geom_point()
=======
plotData = merge(TBCasesDept, TxsDept, by="dept")
ggplot(plotData, aes(x = log(ncases), y = log(txseek/(1-txseek)) )) + geom_point()
>>>>>>> c663dc0437fa5faac512f1807389f0b6c43718fb

# Now group by muni

TBCasesMuni = aggregate(NInitiating ~ COD_MUNI, gtmTBMuniMonthC[floor(gtmTBMuniMonthC$YearMonth/100) == 2015,], sum)
names(TBCasesMuni) = c("muni", "ncases")
TxSMuni = aggregate(gtmTxSeeking$seektx_wm / gtmTxSeeking$cough_wm, by = list(gtmTxSeeking$municode), mean, na.rm = TRUE)
names(TxSMuni) = c("muni", "txseek")

plotDataMuni = merge(TBCasesMuni, TxSMuni, by="muni")
ggplot(plotDataMuni, aes(x = log(ncases), y = log(txseek/(1-txseek)) )) + geom_point()

<<<<<<< HEAD
# The adjustment model by municipality

modelDataMuni = merge(plotDataMuni, gtmMuniPM25[,c("COD_MUNI__", "PM25Mean")], by.x = "muni", by.y = "COD_MUNI__")
modelDataMuni = merge(modelDataMuni, gtmMuniAccess[,c("COD_MUNI__", "Access")], by.x = "muni", by.y = "COD_MUNI__")
modelDataMuni$PM25Mean = (modelDataMuni$PM25Mean-mean(modelDataMuni$PM25Mean)) / sd(modelDataMuni$PM25Mean)
modelDataMuni$Access = (modelDataMuni$Access-mean(modelDataMuni$Access)) / sd(modelDataMuni$Access)
modelDataMuni$dept = floor(modelDataMuni$muni/100)
modelMuni = glmer(ncases ~ 1 + (1|muni) + (1+txseek|dept) + txseek + PM25Mean + Access, data = modelDataMuni, family = poisson(link="log"))
summary(modelMuni)

# The adjustment model by department
gtmDeptPM25 = aggregate(gtmMuniPM25[,c("COD_MUNI__", "PM25Mean")], by = list(floor()), FUN = )
modelDataDept = merge(plotDataDept, gtmMuniPM25[, .(PM25Mean = mean(PM25Mean)), by = .(dept = floor(COD_MUNI__/100) )], 
                      by.x = "dept", by.y = "dept")
modelDataDept = merge(modelDataDept, gtmMuniAccess[,.(Access = mean(Access)), by = .(dept = floor(COD_MUNI__/100) )], 
                      by.x = "dept", by.y = "dept")
modelDataDept = data.table(modelDataDept)
modelDataDept[, PM25Mean := (PM25Mean-mean(PM25Mean)) / sd(PM25Mean)]
modelDataDept[, Access := (Access-mean(Access)) / sd(Access)]
modelDept = glmer(ncases ~ 1 + (1|dept) + txseek + PM25Mean + Access, data = modelDataDept, family = poisson(link="log"))
summary(modelDept)
=======

modelDataMuni = merge(plotDataMuni, gtmMuniPM25[,c("COD_MUNI__", "PM25Mean")], by.x = "muni", by.y = "COD_MUNI__")
modelDataMuni = merge(modelDataMuni, gtmMuniAccess[,c("COD_MUNI__", "Access")], by.x = "muni", by.y = "COD_MUNI__")
glmer()
>>>>>>> c663dc0437fa5faac512f1807389f0b6c43718fb
