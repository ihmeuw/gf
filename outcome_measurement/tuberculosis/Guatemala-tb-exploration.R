# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2017-11-14
# Explore TB outcome data from MOH notifications databases.


# ----------------------------------------------
# Dependencies:
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(zoo)
library(ggplot2)
library(gridExtra)
library(rgdal)
library(broom)
library(stringdist)
# ----------------------------------------------
# Configure script
saveGraphs = T
# ----------------------------------------------
# Read the data:
TBNotif2014 = read_excel("PCE/Outcome Measurement Data/TUBERCULOSIS/NOTIFICACIONES 2014 GENERAL anterior.xlsx", sheet = 2, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
TBNotif2014 = data.table(TBNotif2014)[3:.N,][!is.na(X3),]
names(TBNotif2014) = c( "NOMBRES", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD", 
                        "RANGOEDAD", "PESOLBS", "PESOKG", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX",
                        "CLASIFICACION", "TIPODETBPEDIATRICOS", "VIH", "FECHAPRUEBAVIH", "ESQUEMA", "CONTACTOS", 
                        "OTRASPATOLOGIAS", "CONDICIONEGRESO", "FECHAMUERTE", "CAUSADEMUERTE", 
                        "PACIENTEPRIVADOLIBERTAD", "DEPORTADO")

TBNotif2015 = read_excel("PCE/Outcome Measurement Data/TUBERCULOSIS/NOTIFICACIONES 2015.xlsx", sheet = 2, col_names = F)
TBNotif2015 = data.table(TBNotif2015)[3:.N,][!is.na(X3),]
names(TBNotif2015) = c("NOMBRES", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD",
                       "RANGOEDAD", "PESOLBS", "PESOKG", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX", 
                       "CLASIFICACION", "LOCALIZACIONTB", "METODODX", "VIH", "FECHAPRUEBAVIH", "ESQUEMA", "CONTACTOS",
                       "CONTACTO000_014", "CONTACTO_MAYORA_015", "CASOINDICE", "DESARROLLOTBCLASIFICACION", 
                       "OTRASPATOLOGIAS", "EMPLEADOMSPAS", "UNIDADDX", "FALLECIDOS", "FECHAMUERTE", "CAUSADEMUERTE", 
                       "PACIENTEPRIVADOLIBERTAD")
gtmMunisIGN = readOGR("PCE/Outcome Measurement Data/GIS/GT-IGN-cartografia_basica-Division politica Administrativa (Municipios).geojson")
munisPob = read.csv("PCE/Outcome Measurement Data/Covariates/Demographics/Guatemala_Municipios_IGN2017_worldpop2015.csv", encoding = "UTF-8")
deptoIndgnProp = read.csv("PCE/Outcome Measurement Data/Covariates/Demographics/Guatemala_indigenousPobProp.csv")
# Readxl has serious issues with guessing data types and handling empty cells around the document. 
# Thus, the easiest way to load a bad excel, such as these, is to load everything without type 
# conversion and columns definitions.

# ----------------------------------------------
# Helper function to get municipality and department codes
deptos                = unique(munisPob[, c("DEPTO__", "COD_DEPT__") ])
vocalesTildes         = c("á"="a", "é"="e", "í"="i", "ó"= "o", "ú"="u")
deptos$DEPTO__        = str_replace_all(str_to_lower(deptos$DEPTO__), vocalesTildes)
munisPob$lookupNombre = str_replace_all(str_to_lower(munisPob$NOMBRE__), vocalesTildes)
getMuniCodeByName <- function (nombreMuni, nombreDepto, field = "COD_MUNI__") {
    nombreMuni  = str_replace_all(str_to_lower(nombreMuni), vocalesTildes)
    nombreDepto = str_replace_all( str_to_lower(nombreDepto), vocalesTildes)
    depto       = deptos[which.min(stringdist(nombreDepto, deptos$DEPTO__, method = "cosine")),]
    deptoMunis  = munisPob[munisPob$COD_DEPT__ == depto$COD_DEPT__,]
    muni        = deptoMunis[which.min(stringdist(nombreMuni, deptoMunis$lookupNombre, method = "cosine")), field]
    muni
}
# Call the function over the dataset to get municipality codes:

TBNotifAll[, COD_MUNI := getMuniCodeByName(MUNICIPIO, DEPARTAMENTO), by=1:nrow(TBNotifAll)]

# ----------------------------------------------
# Prepare the data
TBNotifAll = rbindlist(list(TBNotif2014, TBNotif2015), fill = TRUE)
TBNotifAll$NotificationDate = as.Date(as.numeric(TBNotifAll$FECHANOTIFICACION)-2, origin="1900-01-01")
TBNotifAll$YearMonth = as.numeric(format(TBNotifAll$NotificationDate, "%Y%m"));
TBNotifAll = TBNotifAll[YearMonth>=201401 & YearMonth<=201512]
TBNotifAll$RANGOEDAD = trimws(TBNotifAll$RANGOEDAD)
TBNotifAll$SEXO = str_to_lower(TBNotifAll$SEXO)
TBNotifAll$VIH = str_to_lower(trimws(TBNotifAll$VIH))
TBNotifAll$VIH = factor(TBNotifAll$VIH, level=c("nr", "r"), labels=c("Not Reactive", "Reactive"))

# TBNotifAll$RANGOEDAD = factor(TBNotifAll$RANGOEDAD, levels = order(unique(TBNotifAll$RANGOEDAD)))
# ----------------------------------------------
# Exploring
# All plots group by gender
# Plot monthly count time series
ggplot(data = TBNotifAll, aes(factor(YearMonth), fill=SEXO))+ geom_bar(position="dodge")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Month", y="Notifications counts per month", title = "Time series of notifications")
if (saveGraphs) 
    ggsave("TB_Gt_Notifications_2014-2015-Monthly.png")
# Bar plot by age range
ggplot(TBNotifAll, aes(x=RANGOEDAD, fill=SEXO))+geom_bar(position="dodge")  + theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Age range", y="Notifications counts per Age range", title="Age range")
if (saveGraphs) 
    ggsave("TB_Gt_Notifications_2014-2015-AgeRange.png")
# Bar plot by weight in pounds (some KG data is just missing, so pounds will be preferred)
# This is only for adults
ggplot(TBNotifAll[(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO))+geom_histogram(bins=20, position="dodge") + labs(fill="Gender", x="Weight (Pounds)", y="Notifications counts per month", title="By weight for children < 10 years old")
if (saveGraphs) 
    ggsave("TB_Gt_Notifications_2014-2015-Weight_NoKids.png")
# This is for childs from 0 to 10 years old
ggplot(TBNotifAll[!(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO ))+geom_histogram(bins=20, position="dodge")  + labs(fill="Gender", x="Weight (Pounds)", y="Notifications count per month", title="By weight for people > 10 years old")
if (saveGraphs) 
    ggsave("TB_Gt_Notifications_2014-2015-WeightKids0-10.png")
# Diagnostic methods:
ggplot(as.data.frame(table(TBNotifAll$METODODX, TBNotifAll$SEXO, useNA = "always")), aes(x=reorder(Var1, Freq), y=Freq, fill=Var2)) + 
    geom_bar(stat="identity", position="dodge") + theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))  + labs(fill="Gender", x="Diagnosis methods", y="Notifications counts per month", title="Diagnosis methods")
if (saveGraphs) 
    ggsave("TB_Gt_Notifications_2014-2015-Dx_method.png")
# ----------------------------------------------
# VIH plots
# Monthly time series of notifications (separate by HIV and gender)
ggplot(data = TBNotifAll, aes(factor(YearMonth), fill=SEXO))+ geom_bar(position="dodge")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Month", y="Notifications counts per month", title = "Time series of notifications") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("TB_Gt_Notifications_2014-2015-Monthly-VIH.png", height=4, width=9)
# Bar plot by age range
ggplot(TBNotifAll, aes(x=RANGOEDAD, fill=SEXO))+geom_bar(position="dodge")  + theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Age range", y="Notifications counts per Age range", title="Age range") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("TB_Gt_Notifications_2014-2015-AgeRange-VIH.png", height=4, width=9)
# Bar plot by weight in pounds (some KG data is just missing, so pounds will be preferred)
# This is only for adults
ggplot(TBNotifAll[(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO))+geom_histogram(bins=20, position="dodge") + labs(fill="Gender", x="Weight (Pounds)", y="Notifications counts per month", title="By weight for children < 10 years old") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("TB_Gt_Notifications_2014-2015-Weight_NoKids-VIH.png", height=4, width=9)
# This is for childs from 0 to 10 years old
ggplot(TBNotifAll[!(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO ))+geom_histogram(bins=20, position="dodge")  + labs(fill="Gender", x="Weight (Pounds)", y="Notifications count per month", title="By weight for people > 10 years old") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("TB_Gt_Notifications_2014-2015-WeightKids0-10-VIH.png", height=4, width=9)
# Diagnostic methods:
ggplot(as.data.frame(table(TBNotifAll$METODODX, TBNotifAll$SEXO, TBNotifAll$VIH, useNA = "always")), aes(x=reorder(Var1, Freq), y=Freq, fill=Var2)) + 
    geom_bar(stat="identity", position="dodge") + theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2, size = 5))  + labs(fill="Gender", x="Diagnosis methods", y="Notifications counts per month", title="Diagnosis methods") + facet_wrap(~ Var3, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("TB_Gt_Notifications_2014-2015-Dx_method-VIH.png", height=4, width=9)

# ----------------------------------------------
gtmMunisIGN@data$id = rownames(gtmMunisIGN@data)
gtmMunisIGN@data = merge(gtmMunisIGN@data, TBNotifAll[, .(TBCases=.N),by=COD_MUNI], by.x = "COD_MUNI__", by.y="COD_MUNI", all.x=TRUE, sort=FALSE)
gtmMunisIGN.df = fortify(gtmMunisIGN)
gtmMunisIGN.df = merge(gtmMunisIGN.df, gtmMunisIGN, by="id", sort=FALSE)
ggplot(data = gtmMunisIGN.df, aes(x=long, y=lat, group=group, fill=TBCases) ) + geom_polygon(colour = rgb(1,1,1,0.5)) + coord_quickmap() +   scale_fill_gradientn(colours = c("#444444", "#ddcc22", "#DD5522", "#AA1111"), values=c(0,0.005,0.5,1), trans="log10")
