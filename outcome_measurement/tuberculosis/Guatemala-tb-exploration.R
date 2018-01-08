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
gtmDeptosIGN = readOGR("PCE/Outcome Measurement Data/GIS/IGN-cartografia_basica-Departamentos.geojson", encoding = "UTF-8")
gtmDeptosIGN@data$CODIGO = floor(as.numeric(as.character(gtmDeptosIGN@data$CODIGO))/100)
munisPob = read.csv("PCE/Outcome Measurement Data/Covariates/Demographics/Guatemala_Municipios_IGN2017_worldpop2015.csv", encoding = "UTF-8")
munisPob = data.table(munisPob)
deptoIndgnProp = read.csv("PCE/Outcome Measurement Data/Covariates/Demographics/Guatemala_indigenousPobProp.csv")
deptoIndgnProp = data.table(deptoIndgnProp)
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
    if (is.na(nombreMuni)) {
        print(paste("Found an NA municipality in", nombreDepto))
        nombreMuni = nombreDepto
    }
    nombreMuni  = str_replace_all(str_to_lower(nombreMuni), vocalesTildes)
    nombreDepto = str_replace_all( str_to_lower(nombreDepto), vocalesTildes)
    depto       = deptos[which.min(stringdist(nombreDepto, deptos$DEPTO__, method = "cosine")),]
    deptoMunis  = munisPob[munisPob$COD_DEPT__ == depto$COD_DEPT__,]
    muni        = deptoMunis[which.min(stringdist(nombreMuni, deptoMunis$lookupNombre, method = "cosine")), field]
    muni
}
# Call the function over the dataset to get municipality codes:

TBNotifAll[, COD_MUNI := getMuniCodeByName(ifelse(is.na(MUNICIPIO), SERVICIODESALUD, MUNICIPIO), DEPARTAMENTO), by=1:nrow(TBNotifAll)]
TBNotifAll[, COD_DEPTO := floor(COD_MUNI/100)]
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
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Monthly.png")
# Bar plot by age range
ggplot(TBNotifAll, aes(x=RANGOEDAD, fill=SEXO))+geom_bar(position="dodge")  + theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Age range", y="Notifications counts per Age range", title="Age range")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-AgeRange.png")
# Bar plot by weight in pounds (some KG data is just missing, so pounds will be preferred)
# This is only for adults
ggplot(TBNotifAll[(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO))+geom_histogram(bins=20, position="dodge") + labs(fill="Gender", x="Weight (Pounds)", y="Notifications counts per month", title="By weight for children < 10 years old")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Weight_NoKids.png")
# This is for childs from 0 to 10 years old
ggplot(TBNotifAll[!(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO ))+geom_histogram(bins=20, position="dodge")  + labs(fill="Gender", x="Weight (Pounds)", y="Notifications count per month", title="By weight for people > 10 years old")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-WeightKids0-10.png")
# Diagnostic methods:
ggplot(as.data.frame(table(TBNotifAll$METODODX, TBNotifAll$SEXO, useNA = "always")), aes(x=reorder(Var1, Freq), y=Freq, fill=Var2)) + 
    geom_bar(stat="identity", position="dodge") + theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))  + labs(fill="Gender", x="Diagnosis methods", y="Notifications counts per month", title="Diagnosis methods")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Dx_method.png")
# ----------------------------------------------
# VIH plots
# Monthly time series of notifications (separate by HIV and gender)
ggplot(data = TBNotifAll, aes(factor(YearMonth), fill=SEXO))+ geom_bar(position="dodge")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Month", y="Notifications counts per month", title = "Time series of notifications") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Monthly-VIH.png", height=4, width=9)
# Bar plot by age range
ggplot(TBNotifAll, aes(x=RANGOEDAD, fill=SEXO))+geom_bar(position="dodge")  + theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Age range", y="Notifications counts per Age range", title="Age range") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-AgeRange-VIH.png", height=4, width=9)
# Bar plot by weight in pounds (some KG data is just missing, so pounds will be preferred)
# This is only for adults
ggplot(TBNotifAll[(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO))+geom_histogram(bins=20, position="dodge") + labs(fill="Gender", x="Weight (Pounds)", y="Notifications counts per month", title="By weight for children < 10 years old") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Weight_NoKids-VIH.png", height=4, width=9)
# This is for childs from 0 to 10 years old
ggplot(TBNotifAll[!(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO ))+geom_histogram(bins=20, position="dodge")  + labs(fill="Gender", x="Weight (Pounds)", y="Notifications count per month", title="By weight for people > 10 years old") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-WeightKids0-10-VIH.png", height=4, width=9)
# Diagnostic methods:
ggplot(as.data.frame(table(TBNotifAll$METODODX, TBNotifAll$SEXO, TBNotifAll$VIH, useNA = "always")), aes(x=reorder(Var1, Freq), y=Freq, fill=Var2)) + 
    geom_bar(stat="identity", position="dodge") + theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2, size = 5))  + labs(fill="Gender", x="Diagnosis methods", y="Notifications counts per month", title="Diagnosis methods") + facet_wrap(~ Var3, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Dx_method-VIH.png", height=4, width=9)

# ----------------------------------------------
# Prepare GIS data. R has a very ugly way of handling this:
gtmMunisIGN@data$id = rownames(gtmMunisIGN@data)
gtmMunisIGN@data = merge(gtmMunisIGN@data, TBNotifAll[, .(TBCases=.N),by=COD_MUNI], by.x = "COD_MUNI__", by.y="COD_MUNI", all.x=TRUE, sort=FALSE)
gtmMunisIGN.map.df = fortify(gtmMunisIGN)
gtmDeptosIGN.map.df = fortify(gtmDeptosIGN)


# Plotting TB cases per municipality
plot = ggplot(data=gtmMunisIGN@data, aes(fill=TBCases)) + geom_map(aes(map_id=id), colour = rgb(1,1,1,0.5), map = gtmMunisIGN.map.df) + expand_limits(x = gtmMunisIGN.map.df$long, y = gtmMunisIGN.map.df$lat) + coord_quickmap() + scale_fill_gradientn(colours = c("#444444", "#ddcc22", "#DD5522", "#AA1111"), values=c(0,0.005,0.7,1), trans="log10") + labs(fill= "Number of cases", title="TB cases by municipality")
# Overlay the departments
plot + geom_polygon(data = gtmDeptosIGN.map.df, aes(long, lat, group=group), fill="#00000000", color="#00000066", size=1)

if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Map-Muncipalities.png", height=8, width=8)

# Indigenous population proportion by department
gtmDeptosIGN@data$id = rownames(gtmDeptosIGN@data)
gtmDeptosIGN@data = merge(gtmDeptosIGN@data, deptoIndgnProp[, .(INDGN, DEPTO)], by.x = "CODIGO", by.y="DEPTO", all.x=TRUE, sort=FALSE)

gtmDeptosIGN.map.df = fortify(gtmDeptosIGN)

ggplot(data = gtmDeptosIGN@data, aes(fill= 100*INDGN)) + geom_map(aes(map_id=id), colour = "#FFFFFF88", map=gtmDeptosIGN.map.df) + expand_limits(x = gtmDeptosIGN.map.df$long, y = gtmDeptosIGN.map.df$lat) + coord_quickmap() + labs(fill="Indigenous population", title="Indigenous population proportion by department")

if (saveGraphs) 
    ggsave("PCE/Graficas/GT_Indigenous population by Depto.png", height=8, width=8)

# Relation between indigenous population proportion and TB prevalence
deptoData = merge(TBNotifAll[, .(TB=.N) ,by=COD_DEPTO], deptoIndgnProp[, .(DEPTO,INDGN)], by.x="COD_DEPTO", by.y="DEPTO")
deptoData = merge(deptoData, munisPob[,.(Pob = sum(Poblacion, na.rm=TRUE)), by=COD_DEPT__], by.x="COD_DEPTO", by.y="COD_DEPT__")
ggplot(data = deptoData, aes(y=log(TB/Pob), x=log(INDGN/(1-INDGN)))) + geom_point() + geom_smooth(method=lm)  + labs(title="Indigenous population proportion vrs TB Cases per department", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="[Logit] Indigenous population proportion")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-IndgnPop vs Incidence by Depto.png", height=8, width=8)
# Histogram of both variables with log and logit transformations
grid.arrange(
    ggplot(deptoData, aes(INDGN)) + geom_histogram(bins = 10),
    ggplot(deptoData, aes(log(INDGN))) + geom_histogram(bins = 10),
    ggplot(deptoData, aes(log(INDGN/(1-INDGN)))) + geom_histogram(bins = 10),
    ggplot(deptoData, aes(TB/Pob)) + geom_histogram(bins = 10),
    ggplot(deptoData, aes(log(TB/Pob))) + geom_histogram(bins = 10),
    ncol=3, nrow=2
) 

if (saveGraphs) { 
    g = arrangeGrob(
        ggplot(deptoData, aes(INDGN)) + geom_histogram(bins = 10),
        ggplot(deptoData, aes(log(INDGN))) + geom_histogram(bins = 10),
        ggplot(deptoData, aes(log(INDGN/(1-INDGN)))) + geom_histogram(bins = 10),
        ggplot(deptoData, aes(TB/Pob)) + geom_histogram(bins = 10),
        ggplot(deptoData, aes(log(TB/Pob))) + geom_histogram(bins = 10),
        ncol=3, nrow=2
    ) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-IndgnPop,Incidence Histograms.png", height=8, width=8, g)
}

# Fitting the linear model
fit_TBIndg = lm(formula = log(TB/Pob) ~ log(INDGN/(1-INDGN) ), data = deptoData)
summary(fit_TBIndg)
