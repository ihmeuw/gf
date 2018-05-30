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
library(stringdist)

# ----Configure------------------------------------------
saveGraphs = T

source("./PCE/gf/core/GT_helper_functions.R", encoding = "UTF-8")

# ----------------------------------------------
# Read the data:
TBNotif2012 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/NOTIFICACIONES 2012.xlsx"), sheet = 1, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
TBNotif2012 = data.table(TBNotif2012)[3:.N,][!is.na(X3),]
names(TBNotif2012) = c( "NOMBRES", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD", 
                        "RANGOEDAD", "PESOLBS", "PESOKG", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX",
                        "CLASIFICACION", "VIH" , "ESQUEMA", "CONTACTOS", 
                        "OTRASPATOLOGIAS")
TBNotif2012[,YEAR := 2012]

uglyTB2012DateToYearMonth <- function (input) {
    matches1 = str_match(input, "^(\\d\\d)\\.?\\.(\\d?\\d)(\\.?\\.|\\-)\\d{0,2}(\\d\\d)$")
    matches2 = str_match(input, "^(\\d{5})$")
    months1 = matches1[!is.na(matches1[,1]),3]
    input[!is.na(matches1[,1])] = paste0("20",matches1[!is.na(matches1[,1]),5], ifelse(nchar(months1)==1, paste0("0", months1), months1))
    input[!is.na(matches2[,1])] = format(as.Date(as.numeric(input[!is.na(matches2[,1])] )-2, origin="1900-01-01"), format="%Y%m")
    input[(is.na(matches2[,1]) & is.na(matches1[,1]))| as.integer(matches1[,3])>12] = NA
    input
}

TBNotif2012[,YearMonth := uglyTB2012DateToYearMonth(FECHANOTIFICACION)]

TBNotif2013 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/NOTIFICACIONES 2013.xlsx"), sheet = 1, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
TBNotif2013 = data.table(TBNotif2013)[3:.N,][!is.na(X3),]
names(TBNotif2013) = c( "NOMBRES", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD", 
                        "RANGOEDAD", "PESOLBS", "PESOKG", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX",
                        "CLASIFICACION", "VIH", "ESQUEMA", "CONTACTOS", 
                        "OTRASPATOLOGIAS")
TBNotif2013[,YEAR := 2013]

TBNotif2014 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/NOTIFICACIONES 2014 GENERAL anterior.xlsx"), sheet = 2, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
TBNotif2014 = data.table(TBNotif2014)[3:.N,][!is.na(X3),]
names(TBNotif2014) = c( "NOMBRES", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD", 
                        "RANGOEDAD", "PESOLBS", "PESOKG", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX",
                        "CLASIFICACION", "TIPODETBPEDIATRICOS", "VIH", "FECHAPRUEBAVIH", "ESQUEMA", "CONTACTOS", 
                        "OTRASPATOLOGIAS", "CONDICIONEGRESO", "FECHAMUERTE", "CAUSADEMUERTE", 
                        "PACIENTEPRIVADOLIBERTAD", "DEPORTADO")
TBNotif2014[,YEAR := 2014]

TBNotif2015 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/NOTIFICACIONES 2015.xlsx"), sheet = 2, col_names = F)
TBNotif2015 = data.table(TBNotif2015)[3:.N,][!is.na(X3),]
names(TBNotif2015) = c("NOMBRES", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", "SEXO", "EDAD",
                       "RANGOEDAD", "PESOLBS", "PESOKG", "CONDICIONINGRESO", "FECHANOTIFICACION", "FECHAINICIOTX", 
                       "CLASIFICACION", "LOCALIZACIONTB", "METODODX", "VIH", "FECHAPRUEBAVIH", "ESQUEMA", "CONTACTOS",
                       "CONTACTO_000_014", "CONTACTO_MAYORA_015", "CASOINDICE", "DESARROLLOTBCLASIFICACION", 
                       "OTRASPATOLOGIAS", "EMPLEADOMSPAS", "UNIDADDX", "FALLECIDOS", "FECHAMUERTE", "CAUSADEMUERTE", 
                       "PACIENTEPRIVADOLIBERTAD")
TBNotif2015[,YEAR := 2015]

TBNotif2016 = read_excel(paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/NOTIFICACIONES 2016.xlsx"), sheet = 1, col_names = F)
# Ignore rows without a department. This is to ignore extra rows with no data at all at the end of the spreadsheet.
TBNotif2016 = data.table(TBNotif2016)[3:.N,][!is.na(X3),]
names(TBNotif2016) = c( "CORRELATIVO", "NOMBRES", "CODIGOPACIENTE", "DIRECCION", "MUNICIPIO", "DEPARTAMENTO", "SERVICIODESALUD", 
                        "DAS", "SEXO", "FECHANACIMIENTO", "FECHAACTUAL", "EDAD", "RANGOEDAD", "EDUCACION", "PUEBLO", "OCUPACION",
                        "PESOLBS", "PESOKG", "CONDICIONINGRESO", "NUEVACONDICIONINGRESO", "CLASIFICACION", "LOCALIZACIONTB",
                        "FECHANOTIFICACION", "FECHAINICIOTX", "ESQUEMA", "FECHADX", "METODODX", "NUEVOMETODODX", "VIH", "FECHAPRUEBAVIH",
                        "OTRASPATOLOGIAS", "PDS", "PACIENTEPRIVADOLIBERTAD", "DEPORTADO", "UNIDADDX", "EMPLEADOMSPAS", "CONTACTOS",
                        "CONTACTO_000-0004", "CONTACTO_MAYORA_005", "QUIMIO_VIH", "CASOINDICE", "FALLECIDOS", "FECHAMUERTE", 
                        "CAUSADEMUERTE")
TBNotif2016[,YEAR := 2016]

deptoIndgnProp = read.csv(paste0(dataPath, "Outcome Measurement Data/Covariates/Demographics/Guatemala_indigenousPobProp.csv"))
deptoIndgnProp = data.table(deptoIndgnProp)

pobrezaGT11 = data.table(read.csv(paste0(dataPath, "Covariates and Other Data/Demographics/Guatemala-Pobreza-2011.csv")))

# Readxl has serious issues with guessing data types and handling empty cells around the document. 
# Thus, the easiest way to load a bad excel, such as these, is to load everything without type 
# conversion and columns definitions.

# ----------------------------------------------
# Prepare the data
TBNotifAll = rbindlist(list(TBNotif2012, TBNotif2013, TBNotif2014, TBNotif2015, TBNotif2016), fill = TRUE)
TBNotifAll$YEAR = as.integer(TBNotifAll$YEAR)
TBNotifAll[TBNotifAll$YEAR > 2012, NotificationDate:= as.Date(as.numeric(TBNotifAll$FECHANOTIFICACION[TBNotifAll$YEAR > 2012])-2, origin="1900-01-01")]
TBNotifAll[TBNotifAll$YEAR > 2012, YearMonth := as.integer(format(TBNotifAll$NotificationDate[TBNotifAll$YEAR > 2012], format = "%Y%m"))];
TBNotifAll = TBNotifAll[YearMonth>=201201 & YearMonth<=201612]
TBNotifAll[, RANGOEDAD := trimws(TBNotifAll$RANGOEDAD)]
TBNotifAll$SEXO = str_to_lower(TBNotifAll$SEXO)
TBNotifAll$VIH = str_to_lower(trimws(TBNotifAll$VIH))
TBNotifAll$VIH = factor(TBNotifAll$VIH, level=c("nr", "r"), labels=c("Not Reactive", "Reactive"))

TBNotifAll[, COD_MUNI := as.integer(getMuniCodeByName(ifelse(is.na(MUNICIPIO) || (MUNICIPIO=="ND"), 
                                                  ifelse(is.na(SERVICIODESALUD) || (SERVICIODESALUD=="ND"), NA, SERVICIODESALUD), 
                                                  MUNICIPIO), 
                                           ifelse(is.na(DEPARTAMENTO) || (DEPARTAMENTO=="ND"), NA, DEPARTAMENTO))), 
           by=1:nrow(TBNotifAll)]

TBNotifAll[, COD_DEPTO := floor(COD_MUNI/100)]

# ----Month by Municipality table:---------------------------------

# write.csv(table(TBNotifAll$YearMonth, TBNotifAll$COD_MUNI), file = paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/MunicipalityMonth.csv") )

write.csv(TBNotifAll[, .(NCases = .N, NInitiating = sum(as.integer(toupper(CONDICIONINGRESO) == "NUEVO")) ), by = .(COD_MUNI, YearMonth, AgeRange = RANGOEDAD, Gender = SEXO, HIV = VIH) ], file = paste0(dataPath, "Outcome Measurement Data/TUBERCULOSIS/MunicipalityMonth.csv") )

# TBNotifAll$RANGOEDAD = factor(TBNotifAll$RANGOEDAD, levels = order(unique(TBNotifAll$RANGOEDAD)))

# --------Monthly Time Series--------------------------------------
# Exploring
# All plots group by gender
# Plot monthly count time series
ggplot(data = TBNotifAll, aes(factor(YearMonth), fill=toupper(SEXO)))+ geom_bar(position="dodge")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Month", y="Notifications counts per month", title = "Time series of notifications")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Monthly.png")
# Bar plot by age range
ggplot(TBNotifAll, aes(x=RANGOEDAD, fill=SEXO))+geom_bar(position="dodge")  + theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Age range", y="Notifications counts per Age range", title="Age range")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-AgeRange.png")
# Bar plot by weight in pounds (some KG data is just missing, so pounds will be preferred)
# This is only for adults
ggplot(TBNotifAll[(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO))+geom_histogram(bins=20, position="dodge") + labs(fill="Gender", x="Weight (Pounds)", y="Notifications counts per month", title="By weight for children < 10 years old")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Weight_Kids0-10.png")
# This is for childs from 0 to 10 years old
ggplot(TBNotifAll[!(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO ))+geom_histogram(bins=20, position="dodge")  + labs(fill="Gender", x="Weight (Pounds)", y="Notifications count per month", title="By weight for people > 10 years old")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Weight GT 10.png")
# Diagnostic methods:
ggplot(as.data.frame(table(TBNotifAll$METODODX, TBNotifAll$SEXO, useNA = "always")), aes(x=reorder(Var1, Freq), y=Freq, fill=Var2)) + 
    geom_bar(stat="identity", position="dodge") + theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))  + labs(fill="Gender", x="Diagnosis methods", y="Notifications counts per month", title="Diagnosis methods")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Dx_method.png")
# ---------Monthly Time Series with HIV-------------------------------------
# VIH plots
# Monthly time series of notifications (separate by HIV and gender)
ggplot(data = TBNotifAll, aes(factor(YearMonth), fill=SEXO))+ geom_bar(position="dodge")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Month", y="Notifications counts per month", title = "Time series of notifications") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Monthly-VIH.png", height=4, width=9)
# Bar plot by age range
ggplot(TBNotifAll, aes(x=RANGOEDAD, fill=SEXO))+geom_bar(position="dodge")  + theme(axis.text.x = element_text(angle = 90)) + labs(fill="Gender", x="Age range", y="Notifications counts per Age range", title="Age range") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-AgeRange-VIH.png", height=4, width=9)
# Bar plot by weight in pounds (some KG data is just missing, so pounds will be preferred)
# This is only for adults
ggplot(TBNotifAll[(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO))+geom_histogram(bins=20, position="dodge") + labs(fill="Gender", x="Weight (Pounds)", y="Notifications counts per month", title="By weight for children < 10 years old") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-Weight_NoKids-VIH.png", height=4, width=9)
# This is for childs from 0 to 10 years old
ggplot(TBNotifAll[!(RANGOEDAD %in% c("000 - 004","005 - 010")),], aes(x= as.numeric(PESOLBS), fill=SEXO ))+geom_histogram(bins=20, position="dodge")  + labs(fill="Gender", x="Weight (Pounds)", y="Notifications count per month", title="By weight for people > 10 years old") + facet_wrap(~ VIH, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-WeightKids0-10-VIH.png", height=4, width=9)
# Diagnostic methods:
ggplot(as.data.frame(table(TBNotifAll$METODODX, TBNotifAll$SEXO, TBNotifAll$VIH, useNA = "always")), aes(x=reorder(Var1, Freq), y=Freq, fill=Var2)) + 
    geom_bar(stat="identity", position="dodge") + theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2, size = 5))  + labs(fill="Gender", x="Diagnosis methods", y="Notifications counts per month", title="Diagnosis methods") + facet_wrap(~ Var3, nrow = 1, ncol=3, scales="free_y")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Dx_method-VIH.png", height=4, width=9)

# ----------Maps------------------------------------
# Prepare GIS data. R has a very ugly way of handling this:
gtmMunisDataCopy = cbind(gtmMunisIGN@data)
gtmMunisIGN@data$id = rownames(gtmMunisIGN@data)
gtmMunisIGN@data = merge(gtmMunisIGN@data, TBNotifAll[toupper(CONDICIONINGRESO)=="NUEVO" & YEAR < 2016 & YEAR > 2011, .(TBCases=.N),by=COD_MUNI], by.x = "COD_MUNI__", by.y="COD_MUNI", all.x=TRUE, sort=FALSE)
gtmMunisIGN@data = merge(gtmMunisIGN@data, dt.munisGT[, .(Poblacion2015, COD_MUNI__)], by.x = "COD_MUNI__", by.y="COD_MUNI__", all.x=TRUE, sort=FALSE)
gtmMunisIGN.map.df = fortify(gtmMunisIGN)
gtmDeptosIGN.map.df = fortify(gtmDeptosIGN)


# Plotting TB cases per municipality
plot = ggplot(data=gtmMunisIGN@data, aes(fill=100000*TBCases/Poblacion2015/4)) + geom_map(aes(map_id=id), colour = rgb(1,1,1,0.5), map = gtmMunisIGN.map.df) + expand_limits(x = gtmMunisIGN.map.df$long, y = gtmMunisIGN.map.df$lat) + coord_quickmap() + scale_fill_gradientn(colours = c("#777777", "#ddcc22", "#DD5522", "#AA1111"), values=c(0,0.005,0.7,1), trans="log10") + labs(fill= "Rate", title="TB incidence rate per 100,000 people per year (Data from 2012 to 2015)")
# Overlay the departments
plot + geom_polygon(data = gtmDeptosIGN.map.df, aes(long, lat, group=group), fill="#00000000", color="#00000066", size=1)  + theme_void()

if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2015-Map-Muncipalities.png", height=8, width=8)

# Restore data
gtmMunisIGN@data = gtmMunisDataCopy

# ----TB Relation with indigenous pop----------------------
# Indigenous population proportion by department
gtmDeptosIGN@data$id = rownames(gtmDeptosIGN@data)
gtmDeptosIGN@data = merge(gtmDeptosIGN@data, deptoIndgnProp[, .(INDGN, DEPTO)], by.x = "CODIGO", by.y="DEPTO", all.x=TRUE, sort=FALSE)

gtmDeptosIGN.map.df = fortify(gtmDeptosIGN)

ggplot(data = gtmDeptosIGN@data, aes(fill= 100*INDGN)) + geom_map(aes(map_id=id), colour = "#FFFFFF88", map=gtmDeptosIGN.map.df) + expand_limits(x = gtmDeptosIGN.map.df$long, y = gtmDeptosIGN.map.df$lat) + coord_quickmap() + labs(fill="Indigenous population", title="Indigenous population proportion by department")

if (saveGraphs) 
    ggsave("PCE/Graficas/GT_Indigenous population by Depto.png", height=8, width=8)

# Relation between indigenous population proportion and TB prevalence
deptoData = merge(TBNotifAll[, .(TB=.N) ,by=COD_DEPTO], deptoIndgnProp[, .(DEPTO,INDGN)], by.x="COD_DEPTO", by.y="DEPTO")
deptoData = merge(deptoData, dt.munisGT[,.(Pob = sum(Poblacion, na.rm=TRUE)), by=COD_DEPT__], by.x="COD_DEPTO", by.y="COD_DEPT__")
ggplot(data = deptoData, aes(y=log(TB/Pob), x=log(INDGN/(1-INDGN)))) + geom_point() + geom_smooth(method=lm)  + labs(title="Indigenous population proportion vrs TB Cases per department", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="[Logit] Indigenous population proportion")
if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2012-2016-IndgnPop vs Incidence by Depto.png", height=8, width=8)
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
# 
# Call:
#     lm(formula = log(TB/Pob) ~ log(INDGN/(1 - INDGN)), data = deptoData)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.2313 -0.4186 -0.1030  0.4059  1.4763 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            -7.82236    0.16247 -48.147   <2e-16 ***
#     log(INDGN/(1 - INDGN)) -0.03544    0.05796  -0.611    0.548    
# ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.6963 on 20 degrees of freedom
# Multiple R-squared:  0.01835,	Adjusted R-squared:  -0.03073 
# F-statistic: 0.3739 on 1 and 20 DF,  p-value: 0.5478

# ----TB relation with poverty-----------------------------

pobrData = merge( merge(TBNotifAll[, .(TBCases=.N),by=COD_MUNI], pobrezaGT11[is.na(IsDepto)], by.x = "COD_MUNI", by.y = "Codigo"), munisGT, by.x ="COD_MUNI", by.y="COD_MUNI__" ) 

hist(log(pobrezaGT11$IncidPobrezaExt))

fit_TBPobr = lm(formula = log(TBCases/Poblacion) ~ log(IncidPobrezaExt), data = pobrData)
summary(fit_TBPobr) 

grid.arrange(
    ggplot(data = pobrData, aes(y=log(TBCases/Poblacion), x=IncidPobrezaT)) + geom_point() + geom_smooth(method=lm)  + labs(title="Total Poverty Incidence vrs TB Cases per municipality", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="Total poverty incidence"),
    ggplot(data = pobrData, aes(y=log(TBCases/Poblacion), x=IncidPobrezaExt)) + geom_point() + geom_smooth(method=lm)  + labs(title="Extreme Poverty Incidence vrs TB Cases per municipality", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="Extreme poverty incidence"),
    ggplot(data = pobrData, aes(y=log(TBCases/Poblacion), x=Brecha_FGT1)) + geom_point() + geom_smooth(method=lm)  + labs(title="Brecha vrs TB Cases per municipality", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="Brecha"),
    ggplot(data = pobrData, aes(y=log(TBCases/Poblacion), x=GINI)) + geom_point() + geom_smooth(method=lm)  + labs(title="GINI vrs TB Cases per municipality", y="[Log] TB incidence, cases per 1,000 persons per 2 years", x="GINI"),
    ncol=2, nrow=2
) 

if (saveGraphs) 
    ggsave("PCE/Graficas/TB_Gt_Notifications_2014-2015-Poverty Vars vs TBIncidence by Muni.png", height=8, width=8)

# ----Monthly counts of deaths--------------------------------------------
# defsData is loaded in ../Guatemala_load_outcomes_data.R
tbDeaths = NULL
for (year in seq(2009, 2015, 1)) {
    temp = defsData[[year]][(CaudefPRE %in% c("A15", "A16", "A17", "A18", "A19","B90")) | 
                                (Caudef %in% c("A301", "A302", "J65X", "K230", "K673", "M011",
                                               "N330", "M490", "M900", "N741", "O980", "K930",
                                               "P370", "Z030", "Z111", "Z201", "Z232")), 
                            .(conteo = .N), 
                            by = .(date = paste0(year, "-", Mesocu, "-01")) ]
    if (is.null(tbDeaths)) {
        tbDeaths = temp
    }
    else { 
        tbDeaths = rbind(tbDeaths, temp)
    }
}  

ggplot(data = tbDeaths, aes(x= as.Date(date), y = conteo)) + geom_line() + labs(title="TB deaths in Gt from 2009 to 2016", y="Cases per month", x="Time")
if (saveGraphs) 
    ggsave(paste0(dataPath, "Graficas/GT_TB_Deaths_TS 2009-2015.png"), height=8, width=8)


tbPrivHospI = NULL
for (year in seq(2009, 2015, 1)) {
    temp = privHospIData[[year]][(CAUFINPRE %in% c("A15", "A16", "A17", "A18", "A19","B90")) | (CAUFIN %in% c("A301", "A302", "J65X", "K230", "K673", "M011", "N330", "M490", "M900", "N741", "O980", "K930", "P370", "Z030", "Z111", "Z201", "Z232")),  .(conteo = .N), 
                                 by = .(date = paste0(year, "-", MES, "-01")) ]
    if (is.null(tbDeaths)) {
        tbPrivHospI = temp
    }
    else { 
        tbPrivHospI = rbind(tbPrivHospI, temp)
    }
}  

ggplot(data = tbPrivHospI, aes(x= as.Date(date), y = conteo)) + geom_line()  + labs(title="TB internal private hospital services in Gt from 2009 to 2016", y="Cases per month", x="Time")
if (saveGraphs) 
    ggsave(paste0(dataPath, "Graficas/GT_TB_PrivHospIntern_TS 2009-2015.png"), height=8, width=8)
