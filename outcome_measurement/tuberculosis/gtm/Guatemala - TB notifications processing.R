# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2017-11-14
# Explore TB outcome data from MOH notifications databases.


# ----------------------------------------------
# Dependencies:
library(data.table)
library(stringr)
library(readxl)
library(ggplot2)
library(gridExtra)


# ----Configure------------------------------------------
saveGraphs = T
codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

# ------- Load TB notifications aggregated database ----------------
tbnots = read.csv("PCE/Outcome Measurement Data/TUBERCULOSIS/GTM - TB notifications 2012-2017.csv")

tbnots = data.table(tbnots)

# Overview of enter condition
ggplot(data = tbnots[CONDICIONINGRESO %in% c("nuevo", "fracaso", "recaida", "abandono recuperado", "abandono recuperada"),
                     .(Count = .N),
                        by=.(YearMonth = floor(YearMonth/100)*100 + floor(( YearMonth%% 100 - 1)/3)*3 + 1 , CONDICIONINGRESO)][,
                     .(Count, CONDICIONINGRESO, YearMonth =factor(YearMonth))]) + 
    geom_col(aes(YearMonth, Count, fill = CONDICIONINGRESO))  + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + 
    labs(x="Quarter", title="TB Notifications time series\nGrouped by condition of entrance (including exits)")
# Confirming our quarter generator works well:
# tbnots[,.N,by = .(YearMonth = floor(YearMonth/100)*100 + floor(( YearMonth%% 100 - 1)/3)*3 + 1, YearMonth) ]


# NAs are kids, and most probably they are new cases. It turns out these are prophylactic cases that receive isoniazide. Mostly children.
ggplot(data = tbnots[is.na(CONDICIONINGRESO), .(Count = .N), by=.(AgeGroup = ceiling(EDAD/5)*5)][order(AgeGroup), 
                   .(AgeGroup = factor(AgeGroup), Count)]) +
    geom_col(aes(AgeGroup, Count))

# Prophylactic cases
ggplot(data = tbnots[CONTACTOS == "quimio", .(AgeGroup = ceiling(EDAD/5)*5), by= .(YEAR)]) +
    geom_violin(aes(YEAR, AgeGroup), fill="blue")  + labs(title="Age distribution of contacts under treatment by year")
ggplot(data = tbnots[CONTACTOS == "quimio", .(Count = .N), by= .(YEAR)]) +
    geom_col(aes(YEAR, Count), fill="blue")  + labs(title="Contacts under treatment by year")

# New cases
ggplot(data = tbnots[CONDICIONINGRESO == "nuevo", .(AgeGroup = ceiling(EDAD/5)*5), by= .(YEAR) ]) +
    geom_violin(aes(factor(YEAR), AgeGroup), fill="blue")  + labs(title="Age distribution of new cases by year")
ggplot(data = tbnots[CONDICIONINGRESO == "nuevo", .(Count=.N), by= .(YEAR) ]) +
    geom_col(aes(factor(YEAR), Count), fill="blue")  + labs(title="New cases by year")

# Incidence time series:
yearly = tbnots[CONDICIONINGRESO %in% c(NA, "nuevo")  & !(CONTACTOS %in% c("quimio")),.(Count = .N),
       by=.(Year = floor(YearMonth/100))]

yearly$Pop = sapply(2012:2017, function (i)     sum(GTMuniPopulation(dt.munisGT$COD_MUNI__, 
                                    rep(i, nrow(dt.munisGT))), na.rm = T))

yearly[,Incidence := 100000*Count/Pop]
ggplot(data=yearly) + geom_line(aes(Year, Incidence)) + ylim(0,40) + labs(title = "Guatemala TB incidence rate per 100,000 people\nby year")

# Map of tb incidence by DAS for 2017.
mapdata = tbnots[CONDICIONINGRESO %in% c(NA, "nuevo") & floor(YearMonth/100) == 2017 & !(CONTACTOS %in% c("quimio")),.(counts = .N),
                by=.(deptocode = COD_DEPT )]
mapdata$pop = GTDeptoPopulation(mapdata$deptocode, rep(2017, nrow(mapdata) ) )
mapdata[, values := 100000*counts/pop]
gtmap_depto(mapdata) + scale_fill_distiller(name="Incidence rate", palette = "Blues", direction = 1, na.value = "#444444") + 
    labs(title="2017 TB incidence rate per 100,000 people\nby department")

# Department trends
mapdataT = tbnots[CONDICIONINGRESO %in% c(NA, "nuevo") & !(CONTACTOS %in% c("quimio")),.(counts = .N),
                 by=.(deptocode = COD_DEPT, Year = floor(YearMonth/100))]

mapdataT$pop = GTDeptoPopulation(mapdataT$deptocode, mapdataT$Year)
mapdataT[,Incidence := 100000 * counts/pop]
dcast(mapdataT, deptocode ~ Year )
mapLinmod = lm(Incidence ~ factor(deptocode)+ Year:factor(deptocode), data = mapdataT)
summary(mapLinmod)
factores = grep("\\d\\d?\\:Year$",names(coef(mapLinmod)))
trends = data.frame(values_ = coef(mapLinmod)[factores], deptocode = str_match( names(coef(mapLinmod))[factores], "(\\d\\d?)\\:Year$" )[,2])
trends$values = cut(trends$values_, c(2.4,1.2,0.6,0,-0.6,-1.2,-2.4), 
                    labels = c("2.4 to 1.2","1.2 to 0.6","0.6 to 0","0 to -0.6","-0.6 to -1.2","-1.2 to -2.4"))
#trends$values = trends$values_
gtmap_depto(trends) + scale_fill_manual(values=c("#552211", "#664411","#AA6622", "#3377DD", "#55AAFF", "#88CCFF"), name="Incidence rate trend", na.value = "#444444") + 
    labs(title="TB incidence annual trend\nby department")
