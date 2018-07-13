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
ggplot(data = tbnots[,.(Count = .N),
                        by=.(YearMonth = floor(YearMonth/100)*100 + floor(( YearMonth%% 100 - 1)/3)*3 + 1 , CONDICIONINGRESO)][,
                     .(Count, CONDICIONINGRESO, YearMonth =factor(YearMonth))]) + 
    geom_col(aes(YearMonth, Count, fill = CONDICIONINGRESO))  + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) + 
    labs(x="Quarter", title="TB Notifications time series\nGrouped by condition of entrance (including exits)")

# Confirming our quarter generator works well:
# tbnots[,.N,by = .(YearMonth = floor(YearMonth/100)*100 + floor(( YearMonth%% 100 - 1)/3)*3 + 1, YearMonth) ]

ggplot(data = tbnots[is.na(CONDICIONINGRESO), .(Count = .N), by=.(AgeGroup = ceiling(EDAD/5)*5)][order(AgeGroup), 
                   .(AgeGroup = factor(AgeGroup), Count)]) +
    geom_col(aes(AgeGroup, Count))

# NAs are kids, and most probably they are new cases.

yearly = tbnots[CONDICIONINGRESO %in% c(NA, "nuevo"),.(Count = .N),
       by=.(Year = floor(YearMonth/100))]

yearly$Pop = sapply(2012:2017, function (i)     sum(GTMuniPopulation(dt.munisGT$COD_MUNI__, 
                                    rep(i, nrow(dt.munisGT))), na.rm = T))

yearly[,Incidence := 100000*Count/Pop]
ggplot(data=yearly) + geom_line(aes(Year, Incidence)) + ylim(0,50) + labs(title = "Guatemala TB incidence rate per 100,000 people\nby year")

# Map of tb incidence by DAS for 2017.
mapdata = tbnots[CONDICIONINGRESO %in% c(NA, "nuevo") & floor(YearMonth/100) == 2017,.(counts = .N),
                by=.(deptocode = COD_DEPT )]
mapdata$pop = GTDeptoPopulation(mapdata$deptocode, rep(2017, nrow(mapdata) ) )
mapdata[, values := 100000*counts/pop]
gtmap_depto(mapdata) + scale_fill_distiller(name="Incidence rate", palette = "Blues", direction = 1, na.value = "#444444") + 
    labs(title="2017 TB incidence rate per 100,000 people\nby department")


