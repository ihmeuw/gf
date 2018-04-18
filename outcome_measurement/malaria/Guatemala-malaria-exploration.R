# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2018-01-15
# Explore HIV outcome data from Guatemala.

# ----Dependencies------------------------------------------
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(stringdist)
library(rgdal)

codePath = "PCE/gf/"

source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")

source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

# ----Configuration------------------------------------------
saveGraphs = T

# ----Load data------------------------------------------
defsData = loadDeathsData()
privHospIData = loadPrivateHospitalAdmsData()

# ----Monthly counts--------------------------------------------
# defsData is loaded in ../Guatemala_load_outcomes_data.R
mDeaths = NULL
for (year in seq(2009, 2016, 1)) {
  temp = defsData[[year]][(CaudefPRE %in% c("B50", "B51", "B52", "B53", "B54")) | (Caudef %in% c("P373", "P374")), 
                          .(conteo = .N), 
                          by = .(date = paste0(year, "-", Mesocu, "-01")) ]
  if (is.null(mDeaths)) {
    mDeaths = temp
  }
  else { 
    mDeaths = rbind(mDeaths, temp)
  }
}  

ggplot(data = mDeaths, aes(x= as.Date(date), y = conteo)) + geom_line() + labs(title="Malaria deaths in Guatemala from 2009 to 2016", y="Deaths per month", x="Time")
if (saveGraphs) 
  ggsave(paste0(dataPath, "Graficas/GT_Malaria_Deaths_TS 2009-2016.png"), height=8, width=8)


mPrivHospI = NULL
for (year in seq(2009, 2016, 1)) {
  temp = privHospIData[[year]][(CAUFINPRE %in% c("B50", "B51", "B52", "B53", "B54")) | (CAUFIN %in% c("P373", "P374")), 
                          .(conteo = .N), 
                          by = .(date = paste0(year, "-", MES, "-01")) ]
  if (is.null(mPrivHospI)) {
    mPrivHospI = temp
  }
  else { 
    mPrivHospI = rbind(mPrivHospI, temp)
  }
}  

ggplot(data = mPrivHospI, aes(x= as.Date(date), y = conteo)) + geom_line()  + labs(title="Malaria internal private hospital services in Gt from 2009 to 2016", y="Cases per month", x="Time")
if (saveGraphs) 
    ggsave(paste0(dataPath, "Graficas/GT_Malaria_PrivHospIntern_TS 2009-2016.png"), height=8, width=8)

# -----------EPIVIGILA-------------------

malariaepi = data.table(read.csv("DATOS/MSPAS/EPIVIGILA - Semana 52 2017 csv/MalariaMunisGt 2001-2017.csv"))

# Map
gtmMunisDataCopy = cbind(gtmMunisIGN@data)
gtmMunisIGN@data$id = rownames(gtmMunisIGN@data)
gtmMunisIGN@data = merge(gtmMunisIGN@data, malariaepi[Year == 2017, .(Cases=sum(TotalCases)),by=Muni], by.x = "COD_MUNI__", by.y="Muni", all.x=TRUE, sort=FALSE)
gtmMunisIGN.map.df = fortify(gtmMunisIGN)
gtmDeptosIGN.map.df = fortify(gtmDeptosIGN)

# Plotting malaria cases per municipality for year 2017
plot = ggplot(data=gtmMunisIGN@data, aes(fill=Cases)) + geom_map(aes(map_id=id), colour = rgb(1,1,1,0.5), map = gtmMunisIGN.map.df) + expand_limits(x = gtmMunisIGN.map.df$long, y = gtmMunisIGN.map.df$lat) + coord_quickmap() + scale_fill_gradientn(colours = c("#ddcc22", "#DD5522", "#AA1111"), values=c(0.1,0.5,1), trans="log10") + labs(fill= "Rate", title="Casos de Malaria en Guatemala durante el 2017")
# Overlay the departments
plot + geom_polygon(data = gtmDeptosIGN.map.df, aes(long, lat, group=group), fill="#00000000", color="#00000066", size=1)  + theme_void()

if (saveGraphs) 
    ggsave("PCE/Graficas/Malaria_Gt_Notifications_2017-Map-Muncipalities.png", height=8, width=8)

gtmMunisIGN@data = gtmMunisDataCopy
