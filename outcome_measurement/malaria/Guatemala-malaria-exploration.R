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
# Some data was loaded in GT_load_data.R script.
# ----Monthly counts--------------------------------------------
# defsData is loaded in ../Guatemala_load_outcomes_data.R
mDeaths = NULL
for (year in seq(2009, 2015, 1)) {
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
  ggsave(paste0(dataPath, "Graficas/GT_Malaria_Deaths_TS 2009-2015.png"), height=8, width=8)


mPrivHospI = NULL
for (year in seq(2009, 2015, 1)) {
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
    ggsave(paste0(dataPath, "Graficas/GT_Malaria_PrivHospIntern_TS 2009-2015.png"), height=8, width=8)
