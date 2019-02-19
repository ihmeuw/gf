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


dataPath = "CIESAR/PCE/"
codePath = "CIESAR/PCE/gf/"

source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

# ----Configuration------------------------------------------
saveGraphs = T

# ----Load data------------------------------------------
gtmDeptosIGN = readOGR(paste0(dataPath, "Covariates and Other Data/GIS/GT-IGN-cartografia_basica-Departamentos.geojson"), encoding = "UTF-8")
gtmDeptosIGN@data$CODIGO = floor(as.numeric(as.character(gtmDeptosIGN@data$CODIGO))/100)

# Load IGSS data on HIV
HIV_IGSS = read_excel(paste0(dataPath, "Outcome Measurement Data/HIV/IGSS/Cohorte activa con TAR a diciembre 2016 para CIESAR.docx.xlsx"), 1 )
IGSSDepto = read.csv(paste0(dataPath, "Covariates and Other Data/Demographics/Guatemala_IGSS_Afiliados_2015.csv"))
IGSSDepto = data.table(IGSSDepto)
IGSSDepto[, DEPTOCODE := getDeptoCodeByName(as.character(Depto)), by=1:nrow(IGSSDepto)]
dt.HIV_IGSS = data.table(HIV_IGSS)
dt.HIV_IGSS[, DEPTOCODE := getDeptoCodeByName(as.character(DEPARTAMENTO)), by=1:nrow(dt.HIV_IGSS)]
deptoHIV = dt.HIV_IGSS[DEPARTAMENTO!="NO DATO" ,.(DEPARTAMENTO = first(DEPARTAMENTO), HIVCases = .N), by = DEPTOCODE]
deptoHIV = merge(deptoHIV, dt.munisGT[, .(Poblacion = sum(Poblacion, na.rm = TRUE)),by=COD_DEPT__], by.x = "DEPTOCODE", by.y = "COD_DEPT__" )
deptoHIV = merge(deptoHIV, IGSSDepto, by="DEPTOCODE")

# ----Process data------------------------------------------
deptoHIV[, incidHIV := 1000 * HIVCases / Poblacion]
deptoHIV[, incidHIVIGSSCov := 1000 * HIVCases / EMA]
deptoHIV[, IGSSCoverage := EMA / Poblacion]

gtmDeptoIGN.data = data.frame(gtmDeptosIGN@data)

gtmDeptosIGN@data$id = rownames(gtmDeptosIGN@data)
gtmDeptosIGN@data = merge(gtmDeptosIGN@data, deptoHIV[, .(incidHIV, incidHIVIGSSCov, DEPTOCODE)], by.x = "CODIGO", by.y="DEPTOCODE", all.x=TRUE, sort=FALSE)

gtmDeptosIGN.map.df = fortify(gtmDeptosIGN)

ggplot(data = gtmDeptosIGN@data, aes(fill= incidHIVIGSSCov)) + geom_map(aes(map_id=id), colour = "#FFFFFF88", map=gtmDeptosIGN.map.df) + 
      expand_limits(x = gtmDeptosIGN.map.df$long, y = gtmDeptosIGN.map.df$lat) + coord_quickmap() + 
      labs(fill="HIV Incidence", title="HIV Incidence (per 1,000 affiliated per 1 year) by department from IGSS 2016 data")

if (saveGraphs) 
  ggsave(paste0(dataPath, "Graficas/GT_HIV IGSS 2016 by Depto per affiliated.png"), height=8, width=8)

ggplot(data = gtmDeptosIGN@data, aes(fill= incidHIV)) + geom_map(aes(map_id=id), colour = "#FFFFFF88", map=gtmDeptosIGN.map.df) + 
  expand_limits(x = gtmDeptosIGN.map.df$long, y = gtmDeptosIGN.map.df$lat) + coord_quickmap() + 
  labs(fill="HIV Incidence", title="HIV Incidence (per 1,000 inhabitants per 1 year) by department from IGSS 2016 data")

if (saveGraphs) 
  ggsave(paste0(dataPath, "Graficas/GT_HIV IGSS 2016 by Depto per inhabitants.png"), height=8, width=8)

# ----Monthly counts--------------------------------------------
# This gets HIV deaths from years 2009 to 2015 
# defsData is loaded in ../Guatemala_load_outcomes_data.R
hivDeaths = NULL
for (year in seq(2009, 2015, 1)) {
  temp = defsData[[year]][(CaudefPRE %in% c("B20", "B21", "B22", "B23", "B24")) | (Caudef %in% c("R75X", "Z114", "Z206", "Z21X", "Z717")), 
                          .(conteo = .N), 
                          by = .(date = paste0(year, "-", Mesocu, "-01")) ]
  if (is.null(hivDeaths)) {
    hivDeaths = temp
  }
  else { 
    hivDeaths = rbind(hivDeaths, temp)
  }
}  

ggplot(data = hivDeaths, aes(x= as.Date(date), y = conteo)) + geom_line() + labs(title="HIV deaths in Gt from 2009 to 2016", y="Cases per month", x="Time")
if (saveGraphs) 
  ggsave(paste0(dataPath, "Graficas/GT_HIV_Deaths 2009-2015.png"), height=8, width=8)

hivPrivHospI = NULL
for (year in seq(2009, 2015, 1)) {
    temp = privHospIData[[year]][(CAUFINPRE %in% c("B20", "B21", "B22", "B23", "B24")) | (CAUFIN %in% c("R75X", "Z114", "Z206", "Z21X", "Z717")), 
                                 .(conteo = .N), 
                                 by = .(date = paste0(year, "-", MES, "-01")) ]
    if (is.null(hivPrivHospI)) {
        hivPrivHospI = temp
    }
    else { 
        hivPrivHospI = rbind(hivPrivHospI, temp)
    }
}  

ggplot(data = hivPrivHospI, aes(x= as.Date(date), y = conteo)) + geom_line() + labs(title="HIV internal private hospital services in Gt from 2009 to 2016", y="Cases per month", x="Time")
if (saveGraphs) 
    ggsave(paste0(dataPath, "Graficas/GT_HIV_PrivHospIntern_TS 2009-2015.png"), height=8, width=8)

