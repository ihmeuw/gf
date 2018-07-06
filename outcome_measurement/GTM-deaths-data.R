# Deaths data. Compare IHME GBD death certificates reclassiffication with INE database
library(data.table)
library(ggplot2)
library(haven)

codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")

# Load deaths INE data up to 2016.
defsData = loadDeathsData()

# IHME  deaths data
IHMEDefsData = read.csv("PCE/Outcome Measurement Data/VR/vr_2009_2016.csv")
IHMEDefsData = data.table(IHMEDefsData)
IHMELocations = data.table(read.csv("PCE/Covariates and Other Data/GIS/GTM_muni_merges_2009_2016.csv"))
IHMEAgeGroups = data.table(read.csv("PCE/Outcome Measurement Data/VR/age_group_ids.csv"))
# Disease cause_id
# TB      297
# HIV     298
# LRI     322
# URI     328
# Malaria 345
# Nutrition 386
head(IHMEDefsData)

head(defsData[[2016]])

# Lets explore TB deaths:
INECounts = defsData[[2016]][(CaudefPRE %in% c("A15", "A16", "A17", "A18", "A19","B90")) | 
                     (Caudef %in% c("A301", "A302", "J65X", "K230", "K673", "M011",
                                    "N330", "M490", "M900", "N740", "N741", "O980", "K930",
                                    "P370", "Z030", "Z111", "Z201", "Z232", "U843")), 
                 .(conteo = .N), 
                 by = .(agegroup = (4 + ceiling((Edadif+1)/5)), Sexo, Mupocu) ]

# First of all lets compare TB deaths according to INE and IHME.
INECounts[,.(t = sum(conteo))] # 313
IHMEDefsData[(year_id == 2016) & (cause_id %in% c(297, 954, 934, 946, 947)), .(t = sum(deaths))] # 448.3
# IHME Corrected data suggests a very large amount of TB deaths.

# Now with HIV:
INECountsHIV = defsData[[2016]][(CaudefPRE %in% c("B20", "B21", "B22", "B23", "B24")) | 
                                 (Caudef %in% c("R75X", "Z114", "Z206", "Z21X", "Z717")),
                             .(conteo = .N), 
                             by = .(agegroup = (4 + ceiling((Edadif+1)/5)), Sexo, Mupocu)]
INECountsHIV[,.(t = sum(conteo))] # 358
IHMEDefsData[(year_id == 2016) & (cause_id %in% c(298, 948, 949, 950, 300)), .(t = sum(deaths))] # 703.7

# Malaria: 
INECountsMlr = defsData[[2016]][(CaudefPRE %in% c("B50", "B51", "B52", "B53", "B54")) |
                                (Caudef %in% c("P373", "P374")),
                                .(conteo = .N), 
                                by = .(agegroup = (4 + ceiling((Edadif+1)/5)), Sexo, Mupocu)]
INECountsMlr[,.(t = sum(conteo))] # 6
IHMEDefsData[(year_id == 2016) & (cause_id %in% c(345, 856, 857, 858)), .(t = sum(deaths))] # 8.1

INECountsHIV[, sum(conteo), by = agegroup]


# Mujeres en edad reproductiva
defsData[[2009]][ ((CaudefPRE %in% c("A15", "A16", "A17", "A18", "A19","B90"
                                     )) | 
                     (Caudef %in% c("A301", "A302", "J65X", "K230", "K673", "M011",
                                    "N330", "M490", "M900", "N740", "N741", "O980", "K930",
                                    "P370", "Z030", "Z111", "Z201", "Z232", "U843"
                                    )))
                  & Sexo == 2 & Edadif >10 & Edadif < 54, 
                 .(conteo = .N) ] # 60
ihme_temp = IHMEDefsData[ sex_id == 2 & age_group_id >= 7 & age_group_id <= 54  
              & (cause_id %in% c(297, 954, 934, 946, 947)), .(sum(deaths)), by=year_id] # 
ihme_temp[order(year_id)]
