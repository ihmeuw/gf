# this file preps a dataset shared by CIESAR which details the Covid-19 response
library(data.table)
library(readxl)

# prepare the covid data to integrate into Tableau

# read in the second and third sheet
data1_original <- read_xlsx("J:\\Project\\Evaluation\\GF\\resource_tracking\\other\\specialized_datasets\\gtm\\AHORROS PARA APOYO COVID-21-04-2020 con observaciones ALF.xlsx",
                   sheet = 2)

data2_original <- read_xlsx("J:\\Project\\Evaluation\\GF\\resource_tracking\\other\\specialized_datasets\\gtm\\AHORROS PARA APOYO COVID-21-04-2020 con observaciones ALF.xlsx",
                   sheet = 3)

# save as datatables
data1 <- as.data.table(data1_original)
data2 <- as.data.table(data2_original)

# subset columns
# Grab module-intervention block of rows - first find the columns you need
partida_col <- 1
module_col <- 2
intervention_col <- 3
activity_col <- 4
cost_cat_col <- 5
budget_col <- 6

data1 = data1[, c(partida_col, module_col, intervention_col, activity_col, cost_cat_col, budget_col), with=FALSE]

# clean column names
names(data1) <- c('partida', 'modulo', 'intervencion', 'descripcion', 'categoria', 'monto_ahorro')

#Drop extra rows that don't have module specified
data1 = data1[!is.na(modulo)]
data1 = data1[!modulo %in% c('Módulo')]

#Make budget and expenditure numeric
data1[, monto_ahorro:=as.numeric(monto_ahorro)]

data1$destinado_para <- "MSPAS"

###### clean second sheet ######
# subset columns
# Grab module-intervention block of rows - first find the columns you need

data2 = data2[, c(partida_col, module_col, intervention_col, activity_col, cost_cat_col, budget_col), with=FALSE]

# clean column names
names(data2) <- c('partida', 'modulo', 'intervencion', 'descripcion', 'categoria', 'monto_ahorro')

#Drop extra rows that don't have module specified
data2 = data2[!is.na(modulo)]
data2 = data2[!modulo %in% c('Módulo')]

#Make budget and expenditure numeric
data2[, monto_ahorro:=as.numeric(monto_ahorro)]

data2$destinado_para <- "equipo_de_proteccion"

# save file
data_covid <- rbind(data1, data2)
write.csv(data_covid, file = "C:/Users/frc2/Box Sync/Global Fund Files/tableau_data/gtm_covid_savings.csv")
