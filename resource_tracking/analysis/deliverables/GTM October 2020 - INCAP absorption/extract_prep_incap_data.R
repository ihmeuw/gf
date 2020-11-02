library(data.table)
library(readxl)

data <- read_xlsx("\\\\ihme.washington.edu\\ihme\\snfs\\Project\\Evaluation\\GF\\resource_tracking\\other\\specialized_datasets\\gtm\\gtm_h_incap_pr_absorption_2020_s1.xlsx")
dt <- as.data.table(data)

verbose<-TRUE

# keep only certain columns
partidas_presup <- grep("PARTIDAS", dt)
presup_original <- grep("Original", dt)
presup_modif <- grep("Modificaciones", dt)
presup_actual <- grep("Actual", dt)
ejec_compromp <- grep("Comprometido", dt)
ejec_devengado <- grep("Devengado", dt)
ejec_pagado <- grep("Pagado", dt)
saldos_porcompr <- grep("Saldos por", dt)
numero_partidos <- grep("Partida", dt)

#Validate these column indices, and assign column names.
# stopifnot(length(part)==1 & length(expenditure_col)==1)
colnames(dt)[partidas_presup] <- "partidas_presupuestarias"
colnames(dt)[presup_actual] <- "presupuesto_actual"
colnames(dt)[ejec_pagado] <- "ejecucion_pagado"
colnames(dt)[numero_partidos] <- "numero_partidas"
colnames(dt)[presup_original] <- "presupuesto_original"

dt <- dt[,.(numero_partidas, partidas_presupuestarias, presupuesto_original, presupuesto_actual, ejecucion_pagado)]

# # keep only certain rows
#Remove 'Actual' rows
actual_rows <- grep("actual", tolower(dt$presupuesto_actual))
if (length(actual_rows) > 0){
  if (verbose == TRUE){
    print(paste0("Actual rows being dropped in GTM Incap prep function. First column: ", dt[actual_rows, 1]))
  }
  dt <- dt[-actual_rows, ,drop = FALSE]
}

# subset <- dt[presupuesto_actual!="Actual",.(numero_partidas, partidas_presupuestarias, presupuesto_actual, ejecucion_pagado)] # remove extra rows with column headers

fondo_rows <- grep("fondo:", tolower(dt$numero_partidas))
if (length(fondo_rows) > 0){
  if (verbose == TRUE){
    print(paste0("fondo rows being dropped in GTM Incap prep function. First column: ", dt[fondo_rows, 1]))
  }
  dt <- dt[-fondo_rows, ,drop = FALSE]
}

entidad_rows <- grep("entidad:", tolower(dt$numero_partidas))
if (length(entidad_rows) > 0){
  if (verbose == TRUE){
    print(paste0("entidad rows being dropped in GTM Incap prep function. First column: ", dt[entidad_rows, 1]))
  }
  dt <- dt[-entidad_rows, ,drop = FALSE]
}

partida_rows <- grep("partida", tolower(dt$numero_partidas))
if (length(partida_rows) > 0){
  if (verbose == TRUE){
    print(paste0("partida rows being dropped in GTM Incap prep function. First column: ", dt[partida_rows, 1]))
  }
  dt <- dt[-partida_rows, ,drop = FALSE]
}

periodo_rows <- grep("período:", tolower(dt$numero_partidas))
if (length(periodo_rows) > 0){
  if (verbose == TRUE){
    print(paste0("Período: rows being dropped in GTM Incap prep function. First column: ", dt[periodo_rows, 1]))
  }
  dt <- dt[-periodo_rows, ,drop = FALSE]
}

resumen_rows <- grep("resumen", tolower(dt$numero_partidas))
if (length(resumen_rows) > 0){
  if (verbose == TRUE){
    print(paste0("resumen rows being dropped in GTM Incap prep function. First column: ", dt[resumen_rows, 1]))
  }
  dt <- dt[-resumen_rows, ,drop = FALSE]
}

date_rows <- grep("13/10/2020", tolower(dt$numero_partidas))
if (length(date_rows) > 0){
  if (verbose == TRUE){
    print(paste0("date rows being dropped in GTM Incap prep function. First column: ", dt[date_rows, 1]))
  }
  dt <- dt[-date_rows, ,drop = FALSE]
}

dt <- dt[rowSums(is.na(dt)) != ncol(dt), ] # remove rows that have only NA

# change value type
dt$numero_partidas <- as.numeric(dt$numero_partidas)

# correct a few row budget items
dt$partidas_presupuestarias[59] <- "Obligaciones MSPAS" #2.064
dt$partidas_presupuestarias[203] <- "SR3 APEVIHS" # 2.225
dt$partidas_presupuestarias[205] <- "SR7 OMES" # 2.228
dt$partidas_presupuestarias[207] <- "SR1 CAS" # 2.230
dt$partidas_presupuestarias[334] <- "Enlace administrativo" # 2.371

# remove rows where the budget line and budget is blank
dt <- dt[!with(dt,is.na(numero_partidas)& is.na(presupuesto_actual)),]

# remove  first row
dt <- dt[-1, ]
dt <- dt[-328, ]

# change missing values to zero for expenditure
dt$ejecucion_pagado[is.na(dt$ejecucion_pagado)] <- 0
dt$presupuesto_actual[is.na(dt$presupuesto_actual)] <- 0

write.csv(dt, "\\\\ihme.washington.edu\\ihme\\snfs\\Project\\Evaluation\\GF\\resource_tracking\\other\\specialized_datasets\\gtm\\gtm_incap_absorption.csv")

# 
# dt <- dt[!is.na(presupuesto_actual)] # remove rows where there is no budget
# dt <- dt[rowSums(is.na(dt)) != ncol(dt),]
# dt <- dt[!is.na(numero_partidas)] # remove extra rows with column headers