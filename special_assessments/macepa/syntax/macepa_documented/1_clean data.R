# Francisco Rios Casas
# MACEPA clean data

# set up
library(data.table)

# read in MACEPA data file
DT <- fread("C:/Users/frc2/Documents/data/macepa/Documented cases (MACEPA).csv")

# subset data into the following columns: region, district, nom_poste, date_diagnos
DT <- DT[,.(region, district, nom_poste, date_diagnos, forme_palu)]
DT$count = 1

# sum by date for each region: MATAM
DT1 <- DT[, .(cases=sum(count, na.rm = TRUE)), by=c("region", "date_diagnos")]

