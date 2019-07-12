# cleaning and checking data file
# for Senegal
# Data on MR-TB; Filename= PANEL PNT TB-MR final_01072019

# set-up
library(data.table)

# read in data
# CSV file encoded in UTF-8 will preserve the accent marks
DT <- fread("C:\\Users\\frc2\\Documents\\data\\tb\\PANEL PNT TB-MR final_01072019.csv", encoding = "UTF-8")

# ------------------------------
# clean data
# ------------------------------

# remove unecessary columns at end
DT[,76:261:=NULL]

# remove rows that present summed values
DT <- DT[ !(DT$Centre %in% c("TOTAL TRIMESTRE", "TOTAL ANNEE")),]
DT <- DT[ !(DT$REGION %in% c("SENEGAL"))]

# create unique identifier: region_centre_year_quarter
DT$id <- paste(DT$REGION,DT$Centre,DT$Année,DT$Trimestre, sep="-")

# remove duplicate entries
DT <- unique(DT, by=c("id"))

# remove any rows in which there is no identifying information
DT <- DT[ !(DT$id %in% c("--NA-NA"))]

# ------------------------------
# value types
# ------------------------------

# change percent values from characters to numeric integers

# ------------------------------
# subset
# ------------------------------

# select variables to keep
Cols.chosen=c('REGION', 'Centre', 'Année', 'Trimestre', 
              'Cas toutes formes détectés', 
              'TOTAL Guéris', 'Taux de guérison', 
              'TOTAL traitement terminé', 'Pourcentage de traitement terminé',
              'id')

# subset data
DT1 <- DT[,Cols.chosen, with=FALSE]

# save data in new R object
saveRDS(DT1, "sen_mrtb_data.rds")
