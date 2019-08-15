# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas 
# PURPOSE: Clean MDR-TB Data from 2014-2018  
# DATE: Last updated 8/15/2019
# ----------------------------------------------------------

# set-up
library(data.table)

# set directory where data is saved
dir <- "C:/Users/frc2/Documents/Data/tb/raw_data/tb_mdr"
setwd(dir)

# load datasets
mdr2014 <- fread("tb_mdr_2014.csv")
mdr2015 <- fread("tb_mdr_2015.csv", header = TRUE)
mdr2016 <- fread("tb_mdr_2016.csv", header = TRUE)
mdr2017 <- fread("tb_mdr_2017.csv", header = TRUE)
mdr2018 <- fread("tb_mdr_2018.csv", header = TRUE)

# change column names
setnames(mdr2014, 
         names(mdr2014),
         c("annee", "id", "patient", "sexe", "age", "site","region", "date_diag", "date_trait", "regime", "resultat"))

setnames(mdr2015, 
         names(mdr2015),
         c("id", "patient", "sexe", "age", "site", "region", "V7", "date_trait", "regime", "resultat"))

setnames(mdr2016, 
         names(mdr2016),
         c("id", "patient", "sexe", "age", "site", "region", "date_diag", "date_trait", "regime", "resultat"))

setnames(mdr2017, 
         names(mdr2017),
         c())

setnames(mdr2018,
         names(mdr2018),
         )

# clean data

# remove white space
