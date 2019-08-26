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

#---------------------------------------------------
# load datasets
mdr2014 <- fread("tb_mdr_2014.csv")
mdr2015 <- fread("tb_mdr_2015.csv", header = TRUE)
mdr2016 <- fread("tb_mdr_2016.csv", header = TRUE)
mdr2017 <- fread("tb_mdr_2017.csv", header = TRUE)
mdr2018 <- fread("tb_mdr_2018.csv", header = TRUE)

# change column names
setnames(mdr2014, 
         names(mdr2014),
         c("annee", "id", "patient", "sexe", "age", "structure","region", "date_diag", "date_trait", "regime", "resultat"))

setnames(mdr2015, 
         names(mdr2015),
         c("id", "patient", "sexe", "age", "structure", "region", "V7", "date_trait", "regime", "resultat"))

setnames(mdr2016, 
         names(mdr2016),
         c("id", "patient", "sexe", "age", "structure", "region", "date_diag", "date_trait", "regime", "resultat"))

setnames(mdr2017, 
         names(mdr2017),
         c("id", "patient", "sexe", "age", "structure", "region", "date_diag", "date_trait", "regime"))

setnames(mdr2018,
         names(mdr2018),
         c("id", "patient", "sexe", "age", "structure", "region", "date_diag", "date_trait", "regime"))
#-------------------------------------------

#------------------------------------------
# Data cleaning
# make sure all datasets have the same variables
# add variable for datasource year
mdr2015$annee <- 2015
mdr2016$annee <- 2016
mdr2017$annee <- 2017
mdr2018$annee <- 2018

mdr2015 <- mdr2015[,c("V7"):=NULL]

mdr2017$resultat <- NA
mdr2018$resultat <- NA

mdr2015$date_diag <- NA

# merge
dt1 <- rbind(mdr2014, mdr2015)
dt2 <- rbind(dt1, mdr2016)
dt3 <- rbind(dt2, mdr2017)
dt4 <- rbind(dt3, mdr2018)

# fix typo in the data for mdr2016
dt4$age[which(dt4$sexe=="30 ans")] <- 30
dt4$sexe[which(dt4$sexe=="30 ans")] <- NA

# remove text from age from age
dt4$age <- gsub("ans","",dt4$age)
dt4$age <- gsub('a', '', dt4$age)
dt4$age <- gsub("1ns", "", dt4$age)
dt4$age[which(dt4$age=="11 mois")] <- 1
# remove whitespace from numbers
dt4[, age:= trimws(age)]

# change values to numeric
dt4[, age:=as.numeric(dt4$age)]

# change values to date
dt4[,date_diag:=as.Date(dt4$date_diag, "%m/%d/%Y")]
dt4[,date_trait:=as.Date(dt4$date_trait, "%m/%d/%Y")]
dt4[,sexe:=factor(dt4$sexe)]

# make region names consistent
dt4$region[which(dt4$region=="Dakar")] <- "DAKAR"
dt4$region[which(dt4$region=="Diourbel")] <- "DIOURBEL"
dt4$region[which(dt4$region=="Thies")] <- "THIES"
dt4$region[which(dt4$region=="Louga")] <- "LOUGA"
dt4$region[which(dt4$region=="Kaolack")] <- "KAOLACK"
dt4$region[which(dt4$region=="Kaffrine")] <- "KAFFRINE"
dt4$region[which(dt4$region=="Tambacounda")] <- "TAMBACOUNDA"
dt4$region[which(dt4$region=="Saint Louis")] <- "ST-LOUIS"
dt4$region[which(dt4$region=="RM Dakar")] <- "DAKAR"
dt4$region[which(dt4$region=="RM Diourbel")] <- "DIOURBEL"
dt4$region[which(dt4$region=="RM Thiès")] <- "THIES"
dt4$region[which(dt4$region=="RM Kolda")] <- "KOLDA"
dt4$region[which(dt4$region=="Thiès")] <- "THIES"
dt4$region[which(dt4$region=="Ziguinchor")] <- "ZIGUINCHOR"
dt4$region[which(dt4$region=="Sédhiou")] <- "SEDHIOU"
dt4$region[which(dt4$region=="Saint- Louis")] <- "ST-LOUIS"
dt4$region[which(dt4$region=="Matam")] <- "MATAM"
dt4$region[which(dt4$region=="Sedhiou")] <- "SEDHIOU"
dt4$region[which(dt4$region=="Fatick")] <- "FATICK"
dt4$region[which(dt4$region=="Dakar/Guinée")] <- "DAKAR"
dt4$region[which(dt4$region=="Kolda")] <- "KOLDA"
dt4$region[which(dt4$region=="")] <- NA

# make resultat consistent
dt4$resultat[which(dt4$resultat=="")] <- NA
dt4$resultat[which(dt4$resultat=="GUERI")] <- "Gueris"
dt4$resultat[which(dt4$resultat=="abandon")] <- "Abandon"
dt4$resultat[which(dt4$resultat=="DECEDE")] <- "Deces"
dt4$resultat[which(dt4$resultat=="Décés")] <- "Deces"
dt4$resultat[which(dt4$resultat=="ECHEC")] <- "Echec"

# make regime consistent
dt4$regime[which(dt4$regime=="")] <- NA
dt4$regime[which(dt4$regime=="long")] <- "Long"
dt4$regime[which(dt4$regime=="court")] <- "Court"
dt4$regime[which(dt4$regime=="COURT")] <- "Court"
dt4$regime[which(dt4$regime=="LONG")] <- "Long"
#------------------------------------------------


#------------------------------------------------
# Get counts using datatable

# Generate counts of MDR-TB cases diagnoses by region and quarter
dt4$count <- 1
mdr_tb_dx <- dt4[,.(region, date_diag, count)]
mdr_tb_tx <- dt4[,.(region, date_trait, count)]

#Split each data set out by quarter
mdr_tb_dx[, quarter:=quarter(date_diag)]
mdr_tb_dx[, year:=year(date_diag)]
mdr_tb_dx[, date_diag:=NULL]

mdr_tb_tx[, quarter:=quarter(date_trait)]
mdr_tb_tx[, year:=year(date_trait)]
mdr_tb_tx[, date_trait:=NULL]

#Create date variable
mdr_tb_dx[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
mdr_tb_dx[, date:=year+quarter]
mdr_tb_tx[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
mdr_tb_tx[, date:=year+quarter]

# sum data
mdr_tb_dx <- mdr_tb_dx[,.(mdr_tb_dx=sum(count)), by=c('region', 'date')]
mdr_tb_tx <- mdr_tb_tx[,.(mdr_tb_tx=sum(count)), by=c('region', 'date')]

# merge data
tb_mdr_data <- merge(mdr_tb_dx, mdr_tb_tx, by=c("region", "date"), all = TRUE)

# save file
setwd("J:/Project/Evaluation/GF/impact_evaluation/sen/prepped_data")
saveRDS(tb_mdr_data, file="prepped_tb_mdr_data.RDS")
