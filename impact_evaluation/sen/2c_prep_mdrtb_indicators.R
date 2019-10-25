# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas 
# PURPOSE: Clean MDR-TB Data from 2014-2018  
# DATE: Last updated 8/15/2019
# ----------------------------------------------------------

# set-up
source('./impact_evaluation/sen/set_up_r.r')

#---------------------------------------------------
# load datasets
mdr2014 <- fread(mdr2014data)
mdr2015 <- fread(mdr2015data, header = TRUE)
mdr2016 <- fread(mdr2016data, header = TRUE)
mdr2017 <- fread(mdr2017data, header = TRUE)
mdr2018 <- fread(mdr2018data, header = TRUE)

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

# clean up text in date columns
dt4$date_diag[which(dt4$date_diag=="NP")] <-NA
dt4$date_diag[which(dt4$date_diag=="NF")] <-NA
dt4$date_trait <- gsub("x/", "", dt4$date_trait)
dt4$date_trait <- gsub("X/", "", dt4$date_trait)
dt4$date_trait <- gsub("20141", "2014", dt4$date_trait)
dt4$date_trait <- gsub("07/2018", "07/01/2018", dt4$date_trait)
dt4$date_trait <- gsub("01/2016", "01/01/2018", dt4$date_trait)
dt4$date_trait <- gsub("1-Aug-17", "08/01/2018", dt4$date_trait)
dt4$date_trait <- gsub("1-août-18", "08/01/2018", dt4$date_trait)
dt4$date_trait <- gsub("17 fev 2017", "02/17/2017", dt4$date_trait)
dt4$date_trait <- gsub("3 fev 2017", "02/03/2017", dt4$date_trait)
dt4$date_trait <- gsub("3 fev 2017", "02/03/2017", dt4$date_trait)
dt4$date_trait <- gsub("31/08/208	", "08/31/2018", dt4$date_trait)
dt4$date_trait <- gsub("14-Apr-17", "04/17/2017", dt4$date_trait)
dt4$date_trait <- gsub("11-May-17", "05/11/2017", dt4$date_trait)
dt4$date_trait <- gsub("15-juil.-18", "07/15/2018", dt4$date_trait)
dt4$date_trait <- gsub("16-mars-18", "03/16/2018", dt4$date_trait)
dt4$date_trait <- gsub("16 fev 2017", "02/16/2018", dt4$date_trait)
dt4$date_trait <- gsub("16 fev 2017", "02/16/2018", dt4$date_trait)
dt4$date_trait <- gsub("16/072018", "16/07/2018", dt4$date_trait)
dt4$date_trait <- gsub("17-Mar-17", "03/17/2017", dt4$date_trait)
dt4$date_trait <- gsub("17-May-17", "05/17/2017", dt4$date_trait)
dt4$date_trait <- gsub("17 fev 2017", "02/17/2017", dt4$date_trait)
dt4$date_trait <- gsub("18-juil.-18", "07/18/2018", dt4$date_trait)
dt4$date_trait <- gsub("18-Sep-17", "09/18/2017", dt4$date_trait)
dt4$date_trait <- gsub("19-juil.-18", "07/19/2018", dt4$date_trait)
dt4$date_trait <- gsub("7-Mar-17", "03/07/2017", dt4$date_trait)
dt4$date_trait <- gsub("7-Jan-17", "07/07/2017", dt4$date_trait)
dt4$date_trait <- gsub("4-May-17", "07/07/2017", dt4$date_trait)
dt4$date_trait <- gsub("30-Jun-17", "07/07/2017", dt4$date_trait)
dt4$date_trait <- gsub("3-Mar-17", "03/03/2017", dt4$date_trait)
dt4$date_trait <- gsub("29-Aug-17", "08/29/2017", dt4$date_trait)
dt4$date_trait <- gsub("26/12 2015", "12/26/2015", dt4$date_trait)
dt4$date_trait <- gsub("26-juil.-18", "07/26/2018", dt4$date_trait)
dt4$date_trait <- gsub("25-May-17", "05/25/2017", dt4$date_trait)
dt4$date_trait <- gsub("24-Mar-17", "03/24/2017", dt4$date_trait)
dt4$date_trait <- gsub("24-juil.-18", "07/24/2018", dt4$date_trait)
dt4$date_trait <- gsub("23-Jan-17", "01/23/2017", dt4$date_trait)
dt4$date_trait <- gsub("22 fev 2017", "02/22/2017", dt4$date_trait)
dt4$date_trait <- gsub("22-May-17", "05/22/2017", dt4$date_trait)
dt4$date_trait <- gsub("202/03/2017", "02/03/2017", dt4$date_trait)
dt4$date_trait <- gsub("20-Mar-17", "03/20/2017", dt4$date_trait)
dt4$date_trait <- gsub("16/07/2018", "07/16/2018", dt4$date_trait)
dt4$date_trait <- gsub("11/004/2018", "11/04/2018", dt4$date_trait)
dt4$date_trait <- gsub("22-Jun-17", "06/22/2017", dt4$date_trait)
dt4$date_trait <- gsub("25-Aug-17", "08/25/2017", dt4$date_trait)
dt4$date_trait <- gsub("28-Aug-17", "08/28/2017", dt4$date_trait)
dt4$date_trait <- gsub(" ", "", dt4$date_trait)

# remove unnessary variables
dt4 <- dt4[,patient:=NULL]

# save file
saveRDS(dt4, outputFile2c)
archive(outputFile2c)
