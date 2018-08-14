# ----------------------------------------------
# Audrey Batzel
# 8/10/18
#
# make spreadsheet of differences in health zones in different data sources
#
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
## -------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(lubridate)
library(readxl)
library(stats)
library(tidyr)
library(dplyr)
library(openxlsx)
library(raster)
library(rgeos)
library(maptools)
# --------------------


# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_pnlp = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/')
dir_snis <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/dhis/all_units/")
dir_shape <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/health2/")

# input file
snis_data <- "master_facilities.rds"
input_pnlp <- "final_data_for_imputation.csv"
shapefile <- "health2.shp"

# output file

# ----------------------------------------------


# ----------------------------------------------
# read in the data

snis <- readRDS(paste0(dir_snis, snis_data))
pnlp <- read.csv(paste0(dir_pnlp, input_pnlp))
pnlp <- as.data.table(pnlp)
shp <- shapefile(paste0(dir_shape, shapefile))
shp <- as.data.table(shp@data)
# ----------------------------------------------


# ----------------------------------------------
# get the necessary variables - dps, health_zone and save original versions of these and a cleaned version to merge on
# ----------------------------------------------
# SHAPEFILE <- to standardize against

# get just the variables to use
shp1 <- shp[, .(PROVNAME, Name_API, Name, zs_id)]
# save a version of the names as they appear in the data
shp1$hz_shp1 <- shp1$Name
shp1$hz_shp2 <- shp1$Name_API
# use Name for health zone names bc it seems to be more complete;
# keep Name_API for alternative spellings
# standardize original names to be merged together
shp1$Name_API <- NULL
shp1$Name <- tolower(shp1$Name)
shp1$Name <- gsub(" ", "-", shp1$Name)
setnames(shp1, "Name", "health_zone")
setnames(shp1, "PROVNAME", "province_shp")
shp1$health_zone <- gsub("_", "-", shp1$health_zone)
# ----------------------------------------------
# PNLP

# get unique combos of health zone and dps names
pnlp <- unique(pnlp[, .(dps, health_zone)])
# save a version of the names as they appear in the data
pnlp$hz_pnlp <- pnlp$health_zone
pnlp$dps_pnlp <- pnlp$dps
# standardize original names to be merged together
pnlp$health_zone <- gsub(" ", "-", pnlp$health_zone)
pnlp$dps <- gsub(" ", "-", pnlp$dps)
# ----------------------------------------------
# SNIS (DHIS2)

# save a version of the names as they appear in the data
snis$hz_snis <- snis$health_zone
snis$dps_snis <- snis$dps
# standardize original names to be merged together
snis$dps <- sapply(str_split(snis$dps, " ", 2), '[', 2)
snis$dps <- gsub(" Province", "", snis$dps)
snis$dps <- tolower(snis$dps)
snis$dps <- gsub(" ", "-", snis$dps)

snis$health_zone <- sapply(str_split(snis$health_zone, " ", 2),'[', 2)
snis$health_zone <- gsub(" Zone de Santé", "", snis$health_zone)
snis$health_zone <- tolower(snis$health_zone)
snis$health_zone <- gsub(" ", "-", snis$health_zone)
# get unique combos of health zone and dps names
snis <- unique(snis[!is.na(dps) & !is.na(health_zone), .(dps, health_zone, dps_snis, hz_snis)])
# ----------------------------------------------


# ----------------------------------------------
# Merge to see where NAs are created (where there are differences)
pnlp_snis <- merge(snis, pnlp, by= c("health_zone", "dps"), all=TRUE)

# Will need to iteratively correct the variables to merge on to get them to be more similar
# this serves as a record of changes made/differences *NOTE change isn't made to original names
# just the one we are merging on to get a complete set of names for each "actual" health zone
snis[dps=="maindombe", dps:= "mai-ndombe"]  
pnlp[dps=="bas-congo", dps:= "kongo-central"] 

pnlp[health_zone=="benatshiadi", health_zone:= "bena-tshadi"]
pnlp[health_zone=="binza-m", health_zone:= "binza-meteo"]
pnlp[health_zone=="binza-o", health_zone:= "binza-ozone"]

pnlp[health_zone=="bandal", health_zone:= "bandalungwa"]
snis[health_zone=="bipemba", health_zone:= "bimpemba"]
pnlp[health_zone=="bogosenusebea", health_zone:= "bogosenubea"]

pnlp[health_zone=="bongandanganda", health_zone:= "bongandanga"]
snis[health_zone=="boso-manzi", health_zone:= "bosomanzi"]
snis[health_zone=="boso-mondanda", health_zone:= "bosomondanda"]

snis[health_zone=="busanga", health_zone:= "bosanga"]
snis[health_zone=="gety", health_zone:= "gethy"]
pnlp[health_zone=="kabond-dianda", health_zone:= "kabondo-dianda"]

pnlp[health_zone=="kalemi", health_zone:= "kalemie"]
pnlp[health_zone=="kalambayi", health_zone:= "kalambayi-kabanga"]
pnlp[health_zone=="kamji", health_zone:= "kamiji"]

pnlp[health_zone=="kanyama", health_zone:= "kaniama"]
pnlp[health_zone=="kasa-vub", health_zone:= "kasa-vubu"]
pnlp[health_zone=="kata-kokombe", health_zone:= "katako-kombe"]

pnlp[health_zone=="kisandji", health_zone:= "kisanji"]
pnlp[health_zone=="kitangua", health_zone:= "kitangwa"]
pnlp[health_zone=="kiyambi", health_zone:= "kiambi"]

pnlp[health_zone=="kwimba", health_zone:= "kuimba"]
pnlp[health_zone=="lilanga-bobanga", health_zone:= "lilanga-bobangi"]
pnlp[health_zone=="lwalaba", health_zone:= "lualaba"]

pnlp[health_zone=="makiso-k", health_zone:= "makiso-kisangani"]
pnlp[health_zone=="maluku1", health_zone:= "maluku-1"]
pnlp[health_zone=="maluku2", health_zone:= "maluku-2"]

pnlp[health_zone=="masimanimba", health_zone:= "masi-manimba"]
pnlp[health_zone=="masina1", health_zone:= "masina-1"]
pnlp[health_zone=="masina2", health_zone:= "masina-2"]

pnlp[health_zone=="masa", health_zone:= "massa"]
pnlp[health_zone=="mont-nga1", health_zone:= "mont-ngafula-1"]
pnlp[health_zone=="mont-nga-2", health_zone:= "mont-ngafula-2"]

snis[health_zone=="muetshi", health_zone:= "mwetshi"]
pnlp[health_zone=="ntandembele", health_zone:= "ntandembelo"]
pnlp[health_zone=="nyakunde", health_zone:= "nyankunde"]

snis[health_zone=="omendjadi", health_zone:= "omondjadi"]
pnlp[health_zone=="rwashi", health_zone:= "ruashi"]
pnlp[health_zone=="saramabil", health_zone:= "saramabila"]

pnlp[health_zone=="sakanya", health_zone:= "sakania"]
pnlp[health_zone=="tshomia", health_zone:= "tchomia"]
snis[health_zone=="tshishimbi", health_zone:= "tshitshimbi"]

pnlp[health_zone=="wamba-luadi", health_zone:= "wamba-lwadi"] 

#visualize where still not matching up
not_matching <-pnlp_snis[!complete.cases(dps_snis, hz_snis, hz_pnlp, dps_pnlp), ]

pnlp[health_zone=="muene-ditu", health_zone:= "mweneditu"]
pnlp[health_zone=="lulenge", health_zone:= "kimbi-lulenge"]
pnlp[health_zone=="gandajika", health_zone:= "ngandajika"]

#less sure about these....
snis[health_zone=="citenge", health_zone:= "tshitenge"]
pnlp[health_zone=="lolanga-mampoko", health_zone:= "mampoko"]
pnlp[health_zone=="bandjow", health_zone:= "banzow-moke"] 

# re-merge to get best matched result:
pnlp_snis <- merge(snis, pnlp, by= c("health_zone", "dps"), all=TRUE)

# merge pnlp_snis merged file with the shapefile
shp_pnlp_snis <- merge(shp1, pnlp_snis, by= c("health_zone"), all=TRUE)
not_matching2 <-shp_pnlp_snis[!complete.cases(hz_shp1, dps_snis, hz_snis, hz_pnlp, dps_pnlp), ]

# the shapefile actually contains multiple spellings (hz_shp1 and hz_shp2) 
# using hz_shp1 since it is more complete than hz_shp2:
pnlp_snis[health_zone=="adja", health_zone:="adia"]
pnlp_snis[health_zone=="bafwagbogbo", health_zone:="bafwabogbo"]
pnlp_snis[health_zone=="bagira", health_zone:="bagira-kasha"]

pnlp_snis[health_zone=="befale", health_zone:="bafale"]
pnlp_snis[health_zone=="bena-leka", health_zone:="bena-leke"]
pnlp_snis[health_zone=="bilomba", health_zone:="bilonda"]

pnlp_snis[health_zone=="banzow-moke", health_zone:="bandjau"]
pnlp_snis[health_zone=="binza-meteo", health_zone:="binza-metéo"]
pnlp_snis[health_zone=="bolenge", health_zone:="bolonge"]

pnlp_snis[health_zone=="damas", health_zone:="damasi"]
pnlp_snis[health_zone=="dikungu", health_zone:="dikungu-tshumbe"]
pnlp_snis[health_zone=="kabeya-kamwanga", health_zone:="kabeya-kamuanga"]
pnlp_snis[health_zone=="kalamu-1", health_zone:="kalamu-i"]
pnlp_snis[health_zone=="kalamu-2", health_zone:="kalamu-ii"]
pnlp_snis[health_zone=="kamonia", health_zone:="kamonya"]

pnlp_snis[health_zone=="kinkondja", health_zone:="kinkonja"]
pnlp_snis[health_zone=="kirotshe", health_zone:="kirotse"]
pnlp_snis[health_zone=="kisanji", health_zone:="kisandji"]
pnlp_snis[health_zone=="limete", health_zone:="limeté"]
pnlp_snis[health_zone=="maluku-1", health_zone:="maluku-i"]
pnlp_snis[health_zone=="maluku-2", health_zone:="maluku-ii"]

pnlp_snis[health_zone=="kimbao", health_zone:="kimbau"]
pnlp_snis[health_zone=="masina-1", health_zone:="masina-i"]
pnlp_snis[health_zone=="masina-2", health_zone:="masina-ii"]
pnlp_snis[health_zone=="mbulula", health_zone:="mbulala"]
pnlp_snis[health_zone=="monieka", health_zone:="monika"]
pnlp_snis[health_zone=="mutena", health_zone:="mutenna"]

pnlp_snis[health_zone=="nia-nia", health_zone:="niania"]
pnlp_snis[health_zone=="mweneditu", health_zone:="mwene-ditu"]
pnlp_snis[health_zone=="mwela-lembwa", health_zone:="mwela-lemba"]
pnlp_snis[health_zone=="mont-ngafula-1", health_zone:="mont-ngafula-i"]
pnlp_snis[health_zone=="mont-ngafula-2", health_zone:="mont-ngafula-ii"]
pnlp_snis[health_zone=="nyankunde", health_zone:="nyakunde"]

pnlp_snis[health_zone=="saramabila", health_zone:="salamabila"]
pnlp_snis[health_zone=="pendjwa", health_zone:="penzwa"]
pnlp_snis[health_zone=="nsona-mpangu", health_zone:="sona-pangu"]
pnlp_snis[health_zone=="tunda", health_zone:="tumba"]
pnlp_snis[health_zone=="viadana", health_zone:="viandana"]
pnlp_snis[health_zone=="vuhovi", health_zone:="vohovi"]

shp1[health_zone=="zs-kamalondo", health_zone:="kamalondo"]
pnlp_snis[health_zone=="nyantende", health_zone:="nyatende"]
pnlp_snis[health_zone=="yasa-bonga", health_zone:="yanga-bosa"]
pnlp_snis[health_zone=="wikong", health_zone:="winkong"]
pnlp_snis[health_zone=="wanierukula", health_zone:="wanie-rukula"]
pnlp_snis[health_zone=="yabaondo", health_zone:="yabahondo"]

pnlp_snis[health_zone=="bogosenubea", health_zone:="bongosenubia"]
pnlp_snis[health_zone=="bunkonde", health_zone:="bukonde"]
pnlp_snis[health_zone=="kalamba", health_zone:="kalemba"]
pnlp_snis[health_zone=="kalehe", health_zone:="kahele"]
pnlp_snis[health_zone=="kalole", health_zone:="kakole"]
pnlp_snis[health_zone=="kanda-kanda", health_zone:="kandakanda"]

pnlp_snis[health_zone=="yalifafu", health_zone:="yalifafo"]
pnlp_snis[health_zone=="yahisuli", health_zone:="yahisule"]
pnlp_snis[health_zone=="ndjoko-mpunda", health_zone:="ndjoko-punda"]
pnlp_snis[health_zone=="masuika", health_zone:="musuika"]
pnlp_snis[health_zone=="muanda", health_zone:="moanda"]
pnlp_snis[health_zone=="moanza", health_zone:="moaza"]

pnlp_snis[health_zone=="kitenda", health_zone:="kikenda"]
pnlp_snis[health_zone=="kansimba", health_zone:="kasimba"]
pnlp_snis[health_zone=="kadutu", health_zone:="kandutu"]
pnlp_snis[health_zone=="baka", health_zone:="kamina-base"]
pnlp_snis[health_zone=="cilundu", health_zone:="tshilundu"]
pnlp_snis[health_zone=="makanza", health_zone:="mankanza"]

pnlp_snis[health_zone=="ntandembelo", health_zone:="tandembele"]
pnlp_snis[health_zone=="kapolowe", health_zone:="kapolobwe"]
pnlp_snis[health_zone=="katoyi", health_zone:="kitoyi"]
pnlp_snis[health_zone=="kalonda-ouest", health_zone:="kalonda"]
pnlp_snis[health_zone=="kalomba", health_zone:="kalomda"]
pnlp_snis[health_zone=="adia", health_zone:="adja"]

pnlp_snis[health_zone=="adia", health_zone:="adja"]
pnlp_snis[health_zone=="adia", health_zone:="adja"]
pnlp_snis[health_zone=="adia", health_zone:="adja"]
pnlp_snis[health_zone=="adia", health_zone:="adja"]
pnlp_snis[health_zone=="adia", health_zone:="adja"]
pnlp_snis[health_zone=="adia", health_zone:="adja"]

shp_pnlp_snis <- merge(shp1, pnlp_snis, by= c("health_zone"), all=TRUE)
not_matching2 <-shp_pnlp_snis[!complete.cases(hz_shp1, dps_snis, hz_snis, hz_pnlp, dps_pnlp), ]







