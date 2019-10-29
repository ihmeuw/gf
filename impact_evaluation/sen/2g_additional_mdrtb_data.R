# script to clean up data obtaine don genexpert tests in Senegal
# francisco rios casas
# added on October 25, 2019



# set-up
source('./impact_evaluation/sen/set_up_r.r')
DT <- fread("J:/Project/Evaluation/GF/impact_evaluation/sen/raw_data/data_genexpert_tests_senegal.csv", encoding = "UTF-8")

# delete unecessary columns
DT[,c('organisationunitid', 'organisationunitcode', 'organisationunitdescription', 'periodid', 'periodname', 'perioddescription'):=NULL]

# change names
setnames(DT,
         names(DT),
         c("region", "date", "patients_prop_genexpert"))

DT[,.(print(region),plot(patients_prop_genexpert))]

# clean region names
DT$region <- gsub("RM ", "", DT$region)
DT$region[which(DT$region=="Dakar")]<-"DAKAR"
DT$region[which(DT$region=="Diourbel")]<-"DIOURBEL"
DT$region[which(DT$region=="Fatick")]<-"FATICK"
DT$region[which(DT$region=="Kaffrine")]<-"KAFFRINE"
DT$region[which(DT$region=="Kaolack")]<-"KAOLACK"
DT$region[which(DT$region=="Kedougou")]<-"KEDOUGOU"
DT$region[which(DT$region=="Kolda")]<-"KOLDA"
DT$region[which(DT$region=="Louga")]<-"LOUGA"
DT$region[which(DT$region=="Matam")]<-"MATAM"
DT$region[which(DT$region=="Saint-Louis")]<-"ST-LOUIS"
DT$region[which(DT$region=="Sedhiou")]<-"SEDHIOU"
DT$region[which(DT$region=="Tambacounda")]<-"TAMBACOUNDA"
DT$region[which(DT$region=="Thies")]<-"THIES"
DT$region[which(DT$region=="Ziguinchor")]<-"ZIGUINCHOR"

# clean 
DT$date <- gsub("Q1", ".00", DT$date)
DT$date <- gsub("Q2", ".25", DT$date)
DT$date <- gsub("Q3", ".50", DT$date)
DT$date <- gsub("Q4", ".75", DT$date)

DT <- na.omit(DT)

# change class
DT$date <- as.numeric(DT$date)

# save data in prepped data file
saveRDS(DT, outputFile2g)
