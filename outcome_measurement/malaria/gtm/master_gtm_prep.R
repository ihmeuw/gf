# ----------------------------------------------
# Audrey Batzel
# 4/5/18
# Master prep file for GTM Malaria supply chain data 
# ----------------------------------------------
  ###### Set up R / install packages  ###### 
# ----------------------------------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(lubridate)
library(readxl)
library(stats)
library(rlang)
library(zoo)
library(readr)

# ----------------------------------------------
###### Call directories and source the functions  ###### 
# ----------------------------------------------
# file path where the files are stored
local_dir <- "J:/Project/Evaluation/GF/outcome_measurement/gtm/"
antimalarials <- "J:/Project/Evaluation/GF/outcome_measurement/gtm/MALARIA/"
bed_nets <- paste0(antimalarials, "Distribucion de MTILD/")


#prep_dir <- " your local repo + gf/outcome_measurement/malaria/gtm/"
prep_dir <- "H:/gf/outcome_measurement/malaria/gtm/"
source(paste0(prep_dir, "prep_antimalarial_drugs.R"))
source(paste0(prep_dir, "prep_bed_nets.R"))

# ----------------------------------------------
  ###### Load the prep file  ###### 
# ----------------------------------------------
file_list <- data.table(read.csv(paste0(antimalarials, "prep_file_list.csv"), fileEncoding = "latin1", stringsAsFactors = FALSE))
file_list$start_date <- ymd(file_list$start_date)
file_list$file_name <- as.character(file_list$file_name)

# ----------------------------------------------
######For loop that appends each data file to our databese  ###### 
# ----------------------------------------------
for(i in 1:length(file_list$file_name)){
  if(file_list$type[i]=="antimal_drug"){
    antimalData <- prep_antimal_data(paste0(antimalarials, file_list$file_name[i]), file_list$sheet[i]
                               , ymd(file_list$start_date[i]), file_list$period[i])
      if(i==1){
        antimal_database <- antimalData
      } else {
        antimal_database <- rbind(antimalData, antimal_database)
      }
  } else {
    #if(ymd(file_list$start_date[i]) == ymd("2015-01-01")){ this was just used for testing
    bnData <- prep_gtm_bed_nets(paste0(bed_nets, file_list$file_name[i]),
                                file_list$start_date[i], file_list$period[i])
    bnData$DB_Year = file_list$start_date[i]
    #if(!(exists('bn_database')&& is.data.frame(get('bn_database')))){
      #bn_database <- copy(bnData)
      if(file_list$type[i]=="bed_nets" && file_list$type[i-1] == "antimal_drug"){ #ymd(file_list$start_date[i-1]) == ymd("2016-01-01")
        bn_database <- bnData
      } else {
        bn_database <- rbind(bnData, bn_database)
      }
    #}
  }
   # if(any(bnData$regional_code==0)){
   #     stop("Stop! No Region!")
   #  }
  print(i)
}

#write.csv(bn_database, "J:/temp/ninip/bn_database_tri.csv")
bn_database = unique(bn_database)

antimal_database$amount <- as.numeric(antimal_database$amount)

# ----------------------------------------------
###### Add more variables that track indicators: 
# ----------------------------------------------

##function to create "drug type" 
get_antimalarial_types <- function(antimal){
  x <- "Antimoniato 10 ml"
  if(grepl("Cloroquina", antimal)){
    x <- "Cloroquina 250 mg"
  } else if (grepl("Primaquina15", antimal)){
    x <- "Primaquina 15 mg"
  } else if (grepl("Primaquina5",antimal)){
    x <- "Primaquina 5 mg"
  } else {
    x <- x
  }
  return(x)
}


## vector dictionary of special characters to regular characters
unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

##get rid of special characters in the dataset (necessary if we want to map these to the shapefiles)
antimal_database$department <- gsub("\\*", "",antimal_database$department) 
antimal_database$department <- trimws(antimal_database$department, "r")

##function to get rid of the special characters and standardize the departments to the shapefiles that we have 
standardize_depts <- function(department){
  x <- department
  if(grepl("Guat", x)){
    x <- "Guatemala"
  } else if (grepl("Peten", x)){
    x <- "Peten"
  } else {
    x <- x
  }
  return(x)
}


antimal_database$drug_type <- mapply(get_antimalarial_types, antimal_database$antimalarial_input)

antimal_database$department <- chartr(paste(names(unwanted_array), collapse=''),
                             paste(unwanted_array, collapse=''),
                             antimal_database$department)
antimal_database$standardized_dept <- mapply(standardize_depts, antimal_database$department)



# ----------------------------------------------
## attach department names to the bed net data
# ----------------------------------------------
dept_muni_names <- data.table(read.csv(paste0(antimalarials, "department_and_municipality_names.csv")))

setnames(dept_muni_names, c("CodReg", "CodDepto", "CodMuni", "Municipio", "Departamento"), 
         c("regional_code", "adm1", "adm2", "municipality","department"))

##check for any municipalities in the BD data that aren't in the CSV file: 
bn_database[!adm2%in%dept_muni_names$adm2]

##merge the two datasets on the codes: 
totalBNData <- merge(bn_database, dept_muni_names, all.x=TRUE, by=c("regional_code", "adm1", "adm2"))

# ----------------------------------------------
##export as CSV 
# ----------------------------------------------
write.csv(antimal_database, paste0(local_dir, "prepped_data/", "antimalarial_prepped_data.csv"))
write.csv(totalBNData, paste0(local_dir, "prepped_data/", "bednet_prepped_data.csv"))
  