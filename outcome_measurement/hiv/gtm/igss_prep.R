# ----------------------------------------------
# Caitlin O'Brien- Carelli
# Format IGSS data sets 

# ----------------------------------------------

#-----------------------------------------------
# Install packages 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(openxlsx)
library(ggplot2)
library(Hmisc)
library(stringr)
#---------------------------------------
# Set up directories 
#----------------------------------------

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')
setwd(paste0(dir, 'igss/'))

# to output prepped files
out_dir = paste0(dir, 'prepped/')

#-----------------------------------------

# --------------------
# read in the data 

# list existing files
files = list.files('./', recursive=TRUE)
length(files)
files

# read in the files
cohort = data.table(read.xlsx(paste0(dir, "igss/Cohorte activa con TAR a diciembre 2016 para CIESAR.docx (1).xlsx")))
drugs = data.table(read.xlsx(paste0(dir, "igss/Compras de Medicamentos año 2016 CIESAR.xlsx")))

#--------------------------
# prep cohort data 

# reset variable names
drug_names = tolower(names(cohort))
setnames(cohort, drug_names)

# translate some variable names
setnames(cohort, c('codigo.de.identificaciónde.los.pacientes','edad', 'sexo', 'departamento'), 
         c('patient_id', 'age', 'sex', 'department'))

#--------------------------
# reset values

# rename sex
cohort[sex=='M', sex:='Male']
cohort[sex=='F', sex:='Female']

# format departments and fix capitalization 
cohort[ ,department:=tolower(department)]

simpleCap = function(x) {
  s = strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep="", collapse=" ") }

cohort[ ,department:=sapply(department, simpleCap)]

# convert missing data to missing
cohort[department=='No dato', department:=NA]

# convert values to logicals
byvars = names(cohort)[1:4]
cohort = cohort[ ,lapply(.SD, function(x) x=='SI'), by=byvars, .SDcols = 5:12]

#--------------------------
# save the cohort data 

saveRDS(cohort, paste0(out_dir, 'igss_cohort_2016.rds' ))
#-------------------------------------------------------------

#-------------------------
# prep the drug data 

# fix variable names
setnames(drugs, c('year', 'drug', 'quantity',
                  'unit_cost', 'dependency')) # not clear what dependency means

# fix capitalization of drugs
drugs[ ,drug:=capitalize(tolower(drug))]
drugs[drug=='Lopinavir/ritonavir', drug:='Lopinavir/Ritonavir'] # two exceptions
drugs[drug=='Darunavir etanolato', drug:='Darunavir Etanolato']

# fix dependency capitalization
drugs[ ,dependency:=capitalize(tolower(dependency))]

# fix capitalization on these fize

drugs[ ,dependency:=str_to_title(dependency)]
drugs[ ,dependency:=gsub("De", "de", dependency)]

#-------------------------
# save the drug data 

saveRDS(drugs, paste0(out_dir, 'igss_drugs_2016.rds' ))
#-------------------------

