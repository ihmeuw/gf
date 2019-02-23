#----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Explore the raw data we have from SIGPRO 
#   and determine where files can be merged. 
# DATE: February 2019 
# ---------------------------------------------

rm(list=ls())
library(data.table) 

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro/')
raw_sigpro_dir <- paste0(dir, "raw_sigpro/")

# translation file
translate_data = fread(paste0(dir, "translation_of_HIV_variables.csv"), encoding = 'Latin-1')


#File Naomi was pulling in. 
sigpro_dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro/raw_sigpro/')
sigpro_file = 'SIGRPRO_masterlist.csv'
file_list = fread(paste0(sigpro_dir, sigpro_file))
test = file_list[type == "HIVTEST"]
testing_person_sigpro = data.table(read_excel(paste0(sigpro_dir, test$file_name[1]), sheet = test$sheet_name[1])) #EKL We're only reading in one file here - how are the others getting brought in? 


#New file I'm reviewing that might be helpful. 

testing_new = file_list[sheet_name == "sigpro_f4_JanNov2018 - PB_TVC"]
testing_sigpro = fread(paste0(sigpro_dir, testing_new$file_name[1]))

new_cols <- c('Age', 'Date', 'Gender', 'impresos', 'municode', )
shared_cols <- c('codanno', 'codDepMun', 'codejecutor', 'codgrupo', 'codigoActividad', 'codigoTipoActividad', 'codigounico', 'codmes', 'codResultadoSif', 'codResultadoVIH', 'codsubgrupo', 
                 'condonesFemeninos', 'condonesMasculinos', 'condonesSabores', 'conoceResultadosSif', 'conoceResultadoVIH', 'departamento', 'direccion', 'educador', 'fechareal', 'grupo', 'lubriSachet', 'lubriTubo', 
                 'lugar', 'municipio', '')
