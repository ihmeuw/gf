# ----------------------------------------------
# Emily Linebarger, based on code by Naomi Provost and Caitlin O'Brien Carelli
# February 2019
# Master code file for GTM HIV data cleaning 
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
rm(list=ls())
library(data.table)
library(readxl)

#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#--------------------------------------------
# Set directories 
#--------------------------------------------
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro')
prep_dir <- paste0(dir, '/raw_sigpro')
save_loc <- paste0(dir, '/prepped_data')

#----------------------------------------------
# fread is resulting in some serious warnings that alter the data sets
# take those warnings seriously! 

# upload the data sets using read.csv
#---------------------------------------------


sigpro_aa = data.table(read.csv(paste0(prep_dir, "/sigpro_f4_JanNov2018 - AA.csv"), stringsAsFactors = F))
sigpro_its = data.table(read.csv(paste0(prep_dir, "/sigpro_f4_JanNov2018 - ITS.csv"), stringsAsFactors = F))
sigpro_pb_tvc = data.table(read.csv(paste0(prep_dir, "/sigpro_f4_JanNov2018 - PB_TVC.csv"), stringsAsFactors = F))
sigpro_psico = data.table(read.csv(paste0(prep_dir, "/sigpro_f4_JanNov2018 - Psico.csv"), stringsAsFactors = F))
sigpro_links = data.table(read.csv(paste0(prep_dir, "/sigpro_f4_JanNov2018 - Vinculacion.csv"), stringsAsFactors = F))

# generate a variable in each that identifies the data set it came from
sigpro_aa[ ,set:='aa']
sigpro_its[ ,set:='its']
sigpro_pb_tvc[, set:='pbtvc']
sigpro_psico[, set:='psico']
sigpro_links[, set:='links']


#-----------------------------------------------------
# Review data before merging/binding 
#-----------------------------------------------------
names(sigpro_its) = tolower(names(sigpro_its))
names(sigpro_pb_tvc) = tolower(names(sigpro_pb_tvc))

sigpro_its = sigpro_its[,-c('x')] 
sigpro_pb_tvc = sigpro_pb_tvc[,-c('x')]
sigpro_psico = sigpro_psico[,-c('x')]

#Subset to the columns that would be useful to analyze. 
sigpro_its = sigpro_its[, .(codigounico, age, gender, date, grupo, subgrupo, tema, lugar, departamento, municipio, codigoactividad, diagnostico, tratamiento,
                            condonesmasculinos, condonesfemeninos, condonessabores, lubrisachet, lubritubo, tipoactividad, tratado)]
sigpro_pb_tvc = sigpro_pb_tvc[, .(codigounico, age, gender, date, grupo, subgrupo, tema, lugar, departamento, municipio, 
                                  codigoactividad, refervih, prepruebavih, pruebavih, postpruebavih, conoceresultadovih, conoceresultadosif, 
                                  codresultadosif, condonesmasculinos, condonesfemeninos, condonessabores, lubrisachet, lubritubo, impresos, resultadovih, tipoactividad, 
                                  resultadosif, educador, fechareal, pqbasico)]
sigpro_psico = sigpro_psico[, .(codigounico, age, gender, date, grupo, subgrupo, tema, lugar, departamento, municipio, 
                                codigoactividad, condonesmasculinos, condonessabores, lubrisachet, codproyecto, codejecutor, fechadiagnostico, 
                                fechaconfirmacion, fechasesion, nosesion, motivoatencion, responsable)]

#Try merging the data together 
dt = merge(sigpro_pb_tvc, sigpro_its, by = c('codigounico', 'age', 'gender', 'date', 'grupo'), suffixes = c(".pbtvc", ".its"), all = TRUE)


#Review variables between two datasets 
#dt[grupo.its != grupo.pbtvc] #Group is always the same between these two. 
dt[subgrupo.its != subgrupo.pbtvc] #There are 8 cases here. 
dt[tema.its != tema.pbtvc] #These are different between the two datasets. 
dt[departamento.its != departamento.pbtvc] #These are always the same. 

#Fix date variable 
dt[ , date:=as.Date(date, '%Y-%m-%d')]

#Decided to not go with rbinding because the number of variables was making reshape difficult. 
# # rbind the data sets together if they have the same columns
# l = list(sigpro_aa, sigpro_its, sigpro_pb_tvc, sigpro_psico, sigpro_links)
# dt = data.table(rbindlist(l, use.names = TRUE, fill = TRUE))
# str(dt) # check which types of variables you have 
# 
# # date is not uploaded as a date variable
# dt[ , Date:=as.Date(Date, '%Y-%m-%d')]
# setnames(dt, names(dt), tolower(names(dt))) 
# 
# #Drop unnecessary variables 
# dt = dt[,-c('x')]

#------------------------------------------------------
# Show descriptive statistics 
#------------------------------------------------------
#Date range by dataset 
dt[ ,range(date, na.rm=T)]

#Number of unique identifiers by dataset
dt[, length(unique(codigounico))]

#------------------------------------------------------
# Data quality concerns 
#------------------------------------------------------
dt[is.na(date)] # there are 22 missing dates 
dt[is.na(codigounico)] #No missing IDs here. 

#------------------------------------------------------
#Reshape data wide by unique code and date 
#------------------------------------------------------

# > names(dt)
# [1] "numeroinforme"       "codigoactividad"     "codigounico"         "codgrupo"            "codsubgrupo"         "coddiag"             "tratado"            
# [8] "condonesmasculinos"  "condonesfemeninos"   "condonessabores"     "lubrisachet"         "lubritubo"           "comentario"          "grupo"              
# [15] "subgrupo"            "diagnostico"         "tratamiento"         "codproyecto"         "codejecutor"         "codanno"             "codmes"             
# [22] "lugar"               "direccion"           "responsable"         "unmovil"             "codmunidep"          "codigotipoactividad" "tipoactividad"      
# [29] "departamento"        "municipio"           "tema"                "date"                "gender"              "age"                 "municode"           
# [36] "set"                 "refervih"            "prepruebavih"        "pruebavih"           "postpruebavih"       "conoceresultadovih"  "codresultadovih"    
# [43] "conoceresultadosif"  "codresultadosif"     "impresos"            "resultadovih"        "resultadosif"        "coddepmun"           "educador"           
# [50] "fechareal"           "pqbasico"            "numerocarnet"        "fechadiagnostico"    "fechaconfirmacion"   "codmotivo"           "fechasesion"        
# [57] "nosesion"            "motivoatencion"      "codigouai"           "uai"  


#Only picking a subset of reshape variables, because some are codes of the same string. 
# dt_reshape = melt(dt, id.vars = c('date', 'codigounico', 'age', 'gender'))
# dt_reshape = dcast(dt_reshape, date+codigounico+age+gender ~ ...)


#------------------------------------------------------
# Write data to .rds file 
#------------------------------------------------------

saveRDS(dt, paste0(save_loc, "/sigpro_wide.rds"))
