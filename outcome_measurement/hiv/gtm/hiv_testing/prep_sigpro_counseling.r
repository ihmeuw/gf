# ----------------------------------------------
# Naomi Provost, updated by Emily Linebarger 
# Last updated: February 2019
# Combine patient level SIGSA and SIGPRO data 
# ----------------------------------------------

#-----------------------------------------------
# Install packages 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)

#---------------------------------------
# Set up directories 
#----------------------------------------
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro/')
raw_dir = paste0(dir, "raw_sigpro/")
output_dir <- paste0(dir, 'prepped_data/')

#-----------------------------------------
# Read in raw files 
#-----------------------------------------
couns_2018 = fread(paste0(raw_dir, "sigpro_f4_JanNov2018 - PB_TVC.csv"))
couns_2016 = data.table(read_excel(paste0(raw_dir, "/sigpro_f3_completo 2018.xlsx"), sheet = 'PaqueteExtendido'))
couns_2014 = data.table(read_excel(paste0(raw_dir, "/sigpro_f2_completo 2018.xlsx"), sheet = 'Pruebas'))
couns_2012 = data.table(read_excel(paste0(raw_dir, "/sigpro_f1_completo 2018.xlsx"), sheet = 'TVC'))

#----------------------------------------------------
# Correct errors in individual files before merging
#----------------------------------------------------

#Rename columns 
names(couns_2018) = tolower(names(couns_2018))
setnames(couns_2018, old = c('codigoactividad', 'codigounico', 'codgrupo', 'codsubgrupo', 'refervih', 'prepruebavih', 'pruebavih', 'postpruebavih', 
                             'conoceresultadovih', 'codresultadovih', 'condonesmasculinos', 'condonesfemeninos', 
                             'condonessabores', 'lubrisachet', 'lubritubo', 'impresos', 'grupo', 'subgrupo', 'resultadovih', 'codigotipoactividad', 'tipoactividad', 
                             'resultadosif', 'coddepmun', 'lugar', 'direccion', 'educador', 'fechareal', 'departamento', 'municipio', 'tema', 'municode', 'responsable', 
                             'numeroinforme', 'conoceresultadosif', 'codresultadosif'),
         new = c('activity_code', 'unique_id', 'group_code', 'subgroup_code', 'hiv_referral', 'pre_hiv_test_couns', 'hiv_test_couns', 'post_hiv_test_couns', 
                 'hiv_results_known', 'hiv_result_code', 'male_condoms', 'female_condoms', 'flavored_condoms', 'lubricant_packets', 'lubricant_tubes', 'flyers', 
                 'group', 'subgroup', 'hiv_result', 'activity_type_code', 'activity_type', 'sifilis_result', 'code_dep_mun', 'location', 'address', 'counselor', 
                 'real_date', 'department', 'municipality', 'theme', 'municipality_code', 'supervisor', 'report_number', 'sifilis_result_known', 'sifilis_result_code'))

names(couns_2016) = tolower(names(couns_2016))
setnames(couns_2016, old = c('codigoactividad', 'codigounico', 'codgrupo', 'codsubgrupo', 'prepruebavih', 'pruebavih', 'postpruebavih', 
                             'conoceresultadovih', 'codresultadovih', 'condonesmasculinos', 'condonesfemeninos', 
                             'condonessabores', 'lubrisachet', 'lubritubo', 'grupo', 'subgrupo', 'resultadovih', 'codigotipoactividad', 'tipoactividad', 
                             'resultadosif', 'coddepmun', 'lugar', 'direccion', 'educador', 'fechareal', 'departamento', 'municipio', 'tema', 'responsable', 
                             'numeroinforme', 'conoceresultadosif', 'codresultadosif'),
         new = c('activity_code', 'unique_id', 'group_code', 'subgroup_code', 'pre_hiv_test_couns', 'hiv_test_couns', 'post_hiv_test_couns', 
                 'hiv_results_known', 'hiv_result_code', 'male_condoms', 'female_condoms', 'flavored_condoms', 'lubricant_packets', 'lubricant_tubes', 
                 'group', 'subgroup', 'hiv_result', 'activity_type_code', 'activity_type', 'sifilis_result', 'code_dep_mun', 'location', 'address', 'counselor', 
                 'real_date', 'department', 'municipality', 'theme', 'supervisor', 'report_number', 'sifilis_result_known', 'sifilis_result_code'))

names(couns_2014) = tolower(names(couns_2014))
setnames(couns_2014, old = c('codgrupo', 'codsubgrupo', 'condonesmasculinosentregados', 'condonesfemeninosentregados', 'lubricantestuboentregados', 'lubricantessachetentregados', 
                             'impresosentregados', 'fechaprueba', 'realizoprueba', 'realizopreprueba', 'realizoposprueba', 'codigoresultado', 'conoceresultado', 
                             'referido', 'grupo', 'subgrupo', 'numeroinforme'), 
         new = c('group_code', 'subgroup_code', 'male_condoms', 'female_condoms', 'lubricant_tubes', 'lubricant_packets', 'flyers', 'real_date', 'hiv_test_couns', 
                 'pre_hiv_test_couns', 'post_hiv_test_couns', 'hiv_result_code', 'hiv_results_known', 'hiv_referral', 'group', 'subgroup', 'report_number'))

names(couns_2012) = tolower(names(couns_2012))
setnames(couns_2012, old = c('genero', 'poblacion', 'numeroinforme', 'grupoedad'), new = c('gender', 'group', 'report_number', 'age_group'))


#Format data consistently across sources 
couns_2012[gender=='Hombres' | gender=='NinosExpuestos' | gender== 'Ninos', gender:='M']
couns_2012[gender=='Mujeres' | gender == 'NinasExpuestas' | gender == 'Ninas', gender:='F'] 
couns_2012[gender == 'Trans', gender:='T'] 

couns_2012[diagnostico == "SÃfilis(Positivo)" | diagnostico == "Sifilis(Positivo)", sifilis_result:='REACTIVO'] #EKL make sure this character match is working! 
couns_2012[diagnostico == "Sifilis(Negativo)", sifilis_result:='NO REACTIVO'] #Emily keep working here to make sure all of these diagnostic codes are classified! 
couns_2012[diagnostico == "VIH(Positivo)", hiv_result:='REACTIVO'] #Emily keep working here to make sure all of these diagnostic codes are classified! 
couns_2012[diagnostico == "VIH(Negativo)", hiv_result:='NO REACTIVO'] #Emily keep working here to make sure all of these diagnostic codes are classified! 
couns_2012[diagnostico == "VIH(Presuntivo)", hiv_result:='INDETERMINADO'] #Emily keep working here to make sure all of these diagnostic codes are classified! 


#--------------------------------------------
# Validate input data 
#-------------------------------------------


#SIF is sifilis! 
names18 = names(couns_2018)
names16 = names(couns_2016)
names14 = names(couns_2014)
names12 = names(couns_2012) 

setdiff(names16, names18)
setdiff(names14, names18)
setdiff(names12, names18)

create_age_group = function(dt, age_col){
  dt[age_col>=15 & age_col < 25, age_group:="15-24"]
  dt[age_col>=25 & age_col < 50, age_group:="25-49"] 
}

#-------------------------------------------------------------
# Drop non-matching columns before appending. 
# Just make sure you've formatted all data correctly first! 
#-------------------------------------------------------------
couns_2018 = couns_2018[, -c('V1')]
couns_2016 = couns_2016[, -c('x__1')]

#-------------------------------------------
# Bind files together 
#-------------------------------------------
# bind_all <- function(x) rbind(x, fill = TRUE)
# all_couns = do.call(bind_all, list(couns_2018, couns_2016, couns_2014, couns_2012))
# setDT(all_couns)

all_couns = rbind(couns_2018, couns_2016, fill = TRUE) 
all_couns = rbind(all_couns, couns_2014, fill = TRUE) 
all_couns = rbind(all_couns, couns_2012, fill = TRUE) 
#-------------------------------------------
# Generate variables 
#-------------------------------------------

# Fixing input errors in the input files
all_couns$real_date = ifelse(all_couns$theme == "San Benito", as.Date(as.numeric(all_couns$department),  origin = "1899-12-30"), as.Date(all_couns$real_date))
all_couns$real_date = as.Date(as.numeric(all_couns$real_date),  origin = "1969-12-30")
all_couns$department = ifelse(all_couns$theme == "San Benito", "Peten", all_couns$department)
all_couns$municipality = ifelse(all_couns$theme == "San Benito", "San Benito", all_couns$municipality)


# calculate age based on birthday
all_couns$DOB = substr(all_couns$unique_id, 2, 7)
all_couns$DOB = as.Date(all_couns$DOB, "%d%m%y")
year(all_couns$DOB) = ifelse(year(all_couns$DOB) > 2018, year(all_couns$DOB) - 100, year(all_couns$DOB))
all_couns$age <-  as.integer((as.Date(all_couns$real_date) - all_couns$DOB) / 365)

all_couns$sex = substr(all_couns$unique_id, 1, 1)
all_couns[is.na(sex), sex:=gender]
all_couns$sexual_orientation = "undetermined"

sigpro_data = all_couns[,c("real_date", "unique_id", "pre_hiv_test_couns", "pruebaVIH", "resultadoVIH", "subgrupo", "sexual_orientation", "age", "sex", "municipality", "department", 'lugar')]
names(sigpro_data) = c("date", "identifier", "pre_orientaiton_test", "completed_hiv_screening_test", "hiv_screening_result", "risk_condition_eng", "sexual_orientation", "age", "sex", "municipality", "hospital_department", "health_service")
sigpro_data$risk_condition = sigpro_data$risk_condition_eng
sigpro_data$risk_condition_eng = NULL
sigpro_data$data_source = "SIGPRO"
sigpro_data$date = as.Date(sigpro_data$date)

testing_data = rbind(sigpro_data, sigsa_data) #We have dates to 2020 here. 

#create a unique anonymus identifier
dt_unique = unique(testing_data[,"identifier"])
dt_unique$id = row(dt_unique)
testing_data = merge(testing_data, dt_unique, by = "identifier")
testing_data$identifier = NULL

# Count which visit number this is (to check how many poeple are attending the clinic)
testing_data = testing_data[order(date)]
testing_data[, visit_num := seq_len(.N), by = id] #We have dates to 2020 here. 

# find sex variable and make all the same
testing_data$sex = substring(testing_data$sex, 1, 1)
testing_data$hiv_screening_result = toupper(testing_data$hiv_screening_result)
testing_data$completed_hiv_screening_test = toupper(testing_data$completed_hiv_screening_test)
testing_data$pre_orientaiton_test = toupper(testing_data$pre_orientaiton_test)

testing_data$completed_hiv_screening_test = ifelse(toupper(testing_data$completed_hiv_screening_test) == "S" | toupper(testing_data$completed_hiv_screening_test) == "REACTIVO", "SI", ifelse(
  toupper(testing_data$completed_hiv_screening_test) == "N", "NO", testing_data$completed_hiv_screening_test))

testing_data$pre_orientaiton_test = ifelse(toupper(testing_data$pre_orientaiton_test) == "S", "SI", ifelse(
  toupper(testing_data$pre_orientaiton_test) == "N", "NO", testing_data$pre_orientaiton_test))

# make departments the same
testing_data$hospital_department = toupper(fix_diacritics(testing_data$hospital_department))
testing_data$municipality = toupper(fix_diacritics(testing_data$municipality))
testing_data[grepl("QUETZALTENANGO", hospital_department), hospital_department := "QUEZALTENANGO"] #We have dates to 2020 here. 


# create binary
testing_data$isTrans = ifelse(grepl("TRANS", testing_data$risk_condition_eng) | grepl("Trans", testing_data$sexual_orientation) | grepl("T", testing_data$sex), 1, 0)
testing_data$isMSM = ifelse(grepl("HSH", testing_data$risk_condition_eng) | testing_data$sex == "M" & (testing_data$sexual_orientation == 'Homosexual' | testing_data$sexual_orientation == 'Bisexual'), 1, 0)

testing_data = testing_data[date != "2001-01-24"]


# remove repeat positive tests for patients, only keep the first postivie test and all negative tests before it
dt_graphing = testing_data
dt_graphing_pos = testing_data[hiv_screening_result == "REACTIVO"]
dt_graphing_pos[,keep := min(visit_num), by = id]
keep_list = unique(dt_graphing_pos[,.(id, keep)])

hiv_pos_patients = dt_graphing_pos$id
# if you want to calculate by patients instead of tests done, make visit_num == 1 for neg
dt_neg = testing_data[!id %in% hiv_pos_patients]

dt_pos = testing_data[id %in% hiv_pos_patients]
dt_pos = merge(dt_pos, keep_list)
# if you want to calculate by patients instead of tests done, make visit_num == keep for pos
dt_pos = dt_pos[visit_num <= keep]
dt_pos$keep =NULL

# bind both positive and negative testing together
total_dt = rbind(dt_neg, dt_pos)

saveRDS(total_dt, paste0(output_dir, "all_couns.rds"))
