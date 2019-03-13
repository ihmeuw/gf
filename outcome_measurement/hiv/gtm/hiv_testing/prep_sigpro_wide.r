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
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/sigsa_sigpro/')
prep_dir <- paste0(dir, 'sigpro/february_transfer/')
save_loc <- paste0(dir, 'sigpro_outputs/')

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
dt[subgrupo.its != subgrupo.pbtvc, .(gender, age, subgrupo.its, subgrupo.pbtvc)] #There are 8 cases here. We might want to go with the more specific one? 
dt[tema.its != tema.pbtvc] #These are different between the two datasets. 

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
unique(dt$age) #There are negative values in age. 
unique(dt$tratado) #There are already some 0's here, which may be typos but will get changed to 'no' when I do a binary conversion below. 
#------------------------------------------------------
# Fixing data concerns - we should see if there's a better way to do this! 
#------------------------------------------------------
dt[age<0, age:=NA]

#------------------------------------------------------
# Write data to .rds file 
#------------------------------------------------------

saveRDS(dt, paste0(save_loc, "/sigpro_wide.rds"))

#------------------------------------------------------
# Run graphs and descriptive statistics 
#------------------------------------------------------

plot_data = dt

#Generate some binary variables to make review easier 
plot_data$completed_hiv_test_ = ifelse(plot_data$resultadovih == 'REACTIVO', 1, 0)
plot_data$is_sex_worker_ = ifelse((plot_data$subgrupo.pbtvc == 'TRANS TRABAJADORAS SEXUALES' | plot_data$subgrupo.its == 'TRANS TRABAJADORAS SEXUALES'), 1,0)
plot_data$completed_sifilis_test_ = ifelse(plot_data$resultadosif == 'REACTIVO', 1, 0)
plot_data$subgrupo = plot_data$subgrupo.pbtvc #Simplify this variable, although it looks like some individuals were differently classified between datasets? 
plot_data$subgrupo = ifelse(is.na(plot_data$subgrupo), plot_data$subgrupo.its, plot_data$subgrupo) 

#Format the binary variables in binary format 
plot_data[prepruebavih == 'S', prepruebavih:='1']
plot_data[prepruebavih == 'N', prepruebavih:='0']
plot_data[, prepruebavih:=as.numeric(prepruebavih)]

plot_data[pruebavih == 'S', pruebavih:='1']
plot_data[pruebavih == 'N', pruebavih:='0']
plot_data[, pruebavih:=as.numeric(pruebavih)]

plot_data[postpruebavih == 'S', postpruebavih:='1']
plot_data[postpruebavih == 'N', postpruebavih:='0']
plot_data[, postpruebavih:=as.numeric(postpruebavih)]

plot_data[tratado == 'S', tratado:='1']
plot_data[tratado == 'N', tratado:='0']
plot_data[tratado == '0', tratado:='0'] #There are some of these observations that are already 0, wanted to make this visible! 
plot_data[, tratado:=as.numeric(tratado)]

#Make an age group variable to make graphs prettier. 
plot_data[, age:=as.numeric(age)]
plot_data[age>=0 & age < 5, age_group:='0-5']
plot_data[age>=5 & age < 18, age_group:='5-18']
plot_data[age>=18 & age < 25, age_group:='18-25']
plot_data[age>=25 & age < 30, age_group:= '25-30']
plot_data[age>=30 & age < 35, age_group:='30-35']
plot_data[age>=35 & age < 40, age_group:='35-40']
plot_data[age>=40 & age < 45, age_group:='40-45']
plot_data[age>=45 & age < 50, age_group:='45-50']

#And some date variables 
plot_data$month = month(plot_data$date)
plot_data$year = year(plot_data$date)
plot_data= plot_data[year == 2018] #There is one 2020 observation in here randomly??

#What was the total number of completed HIV tests? How about by subgroup, gender? By municipality? 
hiv_testing = plot_data[, .(completed_hiv_test_, date, month, year, grupo, subgrupo, is_sex_worker_, age_group)]

total_tests = hiv_testing[, .(total_tests = sum(completed_hiv_test_, na.rm = TRUE)), by=.(month, year)]
all_hiv_tests = ggplot(total_tests, aes(x = month, y=total_tests)) + 
  geom_point() + 
  geom_line() +
  labs(title='Number of HIV Tests done over Time', y='Count', x='Month') + 
  theme_bw() +scale_x_discrete(breaks=seq(1, 12, by=1), labels = seq(1, 12, by=1))

total_tests = hiv_testing[, .(total_tests = sum(completed_hiv_test_, na.rm = TRUE)), by=.(month, year, age_group)]
all_hiv_tests_age = ggplot(total_tests, aes(x = month, y=total_tests, group=age_group, color=age_group)) +
  geom_point() +
  geom_line() +
  labs(title='Number of HIV Tests done over Time by Age', y='Count', x='Month') +
  theme_bw() +scale_x_discrete(breaks=seq(1, 12, by=1), labels = seq(1, 12, by=1))

total_tests = hiv_testing[, .(total_tests = sum(completed_hiv_test_, na.rm = TRUE)), by=.(month, year, grupo)]
all_hiv_tests_group = ggplot(total_tests, aes(x = month, y=total_tests, group=grupo, color=grupo)) + 
  geom_point() + 
  geom_line() +
  labs(title='Number of HIV Tests done over Time by Group', y='Count', x='Month') + 
  theme_bw() +scale_x_discrete(breaks=seq(1, 12, by=1), labels = seq(1, 12, by=1))

total_tests = hiv_testing[, .(total_tests = sum(completed_hiv_test_, na.rm = TRUE)), by=.(month, year, subgrupo)]
all_hiv_tests_subgroup = ggplot(total_tests, aes(x = month, y=total_tests, group=subgrupo, color=subgrupo)) + 
  geom_point() + 
  geom_line() +
  labs(title='Number of HIV Tests done over Time by Subgroup', y='Count', x='Month') + 
  theme_bw() +scale_x_discrete(breaks=seq(1, 12, by=1), labels = seq(1, 12, by=1))


#How many tests are sex workers getting? 
sex_worker_tests = hiv_testing[is_sex_worker_ == 1, .(total_tests = sum(completed_hiv_test_, na.rm = TRUE)), by=.(month, year)]
sex_worker_hiv_tests = ggplot(sex_worker_tests, aes(x = month, y=total_tests)) + 
  geom_point() + 
  geom_line() +
  labs(title='Number of HIV Tests done for sex workers over Time', y='Count', x='Month') + 
  theme_bw() +scale_x_discrete(breaks=seq(1, 12, by=1), labels = seq(1, 12, by=1))

#Did we have any sex workers getting tested in the last data? 
#How is sifilis testing? 
sifilis_testing = plot_data[, .(completed_sifilis_test_, date, month, year, grupo, subgrupo, is_sex_worker_, age)]
total_tests = sifilis_testing[, .(total_tests = sum(completed_sifilis_test_, na.rm = TRUE)), by=.(month, year)]

all_sifilis_tests = ggplot(total_tests, aes(x = month, y=total_tests)) + 
  geom_point() + 
  geom_line() +
  labs(title='Number of Sifilis Tests done over Time', y='Count', x='Month') + 
  theme_bw() +scale_x_discrete(breaks=seq(1, 12, by=1), labels = seq(1, 12, by=1))

total_tests = sifilis_testing[, .(total_tests = sum(completed_sifilis_test_, na.rm = TRUE)), by=.(month, year, grupo)]
all_sifilis_tests_group = ggplot(total_tests, aes(x = month, y=total_tests, group=grupo, color=grupo)) + 
  geom_point() + 
  geom_line() +
  labs(title='Number of Sifilis Tests done over time by group', y='Count', x='Month') + 
  theme_bw() + scale_x_discrete(breaks=seq(1, 12, by=1), labels = as.character(seq(1, 12, by=1)))


#How are pre- and post-counseling numbers? 
counseling = plot_data[, .(refervih, prepruebavih, pruebavih, postpruebavih, conoceresultadosif, conoceresultadovih,
                           date, month, year, grupo, subgrupo, is_sex_worker_, age)]
total_counseling = counseling[, .(total_sessions = sum(prepruebavih, na.rm = TRUE)), by=.(month, year, grupo)]
pre_hiv_couns_group = ggplot(total_counseling, aes(x = month, y=total_sessions, group=grupo, color=grupo)) + 
  geom_point() + 
  geom_line() +
  labs(title='Number of counseling sessions before an HIV test by group', y='Count', x='Month') + 
  theme_bw() + scale_x_discrete(breaks=seq(1, 12, by=1), labels = seq(1, 12, by=1))

total_counseling = counseling[, .(total_sessions = sum(pruebavih, na.rm = TRUE)), by=.(month, year, grupo)]
hiv_couns_group = ggplot(total_counseling, aes(x = month, y=total_sessions, group=grupo, color=grupo)) + 
  geom_point() + 
  geom_line() +
  labs(title='Number of counseling sessions during HIV test by group', y='Count', x='Month') + 
  theme_bw() + scale_x_discrete(breaks=seq(1, 12, by=1), labels = seq(1, 12, by=1))

total_counseling = counseling[, .(total_sessions = sum(postpruebavih, na.rm = TRUE)), by=.(month, year, grupo)]
post_hiv_couns_group = ggplot(total_counseling, aes(x = month, y=total_sessions, group=grupo, color=grupo)) + 
  geom_point() + 
  geom_line() +
  labs(title='Number of counseling sessions after an HIV test by group', y='Count', x='Month') + 
  theme_bw() + scale_x_discrete(breaks=seq(1, 12, by=1), labels = seq(1, 12, by=1))

total_counseling = counseling[is_sex_worker_ == 1, .(total_sessions = sum(prepruebavih, na.rm = TRUE)), by=.(month, year)]
hiv_couns_sex_workers = ggplot(total_counseling, aes(x = month, y=total_sessions)) + 
  geom_point() + 
  geom_line() +
  labs(title='Number of counseling sessions before an HIV test for sex workers over time', y='Count', x='Month') + 
  theme_bw() + scale_x_discrete(breaks=seq(1, 12, by=1), labels = seq(1, 12, by=1))

#Are they getting condoms and lubricant to sex workers? Are they reaching them with any services at all? 

#What are the treatment rates by diagnosis? 
diag_treatment = plot_data[, .(diagnostico, tratado, age_group, grupo, subgrupo, is_sex_worker_)]
diag_treatment[, total_cases_per_diagnosis:=.N, by='diagnostico']
diag_treatment[tratado==1, total_treated_per_diagnosis:=.N, by='diagnostico']
diag_treatment[, treatment_pct:=round(total_treated_per_diagnosis/total_cases_per_diagnosis, 3), by='diagnostico']
diag_treatment[, treatment:=treatment*100]
diag_treatment = unique(diag_treatment[, .(diagnostico, total_cases_per_diagnosis, total_treated_per_diagnosis, treatment_pct)])

stis_treated = ggplot(diag_treatment, aes(x=diagnostico, y=treatment_pct, fill = diagnostico)) +
  geom_bar(stat='identity') +
  labs(title='Percentage of STI cases that were treated', y='Percentage', x='Month') + 
  theme_bw() + 
  theme(axis.text.x = element_blank()) 
#This graph is gross - a better way to see this is just to view the data table. Do we want to pursue this? 
print(diag_treatment)

  
  
  
#Linkage to care? 
#Break these down by municipality? Age? 


outFile = paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/sigsa_sigpro/visualizations/sigpro_feb_transfer_viz.pdf')

pdf(outFile, height=5.5, width=7)
all_hiv_tests
all_hiv_tests_group
all_hiv_tests_subgroup
all_hiv_tests_age
sex_worker_hiv_tests
all_sifilis_tests
all_sifilis_tests_group
pre_hiv_couns_group
hiv_couns_group
post_hiv_couns_group
hiv_couns_sex_workers
stis_treated
dev.off()


#------------------------------------------------
# Printing off a few more descriptive stats here 
#------------------------------------------------
diag_treatment[order(treatment_pct)] #This is an easier way to see this than the bar graph above. 

#Why do we have 0 completed HIV tests for sex workers? 
plot_data[is_sex_worker == 1, .N] #222 
plot_data[is_sex_worker_ == 1 & completed_hiv_test_ == 1, .N]

#Are they reaching sex workers with any other type of care? Or has this all shifted to MoH? 
plot_data[is_sex_worker_ == 1 & refervih == 1, .N] #Zero HIV referrals. 
unique(plot_data[is_sex_worker_ == 1, .(tipoactividad.its, tipoactividad.pbtvc)]) #Some contact with HIVOS then. 

