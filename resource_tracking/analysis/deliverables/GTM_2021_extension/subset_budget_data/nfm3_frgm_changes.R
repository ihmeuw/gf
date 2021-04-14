# prepare three data sets for analysis on changes between FR and GM for three workstreams

# set up
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}

# source files with other functions and common resource tracking filepaths
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/gf_files_prep_functions/id_keywords_for_extension.R")


# read in data set
all_data <- fread("C:\\Users\\frc2\\Box Sync\\Global Fund Files\\tableau_data\\budgetRevisions_with_frBudgets_activityLevel.csv")

##### Subset budget to data on rapid diagnostic tests
dt.1 <-all_data[loc_name=="Guatemala" & grant_period=="2021-2023" & cost_category=="5.4 Pruebas de diagnostico rapido"]

dt.1 <- dt.1[,.(loc_name, gf_module, gf_intervention, activity_description, cost_category, equity, file_name, grant, budget_version, budget)]

write.csv(dt.1, file = paste0(dir, "other/specialized_datasets/gtm/pruebas_diagnostico_rapido.csv"))

##### Subset budget to only data for survey interventions
dt.2 <- all_data[loc_name=="Guatemala" & grant_period=="2021-2023" & gf_module=="Health management information system and monitoring and evaluation" & gf_intervention=="Surveys"]

dt.2 <- dt.2[,.(loc_name, gf_module, gf_intervention, activity_description, cost_category, file_name, grant, budget_version, budget)]

write.csv(dt.2, file = paste0(dir, "other/specialized_datasets/gtm/encuestas_gtm_vih.csv"))

##### run keyword search
dt.3 <- all_data[loc_name=="Guatemala" & grant_period=="2021-2023"]
dt.3 <- dt.3[,.(loc_name, gf_module, gf_intervention, activity_description, cost_category, equity, file_name, grant, budget_version, budget)]
write.csv(dt.3, file = paste0(dir, "other/specialized_datasets/gtm/incap_nfm_nfm3_budgets.csv"))

# set up keyword search
id_keywords_for_extension(country="Guatemala", inFile=paste0(dir, "other/specialized_datasets/gtm/incap_nfm_nfm3_budgets.csv"))
