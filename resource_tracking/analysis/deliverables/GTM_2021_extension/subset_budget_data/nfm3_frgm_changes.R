# prepare three data sets for analysis on changes between FR and GM for three workstreams

# set up
library(data.table)

# read in data set
all_data <- fread("C:\\Users\\frc2\\Box Sync\\Global Fund Files\\tableau_data\\budgetRevisions_with_frBudgets_activityLevel.csv")

##### Subset budget to data on rapid diagnostic tests
dt.1 <-all_data[loc_name=="Guatemala" & grant_period=="2021-2023" & cost_category=="5.4 Pruebas de diagnostico rapido"]

dt.1 <- dt.1[,.(loc_name, gf_module, gf_intervention, activity_description, cost_category, file_name, grant, budget_version, budget)]

##### Subset budget to only data for survey interventions
dt.2 <- all_data[loc_name=="Guatemala" & grant_period=="2021-2023" & gf_module=="Health management information system and monitoring and evaluation" & gf_intervention=="Surveys"]

dt.1 <- dt.1[,.(loc_name, gf_module, gf_intervention, activity_description, cost_category, file_name, grant, budget_version, budget)]

##### run keyword search

