# read file with all FRs
library(data.table)
fr_budgets_all <- fread("C:\\Users\\frc2\\Box Sync\\Global Fund Files\\tableau_data\\fr_budgets_all.csv")

gtm_fr_2021 <- fr_budgets_all[loc_name=="Guatemala" & grant_period=="2021-2023"]

write.csv(gtm_fr_2021, "J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gtm/prepped_data/gtm_2020_hiv_fr.csv")
