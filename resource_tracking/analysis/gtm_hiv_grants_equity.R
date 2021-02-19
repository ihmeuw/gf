# file that subsets guatemala grants for Equity-related analysis

# set up file
library(data.table)

# read in data
data <- fread("C:/Users/frc2/Box Sync/Global Fund Files/tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv")

# subset to Guatemala hiv grants
data <- data[loc_name=="Guatemala"]
data <- data[file_name %in% c('GTM_H_INCAP_DetailedBudget_1.xlsx',
                              '05.Presupuesto_detallado_final.xlsx',
                              'GTM-H-INCAP_DB_final.xlsx',
                              'Copia de GTM-H-INCAP_DB_11.10. Final_For PR.XLSX',
                              '06 FR100-GTM-H_DB_INCAP_05feb2018.xlsx',
                              'FR100-GTM-H_DB_INCAP_22feb2018_2135.xlsx',
                              'GTM-H-INCAP_DB_final_Actualizado08112020.xlsx',
                              'FR100-GTM-H_DB_INCAP_ 04-07-2018.xlsx')]

# subset to certain columns
keep <- c('loc_name', 'gf_module', 'gf_intervention', 'activity_description', 'cost_category', 
          'file_name', 'grant', 'grant_period', 'disease', 'implementer',
          'budget_version',  'budget', 'rssh', 'equity')

data2 <- data[,..keep]

data2 <- data2[!is.na(budget_version)]

View(data2)

# save data file
write.csv(data2, file = "C:/Users/frc2/Desktop/temporary_folder/guatemala_hiv_budgets.csv")
