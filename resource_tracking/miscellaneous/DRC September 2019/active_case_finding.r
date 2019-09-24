dt <- readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/cod/prepped_data/final_budgets.rds")
dt = dt[grant=="COD-C-CORDAID" & grant_period=="2018-2020"]

dt1 = dt[grep("cas contact", activity_description)]

dt2 = dt[grep("mobil", activity_description)]


dt3 = dt[grep("recherche", activity_description)] #The dt2 cases that you're interested in are contained in this search.


missing_cases_funding = rbind(dt1, dt3)

missing_cases_funding[grep("mobil", activity_description), unique(activity_description)]
missing_cases_funding[grep("mobil", activity_description), sum(budget, na.rm=T), by='start_date']


dt1[, sum(budget, na.rm=T)]


saveRDS(missing_cases_funding, "C:/Users/elineb/Desktop/cod_c_cordaid_tb_missing_cases.rds")
