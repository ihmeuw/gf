# Emily Linebarger 
# Calculate numbers to plug into UGA report 
library(data.table)

budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/final_budgets.rds")
revisions = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/budget_revisions.rds")

all_files = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/budget_pudr_iterations.rds")
absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/absorption_uga.rds")

#How much of the UGA-H-MoFPED grant went to procuring ARVs? 
mofped_hiv= all_files[file_name=="UGA-H-MoFPED_IL2_SB2_Optimization_7March19.xlsx"]
stopifnot(length(unique(mofped_hiv$file_name))==1)
mofped_hiv[grep("arv|third line art", tolower(activity_description)), arv_procurement:=TRUE]
View(unique(mofped_hiv[arv_procurement==TRUE, .(gf_module, gf_intervention, activity_description)]))
mofped_hiv[, .(budget=sum(budget, na.rm=T)), by=c('arv_procurement')]

#How much of the UGA-H-MoFPED grant went to HIV testing? 
mofped_hiv= all_files[file_name=="UGA-H-MoFPED_IL2_SB2_Optimization_7March19.xlsx"]
mofped_hiv[grep("test kits|tesk kits", tolower(activity_description)), testing:=TRUE]
View(unique(mofped_hiv[testing==TRUE, .(gf_module, gf_intervention, activity_description)]))
mofped_hiv[, .(budget=sum(budget, na.rm=T)), by=c('testing')]

#How much of the C-TASO budget is devoted to human rights? 
c_taso = all_files[file_name=="UGA-C-TASO_IL1_Catalytic Funding_DetailedBudget_IMPP2_26April18.xlsx"]
taso1 = c_taso[, .(budget=sum(budget, na.rm=T)), by='gf_module']
taso1[, total:=sum(budget)]
taso1[, mod_percent:=round((budget/total)*100, 1)]

# Calculate overall grant-level absorption percentage, using cumulative 18-month data. 
mofped_absorption = absorption[grant=="UGA-H-MoFPED" & grant_period=="2018-2020" & semester=="Semester 3", 
                               .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T))]
mofped_absorption[, absorption:=round((expenditure/budget)*100, 1)]


c_taso_absorption = absorption[grant=="UGA-C-TASO" & grant_period=="2018-2020" & semester=="Semester 3", 
                               .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T))]
c_taso_absorption[, absorption:=round((expenditure/budget)*100, 1)]

# Do the same analyses by module 
mofped_absorption = absorption[grant=="UGA-H-MoFPED" & grant_period=="2018-2020" & semester=="Semester 3", 
                               .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), 
                               by='gf_module']
mofped_absorption[, absorption:=round((expenditure/budget)*100, 1)]


c_taso_absorption = absorption[grant=="UGA-C-TASO" & grant_period=="2018-2020" & semester=="Semester 3", 
                               .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), 
                               by='gf_module']
c_taso_absorption[, absorption:=round((expenditure/budget)*100, 1)]
