# Emily Linebarger 
# Calculate numbers to plug into UGA report 
library(data.table)
library(scales) 

budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/final_budgets.rds")
revisions = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/budget_revisions.rds")

all_files = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/budget_pudr_iterations.rds")
absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/absorption_uga.rds")

#-------------
# HIV
#-------------
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

# How much of the H-MoFPED budget is devoted to each module? 
h_mofped = all_files[file_name=="UGA-H-MoFPED_IL2_SB2_Optimization_7March19.xlsx"]
h_mofped = h_mofped[, .(budget=sum(budget, na.rm=TRUE)), by='gf_module']
h_mofped[, total:=sum(budget)]
h_mofped[, mod_percent:=round((budget/total)*100, 1)]

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

#-------------
# TB
#-------------
# t_mofped = all_files[file_name=="Budget_UGA-T-MoFPED_DB_ IMPP4_IL1_May2019.xlsx"] # If you want to look at most recent revision
t_mofped = budgets[grant=="UGA-T-MoFPED" & grant_period=="2018-2020"]
t_mofped = t_mofped[, .(budget=sum(budget, na.rm=TRUE)), by='gf_module']
t_mofped[, total:=sum(budget)]
t_mofped[, mod_percent:=round((budget/total)*100, 1)]

c_taso_tb = all_files[file_name=="UGA-C-TASO_IL1_Catalytic Funding_DetailedBudget_IMPP2_26April18.xlsx" & (disease=="tb"|gf_module=="TB/HIV")]
c_taso_tb = c_taso_tb[, .(budget=sum(budget, na.rm=TRUE)), by='gf_module']
c_taso_tb[, total:=sum(budget)]
c_taso_tb[, mod_percent:=round((budget/total)*100, 1)]
#-------------
# MALARIA
#-------------

#Review cumulative absorption by module 
cumulative_absorption = absorption[grant_period=="2018-2020" & semester=="Semester 3", .(c_budget=sum(cumulative_budget, na.rm=T), c_expenditure=sum(cumulative_expenditure, na.rm=T)), 
                                   by=c('grant', 'gf_module')]
cumulative_absorption[, c_absorption:=round((c_expenditure/c_budget)*100, 1)]
cumulative_absorption[, c_expenditure:=dollar(c_expenditure)]
cumulative_absorption[, c_budget:=dollar(c_budget)]
