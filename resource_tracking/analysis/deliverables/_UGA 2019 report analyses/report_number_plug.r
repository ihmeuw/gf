# Emily Linebarger 
# Calculate numbers to plug into UGA report 
library(data.table)
library(scales) 

budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/final_budgets.rds")
revisions = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/budget_revisions.rds")

all_files = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/budget_pudr_iterations.rds")
absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/absorption_uga.rds")

source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
#-------------
# HIV
#-------------
#How much of the UGA-H-MoFPED grant went to procuring ARVs? 
mofped_hiv= all_files[file_name=="UGA-H-MoFPED_IL2_SB2_Optimization_7March19.xlsx"]
stopifnot(length(unique(mofped_hiv$file_name))==1)
mofped_hiv[grep("arv|third line art", tolower(activity_description)), arv_procurement:=TRUE]
View(unique(mofped_hiv[arv_procurement==TRUE, .(gf_module, gf_intervention, activity_description)]))
mofped_hiv[, .(budget=sum(budget, na.rm=T)), by=c('arv_procurement')]

#How much of the UGA-H-MoFPED grant went to HIV test kits? 
mofped_hiv= all_files[file_name=="UGA-H-MoFPED_IL2_SB2_Optimization_7March19.xlsx"]
mofped_hiv[grep("test kits|tesk kits", tolower(activity_description)), testing:=TRUE]
View(unique(mofped_hiv[testing==TRUE, .(gf_module, gf_intervention, activity_description)]))
mofped_hiv[, .(budget=sum(budget, na.rm=T)), by=c('testing')]

# How much money went to HIV testing? 
mofped_hiv= all_files[file_name=="UGA-H-MoFPED_IL2_SB2_Optimization_7March19.xlsx"]
mofped_hiv[grepl("tes")]

# How much of the UGA-H-MoFPED grant went to condom procurement? 
mofped_hiv= all_files[file_name=="UGA-H-MoFPED_IL2_SB2_Optimization_7March19.xlsx"]
mofped_hiv[grepl("Condom", gf_intervention), condoms:=TRUE]
mofped_hiv[, sum(budget, na.rm=T), by='condoms']

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

# How much of the C-TASO budget is devoted to each module? 
c_taso = all_files[file_name=="UGA-C-TASO_IL1_Catalytic Funding_DetailedBudget_IMPP2_26April18.xlsx"]
c_taso = c_taso[, .(budget=sum(budget, na.rm=TRUE)), by='gf_module']
c_taso[, total:=sum(budget)]
c_taso[, mod_percent:=round((budget/total)*100, 1)]

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
                               by=c('gf_module')]
c_taso_absorption[, absorption:=round((expenditure/budget)*100, 1)]

# What has TASO's absorption been for the first 18 months, excluding TB modules? 
c_taso_absorption = absorption[grant=="UGA-C-TASO" & grant_period=="2018-2020" & semester=="Semester 3" & disease!="tb", 
                               .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T))]
c_taso_absorption[, absorption:=round((expenditure/budget)*100, 1)]

# What's the overall budget/expenditure for the H-MoFPED grant? 
absorption_all_grants = get_cumulative_absorption(byVars='grant', countrySubset="UGA")


# What's absorption for S1 2018 vs. S1 2019 for the H-MoFPED grant? 
h_mofped = absorption[grant=="UGA-H-MoFPED" & grant_period=="2018-2020", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('semester', 'gf_module')]
h_mofped[, absorption:=round((expenditure/budget)*100, 1)]     

# Calculate S1 2019 absorption by module for C-TASO 
s1_2019_taso = absorption[start_date=="2019-01-01" & grant=="UGA-C-TASO", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by='gf_module']
s1_2019_taso[, absorption:=round((expenditure/budget)*100, 1)] 
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

# How much money was budgeted for Genexpert cartridges? 
cartridges = budgets[grant=="UGA-T-MoFPED" & grant_period=="2018-2020"]
cartridges = cartridges[grep("cartridge", tolower(activity_description))] #Have visually reviewed these activities. EL 11/22/2019
#-------------
# MALARIA
#-------------
#What was total budgeted for malaria? 
budgets[grant_disease=="malaria" & grant_period=="2018-2020" & loc_name=="uga", .(b=sum(budget, na.rm=TRUE)), by='grant']

#What was module breakdown by budget years? 
check = budgets[grant_disease=="malaria" & grant_period=="2018-2020" & loc_name=="uga", .(b=sum(budget, na.rm=TRUE)), by=c('year', 'gf_module')]
check[, year_total:=sum(b), by='year']
check[, mod_pct:=b/year_total]

# What were the biggest components of vector control? 
budgets[grant_disease=="malaria" & grant_period=="2018-2020" & loc_name=="uga" & gf_module=="Vector control", 
        .(b=sum(budget, na.rm=TRUE)), by=c('gf_intervention')]

#Review cumulative absorption by module 
cumulative_absorption = absorption[grant_period=="2018-2020" & semester=="Semester 3" & grant_disease=="malaria", .(c_budget=sum(cumulative_budget, na.rm=T), c_expenditure=sum(cumulative_expenditure, na.rm=T)), 
                                   by=c('grant', 'gf_module')]
cumulative_absorption[, c_absorption:=round((c_expenditure/c_budget)*100, 1)]
cumulative_absorption[, c_expenditure:=dollar(c_expenditure)]
cumulative_absorption[, c_budget:=dollar(c_budget)]

# What's the overall cumulative absorption for MoFPED?
cumulative_absorption = absorption[grant_period=="2018-2020" & semester=="Semester 3" & grant=="UGA-M-MoFPED", .(c_budget=sum(cumulative_budget, na.rm=T), c_expenditure=sum(cumulative_expenditure, na.rm=T))]
cumulative_absorption[, c_expenditure/c_budget]
absorption_mal = absorption[grant_period=="2018-2020" & semester=="Semester 3" & grant=="UGA-M-MoFPED", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T))]
absorption_mal[, expenditure/budget]

# What's the overall cumulative absorption for TASO?
cumulative_absorption = absorption[grant_period=="2018-2020" & semester=="Semester 3" & grant=="UGA-M-TASO", .(c_budget=sum(cumulative_budget, na.rm=T), c_expenditure=sum(cumulative_expenditure, na.rm=T))]
cumulative_absorption[, c_expenditure/c_budget]
absorption_mal = absorption[grant_period=="2018-2020" & semester=="Semester 3" & grant=="UGA-M-TASO", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T))]
absorption_mal[, expenditure/budget]

# What's the cumulative absorption for ALL UGA grants, and is MoFPED malaria the lowest? 
all_cumulative = get_cumulative_absorption(byVars=c('grant'), countrySubset="UGA", currency="USD")
all_cumulative[, absorption:=expenditure/budget]

# What is the absorption for RSSH modules (cumulatively) for both malaria PRs? 
rssh_mal = absorption[grant_disease=="malaria" & disease=="rssh" & loc_name=="uga" & grant_period=="2018-2020", .(b=sum(budget, na.rm=T), e=sum(expenditure, na.rm=T))]
rssh_mal[, e/b]

#-------------
# RSSH
#-------------
#What was the total amount budgeted for RSSH across the portfolio? 
budgets[grant_period=="2018-2020" & disease=="rssh", sum(budget, na.rm=T)]
budgets[grant_period=="2018-2020", sum(budget, na.rm=T)]

# Split RSSH budget among grants 
rssh = budgets[grant_period=="2018-2020" & disease=="rssh", .(rssh_budget=sum(budget, na.rm=T)), by='grant']
rssh[, total_budget:=sum(rssh_budget)]
rssh[, rssh_pct:=rssh_budget/total_budget]

# What was 2018 absorption for all UGA grants? 
full_year = absorption[grant_period=="2018-2020" & grant%in%c('UGA-C-TASO', 'UGA-M-TASO') & semester=="Semester 1-2", .(b=sum(budget, na.rm=T), e=sum(expenditure, na.rm=T))]
semesters = absorption[grant_period=="2018-2020" & grant%in%c('UGA-C-TASO', 'UGA-M-TASO') & semester%in%c('Semester 1', 'Semester 2'), .(b=sum(budget, na.rm=T), e=sum(expenditure, na.rm=T))]
total_budget = full_year$b + semesters$b
total_expenditure = full_year$e + semesters$e
total_expenditure/total_budget

#What was the budget for national health strategies? 
rssh = budgets[grant_period=="2018-2020" & disease=="rssh", .(rssh_budget=sum(budget, na.rm=T)), by='gf_module']
rssh[, total_budget:=sum(rssh_budget)]
rssh[, rssh_pct:=rssh_budget/total_budget]

# What's absorption for RSSH national health strategies? 
rssh = get_cumulative_absorption(byVars=c('abbrev_mod'), countrySubset="UGA", diseaseSubset="rssh", currency="USD")
rssh[, absorption:=expenditure/budget]

# What's absorption for RSSH community systems by int? 
rssh = get_cumulative_absorption(byVars=c('abbrev_int'), countrySubset="UGA", diseaseSubset="rssh", moduleSubset="Community responses and systems", currency="USD")

# What was the total RSSH absorption for all UGA grants? 
rssh = get_cumulative_absorption(byVars='loc_name', countrySubset="UGA", diseaseSubset="rssh")

# How has RSSH absorption been changing over time? 
rssh_semesters = absorption[disease=="rssh" & grant_period=="2018-2020", .(budget=sum(budget), expenditure=sum(expenditure)), by=c('semester')]
rssh_semesters[, absorption:=round((expenditure/budget)*100, 1)]
rssh_semesters[, budget:=dollar(budget)]
rssh_semesters[, expenditure:=dollar(expenditure)]
#-------------
# GENERAL
#-------------
overall_absorption = get_cumulative_absorption(byVars='loc_name', countrySubset="UGA")
