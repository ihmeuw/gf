library(data.table)

# Use most recent budget revision for each grant 
dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/budget_pudr_iterations.rds") #Change to your Box Sync folder
dt = dt[file_name%in%c("Copia de GTM-H-INCAP_DB_11.10. Final_For PR.XLSX", "DB-GTM-M-MSPAS_28.10.19.xlsx", "GTM_T_Full_Budget_9Sept2018.xlsx")]
# These are the most recent budget revision files for the recent grants. 


# get sum of budget by activity and grant. 
rssh= dt[grepl(code, pattern ='R')]
rssh = rssh[, .(activity_budget = sum(budget)), by = .(grant, gf_module, gf_intervention, activity_description)]

# subset to the highest budget activities in the highest budget interventions
# sum budget to intervention level
sums = rssh[, .(intervention_grant_budget = sum(activity_budget)), by = .(grant, gf_intervention, gf_module)]
sums = sums[intervention_grant_budget >= 100000]

# subset to activities in the highest budgeted interventions
dt = merge(sums, rssh, all.x = TRUE, by = c('grant', 'gf_intervention', 'gf_module'))
dt = dt[activity_budget != 0, ]
# order by activity budget within interventions and grants highest->lowest
setorderv(dt, c("activity_budget"), c(-1))

# subset to activities that make up at least 15% of the intervention's budget in each grant 
# (note: just did this as a way to subset the "priority activities")
subset = copy(dt)
subset[, proportion_of_intervention_budget := activity_budget/intervention_grant_budget]
subset = subset[proportion_of_intervention_budget >= 0.15]
write.csv(subset, "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_GTM 2019 annual report/rssh_highest_budget_activities.csv", row.names=F)