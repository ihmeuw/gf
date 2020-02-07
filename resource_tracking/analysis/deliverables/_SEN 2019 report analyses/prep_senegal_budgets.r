library(data.table) 
source("C:/Users/elineb/Documents/gf/resource_tracking/prep/_common/shared_functions.r")


#-------------------------------------
# Shape final budgets wide by country 
final_budgets_sen <- readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets_sen.rds")
dt = final_budgets_sen[grant_period%in%c('2018-2020', '2015-2017') & grant_disease%in%c('hiv', 'hiv/tb'), .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'year', 'grant_period')]
dt = convert_currency(dt, 'year', convertFrom='USD', convertTo='EUR', finVars=c('budget'))
dt = dt[, .(budget=sum(budget)), by=c('grant_period', 'gf_module')]

# Shape this wide by grant period 
dt = dcast(dt, gf_module~grant_period, value.var='budget')

write.csv(dt, "C:/Users/elineb/Desktop/sen_budgets_euros.csv", row.names=F)

# #-------------------------------------
# # Shape final budgets wide by country and grant 
# dt = final_budgets_sen[grant_period%in%c('2018-2020', '2015-2017') & grant_disease%in%c('hiv', 'hiv/tb'), .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'year', 'grant', 'grant_period')]
# dt = convert_currency(dt, 'year', convertFrom='USD', convertTo='EUR', finVars=c('budget'))
# dt = dt[, .(budget=sum(budget)), by=c('grant_period', 'gf_module', 'grant')]
# 
# # Shape this wide by grant period 
# dt = dcast(dt, grant+gf_module~grant_period, value.var='budget')
# 
# write.csv(dt, "C:/Users/elineb/Desktop/sen_budgets_euros_grant.csv", row.names=F)
# 

#Conclusion from the slide deck: The 2018-2020 grants include €11 million for RSSH, with 50% dedicated to strengthening M&E and HMIS
#Comment: What is the source? By looking at the budgets the total sum of RSSH modules is 10.4M
all_files = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/SEN/prepped_data/budget_pudr_iterations_euro.rds")
budgets = all_files[data_source=="budget" & grant_period=="2018-2020" & file_iteration=="final"]
rssh_check = budgets[disease=="rssh", .(budget=sum(budget, na.rm=T)), by=c('gf_module')]
rssh_check[, total:=sum(budget)]
rssh_check[, pct:=round((budget/total)*100, 1)]
print("RSSH breakdown for 2018-2020 final, approved budgets")
print(rssh_check)

#unique(all_files[file_iteration=="revision", .(file_name, update_date)]) # There's only one revision file for H-ANCS and one for H-CNLS. 
budget_revisions = all_files[data_source=="budget" & grant_period=="2018-2020" 
                             & ((file_iteration=="revision" & grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS')) | (file_iteration=="final" & !grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS')))]
stopifnot(length(unique(budget_revisions$file_name))==4) # You should only be pulling 4 files. 
rssh_check_revisions = budget_revisions[disease=="rssh", .(budget=sum(budget, na.rm=T)), by=c('gf_module')]
rssh_check_revisions[, total:=sum(budget)]
rssh_check_revisions[, pct:=round((budget/total)*100, 1)]
print("RSSH breakdown for 2018-2020 revised budgets (most recent revision)")
print(rssh_check_revisions)