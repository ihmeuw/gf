# Senegal RSSH budget breakdown 
# All outputs in Euros! 
# Emily Linebarger 
# January 29, 2020 

library(data.table) 
user = Sys.info()[[6]]

#Conclusion from the slide deck: The 2018-2020 grants include €11 million for RSSH, with 50% dedicated to strengthening M&E and HMIS
#Comment: What is the source? By looking at the budgets the total sum of RSSH modules is 10.4M
all_files = readRDS(paste0("C:/Users/", user, "/Box Sync/Global Fund Files/SEN/prepped_data/budget_pudr_iterations_euro.rds"))
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