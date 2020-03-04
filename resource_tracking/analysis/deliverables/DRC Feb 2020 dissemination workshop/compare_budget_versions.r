# Prep DRC budget iterations to revisions 

library(data.table) 

dt = readRDS("C:/Users/frc2/Box Sync/Global Fund Files/COD/prepped_data/budget_pudr_iterations.rds")
dt = dt[grant_period=="2018-2020" & data_source=="budget"]

# Determine the 'version' of each file 
order = unique(dt[, .(file_name, grant, update_date)])[order(grant, update_date)]
order[, version:=1:.N, by=c('grant')]

# Merge this variable back onto the big dataset
dt = merge(dt, order, by=c('file_name', 'grant', 'update_date'), all.x=T)


# Would you please put together an excel sheet (Uganda first so we have it, and DRC next) 
# that includes: Grant, disease, module (intervention if we have it), dates associated with the 
# $ amount allocated to said module/intervention and broken down by year/semester.
# Please also include (somewhere in the file) the exact file names this info is being pulled
# from so we can refer back to the raw data if necessary. 
# 

byVars=c('file_name', 'grant', 'disease', 'gf_module', 'gf_intervention', 'start_date', 'version', 'update_date')
collapse = dt[, .(budget=sum(budget, na.rm=T)), by=byVars][order(grant, version, start_date)]
write.csv(collapse, "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/DRC dissemination workshop Feb 2020/budget_version_comparison.csv", row.names=F)


# how does this compare to the funding requests? 
collapse[, year:=year(start_date)]
c_taso = collapse[grant=="UGA-C-TASO" & version==1, .(budget=sum(budget)), by=c('gf_module', 'year')]
c_taso = c_taso[year<2021]
c_taso = dcast(c_taso, gf_module~year, value.var='budget')
