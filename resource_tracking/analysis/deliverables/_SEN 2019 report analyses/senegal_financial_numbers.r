library(data.table) 
source("C:/Users/elineb/Documents/gf/resource_tracking/prep/_common/shared_functions.r")


#-------------------------------------
# First, check the total for prevention using only grant periods 2015-2017 and 2018-2020
final_budgets_sen <- readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets_sen.rds")
dt = final_budgets_sen[grant_period%in%c('2018-2020', '2015-2017') & grant_disease%in%c('hiv', 'hiv/tb'), .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'year', 'grant_period')]
dt = convert_currency(dt, 'year', convertFrom='USD', convertTo='EUR', finVars=c('budget'))
dt = dt[, .(budget=sum(budget)), by=c('grant_period', 'gf_module')]

# Shape this wide by grant period 
dt = dcast(dt, gf_module~grant_period, value.var='budget')
dt[`2018-2020`<`2015-2017`, decrease:=TRUE]
dt[is.na(decrease), decrease:=FALSE]
View(dt)

#--------------------------------------
# Now, include 2014-2017 and 2016-2017, and clump these three grant periods together under "NFM1" (2015-2017, 2014-2017, and 2016-2017)
dt2 = final_budgets_sen[grant_period%in%c('2018-2020', '2015-2017', '2014-2017', '2016-2017') & grant_disease%in%c('hiv', 'hiv/tb'), .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'year', 'grant_period')]
dt2[grant_period%in%c('2015-2017', '2016-2017', '2014-2017'), time_period:="NFM1"]
dt2[grant_period=="2018-2020", time_period:="NFM2"]
dt2 = convert_currency(dt2, 'year', convertFrom='USD', convertTo='EUR', finVars=c('budget'))
dt2 = dt2[, .(budget=sum(budget)), by=c('time_period', 'gf_module')]

# Shape this wide by grant period 
dt2 = dcast(dt2, gf_module~time_period, value.var='budget')
dt2[`NFM2`<`NFM1`, decrease:=TRUE]
dt2[is.na(decrease), decrease:=FALSE]
View(dt2)

# -----------------------------------------
# Finally, do a fixed time period of 3 years, comparing 2015-2017 and 2018-2020 even if grant periods don't align. 
dt3a = final_budgets_sen[start_date>="2015-01-01" & start_date< "2018-01-01" & grant_disease%in%c('hiv', 'hiv/tb'), .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'year', 'grant_period')]
dt3a[, time_period:="Jan. 2015 - Dec. 2017"]
dt3b = final_budgets_sen[start_date>="2018-01-01" & start_date< "2021-01-01" & grant_disease%in%c('hiv', 'hiv/tb'), .(budget=sum(budget, na.rm=T)), by=c('gf_module', 'year', 'grant_period')]
dt3b[, time_period:="Jan. 2018 - Dec. 2020"]
dt3 = rbind(dt3a, dt3b, use.names=T)

dt3 = convert_currency(dt3, 'year', convertFrom='USD', convertTo='EUR', finVars=c('budget'))
dt3 = dt3[, .(budget=sum(budget)), by=c('time_period', 'gf_module')]

# Shape this wide by grant period 
dt3 = dcast(dt3, gf_module~time_period, value.var='budget')
dt3[`Jan. 2018 - Dec. 2020`<`Jan. 2015 - Dec. 2017`, decrease:=TRUE]
dt3[is.na(decrease), decrease:=FALSE]
View(dt3)

# ------------------------------------------------------------------------
# Final nice thing to know - how many grants are there in each grant period? 
grants_list = unique(final_budgets_sen[grant_period%in%c('2014-2017', '2015-2017', '2016-2017', '2018-2020'), .(grant, grant_period)])
grants_list[, num_grants_this_period:=.N, by='grant_period']


#-----------------------------------
# Responses to Maria's specific points 
#-----------------------------------
all_files = data.table(read_xlsx("C:/Users/elineb/Desktop/most_recent_budgets_euro_EL.xlsx", sheet="most_recent_budgets_euro" ))
budget_revisions = all_files[data_source=="budget" & grant_period=="2018-2020" 
                             & ((file_iteration=="revision" & grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS')) | (file_iteration=="final" & !grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS')))]

nfm1_hiv = final_budgets_sen[grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS') & grant_period=="2015-2017"]
nfm1_hiv = convert_currency(nfm1_hiv, convertFrom="USD", convertTo="EUR", yearVar='year', finVars='budget')

nfm2_hiv = budget_revisions[grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS') & grant_period=="2018-2020"]
nfm2_hiv = convert_currency(nfm2_hiv, convertFrom="USD", convertTo="EUR", yearVar='year', finVars='budget')


# Total budget for NFM1? 
nfm1_hiv[, sum(budget, na.rm=T)] 

# Total budget for KPs for NFM1? 
kps = nfm1_hiv[, .(budget=sum(budget, na.rm=T)), by=c('gf_module')]
kps[grepl("prevention programs", tolower(gf_module)), kp:=TRUE] # This was verified by a visual check. 
kps[is.na(kp), kp:=FALSE]
kps = kps[, .(budget=sum(budget)), by='kp']
kps[, total:=sum(budget)]
kps[, pct:=budget/total]

# Compare ratio of CNLS to ANCS between funding periods? 
compare_cs = nfm1_hiv[, .(budget=sum(budget, na.rm=T)), by='grant']
compare_cs[, total:=sum(budget)]
compare_cs[, pct:=budget/total]

compare_cs18 = nfm2_hiv[, .(budget=sum(budget, na.rm=T)), by='grant']
compare_cs18[, total:=sum(budget)]
compare_cs18[, pct:=budget/total]

# What is the total after catalytic funding was added to prevention programs? 
# Total budget for prevention for NFM2? 
#all_files = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/SEN/prepped_data/budget_pudr_iterations_euro.rds")
all_files = data.table(read_xlsx("C:/Users/elineb/Desktop/most_recent_budgets_euro_EL.xlsx", sheet="most_recent_budgets_euro" ))
budget_revisions = all_files[data_source=="budget" & grant_period=="2018-2020" 
                             & ((file_iteration=="revision" & grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS')) | (file_iteration=="final" & !grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS')))]
kps = budget_revisions[grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS'), .(budget=sum(budget, na.rm=T)), by=c('gf_module')]
kps[grepl("prevention", tolower(gf_module)), prev:=TRUE] # This was verified by a visual check. 
kps[is.na(prev) | gf_module%in%c('Prevention of mother-to-child transmission', 'Prevention programs for general population'), prev:=FALSE]
kps = kps[, .(budget=sum(budget)), by='prev']
kps[, total:=sum(budget)]
kps[, pct:=budget/total]

# Now calculate all prevention (not including PMTCT)
budget_revisions = all_files[data_source=="budget" & grant_period=="2018-2020" 
                             & ((file_iteration=="revision" & grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS')) | (file_iteration=="final" & !grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS')))]
all_prevention = budget_revisions[grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS'), .(budget=sum(budget, na.rm=T)), by=c('gf_module')]
all_prevention[grepl("prevention", tolower(gf_module)), prev:=TRUE] # This was verified by a visual check. 
all_prevention[is.na(prev) | gf_module%in%c('Prevention of mother-to-child transmission'), prev:=FALSE]
all_prevention = all_prevention[, .(budget=sum(budget)), by='prev']
all_prevention[, total:=sum(budget)]
all_prevention[, pct:=budget/total]

# Funding for human rights barriers in NFM2? 
check = budget_revisions[grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS'), .(budget=sum(budget, na.rm=T)), by='gf_module']
check[, total:=sum(budget)]
check[, pct:=budget/total]


# Calculate the total by module for 2015-2017 summary budget extraction 
summ_15_17 = data.table(read_xlsx("C:/Users/elineb/Box Sync/Global Fund Files/SEN/raw_data/active/2015_2017_HIV_budget_extraction.xlsx", sheet="2015-2017 summary budgets"))
summ_15_17[, .(budget=dollar(sum(budget))), by='gf_module']
