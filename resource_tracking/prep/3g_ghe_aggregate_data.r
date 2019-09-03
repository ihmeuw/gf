#-------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Combine GHE datasets into one user-friendly format 
# DATE: Last updated August 2019 
#-------------------------------------------------

# Algorithm for combining GHE data: 
#   For Guatemala, always use SICOIN. 
#   For every other country, use FGH estimates where they're available because they provide a more accurate time series. 
#   If estimates are not available, use FGH actuals data, using National Health Accounts wherever possible. 
#   If there is overlap in NHA reporting, use the data from the most recent NHA. 

#Read in prepped datasets 
fgh_hiv_est = readRDS(paste0(fgh_ghe_hiv_prepped, "prepped_fgh_estimates.rds"))
fgh_mal_act = readRDS(paste0(fgh_ghe_malaria_prepped, "ghe_actuals_malaria.rds"))
fgh_tb_act = readRDS(paste0(fgh_ghe_tb_prepped, "ghe_actuals_tb.rds"))
sicoin = readRDS(paste0(sicoin_prepped, "prepped_sicoin_data.rds"))

#---------------------------------------------------------------------------------------
# Fix all of the datasets to be in a comparable format. 
#---------------------------------------------------------------------------------------
fgh_hiv_est = fgh_hiv_est[fin_data_type=="model_estimates" & financing_source=="ghe", .(disbursement=sum(disbursement, na.rm=T)), by=c('code', 'module_eng', 'intervention_eng', 'loc_name', 'year')]
setnames(fgh_hiv_est, c('module_eng', 'intervention_eng'), c('gf_module', 'gf_intervention'))
fgh_hiv_est[, disease:='hiv']

fgh_mal_act = fgh_mal_act[, .(disbursement=sum(value, na.rm=T)), by=c('loc_name', 'year', 'source_type')]
fgh_mal_act[, disease:='malaria']

fgh_tb_act = fgh_tb_act[, .(disbursement=sum(expenditure, na.rm=T)), by=c('loc_name', 'year', 'source_type')]
fgh_tb_act[, disease:='tb']

sicoin = sicoin[, .(disbursement=sum(expenditure, na.rm=TRUE)), by=c('code', 'gf_module', 'gf_intervention', 'disease', 'start_date', 'loc_name')]
sicoin[, year:=year(start_date)]

#---------------------------------------------------------------------------------------
# Then, drop Guatemala out of all of the FGH datasets because we want to use SICOIN. 
#---------------------------------------------------------------------------------------
#EL start here! 
fgh_hiv_est = fgh_hiv_est[loc_name!='gtm']
fgh_mal_act = fgh_mal_act[loc_name!='gtm']
fgh_tb_act = fgh_tb_act[loc_name!='gtm']

#---------------------------------------------------------------------------------------
# Only keep NHAs in the actuals datasets (this decision may be revisited depending on data 
# gaps) EL 8/30/19 
#---------------------------------------------------------------------------------------
fgh_mal_act = fgh_mal_act[!is.na(source_type) & source_type=="nha"]
fgh_tb_act = fgh_tb_act[!is.na(source_type) & source_type=="nha"]


#---------------------------------------------------------------------------------------
# Label datasets, and bind them together. 
#---------------------------------------------------------------------------------------
sicoin[, dataset:="sicoin"]
fgh_hiv_est[, dataset:="fgh_hiv_estimates"]
fgh_mal_act[, dataset:="fgh_malaria_actuals"]
fgh_tb_act[, dataset:="fgh_tb_actuals"]

all_ghe = rbindlist(list(sicoin, fgh_hiv_est, fgh_mal_act, fgh_tb_act), use.names=T, fill=T)

#Validate data 
stopifnot(unique(all_ghe$disease)%in%c('hiv', 'tb', 'malaria'))
stopifnot(unique(all_ghe$loc_name)%in%c('cod', 'gtm', 'sen', 'uga'))
stopifnot(nrow(all_ghe[is.na(year)])==0)


#---------------------------------------------------------------------------------------
# Save data, and archive current version. 
#---------------------------------------------------------------------------------------

saveRDS(all_ghe, paste0(all_ghe_prepped, "all_ghe.rds"))
write.csv(all_ghe, paste0(all_ghe_prepped, "all_ghe.csv"), row.names=F)

#Archive version 
write.csv(all_ghe, paste0(all_ghe_prepped, "archive/all_ghe_", Sys.Date(), ".csv"), row.names=F)
