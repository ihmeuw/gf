#-----------------------------------------------------
# Initial absorption finding statements
# For synthesis report  - September 2019 
# This was used to fill in the Synthesis Mad-libs table. 
#-----------------------------------------------------

rm(list=ls())
library(data.table) 
library(ggplot2)

#Read in absorption data 
cod = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/absorption_cod.rds")
gtm = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/absorption_gtm.rds")
sen = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/SEN/prepped_data/absorption_sen.rds")
uga = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/absorption_uga.rds")


#Only keep 2019 PUDRs 
# for (loc in c('cod', 'gtm', 'sen', 'uga')){
#   assign(loc, get(loc)[start_date=="2019-01-01"])
# }


#Malaria 
for (loc in c('cod', 'gtm', 'sen', 'uga')){ 
  assign(paste0(loc, "_malaria"), get(loc)[grant_disease=="malaria", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
    by=c('grant', 'grant_period', 'semester', 'gf_module', 'gf_intervention')])
  get(paste0(loc, "_malaria"))[, absorption:=round((expenditure/budget)*100, 2)]
} 

#HIV
for (loc in c('cod', 'gtm', 'sen', 'uga')){ 
  assign(paste0(loc, "_hiv"), get(loc)[grant_disease=="hiv", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                                           by=c('grant', 'grant_period', 'semester', 'gf_module', 'gf_intervention')])
  get(paste0(loc, "_hiv"))[, absorption:=round((expenditure/budget)*100, 2)]
} 

#TB
for (loc in c('cod', 'gtm', 'sen', 'uga')){ 
  assign(paste0(loc, "_tb"), get(loc)[grant_disease=="tb", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                                       by=c('grant', 'grant_period', 'semester', 'gf_module', 'gf_intervention')])
  get(paste0(loc, "_tb"))[, absorption:=round((expenditure/budget)*100, 2)]
}

#HIV/TB
for (loc in c('cod', 'gtm', 'sen', 'uga')){ 
  assign(paste0(loc, "_hivtb"), get(loc)[grant_disease=="hiv/tb", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                                      by=c('grant', 'grant_period', 'semester', 'gf_module', 'gf_intervention')])
  get(paste0(loc, "_hivtb"))[, absorption:=round((expenditure/budget)*100, 2)]
}


#-------------------------------
#Analyses by country 
#-------------------------------
# DRC 
cod_malaria[order(-absorption), .(gf_intervention, absorption)]
cod_hiv[order(-absorption), .(gf_intervention, absorption)]
cod_tb[order(-absorption), .(gf_intervention, absorption)]
cod_hivtb[order(-absorption), .(gf_intervention, absorption)]

# SEN
sen_malaria[order(-absorption), .(gf_intervention, absorption)]
sen_hiv[order(-absorption), .(gf_intervention, absorption)]
sen_tb[order(-absorption), .(gf_intervention, absorption)]
sen_hivtb[order(-absorption), .(gf_intervention, absorption)]

# UGA 
uga_malaria[order(-absorption)][1:10, .(gf_intervention, absorption)]
uga_hiv[order(-absorption)][1:10, .(gf_intervention, absorption)]
uga_tb[order(-absorption)][1:10, .(gf_intervention, absorption)]
uga_hivtb[order(-absorption)][1:10, .(gf_intervention, absorption)]

# GTM 
gtm_hiv[order(-absorption)][1:10, .(gf_intervention, absorption)]

#------------------------------------------------------------
#Is RSSH absorption higher or lower than the PUDR average ?
#------------------------------------------------------------
for (loc in c('cod', 'gtm', 'sen', 'uga')){
  get(paste0(loc, "_malaria"))[, total_budget:=sum(budget, na.rm=T)]
  get(paste0(loc, "_tb"))[, total_budget:=sum(budget, na.rm=T)]
  get(paste0(loc, "_hiv"))[, total_budget:=sum(budget, na.rm=T)]
  get(paste0(loc, "_hivtb"))[, total_budget:=sum(budget, na.rm=T)]
  
  get(paste0(loc, "_malaria"))[, total_expenditure:=sum(expenditure, na.rm=T)]
  get(paste0(loc, "_tb"))[, total_expenditure:=sum(expenditure, na.rm=T)]
  get(paste0(loc, "_hiv"))[, total_expenditure:=sum(expenditure, na.rm=T)]
  get(paste0(loc, "_hivtb"))[, total_expenditure:=sum(expenditure, na.rm=T)]
  
  get(paste0(loc, "_malaria"))[, total_absorption:=(total_expenditure/total_budget)*100]
  get(paste0(loc, "_tb"))[, total_absorption:=(total_expenditure/total_budget)*100]
  get(paste0(loc, "_hiv"))[, total_absorption:=(total_expenditure/total_budget)*100]
  get(paste0(loc, "_hivtb"))[, total_absorption:=(total_expenditure/total_budget)*100]
  
  get(paste0(loc, "_malaria"))[absorption>=total_absorption, absorption_rating:="ABOVE_AVERAGE"]
  get(paste0(loc, "_malaria"))[absorption<total_absorption, absorption_rating:="BELOW_AVERAGE"]
  
  get(paste0(loc, "_tb"))[absorption>=total_absorption, absorption_rating:="ABOVE_AVERAGE"]
  get(paste0(loc, "_tb"))[absorption<total_absorption, absorption_rating:="BELOW_AVERAGE"]
  
  get(paste0(loc, "_hiv"))[absorption>=total_absorption, absorption_rating:="ABOVE_AVERAGE"]
  get(paste0(loc, "_hiv"))[absorption<total_absorption, absorption_rating:="BELOW_AVERAGE"]
  
  get(paste0(loc, "_hivtb"))[absorption>=total_absorption, absorption_rating:="ABOVE_AVERAGE"]
  get(paste0(loc, "_hivtb"))[absorption<total_absorption, absorption_rating:="BELOW_AVERAGE"]

} 

rssh_mods = c('Health management information system and monitoring and evaluation', "Human resources for health, including community health workers", 
              "Community responses and systems", "National health strategies", "Integrated service delivery and quality improvement", 
              "Procurement and supply chain management systems")

#Generate specific RSSH sub-datasets
cod_malaria_rssh = cod_malaria[gf_module%in%rssh_mods, .(grant, gf_module, gf_intervention, absorption, absorption_rating)]
cod_hiv_rssh = cod_hiv[gf_module%in%rssh_mods, .(grant, gf_module, gf_intervention, absorption, absorption_rating)]
cod_tb_rssh = cod_tb[gf_module%in%rssh_mods, .(grant, gf_module, gf_intervention, absorption, absorption_rating)]
cod_hivtb_rssh = cod_hivtb[gf_module%in%rssh_mods, .(grant, gf_module, gf_intervention, absorption, absorption_rating)]

sen_malaria_rssh = sen_malaria[gf_module%in%rssh_mods, .(grant, gf_module, gf_intervention, absorption, absorption_rating)]
sen_hiv_rssh = sen_hiv[gf_module%in%rssh_mods, .(grant, gf_module, gf_intervention, absorption, absorption_rating)]
sen_tb_rssh = sen_tb[gf_module%in%rssh_mods, .(grant, gf_module, gf_intervention, absorption, absorption_rating)]
sen_hivtb_rssh = sen_hivtb[gf_module%in%rssh_mods, .(grant, gf_module, gf_intervention, absorption, absorption_rating)]

uga_hiv_rssh = uga_hiv[gf_module%in%rssh_mods, .(grant, gf_module, gf_intervention, absorption, absorption_rating)]

#How many of these interventions had "below average" absorption? 
cod_malaria_rssh[, .N, by='absorption_rating']
cod_hiv_rssh[, .N, by='absorption_rating']
cod_tb_rssh[, .N, by='absorption_rating']
cod_hivtb_rssh[, .N, by='absorption_rating']

sen_malaria_rssh[, .N, by='absorption_rating']
sen_hiv_rssh[, .N, by='absorption_rating']
sen_tb_rssh[, .N, by='absorption_rating']
sen_hivtb_rssh[, .N, by='absorption_rating']

uga_hiv_rssh[, .N, by='absorption_rating']

sen_hiv_rssh[, .N, by=c('absorption_rating', 'grant')]

#-----------------------------------------
# Review budget data 
#-----------------------------------------
#Read in absorption data 
cod_budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/final_budgets.rds")
gtm_budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/final_budgets.rds")
sen_budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/SEN/prepped_data/final_budgets.rds")
uga_budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/final_budgets.rds")

#DRC 
cod_budgets_hiv = cod_budgets[grant_period=="2018-2020" & grant_disease=="hiv", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
cod_budgets_tb = cod_budgets[grant_period=="2018-2020" & grant_disease=="tb", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
cod_budgets_malaria = cod_budgets[grant_period=="2018-2020" & grant_disease=="malaria", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
cod_budgets_hivtb = cod_budgets[grant_period=="2018-2020" & grant_disease=="hiv/tb", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]

cod_budgets_hiv[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]
cod_budgets_tb[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]
cod_budgets_malaria[grant=="COD-M-MOH", .(grant, gf_module, gf_intervention, budget)][order(-budget)]
cod_budgets_malaria[grant=="COD-M-SANRU", .(grant, gf_module, gf_intervention, budget)][order(-budget)]

#UGA 
uga_budgets_hiv = uga_budgets[grant_period=="2018-2020" & grant_disease=="hiv", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
uga_budgets_tb = uga_budgets[grant_period=="2018-2020" & grant_disease=="tb", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
uga_budgets_malaria = uga_budgets[grant_period=="2018-2020" & grant_disease=="malaria", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
uga_budgets_hivtb = uga_budgets[grant_period=="2018-2020" & grant_disease=="hiv/tb", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]

uga_budgets_hiv[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]
uga_budgets_malaria[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]
uga_budgets_tb[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]
uga_budgets_hivtb[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]


#SEN 
sen_budgets_hiv = sen_budgets[grant_period=="2018-2020" & grant_disease=="hiv", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
sen_budgets_tb = sen_budgets[grant_period=="2018-2020" & grant_disease=="tb", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
sen_budgets_malaria = sen_budgets[grant_period=="2018-2020" & grant_disease=="malaria", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
sen_budgets_hivtb = sen_budgets[grant_period=="2018-2020" & grant_disease=="hiv/tb", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]

sen_budgets_hiv[grant=="SEN-H-CNLS", .(grant, gf_module, gf_intervention, budget)][order(-budget)]
sen_budgets_hiv[grant=="SEN-H-ANCS", .(grant, gf_module, gf_intervention, budget)][order(-budget)]
sen_budgets_tb[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]
sen_budgets_malaria[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]

#GTM budgets HIV 
gtm_budgets_hiv = gtm_budgets[grant_period=="2018-2020" & grant_disease=="hiv", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
gtm_budgets_tb = gtm_budgets[grant_period=="2019-2022" & grant_disease=="tb", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]
gtm_budgets_malaria = gtm_budgets[grant_period=="2019-2021" & grant_disease=="malaria", .(budget=sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]

gtm_budgets_hiv[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]
gtm_budgets_malaria[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]
gtm_budgets_tb[, .(grant, gf_module, gf_intervention, budget)][order(-budget)]
#---------------------------------------------
# REVIEW RSSH IN THE BUDGETS 
#---------------------------------------------
all_budgets = rbindlist(list(cod_budgets, gtm_budgets, sen_budgets, uga_budgets), use.names=TRUE)
all_budgets = all_budgets[grant_period%in%c("2018-2020", '2019-2021', '2016-2019', '2019-2022')]
all_budgets = all_budgets[current_grant==TRUE]

#Which RSSH modules have had the biggest spend by grant? 
rssh_by_mod = all_budgets[substring(code, 1, 1)=="R", .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'gf_module')]
rssh_by_mod[, grant_total:=sum(budget), by=c('grant', 'grant_period')]
rssh_by_mod[, mod_pct:=(budget/grant_total)*100, by=c('grant', 'grant_period', 'gf_module')]
rssh_by_mod = rssh_by_mod[order(grant, grant_period, mod_pct)]

#What percentage of each grant has been allocated to RSSH? 
rssh_pct_check = all_budgets[, .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'disease')][order(grant, grant_period, disease)]
rssh_pct_check[, grant_total:=sum(budget), by=c('grant', 'grant_period')]
rssh_pct_check[, disease_pct:=(budget/grant_total)*100, by=c('grant', 'grant_period', 'disease')]


# ADD THIS GRAPH INTO THE MAIN PLOTS! 
all_absorption =  rbindlist(list(cod, gtm, uga, sen))

current_absorption = all_absorption[ (start_date=="2018-01-01" & semester=="Semester 1") | 
                                       (start_date=="2018-01-01" & semester=="Semester 1-2")| 
                                       (start_date=="2019-01-01" & semester=="Semester 3")] #This will be different for Guatemala! 

current_by_country = current_absorption[, .(budget=sum(budget, na.rm=T), expenditure = sum(expenditure, na.rm=T)), by=c('grant', 'grant_period', 'semester', 'loc_name')]
current_by_country[, absorption:=round((expenditure/budget)*100, 2) ]

#Turn semester into a factor
current_by_country[semester=="Semester 1", semester:="1"]
current_by_country[semester=="Semester 1-2", semester:="2"]
current_by_country[semester=="Semester 3", semester:="3"]
current_by_country[, semester:=as.integer(semester)]

ggplot(current_by_country, aes(x=semester, y=absorption, color=loc_name)) + 
  geom_point() +
  geom_smooth() + 
  facet_wrap(~loc_name) + 
  theme_bw() + 
  labs(title="Trends in absorption from 2018-present", subtitle="Each point represents one grant", x="PUDR semester", y="Absorption (%)")


#-------------------------------------------------------
# How much of each budget has been program management? 
#-------------------------------------------------------
pm = all_budgets[, .(budget=sum(budget, na.rm=T)), by=c('loc_name', 'grant_disease', 'gf_module')]
pm[gf_module=="Program management", pm:=TRUE]
pm[is.na(pm), pm:=FALSE]

#We just want the % of PM versus everything else. 
pm[, total:=sum(budget), by=c('loc_name', 'grant_disease')]
pm[, pct:=round((budget/total)*100, 1)]
pm = pm[pm==TRUE]

#Does a larger budget correlate with higher program management costs? 
ggplot(pm, aes(x=total, y=pct)) + 
  geom_point() + 
  theme_bw() + 
  labs(title="Does a bigger grant correlate with a higher percentage of program management?")


# Is the size of the budget per intervention a good predictor of absorption? 
absorption_plot = all_absorption[absorption>200, absorption:=200]
ggplot(all_absorption_plot, aes(x=budget, y=absorption)) + 
  geom_point() + 
  theme_bw() 
