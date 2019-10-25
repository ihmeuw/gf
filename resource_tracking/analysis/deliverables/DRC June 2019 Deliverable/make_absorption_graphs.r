#-------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Prepare absorption graphs for DRC on CCM for 
# June malaria model deliverable 
# DATE: June 24, 2019
#--------------------------------------------------------

rm(list=ls())
setwd("C:/Users/elineb/Documents/gf") #Set to root of repository 
source("./resource_tracking/prep/_common/set_up_r.r") #Set up base resource tracking filepaths. 
output_dir = "J:/Project/Evaluation/GF/resource_tracking/visualizations/miscellaneous/DRC Malaria Deliverable June 2019/"

#Read in data 
absorption = readRDS(paste0(dir, "_gf_files_gos/cod/prepped_data/absorption_cod.rds"))
absorption = absorption[grant_disease=="malaria" & grant_period=="2018-2020"]
budgets = readRDS(paste0(dir, "_gf_files_gos/combined_prepped_data/final_budgets.rds"))
budgets = budgets[loc_name=="cod"]

budgets[code%in%c("M2_3", 'R7_1', 'R7_2') & grant_period == "2018-2020" & grant_disease=="malaria", 
        .(budget = sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]

all_ints = fread(paste0(mapping_dir, "all_interventions.csv"))
all_ints = unique(all_ints[disease%in%c("malaria", "rssh"), .(code, abbrev_mod_eng, abbrev_int_eng)])

#Create tables of all unique values of intervention/semester for graphing later. 
absorption = merge(absorption, all_ints, by='code', all.x=T)
stopifnot(nrow(absorption[is.na(abbrev_int_eng)])==0)

potential_ints = expand.grid(abbrev_int_eng = unique(absorption$abbrev_int_eng), grant = c("COD-M-MOH", "COD-M-SANRU"))
#----------------------------------------------------------
#1. Print off descriptive stats using different groupings
#----------------------------------------------------------

#For all of 2018, what was the overall absorption rate for the two malaria grants?
dt1 = absorption[grant_disease=="malaria" & grant_period=="2018-2020" & semester == "Semester 1-2", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T))]
dt1[, absorption:=(expenditure/budget)*100]

#For both S1 and S1-2, what was the absorption by intervention for the target grants? 
byVars = c('grant','abbrev_int_eng', 'code')
dt2 = absorption[grant_disease=="malaria" & grant_period=="2018-2020" & semester=="Semester 1-2", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=byVars]
dt2[, absorption:=(expenditure/budget)*100]
dt2 = merge(dt2, potential_ints, by=c('abbrev_int_eng', 'grant'), allow.cartesian=T, all.y=T)
dt2[is.na(absorption), absorption:=0]

#What did absorption look like for community-based interventions? (Need to review raw data to see what these activity descriptions are)
dt2[code%in%c("M2_3", 'R7_1', 'R7_2')]


#---------------------------------------------------------
# 2. Graph data 
#---------------------------------------------------------
dt2[abbrev_int_eng=="Supply chain infrastructure and development of tools", abbrev_int_eng:="Supply chain infrastructure"]

ggplot(dt2[absorption<=200], aes(x=abbrev_int_eng, y=absorption, fill=grant)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_bw(base_size=12) + 
  coord_flip() + 
  labs(title = "2018 absorption by intervention for current DRC malaria grants",
       x="Intervention", y="Absorption (%)", fill="Grant", 
       caption="*Showing observations with 200% absorption or less")
  
ggsave(paste0(output_dir, "absorption_by_intervention.png"))  
  
  
  
  