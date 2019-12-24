#--------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Combine 
# DATE: Last updated December 4, 2019 
#---------------------------------------------------------

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(readxl)

user = Sys.info()[[7]]
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/") #Change to your Box location if saved in another place
source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
#---------------------------------------
#Read in IHME data and format
#---------------------------------------
ihme_cc = readRDS(paste0(box, "tableau_data/all_cost_categories.rds"))
ihme_cc$cost_category <- NULL
setnames(ihme_cc, c('cleaned_cost_category', 'pudr_semester_financial'), c('cost_category', 'semester'))

# limit time period
ihme_cc = ihme_cc[grant_period=="2018-2020" & semester=="2-A"]

#Collapse
ihme_mi = get_cumulative_absorption(byVars=c('loc_name', 'grant', 'grant_period', 'gf_module'))
setnames(ihme_mi, c('budget', 'expenditure'), c('cumulative_budget', 'cumulative_expenditure'))

ihme_cc = ihme_cc[, .(cumulative_budget=sum(cumulative_budget), 
                    cumulative_expenditure=sum(cumulative_expenditure)), 
                by=c('loc_name', 'grant', 'grant_period', 'semester', 'cost_category')]

ihme_cc[cost_category=="13.1 Payment for Results", cost_category:="13.0 Payment for results"]

# Change country names 
ihme_mi[loc_name=="COD", loc_name:="DRC"]
ihme_mi[loc_name=="SEN", loc_name:="Senegal"]
ihme_mi[loc_name=="UGA", loc_name:="Uganda"]

ihme_cc[loc_name=="cod", loc_name:="DRC"]
ihme_cc[loc_name=="sen", loc_name:="Senegal"]
ihme_cc[loc_name=="uga", loc_name:="Uganda"]

# Merge in original budgets (NOT including revisions)
budgets = readRDS(paste0(box, "tableau_data/final_budgets.rds"))
budgets = budgets[grant_period=="2018-2020" & start_date>="2018-01-01" & start_date<"2019-07-01", .(original_budget=sum(budget)), by=c('grant', 'grant_period', 'gf_module')] #Only want cumulative budget through first 18 months! 

ihme_mi = merge(ihme_mi, budgets, by=c('grant', 'grant_period', 'gf_module'), all.x=T)

#-----------------------------------------------
#Read in EHG data and format 
#-----------------------------------------------
ehg_data = data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/ehg_2019_absorption_synthesis.xlsx"))

names(ehg_data) = c('loc_name', 'grant', 'grant_period', 'semester', 'module_cost_category', 'intervention', 'cumulative_absorption', 
                    'cumulative_expenditure', 'cumulative_budget', 'original_approved_budget', 
                    'expenditure_incl_commitments', 'x1', 'x2')
ehg_data = ehg_data[, -c('x1', 'x2')]

ehg_data[grant_period=="2018-20", grant_period:="2018-2020"]
ehg_data[semester=="1-3", semester:="Semester 3"]

# Split out cost category columns 
ehg_data[grepl(as.character(paste0(1:10, collapse="|")), module_cost_category), is_cc:=TRUE]
ehg_data[is.na(is_cc), is_cc:=FALSE]

#Split this data 
ehg_cc = ehg_data[is_cc==TRUE]
setnames(ehg_cc, 'module_cost_category', 'cost_category')
ehg_mi = ehg_data[is_cc==FALSE]
setnames(ehg_mi, 'module_cost_category', 'gf_module')

# Sum to the module or cost category level 
ehg_mi = ehg_mi[, .(cumulative_budget=sum(cumulative_budget), 
                    cumulative_expenditure=sum(cumulative_expenditure), original_budget=sum(original_approved_budget), 
                    expenditure_incl_commitments=sum(expenditure_incl_commitments)), 
                by=c('loc_name', 'grant', 'grant_period', 'semester', 'gf_module')]
ehg_cc = ehg_cc[, .(cumulative_budget=sum(cumulative_budget), 
                    cumulative_expenditure=sum(cumulative_expenditure), original_budget=sum(original_approved_budget), 
                    expenditure_incl_commitments=sum(expenditure_incl_commitments)), 
                by=c('loc_name', 'grant', 'grant_period', 'semester', 'cost_category')]

#Clean EHG data to match ours 
ehg_cc[, cost_category:=gsub("\\. ", "\\.0 ", cost_category)]
ehg_cc[cost_category=="11.0 Programme Administration costs (PA)", cost_category:="11.0 Indirect and Overhead Costs"]
ehg_cc[cost_category=="13.0 Payment for Results" | cost_category=="13.1 Payment for Results", cost_category:="13.0 Payment for results"]
#-------------
#Merge data 
#-------------
names(ihme_mi)[!names(ihme_mi)%in%names(ehg_mi)]
names(ehg_mi)[!names(ehg_mi)%in%names(ihme_mi)]

all_cost_categories = rbind(ehg_cc, ihme_cc, fill=T)
all_modules = rbind(ehg_mi, ihme_mi, fill=T)

# Fix global fund modules 
all_modules[, gf_module:=gsub("RSSH: ", "", gf_module)]
all_modules[gf_module=="Specific prevention interventions (SPI)", gf_module:="Specific prevention interventions"]
all_modules[gf_module=="PMTCT", gf_module:="Prevention of mother-to-child transmission"]
all_modules[gf_module=="Prevention programs for people who inject drugs (PWID) and their partners", 
            gf_module:="Comprehensive prevention programs for people who inject drugs and their partners"]
all_modules[gf_module=="Prevention programs for TGs", gf_module:="Comprehensive prevention programs for transgender people"]
all_modules[gf_module=="Prevention programs for MSM", gf_module:="Comprehensive prevention programs for men who have sex with men"]
all_modules[gf_module=="Human resources for health (HRH), including community health workers", 
            gf_module:="Human resources for health, including community health workers"]
all_modules[gf_module=="MDR-TB", gf_module:="Multidrug-resistant TB"]
all_modules[gf_module=="Health management information systems and M&E", gf_module:="Health management information system and monitoring and evaluation"]
all_modules[gf_module=="Comprehensive prevention programs for MSM", gf_module:="Comprehensive prevention programs for men who have sex with men"]
all_modules[gf_module=="Comprehensive prevention programs for TGs", gf_module:="Comprehensive prevention programs for transgender people"]
all_modules[gf_module=="Comprehensive prevention programs for people who inject drugs (PWID) and their partners", 
            gf_module:="Comprehensive prevention programs for people who inject drugs and their partners"]
all_modules = all_modules[!is.na(cumulative_budget) & !is.na(cumulative_expenditure)]


# Add grant disease 
all_cost_categories[, disease_split:=strsplit(grant, "-")]
potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')

for (i in 1:nrow(all_cost_categories)){
  if (all_cost_categories$disease_split[[i]][2]%in%potential_diseases){
    all_cost_categories[i, grant_disease:=sapply(disease_split, "[", 2 )]
  } else if (all_cost_categories$disease_split[[i]][3]%in%potential_diseases){
    all_cost_categories[i, grant_disease:=sapply(disease_split, "[", 3 )]
  } else if (all_cost_categories$disease_split[[i]][4]%in%potential_diseases){
    all_cost_categories[i, grant_disease:=sapply(disease_split, "[", 4 )]
  }
}

all_cost_categories[, disease_split:=NULL]

unique(all_cost_categories[!grant_disease%in%potential_diseases, .(grant, grant_disease)]) #Visual check that these all make sense. 

all_cost_categories[grant_disease=='C', grant_disease:='hiv/tb']
all_cost_categories[grant_disease=='H', grant_disease:='hiv']
all_cost_categories[grant_disease=='T', grant_disease:='tb']
all_cost_categories[grant_disease=='S' | grant_disease=='R', grant_disease:='rssh']
all_cost_categories[grant_disease=='M', grant_disease:='malaria']
all_cost_categories[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 
all_cost_categories[grant=="Combined TB", grant_disease:="hiv/tb"]
all_cost_categories[grant=="Combined HIV", grant_disease:="hiv/tb"]
all_cost_categories[grant=="Malaria", grant_disease:="malaria"]
all_cost_categories[grant=="HIV", grant_disease:="hiv"]
all_cost_categories[grant=="TB", grant_disease:="tb"]

stopifnot(unique(all_cost_categories$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))


all_modules[, disease_split:=strsplit(grant, "-")]
potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')

for (i in 1:nrow(all_modules)){
  if (all_modules$disease_split[[i]][2]%in%potential_diseases){
    all_modules[i, grant_disease:=sapply(disease_split, "[", 2 )]
  } else if (all_modules$disease_split[[i]][3]%in%potential_diseases){
    all_modules[i, grant_disease:=sapply(disease_split, "[", 3 )]
  } else if (all_modules$disease_split[[i]][4]%in%potential_diseases){
    all_modules[i, grant_disease:=sapply(disease_split, "[", 4 )]
  }
}

all_modules[, disease_split:=NULL]

unique(all_modules[!grant_disease%in%potential_diseases, .(grant, grant_disease)]) #Visual check that these all make sense. 

all_modules[grant_disease=='C', grant_disease:='hiv/tb']
all_modules[grant_disease=='H', grant_disease:='hiv']
all_modules[grant_disease=='T', grant_disease:='tb']
all_modules[grant_disease=='S' | grant_disease=='R', grant_disease:='rssh']
all_modules[grant_disease=='M', grant_disease:='malaria']
all_modules[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 
all_modules[grant=="Combined TB", grant_disease:="hiv/tb"]
all_modules[grant=="Combined HIV", grant_disease:="hiv/tb"]
all_modules[grant=="Malaria", grant_disease:="malaria"]
all_modules[grant=="HIV", grant_disease:="hiv"]
all_modules[grant=="TB", grant_disease:="tb"]

#Merge on abbreviated module
all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
setnames(all_mods, c('module_eng', 'intervention_eng', 'abbrev_mod_eng', 'abbrev_int_eng'), c('gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int'))
all_mods = unique(all_mods[, .(gf_module, abbrev_mod)])
all_modules = merge(all_modules, all_mods, by=c('gf_module'), all.x=T)
stopifnot(nrow(all_modules[is.na(abbrev_mod)])==0)


stopifnot(unique(all_modules$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))

# -------------------------------
# SAVE DATA 
#---------------------------------
saveRDS(all_cost_categories, "J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_cost_categories.rds")
saveRDS(all_modules, "J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_modules.rds")


