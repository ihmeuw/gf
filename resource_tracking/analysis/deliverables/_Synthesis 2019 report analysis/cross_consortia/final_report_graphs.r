#--------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Print final cross-consortia synthesis report graphs
# DATE: Last updated November 25, 2019 
#---------------------------------------------------------

rm(list=ls()) 

library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(readxl)
library(openxlsx)

#-------------
#Read in data 
#-------------
all_modules = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_modules.rds")
source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
#--------------------
# SO1 analyses
#--------------------
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/SO1/"

# Make a general graph of cumulative country-level absorption 
plot_data = all_modules[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by='loc_name']
plot_data[, label:=paste0(dollar(expenditure), " (", round((expenditure/budget)*100, 1), "%)")]
plot_data = melt(plot_data, id.vars=c('loc_name', 'label'))
plot_data[variable=="budget", label:=""]
plot_data[variable=="budget", variable:="Budget"]
plot_data[variable=="expenditure", variable:="Expenditure"]

p = ggplot(plot_data, aes(x=loc_name, y=value, fill=variable, label=label)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Absorption for PCE countries", subtitle="January 2018-June 2019", x="Country", y="Budget/Expenditure (USD)", fill="")
ggsave(paste0(save_loc, "absorption_summary.png"), p, height=8, width=11)

# Make a general graph of cumulative country-level absorption, by disease 
plot_data = all_modules[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by=c('loc_name', 'grant_disease')]
plot_data[, grant_disease:=toupper(grant_disease)]
plot_data[grant_disease=="MALARIA", grant_disease:="Malaria"]
p = budget_exp_bar(plot_data, xVar='loc_name', facetVar='grant_disease', 
                   altTitle="Absorption for PCE countries, by disease", 
                   altSubtitle="January 2018-June 2019", angleText=TRUE)
ggsave(paste0(save_loc, "absorption_summary_disease.png"), p, height=8, width=11)

# Want a module-level visual for all of the countries combined. 
# Try this two ways. One that shows simply how they're performing in terms of absorptive capacity. 
# Then, one that scales to the average absorption level of the grant. 
plot_data = all_modules[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), by=c('abbrev_mod', 'loc_name')]
# Make disease label. 
all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
setnames(all_mods, c('abbrev_mod_eng'), c('abbrev_mod'))
all_mods = unique(all_mods[, .(abbrev_mod, disease)])
all_mods = all_mods[!abbrev_mod%in%c('Program mgmt', 'Unspecified', 'TB/HIV')] # Drop these three so as not to expand dataset. 
plot_data = merge(plot_data, all_mods, by=c('abbrev_mod'), all.x=T)

# Fix diseases so that there's just one disease per module. 
plot_data[abbrev_mod=="Program mgmt", disease:="all"]
plot_data[abbrev_mod=="Unspecified", disease:="all"]
plot_data[abbrev_mod=="TB/HIV", disease:="hiv"]

# Make sure that there's just one module per disease. 
check = unique(plot_data[, .(abbrev_mod, disease)])
check[, dup:=.N, by='abbrev_mod']
check = check[dup!=1]
stopifnot(nrow(check)==0)
stopifnot(nrow(plot_data[is.na(disease)])==0)

#Collapse data 
plot_data[, absorption:=round((expenditure/budget)*100, 1)]
plot_data[absorption>=75.0, performance:="Excellent (>75%)"]
plot_data[absorption>=50.0 & absorption<75.0, performance:="Average (50-75%)"]
plot_data[absorption<50.0, performance:="Poor (<50%)"]
print(plot_data[is.na(performance)]) #Visual review. 
plot_data[is.na(performance), performance:="Data Unavailable"] # Verify that this is always true. 
plot_data[, num_countries_per_category:=.N, by=c('abbrev_mod', 'disease', 'performance')]
plot_data = unique(plot_data[, .(abbrev_mod, performance, num_countries_per_category, disease)])

# ***** DROPPING AREAS WHERE DATA IS UNAVAILABLE FOR NOW 
plot_data = plot_data[performance!="Data Unavailable"]

# Calculate the absolute difference to use as a sorting variable. 
merge_data = dcast(plot_data, abbrev_mod+disease~performance, value.var='num_countries_per_category')
merge_data[is.na(`Average (50-75%)`), `Average (50-75%)`:=0]
merge_data[is.na(`Excellent (>75%)`), `Excellent (>75%)`:=0]
merge_data[is.na(`Poor (<50%)`), `Poor (<50%)`:=0]

merge_data[, difference:=`Excellent (>75%)`-(`Average (50-75%)`+`Poor (<50%)`)]
merge_data = merge_data[, .(abbrev_mod, disease, difference)]
plot_data = merge(plot_data, merge_data, by=c('abbrev_mod', 'disease'))

# Make it easier to see where the categories are
plot_data[disease=="hiv", abbrev_mod:=paste0("HIV: ", abbrev_mod)]
plot_data[disease=="tb", abbrev_mod:=paste0("TB: ", abbrev_mod)]
plot_data[disease=="malaria", abbrev_mod:=paste0("Malaria: ", abbrev_mod)]
plot_data[disease=="rssh", abbrev_mod:=paste0("RSSH: ", abbrev_mod)]

plot_data[, total_countries_reporting:=sum(num_countries_per_category), by='abbrev_mod']
plot_data[, y_proportion:=num_countries_per_category/total_countries_reporting]
plot_data[, label:=as.character(num_countries_per_category)]

plot_data$performance <- factor(plot_data$performance, levels=c("Excellent (>75%)", "Average (50-75%)", "Poor (<50%)"))

# Show as a bimodal distribution, with under capacity target showing as "negative". 
plot_data[performance%in%c('Average (50-75%)', 'Poor (<50%)'), num_countries_per_category:=-num_countries_per_category]

# Try a bimodal distribution. 
p = ggplot(plot_data, aes(x=reorder(abbrev_mod, difference), y=num_countries_per_category, fill=performance, label=label)) + 
  geom_bar(stat="identity") + 
  geom_text(size=4, position = position_stack(vjust=0.5)) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  scale_fill_brewer(palette=c('RdYlGn'), direction=-1) + 
  theme(axis.text.x=element_blank()) + 
  labs(title="Absorption by module for PCE countries", subtitle="January 2018-June 2019", x="Module",
       y="Number of countries with this performance rating", fill="Meeting absorptive\ncapacity target (75%)?", 
       caption="Numbers shown out of 8 possible countries,\nnot including Guatemala data and\nincluding RAI2E malaria regional grant")

ggsave(paste0(save_loc, "absorption_by_mod1.png"), p, height=8, width=11)


# Make a graph showing absorption for key matching funds areas - HIV (HR, AGYW, KVP), RSSH data systems, TB missing cases 
plot_data = copy(all_modules)
plot_data[abbrev_mod=="Human rights barriers", category:="HIV: Human rights"]
plot_data[abbrev_mod=="Prevention programs for youth/adol.", category:="HIV: AGYW"]
plot_data[abbrev_mod%in%c("Prevention programs for MSM", "Prevention programs for PWID", 
                          "Prevention programs for prisoners", "Prevention programs for CSW & clients",
                          "Prevention programs for other KVP", "Prevention programs for transgender"), 
          category:="HIV: KVP"]
plot_data[abbrev_mod=="Info systems & M&E", category:="RSSH data systems"]

plot_data = plot_data[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), 
          by=c('loc_name', 'category')]
plot_data = plot_data[!is.na(category)]

# Drop rows that didn't actually have matching funds
plot_data = plot_data[loc_name%in%c('DRC', 'Mozambique', 'Myanmar', 'Senegal', 'Uganda')]
plot_data = plot_data[!(loc_name=="DRC" & category%in%c('HIV: AGYW', 'HIV: KVP'))]
plot_data = plot_data[!(loc_name=="Mozambique" & category%in%c('HIV: KVP'))]
plot_data = plot_data[!(loc_name=="Myanmar" & category%in%c('HIV: Human rights', 'HIV: AGYW'))]
plot_data = plot_data[!(loc_name=="Senegal" & category%in%c('HIV: AGYW', 'RSSH data systems'))]
plot_data = plot_data[!(loc_name=="Uganda" & category%in%c('HIV: KVP', "RSSH data systems"))]

p = budget_exp_bar(plot_data, xVar='loc_name', facetVar='category', angleText=TRUE, 
                   altCaption="*Not possible to calculate absorption for missing TB cases", 
                   altTitle="Absorption for strategic priority areas")
ggsave(paste0(save_loc, "strategic_area_absorption.png"), p, height=8, width=14)

# Fill in the sentence "a total of $X million has gone unspent within the first 18 months of grant implementation." 
total_budget = sum(all_modules$cumulative_budget, na.rm=T)
total_expenditure =sum(all_modules$cumulative_expenditure, na.rm=T)
dollar(total_budget - total_expenditure) 
#--------------------
# SO2 analyses
#--------------------
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/SO2/"
# Just make a general graph of RSSH modules 
rssh_mods = c('Health management information system and monitoring and evaluation', "Human resources for health, including community health workers", 
              "Community responses and systems", "National health strategies", "Integrated service delivery and quality improvement", 
              "Procurement and supply chain management systems", "Financial management systems")
rssh = all_modules[gf_module%in%rssh_mods]

plot_data = rssh[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), 
                 by=c('abbrev_mod')]
p = budget_exp_bar(plot_data, xVar='abbrev_mod', altTitle="RSSH absorption across all PCE countries", altSubtitle="January 2018-June 2019")
ggsave(paste0(save_loc, "rssh_overview.png"), p, height=8, width=11)

# Look at what absorption has been like by country and module. Maybe as a table? 
plot_data = rssh[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), 
                 by=c('abbrev_mod', 'loc_name')]
total = plot_data[, .(budget=sum(budget), expenditure=sum(expenditure)), by=c('loc_name')] # Append on the total, collapsed by modules. 
total[, abbrev_mod:="Total"]
plot_data = rbind(plot_data, total, use.names=T)

# Also review what the total is across countries, by module. 
rssh_by_module = plot_data[, .(budget=sum(budget), expenditure=sum(expenditure)), by=c('abbrev_mod')]
rssh_by_module[, absorption:=round((expenditure/budget)*100, 1)]

plot_data[, absorption:=round((expenditure/budget)*100, 1)]
plot_data = plot_data[, .(abbrev_mod, loc_name, absorption)]
plot_data = dcast(plot_data, abbrev_mod~loc_name, value.var='absorption')
setnames(plot_data, 'abbrev_mod', 'Module')

# R doesn't have a really good way of formatting tables, so I'm just exporting this as an Excel and adding a heat map. 
# write.xlsx(plot_data, paste0(save_loc, "rssh_absorption_countries.xlsx"))

#--------------------
# SO3 analyses
#--------------------
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/SO3/"

# Figure of achievement on HIV/TB performance indicators related to KVPs/gender/HR

# Figure of achievement on malaria performance indicators related to pregnant women, and any other available KPs

# Remake percent of HIV budgets devoted to AGYW, HR, and KP modules - distinguish the three. 
kp_mods = c("Prevention programs for men who have sex with men",  "Prevention programs for sex workers and their clients", 
            "Prevention programs for people who inject drugs and their partners", "Prevention programs for transgender people", 
            "Comprehensive programs for people in prisons and other closed settings", "Prevention programs for other vulnerable populations")

plot_data = all_modules[grant_disease%in%c('hiv/tb', 'hiv')]
plot_data[gf_module%in%kp_mods, category:="Key populations"]
plot_data[gf_module=="Programs to reduce human rights-related barriers to HIV services", category:="Human rights"]
plot_data[gf_module=="Prevention programs for adolescents and youth, in and out of school", category:="AGYW"]
plot_data[is.na(category), category:="Other"]

plot_data = plot_data[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by=c('category', 'loc_name')]
plot_data[, label:=paste0(dollar(expenditure), " (", round((expenditure/budget)*100), "%)")] #Add label that shows expenditure amount and absorption percentage. 
plot_data = melt(plot_data, id.vars=c('category', 'loc_name', 'label'))
plot_data[variable=="budget", label:=""]
plot_data[variable=="budget", variable:="Budget"]
plot_data[variable=="expenditure", variable:="Expenditure"]

p = ggplot(plot_data, aes(x=loc_name, y=value, fill=variable, label=label)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  facet_wrap(~category) + 
  theme(axis.text.x=element_text(angle=20, vjust=0.5)) + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="HIV absorption for KP, HR, and AGYW", subtitle="January 2018-June 2019", x="Country", y="Budget/Expenditure (USD)", fill="")
ggsave(paste0(save_loc, "absorption_kp_hr_agyw.png"), p, height=8, width=14)

# Remake percent of TB budgets devoted to KP modules.
# First, re-prep our data and bind with intervention-level data from EHG. 
ihme = get_cumulative_absorption(byVars=c('gf_module', 'gf_intervention', 'loc_name'), diseaseSubset="tb")
ehg = data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/Absorption for synthesis v3.xlsx", 
                           sheet="TB KVPs"))
names(ehg) = c('loc_name', 'grant', 'grant_period', 'semester', 'gf_module', 'gf_intervention', 'absorption', 'expenditure', 'budget', 'original_budget', 
               'expenditure_with_commitments')
ehg = ehg[, .(gf_module, gf_intervention, loc_name, budget, expenditure, absorption)]

ihme = ihme[grepl("Key populations", gf_intervention)]

plot_data = rbind(ihme, ehg, use.names=T)

# Clean data 
plot_data[grep("Others", gf_intervention), gf_intervention:="Others"]
plot_data[grep("Prisoners", gf_intervention), gf_intervention:="Prisoners"]
plot_data[, gf_module:=gsub(" care and prevention", "", gf_module)]

plot_data[loc_name=="COD", loc_name:="DRC"]
plot_data[loc_name=="SEN", loc_name:="Senegal"]
plot_data[loc_name=="UGA", loc_name:="Uganda"]

plot_data[, abbrev_int:=paste0(gf_module, ": ", gf_intervention)]

# Collapse data one more time. 
plot_data = plot_data[, .(budget=sum(budget), expenditure=sum(expenditure, na.rm=T)), by=c('loc_name', 'abbrev_int')]
p = budget_exp_bar(plot_data, xVar='loc_name', facetVar='abbrev_int', yScaleMax=700000, altTitle = "Absorption for TB key populations", altSubtitle="January 2018-January 2019")
ggsave(paste0(save_loc, "absorption_kp_tb.png"), p, height=8, width=18)

# Fill in interventions table 


# Export two annex tables 
# 1. Module, country, budget, expenditure, absorption 
table1 = all_modules[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), by=c('gf_module', 'loc_name')]
table1[, absorption:=round((expenditure/budget)*100, 1)]
write.csv(table1, "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/absorption_annex1.csv", row.names=F)
# 2. Module, budget, expenditure, absorption
table2 = all_modules[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), by=c('gf_module')]
table2[, absorption:=round((expenditure/budget)*100, 1)]
write.csv(table2, "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/absorption_annex2.csv", row.names=F)

