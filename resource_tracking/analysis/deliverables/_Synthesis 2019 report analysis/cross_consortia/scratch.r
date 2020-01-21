#--------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Scratch area for synthesis 
# (graphs might not be used in report)
# DATE: Last updated November 25, 2019 
#---------------------------------------------------------

rm(list=ls())
library(data.table) 
library(ggplot2) 
library(scales) 
library(readxl)

modules = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_modules.rds")
cost_categories = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_cost_categories.rds")
source("C:/Users/elineb/Documents/gf/resource_tracking/analysis/graphing_functions.r")
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/"
#------------------------------
# Analyses 
#------------------------------

#-----------------------------------------------------------------------------------
# What has been the variance, from quarter to quarter, of each activity description? 
# Order
ihme_budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets.rds")
plot_data = ihme_budgets[grant_period%in%c('2018-2020'), .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'disease', 'start_date', 'activity_description')]
plot_data = plot_data[start_date>="2018-01-01" & start_date<="2020-10-01"]
plot_data[, date_col:=paste0("q", quarter(start_date), "-", year(start_date))]
plot_data$start_date<- NULL
plot_data = dcast(plot_data, grant+grant_period+disease+activity_description~date_col, value.var='budget')

# Show how budget has changed between quarters 
quarters = c('q1-2018', 'q2-2018', 'q3-2018', 'q4-2018', 'q1-2019', 'q2-2019', 'q3-2019', 'q4-2019', 'q1-2020', 'q2-2020',
             'q3-2020', 'q4-2020')
for (i in 1:(length(quarters)-1)){
  plot_data[, paste0(quarters[i+1], "-change"):=(get(quarters[i+1])-get(quarters[i]))]
}
keep_cols = names(plot_data)[grepl("-change", names(plot_data))]
keep_cols = c(keep_cols, 'grant', 'grant_period', 'disease', 'activity_description')
plot_data = plot_data[, keep_cols, with=FALSE]

# Melt this data back 
plot_data = melt(plot_data, id.vars=c('grant', 'grant_period', 'disease', 'activity_description'))
plot_data[, quarter:=tstrsplit(variable, "-", keep=1)]
plot_data[, quarter:=gsub("q", "", quarter)]
plot_data[, quarter:=as.numeric(quarter)]
plot_data[, year:=tstrsplit(variable, "-", keep=2)]
plot_data[, year:=as.numeric(year)]

plot_data[, date:=year+((quarter/4)-0.25)]

# How many times is there zero-variance between quarters for a module? 
plot_data[value==0, zero_variance:=TRUE]
plot_data[is.na(zero_variance), zero_variance:=FALSE]
plot_data2 = plot_data[, .(num_quarters=.N), by=c('disease', 'date', 'zero_variance')]
plot_data2[, total:=sum(num_quarters), by=c('disease', 'date')]
plot_data2[, prop_no_variance:=round((num_quarters/total)*100, 1)]
plot_data2 = plot_data2[zero_variance==TRUE]

plot_data2[, disease:=toupper(disease)]
plot_data2[disease=="MALARIA", disease:="Malaria"]

p1 = ggplot(plot_data2, aes(x=date, y=prop_no_variance, fill=disease, label=paste0(prop_no_variance, "%"))) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(vjust=0, position=position_dodge(width=0.2)) + 
  theme_bw(base_size=16) + 
  labs(title="Proportion of activities whose budget did not change from one quarter to the next", x="Budget quarter",
       y="Proportion of activity descriptions across entire portfolio (0-100%)", fill="", caption="*Using data from DRC, Guatemala, Senegal, and Uganda only")

ggsave(paste0(save_loc, "zero_variance.png"), p1, height=8, width=15)


#-----------------------------------------------------------------------------------
# Is there a movement of funds away from RSSH activities towards disease-specific activities through grant revision? 
plot_data = modules[, .(loc_name, grant, abbrev_mod, cumulative_budget, original_budget)]
plot_data[, diff:=cumulative_budget-original_budget]
plot_data[diff>0 & !is.na(diff), change:="Additional funds allocated"]
plot_data[diff<0 & !is.na(diff), change:="Funds removed"]

plot_data = plot_data[, .(num_grants=.N), by=c('abbrev_mod', 'change')]
plot_data = plot_data[!is.na(change)] #Remove NAs at this point; you don't have to have every module in every grant. 
plot_data[, label:=paste0(num_grants)]
plot_data[change=="Funds removed", num_grants:=-num_grants]

#Get the absolute count of what's changed to order the graph. 
metadata = plot_data[, .(abbrev_mod, change, num_grants)]
metadata = dcast(plot_data, abbrev_mod~change, value.var=c('num_grants'))
metadata[is.na(`Additional funds allocated`), `Additional funds allocated`:=0]
metadata[is.na(`Funds removed`), `Funds removed`:=0]
metadata[, absolute_change:=`Additional funds allocated` + `Funds removed`]
metadata = metadata[, .(abbrev_mod, absolute_change)]

plot_data = merge(plot_data, metadata, by=c('abbrev_mod'), all.x=T)

p2 = ggplot(plot_data, aes(x=reorder(abbrev_mod, absolute_change), y=num_grants, fill=change, label=label)) + 
  geom_bar(stat="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  labs(title="PUDR cumulative budget compared to original budget", x="", 
       y="Number of grants with funds allocated or removed", fill="")

#-----------------------------------------------------------------------------------
# Program management - explore activities/cost categories and see what kind of things are happening under this module. 

# This will be difficult to do across consortia with the data we have. 

#-----------------------------------------------------------------------------------
# Has higher program management absorption led to higher overall grant absorption? 

# Just do two bars, one with program management absorption for each grant and one with overall absorption. 
pm = modules[abbrev_mod=="Program mgmt", .(cumulative_budget=sum(cumulative_budget), 
                                           cumulative_expenditure=sum(cumulative_expenditure), 
                                           original_budget_pm=sum(original_budget)), 
             by=c('loc_name', 'grant')]
pm[, pm_absorption:=round((cumulative_expenditure/cumulative_budget)*100, 1)]

all = modules[, .(cumulative_budget=sum(cumulative_budget), 
                  cumulative_expenditure=sum(cumulative_expenditure), 
                  original_budget_all=sum(original_budget)), 
             by=c('loc_name', 'grant')]
all[, overall_absorption:=round((cumulative_expenditure/cumulative_budget)*100, 1)]

plot_data = merge(pm, all, by=c('loc_name', 'grant'), all=T)
plot_data[, pm_pct_of_budget:=round((original_budget_pm/original_budget_all)*100, 1)]

p4 = ggplot(plot_data, aes(x=pm_absorption, y=overall_absorption)) + 
  geom_point() + 
  geom_smooth(method='lm') + 
  theme_bw(base_size=16) + 
  labs(title="Does high program management absorption /ncorrelate with higher overall absorption?", x="Program management absorption (%)", 
       y="Overall grant absorption (%)", subtitle="Absorption calculated over the period Jan. 2018-June 2019", 
       caption="*Each point represents one grant", size="Percentage of budget that is /nprogram management")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/program_mgmt_vs_absorption.png", p4, height=8, width=11)


# Review if a higher percentage of budget devoted to PM correlates to higher absorption overall. 
p5 = ggplot(plot_data, aes(x=pm_pct_of_budget, y=overall_absorption)) + 
  geom_point() + 
  geom_smooth(method='lm') + 
  theme_bw(base_size=16) + 
  labs(title="Does a higher budget percentage for program management/ncorrelate with higher overall absorption?",
       x="Percentage of budget given to program management (%)", 
       y="Overall grant absorption (%)", subtitle="Absorption calculated over the period Jan. 2018-June 2019", 
       caption="*Each point represents one grant", size="Percentage of budget that is /nprogram management")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/program_mgmt_vs_absorption_budget.png", p5, height=8, width=11)

pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/extra_synthesis_analyses.pdf", height=8, width=11)
p1
p2
p4
p5

dev.off() 

#--------------------------------
# How commoditized are the grants, and do more commoditized grants have higher absorption? 

#Map the parent categories 
parent_categories = data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/cost_category_mapping.xlsx"))
plot_data = copy(cost_categories)
plot_data[, category_code:=tstrsplit(cost_category, " ", keep=1)][, category_code:=as.numeric(category_code)]

parent_categories = parent_categories[, .(category_code, parent_category)]
plot_data = merge(plot_data, parent_categories, all.x=T)
stopifnot(nrow(plot_data[is.na(parent_category)])==0)

# What is the commoditization levels in the grants? 
plot_data[parent_category%in%c("Health Products - Pharmaceutical Products (HPPP)", "Health Products - Non-Pharmaceuticals (HPNP)"), 
          type:="Commodities"]
plot_data[is.na(type), type:="Non-commodities"]

plot_data1 = plot_data[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), by=c('loc_name', 'grant', 'type')]
plot_data1[, grant:=paste0(loc_name, ", ", grant)]
plot_data1[, total_budget:=sum(budget), by=c('grant')]
plot_data1[, commodity_pct:=round((budget/total_budget)*100, 1)]
plot_data1 = plot_data1[type=="Commodities"]

p = ggplot(plot_data1, aes(x=grant, y=commodity_pct, label=paste0(commodity_pct, "%"))) + 
  geom_bar(stat="identity", fill="blue") + 
  geom_text(hjust=0) + 
  theme_bw() + 
  coord_flip() + 
  labs(title="Percentage of each grant that is commodities", subtitle="Showing budget for January 2018-June 2019", 
       caption="*Commodities defined as cost categories 4 & 5", x="", y="Budget Percentage that is Commodities")

# What is the absorption level for each grant? 
plot_data1 = plot_data[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), by=c('loc_name', 'grant', 'type')]
plot_data1[, grant:=paste0(loc_name, ", ", grant)]
plot_data1[, absorption:=round((expenditure/budget)*100, 1)]
plot_data1 = plot_data1[, .(grant, budget, expenditure)]
p2 = budget_exp_bar(plot_data1, xVar='grant', altTitle="Absorption by grant")

pdf(paste0(save_loc, "commodity_absorption.pdf"), height=8, width=13)
p
p2
dev.off() 

# How many activities are in each of our 2018-2020 grant budgets? 
budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets.rds")
budgets = budgets[current_grant==TRUE]

acts_by_grant = unique(budgets[, .(grant, grant_period, activity_description)])
acts_by_grant = acts_by_grant[, .N, by=c('grant', 'grant_period')]
acts_by_disease = unique(budgets[, .(grant, grant_period, activity_description, disease)])
acts_by_disease = acts_by_disease[, .N, by=c('grant', 'grant_period', 'disease')]

# What's the average number of activities per grant? 
mean_activities = acts_by_disease[, .(activities = sum(N)), by=c('grant', 'grant_period')]
mean_activities[, mean(activities)]

# Can you plot the breakdown by disease graphically? 
plot_data = copy(acts_by_disease) 
plot_data[disease=="rssh", category:="RSSH"]
plot_data[is.na(category), category:="Not RSSH"]
plot_data = plot_data[, .(count=sum(N)), by=c('grant', 'grant_period', 'category')]

# merge_data = expand.grid(grant=unique(plot_data$grant), category=c('Not RSSH', 'RSSH'))
# plot_data = merge(plot_data, merge_data, by=c('grant', 'category'), all=T)
p = ggplot(plot_data, aes(x=paste0(grant, ", ", grant_period), y=count, fill=category)) + 
  geom_bar(stat="identity", position='dodge') + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  labs(title="Count of activity descriptions by grant", x="", y="Number of activities", fill="", caption="*Using data from DRC, Guatemala, Senegal, and Uganda only")

ggsave(paste0(save_loc, "count_of_activities.png"), p, height=8, width=12)

# Can you count how many times an activity's budget DOES NOT CHANGE AT ALL over the whole budget? 
acts_by_grant = unique(budgets[, .(grant, grant_period, activity_description, gf_module, gf_intervention, disease, budget, start_date)])
acts_by_grant = acts_by_grant[grant_period=="2018-2020"]

#Exclude all activities that have a sum total of $0 budgeted. 
acts_by_grant[, check:=sum(budget, na.rm=T), by=c('grant', 'grant_period', 'activity_description', 'gf_module', 
                                                  'gf_intervention', 'disease')]
acts_by_grant = acts_by_grant[check!=0]
acts_by_grant$check <- NULL

acts_wide = dcast(acts_by_grant, grant+grant_period+activity_description+gf_module+gf_intervention+disease~start_date, value.var='budget', fun.aggregate=sum)
acts_wide[abs(`2018-04-01`-`2018-01-01`)!=0 | 
            abs(`2018-07-01`-`2018-04-01`)!=0 |
            abs(`2018-10-01`-`2018-07-01`)!=0 |
            abs(`2019-01-01`-`2018-10-01`)!=0 |
            abs(`2019-04-01`-`2019-01-01`)!=0 |
            abs(`2019-07-01`-`2019-04-01`)!=0 |
            abs(`2019-10-01`-`2019-07-01`)!=0 |
            abs(`2020-01-01`-`2019-10-01`)!=0 |
            abs(`2020-04-01`-`2020-01-01`)!=0 |
            abs(`2020-07-01`-`2020-04-01`)!=0 |
            abs(`2020-10-01`-`2020-07-01`)!=0, diff:=TRUE]
acts_wide[is.na(diff), diff:=FALSE]

#Sum these up.
any_change = acts_wide[, .(count=.N), by=c('grant', 'grant_period', 'disease', 'gf_module', 'gf_intervention', 'diff')]
any_change[, country:=substr(grant, 1, 3)]
#review. 
by_disease_change = any_change[, .(count=sum(count)), by=c('disease', 'diff')][order(disease, diff)]
by_disease_change[, sum:=sum(count), by=c('disease')]
by_disease_change[, pct:=count/sum]

by_country_change = any_change[, .(count=sum(count)), by=c('disease', 'country', 'diff')][order(country, disease, diff)]
by_country_change[, sum:=sum(count), by=c('disease', 'country')]
by_country_change[, pct:=count/sum]
#---------------------------------------------------------------
# How does absorption look now as compared to earlier in the grants? 
consortia_2018 = readRDS("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/Synthesis 2018/prepped_2018_data.rds")
consortia_2019 = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_modules.rds")

# NAs and 0s aren't delineated in the 2018 cross consortia data. So, if budget is entered for a grant but all absorption is NA, assume it's 0. 
# EL 1/8/2020
check = consortia_2018[, .(budget=sum(budget, na.rm=T), absorption=sum(absorption, na.rm=T)), by='grant']
check = check[absorption==0] # Review these cases in the main dataset by hand. 
View(consortia_2018[grant%in%c('SEN-M-PNLP', 'GTM-H-HIVOS (Q1-Q2 2018)', 'SDN-H-UNDP (Q1-Q2 2018)', 'GTM-T-MSPAS (Q3 2017)', 'SEN-Z-MOH')])
# Everything BUT SEN-H-UNDP has reported budget. 
consortia_2018[grant%in%c('SEN-M-PNLP', 'GTM-H-HIVOS (Q1-Q2 2018)', 'GTM-T-MSPAS (Q3 2017)', 'SEN-Z-MOH'), absorption:=0]

# Calculate expenditure number for 2018 data so it can be collapsed. 
consortia_2018[, budget:=as.numeric(budget)]
consortia_2018[, absorption:=as.numeric(absorption)]
consortia_2018[, expenditure:=budget*absorption]

# Tag RSSH specifically. 
rssh_mods = c("Community responses and systems", "Financial management systems", "Health management information system and monitoring and evaluation", 
              "Human resources for health, including community health workers", "Integrated service delivery and quality improvement", "National health strategies", 
              "Procurement and supply chain management systems")
consortia_2018[, disease:=tolower(grant_disease)]
consortia_2019[, disease:=tolower(grant_disease)]
consortia_2018[gf_module%in%rssh_mods, disease:='rssh']
consortia_2019[gf_module%in%rssh_mods, disease:='rssh']

# Also separate HIV/TB. 
hiv_mods = c("HIV Testing Services", "Programs to reduce human rights-related barriers to HIV services", 
             "Prevention of mother-to-child transmission", "Comprehensive prevention programs for sex workers and their clients", 
             "Comprehensive prevention programs for people who inject drugs and their partners", "Prevention programs for general population", 
             "Comprehensive prevention programs for transgender people", "Prevention programs for adolescents and youth, in and out of school", 
             "Program management", "TB/HIV", "Treatment, care & support", "Comprehensive prevention programs for men who have sex with men", 
             "Prevention programs for other vulnerable populations", "Treatment, care and support")
tb_mods = c('TB care and prevention', "Multidrug-resistant TB", "Comprehensive programs for people in prisons and other closed settings")
consortia_2018[gf_module%in%hiv_mods, disease:='hiv']
consortia_2019[gf_module%in%hiv_mods, disease:='hiv']
consortia_2018[gf_module%in%tb_mods, disease:='tb']
consortia_2019[gf_module%in%tb_mods, disease:='tb']

unique(consortia_2018$disease)
unique(consortia_2019$disease)

# What has the absorption by disease area been overall? 
collapse_2018 = consortia_2018[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('disease')]
collapse_2018[, absorption:=round((expenditure/budget)*100, 1)]
collapse_2019 = consortia_2019[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by=c('disease')]
collapse_2019[, absorption:=round((expenditure/budget)*100, 1)]

# What's the average absorption for ALL grants/modules across 18 months? 
collapse_2019 = consortia_2019[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T))]
collapse_2019[, absorption:=round((expenditure/budget)*100, 1)]

# What's the variability in the cumulative 18 month absorption by module, and grant? 
collapse_2019 = consortia_2019[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by=c('gf_module')]
collapse_2019[, absorption:=round((expenditure/budget)*100, 1)]

collapse_2019 = consortia_2019[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by=c('grant')]
collapse_2019[, absorption:=round((expenditure/budget)*100, 1)]
