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
acts_by_grant = budgets[current_grant==TRUE & file_iteration=="final", .(budget=sum(budget)), by=c('grant', 'grant_period', 'activity_description', 'gf_module', 
                                                      'gf_intervention', 'disease', 'start_date')]
acts_by_grant = acts_by_grant[grant_period!="2016-2019"] # Drop the previous GTM grants because we have the new budget now.  

#Exclude all activities that have a sum total of $0 budgeted. 
acts_by_grant[, check:=sum(budget, na.rm=T), by=c('grant', 'grant_period', 'activity_description', 'gf_module', 
                                                  'gf_intervention', 'disease')]
acts_by_grant = acts_by_grant[check!=0]
acts_by_grant$check <- NULL

# Strangely, there are often more quarters listed in the detailed budgets than is planned for the grants. 
# Drop these extra quarters to calculate variance correctly. 
acts_by_grant = acts_by_grant[order(grant, grant_period, disease, gf_module, gf_intervention, activity_description, start_date)]
acts_by_grant[, num_quarters:=seq(1, 20, by=1), by=c('grant', 'grant_period', 'disease', 'gf_module', 'gf_intervention', 'activity_description')]
unique(acts_by_grant[, .(grant_period, max(num_quarters))])
acts_by_grant = acts_by_grant[!(grant_period%in%c('2018-2020', '2019-2021') & num_quarters>12)] # We should have max 12 quarters for these grant periods. 

# Calculate the variance across start dates. 
acts_by_grant[, variance:=var(budget), by=c('grant', 'grant_period', 'activity_description', 'gf_module', 'gf_intervention', 'disease')]
acts_by_grant[variance!=0, any_change:=TRUE]
acts_by_grant[variance==0, any_change:=FALSE]

#Make plot data.  
acts_by_grant[, country:=substr(grant, 1, 3)]
no_diff_pct = acts_by_grant[, .(count=.N), by=c('grant', 'country', 'any_change')][order(country, grant, any_change)]
no_diff_pct[, sum:=sum(count), by=c('grant', 'country')]
no_diff_pct[, pct:=round((count/sum)*100, 1)]
no_diff_pct = no_diff_pct[any_change==FALSE | pct==100] # The Guatemala grants always show variance. 
no_diff_pct[any_change==TRUE & pct==100, pct:=0]

# Graph by disease and country. 
p = ggplot(no_diff_pct, aes(x=reorder(grant, pct), y = pct, fill=country, label=paste0(pct, "%"))) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(position = position_dodge(width=0), size=5, vjust=-0.5) + 
  theme_bw(base_size=16) + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +  
  scale_y_continuous(lim=c(0, 50)) + 
  scale_fill_viridis_d() + 
  labs(title="Percentage of activities that had no change in budget for 3-year grant period", 
       y="Percentage of activities (%)", fill="", x="", caption="*GTM-T-MSPAS had a 4-year grant period, from 2019-2022")

ggsave(paste0(save_loc, "no_change_3_year.png"), p, height=8, width=14)

#-----------------------------------------------------------
# How many activities are added but have $0 (or NA) budgeted? 
acts_by_grant = budgets[current_grant==TRUE & file_iteration=="final", .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'activity_description')]
acts_by_grant = acts_by_grant[!is.na(activity_description) | activity_description==""] # Only count cases that actually have data in them. 
acts_by_grant = acts_by_grant[grant_period!="2016-2019"] # Drop the previous GTM grants because we have the new budget now.  
acts_by_grant[budget==0, zero_budget:=TRUE]
acts_by_grant[budget!=0, zero_budget:=FALSE]

#Calculate percentage. 
acts_by_grant[, country:=substr(grant, 1, 3)]
zero_pct = acts_by_grant[, .(count=.N), by=c('grant', 'grant_period', 'country', 'zero_budget')][order(country, grant, zero_budget)]
zero_pct[, sum:=sum(count), by=c('grant', 'country')]
zero_pct[, pct:=round((count/sum)*100, 1)]
zero_pct = zero_pct[zero_budget==TRUE | pct==100]
zero_pct[zero_budget==FALSE & pct==100, pct:=0]

p = ggplot(zero_pct, aes(x=reorder(grant, pct), y = pct, fill=country, label=paste0(pct, "%"))) + 
  geom_bar(stat="identity", position="dodge") + 
  geom_text(position = position_dodge(width=0), size=5, vjust=-0.5) + 
  theme_bw(base_size=16) + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +  
  scale_y_continuous(lim=c(0, 100)) + 
  scale_fill_viridis_d() + 
  labs(title="Percentage of activities with zero budget allocations for 3-year grant period", 
       y="Percentage of activities (%)", fill="", x="", caption="*GTM-T-MSPAS had a 4-year grant period, from 2019-2022")

ggsave(paste0(save_loc, "zero_budget_3_year.png"), p, height=8, width=14)

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


#--------------------------------
# PROGRAM MANAGEMENT 
#---------------------------------
plot_data = copy(cost_categories) 
plot_data[, category_code:=tstrsplit(cost_category, " ", keep=1)]
plot_data[, category_code:=as.numeric(category_code)]
category_map = data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/cost_category_mapping.xlsx"))

plot_data = merge(plot_data, category_map, by='category_code', all=TRUE)
stopifnot(nrow(plot_data[is.na(parent_category)])==0)

# I think a better way to visualize this will be with cost categories, rather than with interventions under the "program management" module. 
# Copying visualization code from SO1 graph. 
plot_data = plot_data[, .(budget=sum(cumulative_budget), expenditure=sum(cumulative_expenditure)), by=c('parent_category', 'loc_name')]
# Make disease label. 

#Collapse data 
plot_data[, absorption:=round((expenditure/budget)*100, 1)]
plot_data[absorption>=75.0, performance:="Excellent (>75%)"]
plot_data[absorption>=50.0 & absorption<75.0, performance:="Average (50-75%)"]
plot_data[absorption<50.0, performance:="Poor (<50%)"]
print(plot_data[is.na(performance)]) #Visual review. 
plot_data[is.na(performance), performance:="Data Unavailable"] # Verify that this is always true. 
plot_data[, num_countries_per_category:=.N, by=c('parent_category', 'performance')]
plot_data = unique(plot_data[, .(parent_category, performance, num_countries_per_category)])

# ***** DROPPING AREAS WHERE DATA IS UNAVAILABLE FOR NOW 
plot_data = plot_data[performance!="Data Unavailable"]

# Calculate the absolute difference to use as a sorting variable. 
merge_data = dcast(plot_data, parent_category~performance, value.var='num_countries_per_category')
merge_data[is.na(`Average (50-75%)`), `Average (50-75%)`:=0]
merge_data[is.na(`Excellent (>75%)`), `Excellent (>75%)`:=0]
merge_data[is.na(`Poor (<50%)`), `Poor (<50%)`:=0]

merge_data[, difference:=`Excellent (>75%)`-(`Average (50-75%)`+`Poor (<50%)`)]
merge_data = merge_data[, .(parent_category, difference)]
plot_data = merge(plot_data, merge_data, by=c('parent_category'))


plot_data[, total_countries_reporting:=sum(num_countries_per_category), by='parent_category']
plot_data[, y_proportion:=num_countries_per_category/total_countries_reporting]
plot_data[, label:=copy(as.character(num_countries_per_category))]

# Show as a bimodal distribution, with under capacity target showing as "negative". 
plot_data[performance%in%c('Average (50-75%)', 'Poor (<50%)'), num_countries_per_category:=-num_countries_per_category]

# Factor for grant ordering 
plot_data$performance <- factor(plot_data$performance, levels=c("Excellent (>75%)", "Poor (<50%)", "Average (50-75%)")) # Poor needs to come first because when 'average' and 'poor' are made negative, we want them to be in order still. 

# Try a bimodal distribution. 
p = ggplot(plot_data, aes(x=reorder(parent_category, difference), y=num_countries_per_category, fill=performance, label=label)) + 
  geom_bar(stat="identity") + 
  geom_text(size=4, position = position_stack(vjust=0.5)) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  scale_fill_manual(values=c('lightgreen', 'coral2', 'khaki1')) + 
  theme(axis.text.x=element_blank()) + 
  labs(title="Absorption by cost-category for PCE countries", subtitle="January 2018-June 2019", x="",
       y="Number of countries with this performance rating", fill="Meeting absorptive\ncapacity target (75%)?", 
       caption="Numbers shown out of 7 possible countries and\na multicountry RAI2E regional grant")

ggsave(paste0(save_loc, "absorption_by_cc.png"), p, height=8, width=14)

# ------------------------------------------
# Request after Feb. 2020 TERG meeting 
# How does absorption at 18 months in during this grant cycle compare with the same period during the last grant cycle? 

dt = data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gos/raw_data/Expenditure_at_InterventionSDA_Level_23_08_IHME.xlsx"))
names(dt) = c('department', 'region', 'country', 'grant', 'grant_name', 'grant_period_start', 'grant_period_end',
              'start_date', 'end_date', 'module', 'intervention', 'budget', 'expenditure')

dt = dt[, .(country, grant, grant_period_start, grant_period_end, start_date, end_date, module, intervention, budget, expenditure)]

dt[, start_year:=substr(grant_period_start, 1, 4)]
dt[, start_year:=as.integer(start_year)]
dt[, end_year:=substr(grant_period_end, 1, 4)]
dt[, end_year:=as.integer(end_year)]

dt_15_17 = dt[start_year==2015 & end_year==2017]
dt_15_17[, start_date:=as.Date(start_date)]
dt_15_17[, end_date:=as.Date(end_date)]
dt_15_17[, days_in_period:=end_date-start_date]

# Add in grant disease 
dt_15_17[, disease_split:=strsplit(grant, "-")]
potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')

for (i in 1:nrow(dt_15_17)){
  if (dt_15_17$disease_split[[i]][2]%in%potential_diseases){
    dt_15_17[i, grant_disease:=sapply(disease_split, "[", 2 )]
  } else if (dt_15_17$disease_split[[i]][3]%in%potential_diseases){
    dt_15_17[i, grant_disease:=sapply(disease_split, "[", 3 )]
  } else if (dt_15_17$disease_split[[i]][4]%in%potential_diseases){
    dt_15_17[i, grant_disease:=sapply(disease_split, "[", 4 )]
  }
}

dt_15_17[, disease_split:=NULL]

unique(dt_15_17[!grant_disease%in%potential_diseases, .(grant, grant_disease)]) #Visual check that these all make sense. 

dt_15_17[grant_disease=='C', grant_disease:='HIV/TB']
dt_15_17[grant_disease=='H', grant_disease:='HIV']
dt_15_17[grant_disease=='T', grant_disease:='TB']
dt_15_17[grant_disease=='S' | grant_disease=='R', grant_disease:='RSSH']
dt_15_17[grant_disease=='M', grant_disease:='Malaria']
dt_15_17[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 

stopifnot(unique(dt_15_17$grant_disease)%in%c('HIV', 'TB', 'HIV/TB', 'RSSH', 'Malaria'))

# tag diseases by module 
dt_15_17[grepl("RSSH|HSS", module), disease:="RSSH"]
dt_15_17[grepl("prevention programs|hiv", tolower(module)), disease:="HIV"] #This includes TB/HIV
dt_15_17[module%in%c('Vector control', 'Case management', 'Specific prevention interventions (SPI)'), disease:="Malaria"]
dt_15_17[module%in%c('PMTCT', 'Treatment, care and support', 'Removing legal barriers to access'), disease:="HIV"]
dt_15_17[module%in%c('MDR-TB', 'TB care and prevention'), disease:="TB"]

#Visual checks
unique(dt_15_17[, .(disease, module)][order(disease)]) #Only program management, other, and PBF left. 
mods_by_grant_disease = unique(dt_15_17[is.na(disease), module])

# label these modules that can be in any grant by their grant disease 
dt_15_17[module%in%mods_by_grant_disease, disease:=grant_disease]
dt_15_17[disease=="HIV/TB", disease:="HIV"] #Call all of these HIV for simplicity. 

#-------------------------------
# Analysis 

#How many grants can you make a perfect 18 month timeline for? 
reporting_matrix = unique(dt_15_17[, .(grant, grant_period_start, start_date)][order(grant, grant_period_start, start_date)])
reporting_matrix[, period_id:=paste0("period", seq(0, 25, by=1)), by=c('grant', 'grant_period_start')]
reporting_matrix = dcast(reporting_matrix, grant+grant_period_start~period_id, value.var=c('start_date'))

# How many unique lines by grant and disease are reporting for each period? 
collapse = dt_15_17[grant_period_start == start_date] # Make sure that you're getting the FIRST 18 months. 
collapse = dt_15_17[, .(budget=sum(budget), expenditure=sum(expenditure)), by=c('grant', 'country', 'start_date', 'end_date', 'disease', 'days_in_period')]
collapse[, .N, by=c('days_in_period')][order(days_in_period)]

# If you just subset to 540 days (18 months), what is the average absorption by disease? 
collapse[days_in_period==540, .(absorption=round((sum(expenditure, na.rm=T)/sum(budget, na.rm=T))*100, 1)), by='disease']

# How about 540 - 724 (18 months to 24 months?)
collapse[days_in_period>=540 & days_in_period<=724, .(absorption=round((sum(expenditure, na.rm=T)/sum(budget, na.rm=T))*100, 1)), by='disease']
