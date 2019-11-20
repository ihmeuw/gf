#------------------------------------------------------
# Review how denominators between budgets and PUDRs have changed 
# Through revision, and through absorption. 
#-------------------------------------------------------

library(data.table) 
library(networkD3) 
library(ggplot2) 

options(scipen=100)

# Want to show a Sankey diagram where the first column is the original grant budget, 
# The second column is the first PUDR denominator, 
# the third column is the third PUDR denominator, and fourth is the fourth PUDR denominator. 
# Amounts should be percentage of the total document so these amounts are comparable. 

#--------------------------------
# DRC 
#--------------------------------
cod_absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/absorption_cod.rds")
gtm_absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/absorption_gtm.rds")
sen_absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/SEN/prepped_data/absorption_sen.rds") 
uga_absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/UGA/prepped_data/absorption_uga.rds") 

absorption = rbindlist(list(cod_absorption, gtm_absorption, sen_absorption, uga_absorption))

budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets.rds")

# Make a budget dataset that has the same time periods as the PUDRs. 
s1_budget = budgets[grant_period=="2018-2020" & start_date>="2018-01-01" & start_date<"2018-07-01", 
                    .(budget = sum(budget, na.rm=T), semester="Semester 1"), 
                    by=c('grant', 'gf_module')]
s1_2_budget = budgets[grant_period=="2018-2020" & start_date>="2018-01-01" & start_date<"2019-01-01", 
                    .(budget = sum(budget, na.rm=T), semester="Semester 1-2"), 
                    by=c('grant', 'gf_module')]
s2_budget = budgets[grant_period=="2018-2020" & start_date>="2018-07-01" & start_date<"2019-01-01", 
                    .(budget = sum(budget, na.rm=T), semester="Semester 2"), 
                    by=c('grant', 'gf_module')]
s3_budget = budgets[grant_period=="2018-2020" & start_date>="2019-01-01" & start_date<"2019-04-01", 
                    .(budget = sum(budget, na.rm=T), semester="Semester 3"), 
                    by=c('grant', 'gf_module')]

budgets_by_semester = rbindlist(list(s1_budget, s1_2_budget, s2_budget, s3_budget))

absorption = absorption[grant_period=="2018-2020", .(pudr_budget=sum(budget)), by=c('grant', 'gf_module', 'semester')]

#Merge all of these datasets together. 
dt = merge(budgets_by_semester, absorption, by=c('grant', 'gf_module', 'semester'), all=T)
dt[is.na(budget)] #Just review these cases where there might have been a problematic merge. 

# Merge on abbreviated module.
all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
setnames(all_mods, c('module_eng', 'intervention_eng', 'abbrev_mod_eng', 'abbrev_int_eng'), c('gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int'))
all_mods = unique(all_mods[, .(gf_module, abbrev_mod)])
dt = merge(dt, all_mods, by=c('gf_module'), allow.cartesian=TRUE)

#Generate the difference between budget and PUDR budget. 
dt[, diff:=round(budget-pudr_budget)]
dt[, pct_change_from_original:=round(((pudr_budget-budget)/budget)*100, 1)]

#-------------------------------------------------------
# Make a dataset that compares differences in PUDR budget denominators 
# by module. 
# ------------------------------------------------------
module_wide = copy(absorption)
module_wide[semester=="Semester 1", semester:="S1"]
module_wide[semester=="Semester 1-2", semester:="S1-2"]
module_wide[semester=="Semester 2", semester:="S2"]
module_wide[semester=="Semester 3", semester:="S3"]

module_wide = merge(module_wide, all_mods, by='gf_module', all.x=T)
module_wide[, pudr_total:=sum(pudr_budget), by=c('grant', 'semester')]
module_wide[, label:=paste0(round((pudr_budget/pudr_total), 1))]

pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/budget_pudr_denominators.pdf", height=8, width=11)
for (g in unique(dt$grant)) {
  subset = dt[grant==g]
  subset = subset[!is.na(pudr_budget)]
  melt = subset[, .(grant, abbrev_mod, semester, diff, budget, pudr_budget)]
  melt = melt(melt, id.vars=c('grant', 'abbrev_mod', 'semester', 'diff'))
  melt[variable=="budget", concat:=paste0("Detailed Budget, ", semester)]
  melt[variable=="pudr_budget", concat:=paste0("PUDR Budget, ", semester)]
  melt$concat = factor(melt$concat, levels=c("Detailed Budget, Semester 3", "PUDR Budget, Semester 3", 
                                             "Detailed Budget, Semester 2", "PUDR Budget, Semester 2",
                                             "Detailed Budget, Semester 1-2", "PUDR Budget, Semester 1-2",
                                             "Detailed Budget, Semester 1", "PUDR Budget, Semester 1"), 
                       labels=c("Detailed Budget, Semester 3", "PUDR Budget, Semester 3", 
                                "Detailed Budget, Semester 2", "PUDR Budget, Semester 2",
                                "Detailed Budget, Semester 1-2", "PUDR Budget, Semester 1-2",
                                "Detailed Budget, Semester 1", "PUDR Budget, Semester 1"))
  melt[, label:=dollar(value)]
  
  # Then plot the difference above or below the x-axis. 
  p1 = ggplot(subset, aes(x=semester, y=pct_change_from_original, color=abbrev_mod, group=abbrev_mod)) + 
    geom_point() + 
    geom_line() + 
    geom_hline(yintercept=0, color="black") + 
    theme_bw(base_size=18) + 
    labs(title=paste0("Comparison of differences for ", g, " between PUDR budgets \nand detailed budgets in same time period"), 
         x="", y="Percent difference between detailed \nbudget and PUDR budget", color="", subtitle="January 2018-June 2019", 
         caption="*When dot is below the x-axis, the PUDR was smaller than the original budget")
  print(p1)
  
  # Make a bar plot showing how each module has changed for each grant. 
  p2 = ggplot(module_wide[grant==g], aes(x=semester, y=pudr_budget, fill=abbrev_mod)) + 
    geom_bar(stat="identity", position="fill") + 
    theme_bw() + 
    labs(title=paste0("Percentage of each module in each\nPUDR budget for ", g), x="PUDR semester", y="Percentage of PUDR budget", fill="")
  print(p2) 
}
dev.off() 

# Make a graph that answers the question - when they have cost savings from absorption, do they allocate all of them to the same module? 
# Or when a module is low-absorbing, do they move money away? 
# Things to quantify: 
# 1.) How much do they absorb in each period. 
# 2.) How much money is moved away in the next period. 

absorption = rbindlist(list(cod_absorption, gtm_absorption, sen_absorption, uga_absorption))
absorption = absorption[grant_period=="2018-2020", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                        by=c('grant', 'gf_module', 'gf_intervention', 'semester', 'loc_name')]

all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
setnames(all_mods, c('module_eng', 'intervention_eng', 'abbrev_mod_eng', 'abbrev_int_eng'), c('gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int'))
all_mods = unique(all_mods[, .(gf_module, gf_intervention, abbrev_mod, abbrev_int)])
absorption = merge(absorption, all_mods, by=c('gf_module', 'gf_intervention'), all.x=T)

absorption[semester=="Semester 1", semester:="S1"]
absorption[semester=="Semester 1-2", semester:="S1-2"]
absorption[semester=="Semester 2", semester:="S2"]
absorption[semester=="Semester 3", semester:="S3"]

# Try just doing this for RSSH, KP, and HR modules. 
rssh_mods = c("Info systems & M&E", "HR & health workers", "Community systems", "PSM",  "Nat. health strategies", "Service delivery", "Financial systems")
kp_mods = c("Prevention programs for CSW & clients", "Prevention programs for IJU", "Prevention programs for MSM",           
            "Prevention programs for transgender", "Prevention programs for youth/adol.", "Prevention programs for other KVP", 
            "Prevention programs for prisoners")
kp_ints = unique(absorption$abbrev_mod[grep("key populations", tolower(absorption$abbrev_mod))])
hr_mods = c("Human rights barriers")

absorption[abbrev_mod%in%rssh_mods, type:="RSSH"]
absorption[abbrev_mod%in%kp_mods | abbrev_int%in%kp_ints, type:="KP"]
absorption[abbrev_mod%in%hr_mods, type:="Human rights"]
absorption[is.na(type), type:="All other modules"]

absorption[, absorption:=round((expenditure/budget)*100, 1), by=c('grant', 'type', 'semester', 'loc_name')]

abs_wide = absorption[, .(grant, loc_name, semester, type, budget, expenditure)]
abs_wide = dcast(abs_wide, grant+loc_name+type~semester, value.var=c('budget', 'expenditure'), fun.aggregate = sum)

for (p in c('S1', 'S1-2', 'S2', 'S3')){
  abs_wide[, (paste0(p, "_savings")):=get(paste0("budget_", p))-get(paste0("expenditure_", p))]
} 

# An "ideal" situation would be for them to allocate all savings for a given module back into that module. (Not really, 
# but that would be the same as the budget plan). By comparing deviances from this, we can see if there are general flows
# when there are absorption savings within a module. 

#Just ignore Semester 2 for now. 
abs_wide[, `diff_from_expected_S1-2`:=`budget_S1-2`-(budget_S1 + S1_savings)]
abs_wide[, `diff_from_expected_S3`:=budget_S3-(`budget_S1-2` + `S1-2_savings`)]

plot_data = abs_wide[, .(grant, loc_name, type,`diff_from_expected_S1-2`, diff_from_expected_S3)]

#Melt this data table 
plot_data = melt(plot_data, id.vars=c('grant', 'loc_name', 'type')) 
plot_data[grepl("S1-2", variable), semester:="S1-2"]
plot_data[grepl("S3", variable), semester:="S3"]

plot_data[, variable:="Deviation from carryover absorption"]

plot_data[, loc_name:=toupper(loc_name)]

p = ggplot(plot_data, aes(x=semester, y=value, color=type)) + 
  geom_point(position="jitter", size=3) + 
  facet_wrap(~loc_name) + 
  theme_bw(base_size=16) + 
  labs(title="Discrepancies in 'carryover' absorption", x="PUDR Semester", 
       y="Difference between expected budget\nand actual budget", color="",
       subtitle="If point is at zero, cost savings from absorption were kept in the module")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/carryover_abs1.png", p, height=8, width=11)

p2 = ggplot(plot_data[type!="All other modules"], aes(x=semester, y=value, color=type)) + 
  geom_point(position="jitter", size=3) + 
  facet_wrap(~loc_name) + 
  theme_bw(base_size=16) + 
  labs(title="Discrepancies in 'carryover' absorption", x="PUDR Semester", 
       y="Difference between expected budget\nand actual budget", color="",
       subtitle="If point is at zero, cost savings from absorption were kept in the module")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/carryover_abs2.png", p2, height=8, width=11)

p3 = ggplot(plot_data[type=="All other modules"], aes(x=semester, y=value, color=type)) + 
  geom_point(position="jitter", size=3) + 
  facet_wrap(~loc_name) + 
  theme_bw(base_size=16) + 
  labs(title="Discrepancies in 'carryover' absorption", x="PUDR Semester", 
       y="Difference between expected budget\nand actual budget", color="",
       subtitle="If point is at zero, cost savings from absorption were kept in the module")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/carryover_abs3.png", p3, height=8, width=11)



#--------------------------------------------
# ONE WAY OF DOING THIS 
#--------------------------------------------

abs_wide = absorption[, .(grant, semester, type, budget, expenditure, absorption)]
abs_wide = dcast(abs_wide, grant+type~semester, value.var=c('budget', 'expenditure', 'absorption'), fun.aggregate = sum)

abs_wide[, `budget_diff_S1`:=`budget_S1-2`-budget_S1]
abs_wide[, `budget_diff_S1-2`:=budget_S2-`budget_S1-2`]
abs_wide[, budget_diff_S2:=budget_S3-budget_S2]

abs_wide = abs_wide[, -c('budget_S1', 'budget_S1-2', 'budget_S2', 'budget_S3', 
                         'expenditure_S1', 'expenditure_S1-2', 'expenditure_S2', 'expenditure_S3', 'absorption_S3')]

#Melt this again and reshape. 
abs_melt = melt(abs_wide, id.vars=c('grant', 'type'))     

abs_melt[grepl("absorption", variable), value_type:="absorption"]
abs_melt[grepl("budget_diff", variable), value_type:="budget_diff"]

abs_melt[, semester:=gsub("absorption_|budget_diff_", "", variable)]

abs_melt$variable <- NULL
abs_melt = dcast(abs_melt, grant+type+semester~value_type, value.var='value', fun.aggregate=sum)
abs_melt[absorption>200, absorption:=200]

#Drop NA values. 
abs_melt = abs_melt[!is.na(type)]

pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_Synthesis 2019/denominator_changes.pdf", height=8, width=11)
for (g in unique(abs_melt$grant)){
  p = ggplot(abs_melt[grant==g], aes(x=semester, y=budget_diff, size=absorption, group=type, color=type)) + 
    geom_point() + 
    geom_line(size=0.2) + 
    theme_bw(base_size=18) + 
    labs(title=g, x="PUDR Semester", y="Amount budget was changed in subsequent period", color="")
  
  print(p)
  
} 
dev.off() 


# # Example code. 
# URL <- paste0(
#   "https://cdn.rawgit.com/christophergandrud/networkD3/",
#   "master/JSONdata/energy.json")
# Energy <- jsonlite::fromJSON(URL)
# # Plot
# sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
#               Target = "target", Value = "value", NodeID = "name",
#               units = "TWh", fontSize = 12, nodeWidth = 30)
