#------------------------------------------
# Specific graphs for the DRC report 
# Updated by Emily Linebarger November 2019 
#------------------------------------------

rm(list=ls()) 

library(data.table) 
library(ggplot2) 
library(RColorBrewer) 
library(scales)

options(scipen=100)

#Read in data 
revisions = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/budget_revisions.rds")
absorption = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/COD/prepped_data/absorption_cod.rds")

all_mods = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
setnames(all_mods, c('module_eng', 'intervention_eng', 'abbrev_mod_eng', 'abbrev_int_eng'), c('gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int'))
all_mods = unique(all_mods[, .(gf_module, gf_intervention, disease, abbrev_mod, abbrev_int)])
absorption = merge(absorption, all_mods, by=c('gf_module', 'gf_intervention', 'disease'), allow.cartesian=TRUE)

#make sure this merge worked correctly. 
stopifnot(nrow(absorption[is.na(abbrev_int)])==0)

#Create a cumulative dataset
# Flag cases where you don't have a complete time series. A complete time series is either S1, S2, and S3 or S1-2 and S3. 
cumulative_absorption = data.table()
for (g in unique(absorption$grant)){ 
  subset = absorption[grant_period=="2018-2020" & grant==g]
  if (nrow(subset)!=0) {
    sequence = sort(unique(subset$semester))
    if ("Semester 1-2"%in%sequence & "Semester 3"%in%sequence) {
      subset[, full_time_series:=TRUE]
      subset = subset[semester%in%c('Semester 1-2', 'Semester 3'), .(loc_name, grant, grant_disease, gf_module, gf_intervention, abbrev_mod, abbrev_int, disease, full_time_series, semester, budget, expenditure)]
      subset = melt(subset, id.vars=c('loc_name', 'grant', 'grant_disease', 'gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int', 'disease', 'full_time_series', 'semester'))
      subset_wide = dcast(subset, loc_name+grant+grant_disease+gf_module+gf_intervention+abbrev_mod+abbrev_int+disease+full_time_series~semester+variable, fun.aggregate=sum)
      
      subset_wide[, cumulative_budget:=sum(`Semester 1-2_budget`, `Semester 3_budget`), by=c('loc_name', 'grant', 'grant_disease', 'disease', 'full_time_series', 'abbrev_mod', 'abbrev_int', 'gf_module', 'gf_intervention')]
      subset_wide[, cumulative_expenditure:=sum(`Semester 1-2_expenditure`, `Semester 3_expenditure`), by=c('loc_name', 'grant', 'grant_disease', 'disease', 'full_time_series', 'abbrev_mod', 'abbrev_int', 'gf_module', 'gf_intervention')]
      subset_wide = subset_wide[, .(loc_name, grant,  grant_disease, gf_module, gf_intervention, abbrev_mod, abbrev_int, disease, full_time_series, cumulative_budget, cumulative_expenditure)]
      
    } else if ("Semester 1"%in%sequence & "Semester 2"%in%sequence & "Semester 3"%in%sequence) { 
      subset[, full_time_series:=TRUE]
      subset = subset[semester%in%c('Semester 1', 'Semester 2', 'Semester 3')]
      
      subset = subset[semester%in%c('Semester 1', 'Semester 2', 'Semester 3'), .(loc_name, grant, grant_disease, gf_module, gf_intervention, abbrev_mod, abbrev_int, disease, pr_type, full_time_series, semester, budget, expenditure)]
      subset = melt(subset, id.vars=c('loc_name', 'grant', 'grant_disease', 'gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int', 'disease', 'pr_type', 'full_time_series', 'semester'))
      subset_wide = dcast(subset, loc_name+grant+pr_type+grant_disease+gf_module+gf_intervention+abbrev_mod+abbrev_int+disease+full_time_series~semester+variable, fun.aggregate=sum)
      
      subset_wide[, cumulative_budget:=sum(`Semester 1_budget`, `Semester 2_budget`, `Semester 3_budget`), by=c('loc_name', 'grant', 'grant_disease', 'disease', 'full_time_series', 'abbrev_mod', 'abbrev_int', 'gf_module', 'gf_intervention')]
      subset_wide[, cumulative_expenditure:=sum(`Semester 1_expenditure`, `Semester 2_expenditure`, `Semester 3_expenditure`), by=c('loc_name', 'grant', 'grant_disease', 'disease', 'full_time_series', 'abbrev_mod', 'abbrev_int', 'gf_module', 'gf_intervention')]
      subset_wide = subset_wide[, .(loc_name, grant, grant_disease, gf_module, gf_intervention, abbrev_mod, abbrev_int, disease, full_time_series, cumulative_budget, cumulative_expenditure)]
      
    } else {
      subset[, full_time_series:=FALSE]
      subset = subset[, .(loc_name, grant, grant_disease, gf_module, gf_intervention, abbrev_mod, abbrev_int, disease, full_time_series, budget, expenditure)]
      subset = melt(subset, id.vars=c('loc_name', 'grant', 'grant_disease', 'gf_module', 'gf_intervention', 'abbrev_mod', 'abbrev_int', 'disease', 'full_time_series'))
      subset_wide = dcast(subset, loc_name+grant+grant_disease+gf_module+gf_intervention+abbrev_mod+abbrev_int+disease+full_time_series~variable, fun.aggregate=sum)
      
      subset_wide[, cumulative_budget:=sum(budget, na.rm=T), by=c('loc_name', 'grant', 'grant_disease', 'disease', 'full_time_series', 'abbrev_mod', 'abbrev_int', 'gf_module', 'gf_intervention')]
      subset_wide[, cumulative_expenditure:=sum(expenditure, na.rm=T), by=c('loc_name', 'grant', 'grant_disease', 'disease', 'full_time_series', 'abbrev_mod', 'abbrev_int', 'gf_module', 'gf_intervention')] 
      subset_wide = subset_wide[, .(loc_name, grant, grant_disease, gf_module, gf_intervention, abbrev_mod, abbrev_int, disease, full_time_series, cumulative_budget, cumulative_expenditure)]
      
    }
    cumulative_absorption = rbind(cumulative_absorption, subset_wide, use.names=TRUE)
  } 
}

# 1. Bar graph that shows 18-month cumulative absorption by grant. 
by_grant = cumulative_absorption[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), 
                                 by=c('grant')]
by_grant[, absorption:=round((expenditure/budget)*100, 1)]
by_grant = melt(by_grant, id.vars=c('grant', 'absorption'), value.name="amount")

by_grant[variable=="budget", label:=""] #Don't display the expenditure amount on the budget bar. 
by_grant[variable=="expenditure", label:=paste0(dollar(amount), " (", absorption, "%)")]
by_grant[variable=="budget", variable:="Budget"]
by_grant[variable=="expenditure", variable:="Expenditure"]


p1 = ggplot(by_grant, aes(x=grant, y=amount, fill=variable, label=label)) + 
  geom_bar(stat="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=14) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Absorption by grant", subtitle="January 2018-June 2019", x="Grant", 
       y="Absorption (%)", fill="", caption="*Labels show expenditure amounts and absorption percentages")
  

# 2. Show absorption by grant over the last 18 months compared to the full 3-year grant budget (most recent version) 
plot_data = cumulative_absorption[, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), by=c('grant', 'gf_module', 'abbrev_mod')]
plot_data[, absorption:=round((expenditure/budget)*100, 1)]

melt = melt(plot_data, id.vars=c('grant', 'gf_module', 'abbrev_mod', 'absorption'))
melt[variable=="expenditure", label:=paste0(dollar(value), " (", absorption, "%)")]
melt[is.na(label), label:=""]
melt[variable=="budget", variable:="Budget"]
melt[variable=="expenditure", variable:="Expenditure"]

p2 = ggplot(melt[grant=='COD-C-CORDAID'], aes(x=abbrev_mod, y=value, fill=variable, label=label)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=18) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Cumulative absorption for COD-C-CORDAID", subtitle="Jan 2018-June 2019", x="Module", y="Budget (USD)", 
       fill="", caption="*Labels show expenditure amounts and absorption percentages")

p3 = ggplot(melt[grant=='COD-M-MOH'], aes(x=abbrev_mod, y=value, fill=variable, label=label)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=18) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Cumulative absorption for COD-M-MOH", subtitle="Jan 2018-June 2019", x="Module", y="Budget (USD)", 
       fill="", caption="*Labels show expenditure amounts and absorption percentages")

p4 = ggplot(melt[grant=='COD-M-SANRU'], aes(x=abbrev_mod, y=value, fill=variable, label=label)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_text(hjust=0) + 
  theme_bw(base_size=18) + 
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="Cumulative absorption for COD-M-SANRU", subtitle="Jan 2018-June 2019", x="Module", y="Budget (USD)", 
       fill="", caption="*Labels show expenditure amounts and absorption percentages")



# 3. Show a line graph of absorption for catalytic funds in DRC 

# First, tag catalytic modules/interventions. 
catalytic_ints = data.table(abbrev_mod=c("Care & prevention", "MDR-TB"), 
                       abbrev_int=c("Case detection and diagnosis", "Case detection and diagnosis"))

catalytic_mods = c("Human rights barriers", "Info systems & M&E")

for (i in 1:nrow(catalytic_ints)){
  absorption[abbrev_mod==catalytic_ints$abbrev_mod[i] & abbrev_int==catalytic_ints$abbrev_int[i], catalytic:=TRUE]
}

absorption[abbrev_mod%in%catalytic_mods, catalytic:=TRUE]
absorption[is.na(catalytic), catalytic:=FALSE]

plot_data = absorption[grant_period=="2018-2020" & catalytic==TRUE, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('abbrev_mod', 'grant', 'semester')]
plot_data[, absorption:=round((expenditure/budget)*100, 1)]
#Drop grants where the catalytic module was not applied
plot_data = plot_data[!(abbrev_mod=="Info systems & M&E" & grant!="COD-M-MOH")]
# Show absorption for matching funds in DRC. 

plot_data[, concat:=paste0(grant, ", ", abbrev_mod)]
p5 = ggplot(plot_data, aes(x=semester, y=absorption, color=concat, group=concat, label=paste0(absorption, "%"))) + 
  geom_point() + 
  geom_line() + 
  geom_text(position="jitter") + 
  theme_bw(base_size=14) + 
  labs(title="Absorption for catalytic funds, over time", subtitle="January 2018-June 2019", x="PUDR Semester", y="Absorption (%)", color="")

pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_DRC 2019 annual report/report_graphs.pdf", height=8, width=11)
p1 
p2
p3
p4
p5
dev.off() 
