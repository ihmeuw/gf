#--------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Review differences in DRC budgets for RT presentation
# DATE: June 28, 2019
#--------------------------------------------------

rm(list=ls())

#Install packages and read in data 
library(data.table)
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(scales)
options(scipen=20)


# Read in data 
orig = data.table(read.xlsx("J:/Project/Evaluation/GF/resource_tracking/visualizations/verification/DRC_budget_overlaps_COD-M-MOH.xlsx",
                 sheet="original", detectDates = T))
setnames(orig, c('Module', 'budget'), c('module', 'budget0'))

#Fix module names 
orig[module=="Prise en charge", module:="Case management"]
orig[grepl("prestation de services", module), 
     module:="RSSH: Integrated service delivery and quality improvement"]
orig[grepl("ressources humaines", module), 
     module:="RSSH: Human resources"]
orig[grepl("gestion des achats", module), 
     module:="RSSH: Procurement and supply chain management"]
orig[grepl("ripostes", module), module:="RSSH: Community responses and systems"]
orig[grepl("suivi et", module), 
     module:="RSSH: HMIS"]
orig[module=="Gestion des subventions", module:="Program management"]


update = data.table(read.xlsx("J:/Project/Evaluation/GF/resource_tracking/visualizations/verification/DRC_budget_overlaps_COD-M-MOH.xlsx",
                 sheet="modified", detectDates = T))
setnames(update, c('Module', 'budget'), c('module', 'budget1'))

#Fix module names
update[module=="Prise en charge", module:="Case management"]
update[grepl("prestation de services", module), 
     module:="RSSH: Integrated service delivery and quality improvement"]
update[grepl("ressources humaines", module), 
     module:="RSSH: Human resources"]
update[grepl("gestion des achats", module), 
     module:="RSSH: Procurement and supply chain management"]
update[grepl("ripostes", module), module:="RSSH: Community responses and systems"]
update[grepl("suivi et", module), 
     module:="RSSH: HMIS"]
update[module=="Gestion des subventions", module:="Program management"]
update[module=="Lutte antivectorielle", module:="Vector control"]


merge = merge(orig, update, by=c('module', 'date'), all=T)
setDT(merge)

#What's the difference?
merge[is.na(budget0), budget0:=0]
merge[is.na(budget1), budget1:=0]
merge[, budget_diff:=budget1-budget0]

#Where was less money allocated in the revised budget than in the first budget?
merge[budget_diff<0 ]
unique(merge[budget_diff<0, .(date, module)])
#All health systems strengthening - HMIS.

#Where was more money allocated in the revised budget than in the first budget?
merge[budget_diff>0]
unique(merge[budget_diff>0, .(date, module)])


#Where has money not changed?
merge[budget_diff==0]
unique(merge[budget_diff==0, .(date, module)])


#Make some visualizations for RT presentation 

blank_theme <- theme_bw()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x=element_blank(), 
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

orig_total = sum(orig$budget0, na.rm=T)
update_total = sum(update$budget1, na.rm=T)

orig_collapse = orig[, .(budget0 = sum(budget0, na.rm=T)), by=c('module')]
update_collapse = update[, .(budget1 = sum(budget1, na.rm=T)), by=c('module')]

# Create pie chart for original budget 
pie1 = ggplot(orig_collapse, aes(x="", y=budget0, fill=module)) + geom_bar(stat="identity", width=1) + 
  blank_theme + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(budget0/orig_total, 2)*100, "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#AA4371")) +
  labs(x=NULL, y=NULL, fill="Module", title="Original budget, by module (all years combined)")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/original_budget_all_years.png")

# Create pie chart for updated budget
pie2 = ggplot(update_collapse, aes(x="", y=budget1, fill=module)) + geom_bar(stat="identity", width=1) + 
  blank_theme + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(budget1/update_total, 2)*100, "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#AA4371", "#00EE76")) +
  labs(x=NULL, y=NULL, fill="Module", title="Updated budget, by module (all years combined)")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/updated_budget_all_years.png")


# Create pie chart for original budget - with dollar amounts as labels 
pie3 = ggplot(orig_collapse, aes(x="", y=budget0, fill=module)) + geom_bar(stat="identity", width=1) + 
  blank_theme + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0("$", round(budget0))), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#AA4371")) +
  labs(x=NULL, y=NULL, fill="Module", title="Original budget, by module (all years combined)")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/original_budget_all_years1.png")

# Create pie chart for updated budget
pie4 = ggplot(update_collapse, aes(x="", y=budget1, fill=module)) + geom_bar(stat="identity", width=1) + 
  blank_theme + 
  coord_polar("y", start=0) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#AA4371", "#00EE76")) +
  geom_text(aes(label = paste0("$", round(budget1))), position = position_stack(vjust=0.5)) + 
  labs(x=NULL, y=NULL, fill="Module", title="Updated budget, by module (all years combined)")

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/updated_budget_all_years1.png")


# Create pie chart for original budget 
orig_collapse_2018 = orig[year(date)==2018, .(budget=sum(budget0, na.rm=T)), by=c('module')]
total = sum(orig_collapse_2018$budget)
pie = ggplot(orig_collapse_2018, aes(x="", y=budget, fill=module)) + geom_bar(stat="identity", width=1) + 
  blank_theme + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(budget/total, 2)*100, "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#AA4371")) +
  labs(x=NULL, y=NULL, fill="Module", title="Original budget, by module for 2018")

pie
ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/original_budget_2018.png")

# Create pie chart for updated budget
update_collapse_2018 = update[year(date)==2018, .(budget=sum(budget1, na.rm=T)), by=c('module')]
total = sum(update_collapse_2018$budget)
pie = ggplot(update_collapse_2018, aes(x="", y=budget, fill=module)) + geom_bar(stat="identity", width=1) + 
  blank_theme + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(budget/total, 2)*100, "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#AA4371", "#00EE76")) +
  labs(x=NULL, y=NULL, fill="Module", title="Updated budget, by module for 2018")
pie

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/updated_budget_2018.png")


# Create pie chart for original budget 
orig_collapse_2019 = orig[year(date)==2019, .(budget=sum(budget0, na.rm=T)), by=c('module')]
total = sum(orig_collapse_2019$budget)
pie = ggplot(orig_collapse_2019, aes(x="", y=budget, fill=module)) + geom_bar(stat="identity", width=1) + 
  blank_theme + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(budget/total, 2)*100, "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#AA4371")) +
  labs(x=NULL, y=NULL, fill="Module", title="Original budget, by module for 2019")

pie
ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/original_budget_2019.png")

# Create pie chart for updated budget
update_collapse_2019 = update[year(date)==2019, .(budget=sum(budget1, na.rm=T)), by=c('module')]
total = sum(update_collapse_2019$budget)
pie = ggplot(update_collapse_2019, aes(x="", y=budget, fill=module)) + geom_bar(stat="identity", width=1) + 
  blank_theme + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(budget/total, 2)*100, "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#AA4371", "#00EE76")) +
  labs(x=NULL, y=NULL, fill="Module", title="Updated budget, by module for 2019")
pie

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/updated_budget_2019.png")

# Create pie chart for original budget 
orig_collapse_2020 = orig[year(date)==2020, .(budget=sum(budget0, na.rm=T)), by=c('module')]
total = sum(orig_collapse_2020$budget)
pie = ggplot(orig_collapse_2020, aes(x="", y=budget, fill=module)) + geom_bar(stat="identity", width=1) + 
  blank_theme + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(budget/total, 2)*100, "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#AA4371")) +
  labs(x=NULL, y=NULL, fill="Module", title="Original budget, by module for 2020")

pie
ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/original_budget_2020.png")

# Create pie chart for updated budget
update_collapse_2020 = update[year(date)==2020, .(budget=sum(budget1, na.rm=T)), by=c('module')]
total = sum(update_collapse_2020$budget)
pie = ggplot(update_collapse_2020, aes(x="", y=budget, fill=module)) + geom_bar(stat="identity", width=1) + 
  blank_theme + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(budget/total, 2)*100, "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#AA4371", "#00EE76")) +
  labs(x=NULL, y=NULL, fill="Module", title="Updated budget, by module for 2020")
pie

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/updated_budget_2020.png")




