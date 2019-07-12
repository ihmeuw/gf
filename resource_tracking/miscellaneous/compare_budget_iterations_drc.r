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

#ORIGINAL APPROVED BUDGET
# Read in data 
orig = data.table(read.xlsx("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/COD-M-MOH budget revisions 7.11.2019.xlsx",
                 sheet="08.08.2018", detectDates = T))
orig = orig[, -c('Year.1', 'Year.2', 'Year.3')]
names(orig) = tolower(names(orig))

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
orig[module=="Lutte antivectorielle", module:="Vector control"]

orig = melt(orig, id.vars='module', variable.name='quarter', value.name='budget0')


# SECOND BUDGET
update1 = data.table(read.xlsx("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/COD-M-MOH budget revisions 7.11.2019.xlsx",
                            sheet="11.30.2018", detectDates = T))
update1 = update1[, -c('Year.1', 'Year.2', 'Year.3')]
names(update1) = tolower(names(update1))

#Fix module names
update1[module=="Prise en charge", module:="Case management"]
update1[grepl("prestation de services", module), 
     module:="RSSH: Integrated service delivery and quality improvement"]
update1[grepl("ressources humaines", module), 
     module:="RSSH: Human resources"]
update1[grepl("gestion des achats", module), 
     module:="RSSH: Procurement and supply chain management"]
update1[grepl("ripostes", module), module:="RSSH: Community responses and systems"]
update1[grepl("suivi et", module), 
     module:="RSSH: HMIS"]
update1[module=="Gestion des subventions", module:="Program management"]
update1[module=="Lutte antivectorielle", module:="Vector control"]

update1 = melt(update1, id.vars='module', variable.name='quarter', value.name='budget1')

# THIRD BUDGET 
update2 = data.table(read.xlsx("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/COD-M-MOH budget revisions 7.11.2019.xlsx",
                               sheet="04.08.2019", detectDates = T))
update2 = update2[, -c('Year.1', 'Year.2', 'Year.3')]
names(update2) = tolower(names(update2))

#Fix module names
update2[module=="Prise en charge", module:="Case management"]
update2[grepl("prestation de services", module), 
        module:="RSSH: Integrated service delivery and quality improvement"]
update2[grepl("ressources humaines", module), 
        module:="RSSH: Human resources"]
update2[grepl("gestion des achats", module), 
        module:="RSSH: Procurement and supply chain management"]
update2[grepl("ripostes", module), module:="RSSH: Community responses and systems"]
update2[grepl("suivi et", module), 
        module:="RSSH: HMIS"]
update2[module=="Gestion des subventions", module:="Program management"]
update2[module=="Lutte antivectorielle", module:="Vector control"]

update2 = melt(update2, id.vars='module', variable.name='quarter', value.name='budget2')

#--------------------------------
# MERGE DATA 
# -------------------------------
merge = merge(orig, update1, by=c('module', 'quarter'), all=T)
merge = merge(merge, update2, by=c('module', 'quarter'), all=T)
setDT(merge)

# COMPARE FIRST REVISION TO ORIGINAL 
{
  merge[, budget_diff1:=budget1-budget0]
  
  #Where was less money allocated in the revised budget than in the first budget?
  merge[budget_diff1<0 ]
  unique(merge[budget_diff1<0, .(quarter, module)])
  #All health systems strengthening - HMIS.
  
  #Where was more money allocated in the revised budget than in the first budget?
  merge[budget_diff1>0]
  unique(merge[budget_diff1>0, .(quarter, module)])
  
  
  #Where has money not changed?
  merge[budget_diff1==0]
  unique(merge[budget_diff1==0, .(quarter, module)])
}

# COMPARE FIRST REVISION TO SECOND REVISION
{
  merge[, budget_diff2:=budget2-budget1]
  
  #Where was less money allocated in the revised budget than in the first budget?
  merge[budget_diff2<0 ]
  unique(merge[budget_diff2<0, .(quarter, module)])
  #All health systems strengthening - HMIS.
  
  #Where was more money allocated in the revised budget than in the first budget?
  merge[budget_diff2>0]
  unique(merge[budget_diff2>0, .(quarter, module)])
  
  
  #Where has money not changed?
  merge[budget_diff2==0]
  unique(merge[budget_diff2==0, .(quarter, module)])
}


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
pal8 =  c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

original = orig[, .(budget = sum(budget0, na.rm=T)), by=c('module')]
version1 = update1[, .(budget = sum(budget1, na.rm=T)), by=c('module')]
version2 = update2[, .(budget = sum(budget2, na.rm=T)), by=c('module')]

for (dt in c("original", "version1", "version2")){
  if (dt=="original"){
    label="Original"
  } else if (dt == "version1"){
    label = "Version 1"
  } else {
    label = "Version 2"
  }
  total = sum(get(dt)$budget)
  pie = ggplot(get(dt), aes(x="", y=budget, fill=module)) + geom_bar(stat="identity", width=1) + 
    blank_theme + 
    coord_polar("y", start=0) + 
    geom_text(aes(label = paste0(round(budget/total, 2)*100, "%")), position = position_stack(vjust = 0.5)) + 
    scale_fill_manual(values=pal8) +
    labs(x=NULL, y=NULL, fill="Module", title=paste0(label, " budget, by module (all years combined)"))
  pie
  ggsave(paste0("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/", dt, "_budget_all_years.png"), width=12, height=8)

  
  pie2 = ggplot(get(dt), aes(x="", y=budget, fill=module)) + geom_bar(stat="identity", width=1) + 
    blank_theme + 
    coord_polar("y", start=0) + 
    geom_text(aes(label = paste0("$", round(budget))), position = position_stack(vjust = 0.5)) + 
    scale_fill_manual(values=pal8) +
    labs(x=NULL, y=NULL, fill="Module", title=paste0(label, " budget, by module (all years combined)"))
  pie2
  ggsave(paste0("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/", dt, "_budget_all_years1.png"), width=12, height=8)
  
}

merge[quarter%in%c('q1', 'q2', 'q3', 'q4'), year:=2018]
merge[quarter%in%c('q5', 'q6', 'q7', 'q8'), year:=2019]
merge[quarter%in%c('q9', 'q10', 'q11', 'q12'), year:=2020]

merge_collapse = merge[, .(budget0=sum(budget0, na.rm=T), 
                           budget1=sum(budget1, na.rm=T), 
                           budget2=sum(budget2, na.rm=T)), by=c('module', 'year')]
merge_collapse = melt(merge_collapse, id.vars=c('module', 'year'), value.name = 'budget', variable.name='version')
merge_collapse[version=="budget0", version:="August 8, 2018"]
merge_collapse[version=='budget1', version:="Version 1"]
merge_collapse[version=="budget2", version:="April 8, 2019"]

merge_collapse[, version:=as.character(version)]

#Only use original and final version, and only run for 2020. 
merge_collapse = merge_collapse[version%in%c("August 8, 2018", 'April 8, 2019') & year == 2020]

for (v in unique(merge_collapse$version)){
  dt = merge_collapse[version==v]
  pie = ggplot(dt, aes(x="", y=budget, fill=module)) + geom_bar(stat="identity", width=1) + 
    blank_theme + 
    coord_polar("y", start=0) + 
    geom_text(aes(label = paste0("$", round(budget))), position = position_stack(vjust = 0.5)) + 
    scale_fill_manual(values=pal8) +
    facet_wrap(~version) + 
    labs(x=NULL, y=NULL, fill="Module", title=paste0(v, " budget by module for 2020"), caption=paste0("Budget total: $", round(sum(dt$budget))))
  pie
  ggsave(paste0("J:/Project/Evaluation/GF/resource_tracking/visualizations/research_team_meetings/", v, "_budget_by_module_2020.png"), width=10, height=7)
  
}