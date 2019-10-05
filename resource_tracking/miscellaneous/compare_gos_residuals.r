#Compare old and new GOS files 
# Emily Linebarger 9/30/2019 
library(data.table)
library(ggplot2) 

old = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gos/raw_data/raw_prepped_gos1.rds") 
old[, start_date:=as.Date(start_date)]
#This file was saved on Sep. 30th, but was generated using the old GOS data. EL 
new = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gos/raw_data/raw_prepped_gos2.rds")

#Just look at DRC malaria 
drc_mal_old = old[country=="Congo (Democratic Republic)" & disease=="malaria", 
                  .(budget_old=sum(budget, na.rm=T), expenditure_old=sum(expenditure, na.rm=T)), 
                  by=c('country', 'grant', 'grant_period', 'start_date', 'module')]
drc_mal_new = new[country=="Congo (Democratic Republic)" & disease=="malaria", 
                  .(budget_new=sum(budget, na.rm=T), expenditure_new=sum(expenditure, na.rm=T)), 
                  by=c('country', 'grant', 'grant_period', 'start_date', 'module')]
drc_mal_compare = merge(drc_mal_old, drc_mal_new, by=c('country', 'grant', 'grant_period', 'start_date', 'module'))
drc_mal_compare[, budget_diff:=budget_new-budget_old]
drc_mal_compare[, expenditure_diff:=expenditure_new-expenditure_old]

#Collapse data, and merge 
old = old[, .(budget_old=sum(budget, na.rm=T), expenditure_old=sum(expenditure, na.rm=T)), 
          by=c('country', 'grant', 'grant_period', 'start_date')]
new = new[, .(budget_new=sum(budget, na.rm=T), expenditure_new=sum(expenditure, na.rm=T)), 
          by=c('country', 'grant', 'grant_period', 'start_date')]

dt = merge(old, new, by=c('country', 'grant', 'grant_period', 'start_date'), all=T)

#Calculate difference between values to plot with ggplot. 
dt[, budget_diff:=budget_new-budget_old]
dt[, expenditure_diff:=expenditure_new-expenditure_old]

#Go ahead and subset down to 2013 on, because nothing in the GMS has changed. 

p1 = ggplot(dt, aes(x=start_date, y=budget_diff, color=country)) + 
  geom_point() + 
  theme_bw() + 
  labs(x="Start date", y="Difference in Budget (New-Old)", title="GOS Differences", color="Country")

p2 = ggplot(dt, aes(x=start_date, y=expenditure_diff, color=country)) + 
  geom_point() + 
  theme_bw() +
  labs(x="Start date", y="Difference in Expenditure (New-Old)", title="GOS Differences", color="Country")

ggsave("C:/Users/elineb/Desktop/budget_differences.png", p1)
ggsave("C:/Users/elineb/Desktop/expenditure_differences.png", p2)

