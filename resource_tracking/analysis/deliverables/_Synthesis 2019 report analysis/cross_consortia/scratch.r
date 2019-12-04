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

modules = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_modules.rds")
cost_categories = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_cost_categories.rds")

#------------------------------
# Analyses 
#------------------------------

#-----------------------------------------------------------------------------------
# Show how RSSH has just been divided proportionally among all quarters in a budget
ihme_budgets = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets.rds")
plot_data = ihme_budgets[grant_period%in%c('2018-2020'), .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'disease', 'start_date', 'gf_module')]
plot_data[, date_col:=paste0("q", quarter(start_date), "-", year(start_date))]
plot_data$start_date<- NULL
plot_data = dcast(plot_data, grant+grant_period+disease+gf_module~date_col, value.var='budget')

# Show how budget has changed between quarters 
quarters = c('q1-2018', 'q2-2018', 'q3-2018', 'q4-2018', 'q1-2019', 'q2-2019', 'q3-2019', 'q4-2019', 'q1-2020', 'q2-2020',
             'q3-2020', 'q4-2020')
for (i in 1:(length(quarters)-1)){
  plot_data[, paste0(quarters[i+1], "-change"):=(get(quarters[i+1])-get(quarters[i]))]
}
keep_cols = names(plot_data)[grepl("-change", names(plot_data))]
keep_cols = c(keep_cols, 'grant', 'grant_period', 'disease', 'gf_module')
plot_data = plot_data[, keep_cols, with=FALSE]

# Melt this data back 
plot_data = melt(plot_data, id.vars=c('grant', 'grant_period', 'disease', 'gf_module'))
plot_data[, quarter:=tstrsplit(variable, "-", keep=1)]
plot_data[, quarter:=gsub("q", "", quarter)]
plot_data[, quarter:=as.numeric(quarter)]
plot_data[, year:=tstrsplit(variable, "-", keep=2)]
plot_data[, year:=as.numeric(year)]

plot_data[, date:=year+((quarter/4)-0.25)]

# How many times is there zero-variance between quarters for a module? 
zero_variance = plot_data[value==0]
plot_data2 = zero_variance[, .(num_quarters=.N), by=c('disease', 'date')]

ggplot(plot_data2, aes(x=date, y=num_quarters, fill=disease)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_bw(base_size=16) + 
  labs(title="Number of grants whose budget did not change from one quarter to the next, by disease", x="Date", y="Number of grants with zero variance", fill="")

#-----------------------------------------------------------------------------------
# Is there a movement of funds away from RSSH activities towards disease-specific activities through grant revision? 

#-----------------------------------------------------------------------------------
# Program management - explore activities/cost categories and see what kind of things are happening under this module. 

#-----------------------------------------------------------------------------------
# Has higher program management absorption led to higher overall grant absorption? 




