#---------------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Create DRC financial graphs for August workshop 2019
# DATE: August 5, 2019 
#----------------------------------------------------

rm(list=ls())
library(data.table) 
library(ggplot2)
library(scales)

# Load data 
budgets = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/final_budgets.rds") #Read this in, so you have the GOS appended. 
budgets = budgets[loc_name=="cod"]
absorption = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/cod/prepped_data/absorption_cod.rds")
other_dah = readRDS("J:/Project/Evaluation/GF/resource_tracking/_fgh/prepped_data/other_dah_actuals_all_cod.rds")

#File paths 
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/random/cod/august_2019_workshop/"

#--------------------------------------------------------------------------------------------------
# •	Make a graph of funding for HIV testing in the Global Fund grants over time? 
# This would just be the total amount allocated to HIV testing as a time trend. 
#It would be helpful to have this as an annual stacked bar graphs as well 
#(e.g. a stacked bar for each year by module or aggregated module, to show the 
#percentage of all funding it constitutes, with proportions labelled). 

#First, tag all HIV-testing interventions. 
testing_codes = c('H2_7', 'H3_7', 'H4_6', 'H5_7', 'H6_7', 'H7_3', 'H8_5', 'H14', 'H14_1')
testing = budgets[code%in%testing_codes] #HIV testing was only split out as a unique module in 2015. I have general interventions for the general population before that. 

test1 = testing[, .(budget=sum(budget, na.rm=T)), by=c('quarter', 'year')]
test1[, date:=((quarter/4)-0.25)+year]

test_plot1 = ggplot(test1, aes(x=date, y=budget)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() +
  scale_y_continuous(labels=dollar_format()) + 
  labs(title="Global Fund investment in HIV Testing, DRC", x="Date", y="Budget", caption="*Specific investments in HIV testing are only traceable from 2015 on")
ggsave(paste0(save_loc, "testing_time_trend.png"), test_plot1, height=10, width=13)

test2 = testing[, .(budget=sum(budget, na.rm=T)), by=c('year', 'gf_module')]
test2[gf_module=="Comprehensive prevention programs for men who have sex with men", gf_module:="Men who have sex with men"]
test2[gf_module=="Comprehensive prevention programs for people who inject drugs and their partners", gf_module:="People who inject drugs and their partners"]
test2[gf_module=="Comprehensive prevention programs for sex workers and their clients", gf_module:="Sex workers and their clients"]
test2[gf_module=="Comprehensive prevention programs for transgender people", gf_module:="Transgender people"]
test2[gf_module=="HIV Testing Services", gf_module:="General HIV Testing"]
mod_by_year = function(date){
  dt = test2[year==date]
  dt[, total:=sum(budget)]
  dt[, prop:=paste0(round((budget/total)*100, 2), "%")]
  
  plot = ggplot(dt, aes(x=gf_module, y=budget, fill=gf_module)) + 
    geom_bar(stat="identity", position="dodge") + 
    coord_flip() +
    theme_bw() + 
    theme(axis.text.y=element_blank(), axis.title.y=element_blank()) + 
    geom_text(aes(label=prop), vjust=0) + 
    scale_y_continuous(labels=dollar_format()) + 
    labs(title=paste0("Global Fund investment in HIV Testing for ", date), y="Budget", fill="Subpopulation", caption="*Specific investments in HIV testing are only traceable from 2015 on")
  return(plot)
}

pdf(paste0(save_loc, 'testing_stacked_bar.pdf'), height=6, width=12)
for (y in sort(unique(test2$year))){
  plot = mod_by_year(y)
  print(plot)
}
dev.off()

#--------------------------------------------------------------------------------------------------
# •	Do you have the HIV funding landscape by type of activity? I seem to remember you did. 
#Could you make this specific to HIV testing – so, a graph like the funding landscape graph
#(which you already made for DRC), but just for testing. I just want to know who is 
#contributing to HIV testing and at what levels if we have it. 1 – 2 bullet points on what’s 
#“of note” (e.g. “PEPFAR was the largest contributor to HIV testing from 2015 to present…”) would also be great.


# I'm unable to do this because I don't have HIV testing in the Other DAH activities. 

#--------------------------------------------------------------------------------------------------
# •	Could you e-mail me the PU/DRs just for this grant (labelled PU/DRs)? If you noticed
#anything specific to testing absorption, a few bullet points would be very helpful 
#(e.g. “Expenditure on HIV test kit procurement was high in 2018, with 100% absorption in Q1 and 90% in Q2”; I made those numbers up). 

#Done. 

#--------------------------------------------------------------------------------------------------
# •	Could you ask David if he has any thoughts on what might be helpful on absorption? 
#I’m not familiar with the absorption data, so if you brainstormed a slide or two together that would be amazing. 

#-----------------------
# EMILY HOLD ON THIS
#-----------------------

testing_absorption = absorption[code%in%testing_codes]
testing_absorption[gf_module=="Comprehensive prevention programs for men who have sex with men", gf_module:="Men who have sex with men"]
testing_absorption[gf_module=="Comprehensive prevention programs for people who inject drugs and their partners", gf_module:="People who inject drugs and their partners"]
testing_absorption[gf_module=="Comprehensive prevention programs for sex workers and their clients", gf_module:="Sex workers and their clients"]
testing_absorption[gf_module=="Comprehensive prevention programs for transgender people", gf_module:="Transgender people"]
testing_absorption[gf_module=="HIV Testing Services", gf_module:="General HIV Testing"]

s1_by_mod = testing_absorption[semester=="Semester 1" & grant_period=="2018-2020", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('gf_module')]
s1_by_mod[, absorption:=round((expenditure/budget)*100, 2)]
s1_by_grant = testing_absorption[semester=="Semester 1" & grant_period=="2018-2020", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'gf_module')]
s1_by_grant[, absorption:=round((expenditure/budget)*100, 2)]

y1_by_mod = testing_absorption[semester=="Semester 1-2" & grant_period=="2018-2020", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('gf_module')]
y1_by_mod[, absorption:=round((expenditure/budget)*100, 2)]
y1_by_grant = testing_absorption[semester=="Semester 1-2" & grant_period=="2018-2020", .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'gf_module')]
y1_by_grant[, absorption:=round((expenditure/budget)*100, 2)]

#The semester 1 PUDRs I have didn't report any absorption, even though they had amounts budgeted for HIV testing for PWID and transgender people. 

plot = ggplot(s1_by_mod, aes(x=gf_module, y=absorption, fill=gf_module)) + 
  geom_bar(stat="identity", position="dodge") + 
  coord_flip() +
  theme_bw() + 
  theme(axis.text.y=element_blank(), axis.title.y=element_blank()) + 
  geom_text(aes(label=paste0(absorption, "%"))) + 
  labs(title=paste0("Global Fund HIV testing absorption for COD-C-CORDAID, S1 2018"), y="Absorption (%)", fill="Subpopulation")

ggsave(paste0(save_loc, "s1_absorption_by_module.png"), plot, height=10, width=13)

plot = ggplot(y1_by_mod, aes(x=gf_module, y=absorption, fill=gf_module)) + 
  geom_bar(stat="identity", position="dodge") + 
  coord_flip() +
  theme_bw() + 
  theme(axis.text.y=element_blank(), axis.title.y=element_blank()) + 
  geom_text(aes(label=paste0(absorption, "%"))) + 
  labs(title=paste0("Global Fund HIV testing absorption for COD-C-CORDAID, S1-S2 2018"), y="Absorption (%)", fill="Subpopulation")

ggsave(paste0(save_loc, "y1_absorption_by_module.png"), plot, height=10, width=13)

