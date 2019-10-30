#--------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Run a few financial graphs for 
#Guatemala to support SIGSA/SIGPRO analyses 
# DATE: October 17, 2019 
#-------------------------------------------

#Prep workspace 
rm(list=ls())
library(data.table) 
library(ggplot2)
library(scales)
setwd("C:/Users/elineb/Documents/gf") #Set to the root of your repository 
source("resource_tracking/analysis/graphing_functions.r")

saveloc="J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/GTM October 2019 SIGSA-SIGPRO/"

#Make funding landscape graph 
p1 = funding_landscape("gtm", "hiv", 2010, 2017, graphType="ribbon", includeGHE=TRUE)

# What did INCAP spend by module? 
p2 = absorption_by_loc_disease("gtm", "hiv", "2018-2020", byModule=TRUE, grantName="GTM-H-INCAP", barLabels=TRUE)

#Within key populations, what % went to testing? 
dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/absorption_gtm.rds")
testing_codes = c("H2_7", "H3_7", "H4_6", "H5_7", "H6_7", "H7_3", "H8_5", "H14", "H14_1")
test_data = dt[code%in%testing_codes & grant%in%c('GTM-H-HIVOS', 'GTM-H-INCAP')]
test_data[, absorption:=round(absorption, 1)]

#Fix a few intervention names 
test_data[gf_intervention=="HIV testing services for transgender people", gf_intervention:="Transgender people"]
test_data[gf_intervention=="HIV testing services for men who have sex with men", gf_intervention:="Men who have sex with men"]
test_data[gf_intervention=="Differentiated HIV testing services", gf_intervention:="Differentiated testing"]

p3 = ggplot(test_data, aes(x=gf_intervention, y=absorption, fill=grant, label=paste0(round(absorption, 1), "%"))) + 
  geom_bar(stat="identity") + 
  geom_text(size=4) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  theme(legend.position="none") + 
  scale_fill_manual(values=ihme_divergent[2:3]) + 
  facet_wrap(~grant) + 
  labs(title="2018 absorption for testing interventions by grant", x="Module", y="Absorption (%)", fill="Grant")

melt = test_data[, .(gf_intervention, grant, budget, expenditure)]
melt = melt(melt, id.vars=c('gf_intervention', 'grant'), value.name='amount')
melt[, label:=dollar(amount)]
melt[variable=="budget", label:=""]
melt[variable=="budget", variable:="Budget"]
melt[variable=="expenditure", variable:="Expenditure"]

p4 = ggplot(melt, aes(x=gf_intervention, y=amount, label=label, fill=variable)) + 
  geom_bar(position="identity", stat="identity") + 
  theme_bw() + 
  geom_text() + 
  facet_wrap(~grant) +
  coord_flip() + 
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="2018 budget/expenditure for testing interventions by grant", x="Module", y="", fill="")

#Output graphs 
pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/GTM October 2019 SIGSA-SIGPRO/supporting_financial_graphs.pdf", height=8, width=11)
p1
p2
p3
p4
dev.off() 

ggsave(paste0(saveloc, "funding_landscape.png"), p1, height=8, width=12)
ggsave(paste0(saveloc, "incap_absorption.png"), p2, height=8, width=12)
ggsave(paste0(saveloc, "testing_absorption.png"), p3, height=8, width=12)
ggsave(paste0(saveloc, "testing_budget_exp.png"), p4, height=8, width=12)



