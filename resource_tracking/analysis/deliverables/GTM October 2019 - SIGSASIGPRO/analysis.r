#--------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Run a few financial graphs for 
#Guatemala to support SIGSA/SIGPRO analyses 
# DATE: October 17, 2019 
#-------------------------------------------

#Prep workspace 
rm(list=ls())
library(data.table) 
setwd("C:/Users/elineb/Documents/gf") #Set to the root of your repository 
source("resource_tracking/visualizations/graphing_functions.r")

#Make funding landscape graph 
p1 = funding_landscape("gtm", "hiv", 2010, 2018, altCaption = "*Other DAH data only available until 2017")

# What did INCAP spend by module? 
p2 = absorption_by_loc_disease("gtm", "hiv", "2018-2020", byModule=TRUE, grantName="GTM-H-INCAP", barLabels=TRUE)

#Within key populations, what % went to testing? 
dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/GTM/prepped_data/absorption_gtm.rds")
testing_codes = c("H2_7", "H3_7", "H4_6", "H5_7", "H6_7", "H7_3", "H8_5", "H14", "H14_1")
test_data = dt[code%in%testing_codes & grant%in%c('GTM-H-HIVOS', 'GTM-H-INCAP')]

#Fix a few intervention names 
test_data[gf_intervention=="HIV testing services for transgender people", gf_intervention:="Transgender people"]
test_data[gf_intervention=="HIV testing services for men who have sex with men", gf_intervention:="Men who have sex with men"]
test_data[gf_intervention=="Differentiated HIV testing services", gf_intervention:="Differentiated testing"]

p3 = ggplot(test_data, aes(x=gf_intervention, y=absorption, fill=grant, label=paste0(round(absorption, 1), "%"))) + 
  geom_bar(stat="identity") + 
  geom_text(size=4) + 
  theme_bw(base_size=16) + 
  coord_flip() + 
  scale_fill_manual(values=ihme_divergent[2:3]) + 
  facet_wrap(~grant) + 
  labs(title="2018 absorption for testing interventions by grant", x="Module", y="Absorption (%)", fill="Grant")

#Output graphs 
pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/GTM October 2019 SIGSA-SIGPRO/supporting_financial_graphs.pdf", height=8, width=11)
p1
p2
p3
dev.off() 