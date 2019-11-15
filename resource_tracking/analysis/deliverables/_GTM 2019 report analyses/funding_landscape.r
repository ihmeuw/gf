# Make graphs for Guatemala annual report 2019 
# Emily linebarger, updated November 2019 

rm(list=ls()) 
library(data.table) 
library(ggplot2) 

setwd("C:/Users/elineb/Documents/gf") #Set to root of your repository 
source("./resource_tracking/analysis/graphing_functions.r")

hiv_ribbon = funding_landscape("ribbon", "gtm", "hiv", 2010, 2017, includeGHE=TRUE)
tb_ribbon = funding_landscape("ribbon", "gtm", "tb", 2010, 2017, includeGHE=TRUE)
malaria_ribbon = funding_landscape("ribbon", "gtm", "malaria", 2010, 2017, includeGHE=TRUE)

hiv_prop = funding_landscape("proportion", "gtm", "hiv", 2010, 2017, includeGHE=TRUE)
tb_prop = funding_landscape("proportion", "gtm", "tb", 2010, 2017, includeGHE=TRUE)
malaria_prop = funding_landscape("proportion", "gtm", "malaria", 2010, 2017, includeGHE=TRUE)


pdf("C:/Users/elineb/Documents/gf/resource_tracking/analysis/deliverables/_GTM 2019 report analyses/funding_landscape_graphs.pdf", width=11, height=8)
hiv_ribbon
tb_ribbon
malaria_ribbon

hiv_prop
tb_prop
malaria_prop
dev.off() 