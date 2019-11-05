# Senegal financial analyses for 2019 report 
# Emily Linebarger, started November 4, 2019 

rm(list=ls()) 
library(data.table) 
library(ggplot2) 

dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_budgets_sen.rds")


# I did a little more digging and according to the TRP review, the matching funds request met the conditions 
#for being catalytic (at the time of the funding request review). 
#I think the question is whether the funding levels for KP and HR that were approved in the final budgets
#(after grant negotiation) respect the original levels proposed in the funding request.  
#Since we know that a lot can change during grant negotiation between the PRs and GF, 
#it would be interesting to double check what was approved in the final grant budgets.  
#According to the table below, I would expect that for the CNLS and ANCS budgets combined, the budget levels should be at least:
# •	EUR 2,970,625 for HIV: Key Populations (1,990,417 + 980,208)
# •	EUR 2,290,132 for HIV: Programs to remove HR barriers to services (1,199,357 + 1,090,775)

kp_mods = c("Comprehensive prevention programs for men who have sex with men", "Comprehensive prevention programs for people who inject drugs and their partners",
            "Comprehensive prevention programs for sex workers and their clients", "Prevention of mother-to-child transmission", 
            "Prevention programs for adolescents and youth, in and out of school", "Comprehensive prevention programs for transgender people")
dt[gf_module%in%kp_mods, is_hiv_key_pop:=TRUE]
dt[is.na(is_hiv_key_pop), is_hiv_key_pop:=FALSE]
analysis1 = dt[grant%in%c("SEN-H-ANCS", "SEN-H-CNLS") & grant_period=="2018-2020", .(budget=sum(budget, na.rm=T)), by=c('is_hiv_key_pop', 'gf_module')][order(is_hiv_key_pop)]

analysis1[, total_key_pop:=sum(budget), by='is_hiv_key_pop']