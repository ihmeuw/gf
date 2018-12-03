rm(list = ls())
library(data.table) 

allData <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv")

#--------------------------------------------------------
#Pull most recent budgets 
#--------------------------------------------------------
mr_budgets <- allData[data_source == "fpm"]
gtm_grants <- c('GTM-H-HIVOS', 'GTM-H-INCAP', 'GTM-M-MSPAS', 'GTM-T-MSPAS')
mr_budgets <- mr_budgets[(grant_period == "2018-2020" & country %in% c('Congo (Democratic Republic)', 'Uganda')) 
                         | (country == "Guatemala" & grant_number %in% gtm_grants)]

#Fix Guatemala grants 
mr_budgets <- mr_budgets[!year < 2017, ]
mr_budgets <- mr_budgets[!(grant_number == "GTM-T-MSPAS" & grant_period == "2016-2019"), ]

#Still don't have current GTM malaria budget?? 

stopifnot(length(unique(mr_budgets$fileName))==length(unique(mr_budgets$grant_number)))
combined_grants <- c('UGA-C-TASO', 'COD-C-CORDAID')
mr_budgets[grant_number %in% combined_grants, disease:="hiv/tb"]

#--------------------------------------------------------
#Check table 1 on page 4
#--------------------------------------------------------
mr_budgets[, sum(budget), by = c('grant_number', 'disease')]
mr_budgets[, sum(budget), by = c('country', 'disease')]

#HIV- key populations impact 
kp_mods <- c('Prevention programs for MSM', 'Prevention programs for IJU', 'Prevention programs for CSW & clients', 'Prevention programs for transgender', 'Prevention programs for prisoners')
mr_budgets[abbrev_module %in% kp_mods & disease == "hiv", sum(budget), by = c('country', 'disease')]

#Human rights barriers 
mr_budgets[abbrev_module == "Human rights barriers" & disease == "hiv", sum(budget), by = c('country', 'disease')]

#Adolescent girls and young women 
mr_budgets[abbrev_module == "Prevention programs for youth/adol." & disease == "hiv", sum(budget), by = c('country', 'disease')]

#Finding missing TB cases (?) 

#RSSH 
rssh_mods <- c('Service delivery, Nat. health strategies', 'Community systems', 'HR & health workers', 'Info systems & M&E', 'Financial systems', 'PSM')
mr_budgets[abbrev_module %in% rssh_mods, sum(budget), by = c('country')]
mr_budgets[abbrev_module %in% rssh_mods, sum(budget), by = c('country', 'abbrev_module')]

#--------------------------------------------------------
#Check table 3 on page 24
#--------------------------------------------------------
#??? I don't know how to confirm this.  

#--------------------------------------------------------
#Check table 3 on page 24
#--------------------------------------------------------
#It looks like Kath created this so we might be okay. 

#--------------------------------------------------------
#Check figure 8 and 9 on page 27. Could really verify all numbers in this section more, but because of differences in defining RSSH, it will be tricky. 
#Overall the message looks right, and I believe PATH generated this section. 
#--------------------------------------------------------
#We should probably review these further for the next round. It looks like my numbers are off a bit, especially for Guatemala, but I think PATH may have generated this?? 
#Figure 8 - I'm getting differnet percentages here. 
rssh_by_country = mr_budgets[abbrev_module %in% rssh_mods, .(rssh = sum(budget)), by = 'country']
total_budget_by_country = mr_budgets[, .(budget = sum(budget)), by = 'country']
total_rssh = merge(total_budget_by_country, rssh_by_country, by = 'country', fill = T)
total_rssh[, rssh_percent:=(rssh/budget)*100]

#Figure 9 - on a cursory glance we seem okay here. Proportions line up correctly. 
mr_budgets[abbrev_module %in% rssh_mods, sum(budget), by = c('country', 'abbrev_module')]

#Page 7 - verify extremely high budget absorption numbers. 
pudrs<- allData[data_source == "pudr" & grant_period == "2018-2020"]
cod_pudrs <- pudrs[country == "Congo (Democratic Republic)"]

duplicate_files <- c('Copy of LFA Review_COD-H-MOH_Progress  Report_30Jun2018_07092018 ok_Sent....xlsb.xlsx', 'COD-M-SANRU_Progress Report_30Jun2018_v4_15092018.xlsx',
                     'Copy of LFA_Review_COD-T-MOH_Progress Report_30Jun2018_Sent_02102018-Brk....xlsx')
cod_pudrs = cod_pudrs[!fileName %in% duplicate_files]

cod_pudrs[, absorption:=expenditure/budget, by = c('grant_number', 'abbrev_module')]
