#-------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Run analyses for Uganda TB deep dive
# DATE: July 10, 2018
#--------------------------------------------

rm(list=ls())
library(data.table) 

# 
# Would it be possible to look at GeneXpert machine procurement and supply chain management 
# in the current and previous Uganda HIV/TB grants? This would be simply a list of any activity description 
# that contains the word “GeneXpert” or possibly “Xpert” for the previous 2015 – 2017 grant cycle and 
# the current cycle 2018 – 2020. I think this will likely be only a few activities. Up to you whether you 
# just look in R or Excel for this. Also, I know the titles are “Y1 – Y4 total cash outflow” but both grants are only 
# three years so I changed the title (below). This could just be a CSV list.

#Read in budget data 
budgets = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/uga/prepped_data/final_budgets.rds")

#First, just make a dataset of all cases where Xpert procurement might be happning. 
xpert = budgets[grepl("xpert|genexpert|x-pert", tolower(activity_description))]
# This is pulling an activity description from UGA-M-MoFPED that has the word 'expert' in it. 
# "Conduct periodic competency assessments for the current pool of 45 experts"
# So only keep TB grants. 
unique(xpert$grant)
xpert = xpert[grant=="UGA-T-MoFPED"]


#What grant periods do you have? 
unique(xpert$grant_period)


write.csv(xpert, "J:/Project/Evaluation/GF/resource_tracking/other/specialized_datasets/uga/genexpert_activities.csv", row.names=F)


#Side note - this is a definite use case for NLP! 
unique(xpert[, .(activity_description, orig_module, orig_intervention, grant, grant_period)])
