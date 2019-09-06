#--------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Generate financial graphs for GTM mixed 
# methods presentation 
# DATE: August 29, 2019 
#-------------------------------------------------

# "GF planned $X on TB and are struggling to spend all of it" 

repo_root = "C:/Users/elineb/Documents/gf/"
setwd(repo_root)

source("./resource_tracking/visualizations/graphing_functions.r")

save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/GTM TB TERG Meeting September 2019/"

#What is overall absorption for TB for GTM-T-MSPAS in 2016-2019? 
p1 = absorption_by_loc_disease('gtm', 'tb', '2016-2019', bySemester=TRUE, stackBudgetExp = TRUE, grantName = "GTM-T-MSPAS", barLabels=T)
ggsave(paste0(save_loc, "overall_tb_absorption.png"), p1, height=8, width=11)

#What about specific modules - case detection? 
p2 = absorption_by_loc_disease('gtm', 'tb', '2016-2019', byIntervention=TRUE, limitModules="TB care and prevention", grantName = "GTM-T-MSPAS", barLabels=TRUE, 
                               barColor="mediumpurple2", facetSemester=F, poolSemester=c('Semester 1-2', 'Semester 3-4', 'Semester 5'), yScaleMax=100, baseSize=18)
ggsave(paste0(save_loc, "care_and_prev_by_intervention.png"), p2, height=8, width=11)

#What about specific modules - MDR-TB? 
p3 = absorption_by_loc_disease('gtm', 'tb', '2016-2019', byIntervention=TRUE, limitModules="Multidrug-resistant TB", grantName = "GTM-T-MSPAS", 
                               barLabels=TRUE, barColor="firebrick3", facetSemester=F, poolSemester=c('Semester 1-2', 'Semester 3-4', 'Semester 5'), yScaleMax=100, baseSize=18)
ggsave(paste0(save_loc, "mdr_by_intervention.png"), p3, height=8, width=11)


#Funding landscape including GHE 
p4 = funding_landscape('gtm', 'tb', 2010, 2018, includeGHE=TRUE, altCaption="*GHE data only available from 2011-2018")
ggsave(paste0(save_loc, "tb_funding_landscape.png"), p4, height=8, width=11)







#What proportion of the GF TB grant is for the MDR module? 
p5 = absorption_by_loc_disease('gtm', 'tb', '2016-2019', byModule=TRUE, grantName = "GTM-T-MSPAS", barLabels=TRUE)
ggsave(paste0(save_loc, "tb_by_module.png"), p5, height=8, width=11)

#What about specific modules - MDR-TB? 


dt = fread("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.csv")
saveRDS(dt, "J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
