# ----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Map activities as part of results chain. 
#       
# DATE: Last updated November 2018. 
#-----------------------------------------------------

#-----------------------------------------------------
# Current issues to iron out: 
# We have over 700 unique activities. How can we merge them? 
#-----------------------------------------------------


#Want to map activities over time. 

#DRC malaria from 2013-2018
budget_by_module_13_18 <- budget_over_time("Congo (Democratic Republic)", "malaria", 2013, 2018, "gf_module")
budget_by_module_13_18

ggsave(paste0(cod_save, "/budget_by_module.png"), width = 25, height = 25, units = "cm", dpi = "retina")

#GTM TB from 2013-2018
budget_by_module_13_18 <- budget_over_time("Guatemala", "tb", 2013, 2018, "gf_module")
budget_by_module_13_18

ggsave(paste0(gtm_save, "/budget_by_module.png"), width = 25, height = 25, units = "cm", dpi = "retina")

#UGA HIV from 2013-2018
budget_by_module_13_18 <- budget_over_time("Uganda", "hiv", 2013, 2018, "gf_module")
budget_by_module_13_18

ggsave(paste0(uga_save, "/budget_by_module.png"), width = 25, height = 25, units = "cm", dpi = "retina")