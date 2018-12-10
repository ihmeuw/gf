# ----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Map inputs as part of results chain. 
#       
# DATE: Last updated November 2018. 
#-----------------------------------------------------

#Take milestones table, and graph 5 categories over time.

#----------
# Inputs 
#----------

#1. Funding landscape- think about a stacked area plot for this. Especially global fund, GHE, other. Split graph by data source and financing source.

#Guatemala TB Funding Landscape
gtm_landscape_13_18 <- funding_landscape("Guatemala", "tb", 2013, 2018)
gtm_landscape_13_18

ggsave(paste0(gtm_save, "/funding_landscape.png"), width = 25, height = 25, units = "cm", dpi = "retina")

#DRC Malaria Funding Landscape 
drc_landscape_13_18 <- funding_landscape("Congo (Democratic Republic)", "malaria", 2013, 2018)
drc_landscape_13_18

ggsave(paste0(cod_save, "/funding_landscape.png"), width = 25, height = 25, units = "cm", dpi = "retina")

#Uganda HIV Funding Landscape 
uga_landscape_13_18 <- funding_landscape("Uganda", "hiv", 2013, 2018)
uga_landscape_13_18

ggsave(paste0(uga_save, "/funding_landscape.png"), width = 25, height = 25, units = "cm", dpi = "retina")

