# ----------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Map inputs as part of results chain. 
#       
# DATE: Last updated November 2018. 
#-----------------------------------------------------

#------------------------
# Map funding landscape - right now only have data through 2016. 
#------------------------


#-----------
# Guatemala 
# ----------
#Guatemala TB Funding Landscape
gtm_tb_landscape_10_16 <- funding_landscape("Guatemala", "tb", 2010, 2016)
ggsave(paste0(gtm_save, "/funding_landscape_tb.png"), plot = gtm_tb_landscape_10_16, width = 25, height = 25, units = "cm", dpi = "retina")

#Guatemala malaria funding landscape 
gtm_mal_landscape_10_16 <- funding_landscape("Guatemala", "malaria", 2010, 2016)
ggsave(paste0(gtm_save, "/funding_landscape_mal.png"), plot = gtm_mal_landscape_10_16, width = 25, height = 25, units = "cm", dpi = "retina")

#Guatemala HIV funding landscape 
gtm_hiv_landscape_10_16 <- funding_landscape("Guatemala", "hiv", 2010, 2016)
ggsave(paste0(gtm_save, "/funding_landscape_hiv.png"), plot = gtm_hiv_landscape_10_16, width = 25, height = 25, units = "cm", dpi = "retina")

#-----------
#   DRC
# ----------
#DRC Malaria Funding Landscape 
drc_mal_landscape_10_16 <- funding_landscape("Congo (Democratic Republic)", "malaria", 2010, 2016)
ggsave(paste0(cod_save, "/funding_landscape_mal.png"), plot = drc_mal_landscape_10_16, width = 25, height = 25, units = "cm", dpi = "retina")

#DRC HIV Funding Landscape 
drc_hiv_landscape_10_16 <- funding_landscape("Congo (Democratic Republic)", "hiv", 2010, 2016)
ggsave(paste0(cod_save, "/funding_landscape_hiv.png"), plot = drc_hiv_landscape_10_16, width = 25, height = 25, units = "cm", dpi = "retina")

#DRC TB Funding Landscape 
drc_tb_landscape_10_16 <- funding_landscape("Congo (Democratic Republic)", "tb", 2010, 2016)
ggsave(paste0(cod_save, "/funding_landscape_tb.png"), plot = drc_tb_landscape_10_16, width = 25, height = 25, units = "cm", dpi = "retina")

#-----------
# Uganda
# ----------
#Uganda HIV Funding Landscape 
uga_hiv_landscape_10_16 <- funding_landscape("Uganda", "hiv", 2010, 2016)
ggsave(paste0(uga_save, "/funding_landscape_hiv.png"), plot = uga_hiv_landscape_10_16, width = 25, height = 25, units = "cm", dpi = "retina")

#Uganda TB Funding Landscape 
uga_tb_landscape_10_16 <- funding_landscape("Uganda", "tb", 2010, 2016)
ggsave(paste0(uga_save, "/funding_landscape_tb.png"), plot = uga_tb_landscape_10_16, width = 25, height = 25, units = "cm", dpi = "retina")

#Uganda Malaria Funding Landscape 
uga_mal_landscape_10_16 <- funding_landscape("Uganda", "malaria", 2010, 2016)
ggsave(paste0(uga_save, "/funding_landscape_mal.png"), plot = uga_mal_landscape_10_16, width = 25, height = 25, units = "cm", dpi = "retina")


#----------------------------------------------
# Map budgets for clusters of modules over time 
#----------------------------------------------

#-----------
# Guatemala 
# ----------

gtm_hiv_modules_10_18 <- modules_over_time("Guatemala", "hiv", 2010, 2018)
ggsave(paste0(gtm_save, "/module_categories_hiv.png"), plot = gtm_hiv_modules_10_18, width = 25, height = 25, units = "cm", dpi = "retina")

gtm_tb_modules_10_18 <- modules_over_time("Guatemala", "tb", 2010, 2018)
ggsave(paste0(gtm_save, "/module_categories_tb.png"), plot = gtm_tb_modules_10_18, width = 25, height = 25, units = "cm", dpi = "retina")

gtm_mal_modules_10_18 <- modules_over_time("Guatemala", "malaria", 2010, 2018)
ggsave(paste0(gtm_save, "/module_categories_mal.png"), plot = gtm_mal_modules_10_18, width = 25, height = 25, units = "cm", dpi = "retina")


#-----------
#   DRC
# ----------

cod_hiv_modules_10_18 <- modules_over_time("Congo (Democratic Republic)", "hiv", 2010, 2018)
ggsave(paste0(cod_save, "/module_categories_hiv.png"), plot = cod_hiv_modules_10_18, width = 25, height = 25, units = "cm", dpi = "retina")

cod_tb_modules_10_18 <- modules_over_time("Congo (Democratic Republic)", "tb", 2010, 2018)
ggsave(paste0(cod_save, "/module_categories_tb.png"), plot = cod_tb_modules_10_18, width = 25, height = 25, units = "cm", dpi = "retina")

cod_mal_modules_10_18 <- modules_over_time("Congo (Democratic Republic)", "malaria", 2010, 2018)
ggsave(paste0(cod_save, "/module_categories_mal.png"), plot = cod_mal_modules_10_18, width = 25, height = 25, units = "cm", dpi = "retina")

#-----------
# Uganda
# ----------

uga_hiv_modules_10_18 <- modules_over_time("Uganda", "hiv", 2010, 2018)
ggsave(paste0(uga_save, "/module_categories_hiv.png"), plot = uga_hiv_modules_10_18, width = 25, height = 25, units = "cm", dpi = "retina")

#Why do we only have HIV as a disease in gf_budgets??
# uga_tb_modules_10_18 <- modules_over_time("Uganda", "tb", 2010, 2018)
# ggsave(paste0(uga_save, "/module_categories_tb.png"), plot = uga_tb_modules_10_18, width = 25, height = 25, units = "cm", dpi = "retina")
# 
# uga_mal_modules_10_18 <- modules_over_time("Uganda", "malaria", 2010, 2018)
# ggsave(paste0(uga_save, "/module_categories_mal.png"), plot = uga_mal_modules_10_18, width = 25, height = 25, units = "cm", dpi = "retina")
