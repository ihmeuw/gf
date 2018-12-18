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
gtm_tb_landscape_10_16 <- funding_landscape("Guatemala", "tb", 2012, 2016, TRUE)
ggsave(paste0(gtm_save, "/funding_landscape_tb.png"), plot = gtm_tb_landscape_10_16, height=6, width=14)
ggsave(paste0(gtm_save, "/funding_landscape_tb.pdf"), plot = gtm_tb_landscape_10_16, height=6, width=14)

#Guatemala malaria funding landscape 
gtm_mal_landscape_10_16 <- funding_landscape("Guatemala", "malaria", 2012, 2016, TRUE)
ggsave(paste0(gtm_save, "/funding_landscape_mal.png"), plot = gtm_mal_landscape_10_16, height=6, width=14)
ggsave(paste0(gtm_save, "/funding_landscape_mal.pdf"), plot = gtm_mal_landscape_10_16, height=6, width=14)

#Guatemala HIV funding landscape 
gtm_hiv_landscape_10_16 <- funding_landscape("Guatemala", "hiv", 2012, 2016, TRUE)
ggsave(paste0(gtm_save, "/funding_landscape_hiv.png"), plot = gtm_hiv_landscape_10_16, height=6, width=14)
ggsave(paste0(gtm_save, "/funding_landscape_hiv.pdf"), plot = gtm_hiv_landscape_10_16, height=6, width=14)


#-----------
#   DRC
# ----------
#DRC Malaria Funding Landscape 
drc_mal_landscape_10_16 <- funding_landscape("Congo (Democratic Republic)", "malaria", 2010, 2016, FALSE)
ggsave(paste0(cod_save, "/funding_landscape_mal.png"), plot = drc_mal_landscape_10_16, height=6, width=14)
ggsave(paste0(cod_save, "/funding_landscape_mal.pdf"), plot = drc_mal_landscape_10_16, height=6, width=14)

#DRC HIV Funding Landscape 
drc_hiv_landscape_10_16 <- funding_landscape("Congo (Democratic Republic)", "hiv", 2010, 2016, FALSE)
ggsave(paste0(cod_save, "/funding_landscape_hiv.png"), plot = drc_hiv_landscape_10_16, height=6, width=14)
ggsave(paste0(cod_save, "/funding_landscape_hiv.pdf"), plot = drc_hiv_landscape_10_16, height=6, width=14)

#DRC TB Funding Landscape 
drc_tb_landscape_10_16 <- funding_landscape("Congo (Democratic Republic)", "tb", 2010, 2016, FALSE)
ggsave(paste0(cod_save, "/funding_landscape_tb.png"), plot = drc_tb_landscape_10_16, height=6, width=14)
ggsave(paste0(cod_save, "/funding_landscape_tb.pdf"), plot = drc_tb_landscape_10_16, height=6, width=14)


#-----------
# Uganda
# ----------
#Uganda HIV Funding Landscape 
uga_hiv_landscape_10_16 <- funding_landscape("Uganda", "hiv", 2010, 2016, FALSE)
ggsave(paste0(uga_save, "/funding_landscape_hiv.png"), plot = uga_hiv_landscape_10_16, height=6, width=14)
ggsave(paste0(uga_save, "/funding_landscape_hiv.pdf"), plot = uga_hiv_landscape_10_16, height=6, width=14)

#Uganda TB Funding Landscape 
uga_tb_landscape_10_16 <- funding_landscape("Uganda", "tb", 2010, 2016, FALSE)
ggsave(paste0(uga_save, "/funding_landscape_tb.png"), plot = uga_tb_landscape_10_16, height=6, width=14)
ggsave(paste0(uga_save, "/funding_landscape_tb.pdf"), plot = uga_tb_landscape_10_16, height=6, width=14)

#Uganda Malaria Funding Landscape 
uga_mal_landscape_10_16 <- funding_landscape("Uganda", "malaria", 2010, 2016, FALSE)
ggsave(paste0(uga_save, "/funding_landscape_mal.png"), plot = uga_mal_landscape_10_16, height=6, width=14)
ggsave(paste0(uga_save, "/funding_landscape_mal.pdf"), plot = uga_mal_landscape_10_16, height=6, width=14)


#----------------------------------------------
# Map budgets for clusters of modules over time 
#----------------------------------------------

#-----------
# Guatemala 
# ----------

gtm_hiv_modules_10_18 <- modules_over_time("Guatemala", "hiv", 2010, 2018)
ggsave(paste0(gtm_save, "/module_categories_hiv.png"), plot = gtm_hiv_modules_10_18, height=6, width=14)
ggsave(paste0(gtm_save, "/module_categories_hiv.pdf"), plot = gtm_hiv_modules_10_18, height=6, width=14)

gtm_tb_modules_10_18 <- modules_over_time("Guatemala", "tb", 2010, 2018)
ggsave(paste0(gtm_save, "/module_categories_tb.png"), plot = gtm_tb_modules_10_18, height=6, width=14)
ggsave(paste0(gtm_save, "/module_categories_tb.pdf"), plot = gtm_tb_modules_10_18, height=6, width=14)

gtm_mal_modules_10_18 <- modules_over_time("Guatemala", "malaria", 2010, 2018)
ggsave(paste0(gtm_save, "/module_categories_mal.png"), plot = gtm_mal_modules_10_18, height=6, width=14)
ggsave(paste0(gtm_save, "/module_categories_mal.pdf"), plot = gtm_mal_modules_10_18, height=6, width=14)


#-----------
#   DRC
# ----------

cod_hiv_modules_10_18 <- modules_over_time("Congo (Democratic Republic)", "hiv", 2010, 2018)
ggsave(paste0(cod_save, "/module_categories_hiv.png"), plot = cod_hiv_modules_10_18, height=6, width=14)
ggsave(paste0(cod_save, "/module_categories_hiv.pdf"), plot = cod_hiv_modules_10_18, height=6, width=14)

cod_tb_modules_10_18 <- modules_over_time("Congo (Democratic Republic)", "tb", 2010, 2018)
ggsave(paste0(cod_save, "/module_categories_tb.png"), plot = cod_tb_modules_10_18, height=6, width=14)
ggsave(paste0(cod_save, "/module_categories_tb.pdf"), plot = cod_tb_modules_10_18, height=6, width=14)

cod_mal_modules_10_18 <- modules_over_time("Congo (Democratic Republic)", "malaria", 2010, 2018)
ggsave(paste0(cod_save, "/module_categories_mal.png"), plot = cod_mal_modules_10_18, height=6, width=14)
ggsave(paste0(cod_save, "/module_categories_mal.pdf"), plot = cod_mal_modules_10_18, height=6, width=14)

#-----------
# Uganda
# ----------

uga_hiv_modules_10_18 <- modules_over_time("Uganda", "hiv", 2010, 2018)
ggsave(paste0(uga_save, "/module_categories_hiv.png"), plot = uga_hiv_modules_10_18, height=6, width=14)
ggsave(paste0(uga_save, "/module_categories_hiv.pdf"), plot = uga_hiv_modules_10_18, height=6, width=14)


#Why do we only have HIV as a disease in gf_budgets??
# uga_tb_modules_10_18 <- modules_over_time("Uganda", "tb", 2010, 2018)
# ggsave(paste0(uga_save, "/module_categories_tb.png"), plot = uga_tb_modules_10_18, width = 35, height = 25, units = "cm", dpi = "retina")
# 
# uga_mal_modules_10_18 <- modules_over_time("Uganda", "malaria", 2010, 2018)
# ggsave(paste0(uga_save, "/module_categories_mal.png"), plot = uga_mal_modules_10_18, width = 35, height = 25, units = "cm", dpi = "retina")
