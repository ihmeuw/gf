# ----------------------------------------------
# Audrey Batzel
#
# 4/8/19
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(raster)
library(rgeos)
library(ggplot2)
library(maptools)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
snis_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')
shape_dir <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/gadm36_COD_shp/")
out_dir = 'J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/SNIS_PATI_TB/'

# input files
pati_cases <- paste0(snis_dir, "pati_tb/tb_pati_new_tb_cases_relapses_by_age_sex.rds")
pati_registered <- paste0(snis_dir, "pati_tb/tb_pati_cases_registered.rds")
pati_results <- paste0(snis_dir, "pati_tb/tb_pati_case_results.rds")
dps_shapefile <- "gadm36_COD_1.shp"

# output files
outFile = "dps_maps_2018_quarterly.pdf"

# functions
source('./core/standardizeDPSNames.R')
# ----------------------------------------------

# ----------------------------------------------
# Read in data
# ----------------------------------------------
snis_cases <- readRDS(pati_cases)
snis_reg <- readRDS(pati_registered)
snis_res <- readRDS(pati_results)
drcShape <- shapefile( paste0(shape_dir, dps_shapefile) )
# ----------------------------------------------

# ----------------------------------------------
# agg to DPS level for mapping
# ----------------------------------------------
# first, aggregate snis_res to dps level
sd_cols <- colnames(snis_res)[grepl("TB_", colnames(snis_res))]
snis_res_dps = snis_res[date >= "2018-01-01"] 
snis_res_dps <- snis_res_dps[, lapply(.SD, sum, na.rm=TRUE), by = .(dps, element, element_eng, quarter, date), .SDcols = sd_cols]

# rename vars
names(snis_res_dps) <- c("dps", "element", "element_eng", "quarter", "date", "died", "trt_failed", "healed", "cas_not_eval", "lost_to_followup", "trt_complete")
snis_res_dps$element <- as.character(snis_res_dps$element)
snis_res_dps$element <- trimws(snis_res_dps$element)
snis_res_dps[element== "TB_Enfants 0-14 ans", element_eng:= "children_0to14yrs"]
snis_res_dps[element== "TB_Patients co infectés TB/VIH+ (Np+Rech)", element_eng:= "tbhiv_patients_new_relapse"]
snis_res_dps[element== "TB_Cas de la TB Extra pulmonaire", element_eng:= "tep_case"]
snis_res_dps[element== "TB_Patients  suivis par la communauté", element_eng:= "patients_followed_by_community"]
snis_res_dps[element== "TB_Cas confirmés bactériologiquement (Nouveaux et rechutes)", element_eng:= "tpp_new_relapse"]
snis_res_dps[element== "TB_Cas diagnostiqués cliniquement (Nouveaux et rechutes)", element_eng:= "tpc_new_relapse"]
snis_res_dps[element== "TB_Déjà traité Hors Rechutes", element_eng:= "case_alreadyTreated_notRelapse"]
snis_res_dps[, cas_eval := died + trt_failed + healed + lost_to_followup + trt_complete ]
snis_res_dps[, treatment_success_rate := ((trt_complete + healed) / cas_eval) * 100 ]

# ----------------------------------------------
# ----------------------------------------------

snis_reg_dps = snis_reg[date >= "2018-01-01"] 
snis_reg_dps = snis_reg_dps[, .(value = sum(value, na.rm = TRUE)), by = .(quarter, dps, element)]
snis_reg_dps$element <- as.character(snis_reg_dps$element)

#cast wide and rename vars for calculation
snis_reg = snis_reg[, .(quarter, date, org_unit, health_zone, dps, element, value)]
snis_reg = snis_reg[, .(value = sum(value)), by = .(quarter, date, org_unit, health_zone, dps, element)]
wide = dcast.data.table(snis_reg, quarter + date + org_unit + health_zone + dps ~ element)

# wide = dcast.data.table(snis_reg_dps, quarter + date + health_zone + dps ~ element)
setnames(wide, "TB-Patients (nouveaux et rechutes) avec résultat de test VIH connu", "TB_patients_known_HIV_test_result")
setnames(wide, "TB-Patients (nouveaux et rechutes) séropositifs pour le VIH", "TB_patients_positive_HIV")
setnames(wide, "TB-Patients séropositifs pour le VIH sous traitement antirétroviral (TAR)", "TB_patients_positive_HIV_on_ART")
setnames(wide, "TB-Rechute Cas de tuberculose pulmonaire, diagnostiqués cliniquement", "tpc_relapse")
setnames(wide, "TB-Rechute Cas de tuberculose pulmonaire, confirmés bactériologiquement", "tpp_relapse")
setnames(wide, "TB-Rechute Cas de tuberculose extrapulmonaire, confirmés bactériologiquement ou diagnostiqués cliniquement", "tep_relapse")
setnames(wide, "TB-Nouveau Cas de tuberculose pulmonaire, diagnostiqués cliniquement", "tpc_new")
setnames(wide, "TB-Nouveau Cas de tuberculose pulmonaire, confirmés bactériologiquement", "tpp_new")
setnames(wide, "TB-Nouveau Cas de tuberculose extrapulmonaire, confirmés bactériologiquement ou diagnostiqués cliniquement", "tep_new")

#calculate vars
wide[, notified_cases := tpc_relapse + tpp_relapse + tep_relapse + tpc_new + tpp_new + tep_new]
wide[, perc_notif_cases_with_hiv_test := (TB_patients_known_HIV_test_result / notified_cases) * 100]
wide[, perc_hiv_pos_on_art := (TB_patients_positive_HIV_on_ART / TB_patients_positive_HIV) * 100]

#scatterplots to look at the vars
ggplot(wide, aes(notified_cases, TB_patients_known_HIV_test_result, color = as.character(date))) + geom_point() + geom_abline() + theme_bw()
check = wide[ TB_patients_known_HIV_test_result > notified_cases, ]
check = check[,.(dps, date, quarter, TB_patients_known_HIV_test_result, notified_cases, tpp_new, tpp_relapse, tpc_new, tpc_relapse, tep_new, tep_relapse)]

ggplot(wide, aes(TB_patients_positive_HIV, TB_patients_positive_HIV_on_ART)) + geom_point() + geom_abline() + theme_bw() + xlim(0, 500)+ ylim(0, 500)
check = wide[ TB_patients_positive_HIV_on_ART > TB_patients_positive_HIV, ]

#melt
dt = melt.data.table(wide, id.vars = c('quarter', 'dps'))
# ----------------------------------------------

# ----------------------------------------------
# fortify the shapefile to get coordinates and convert it to a data table / standardize dps names for merging
# ----------------------------------------------
# list of vars to map
vars = c("perc_notif_cases_with_hiv_test", "perc_hiv_pos_on_art")
dt = dt[variable %in% vars]

coordinates = as.data.table(fortify(drcShape, region='NAME_1'))
coordinates$id <- standardizeDPSNames(coordinates$id)

dt_expanded = expand.grid(quarter = unique(dt$quarter), dps = unique(dt$dps), variable = unique(dt$variable))
dt_full = merge(dt, dt_expanded, all = TRUE, by = c("quarter", "dps", "variable"))

graphData_full <- merge(dt_full, coordinates, by.y='id', by.x='dps', allow.cartesian = TRUE)
# ----------------------------------------------

# ----------------------------------------------
# make plots
# ----------------------------------------------
# create a list of plots
plots = NULL
i=1

# loop through vars and make plots
for (var in vars) {
  max = max(graphData_full[variable == var, ]$value, na.rm=TRUE)
  min = min(graphData_full[variable == var, ]$value, na.rm=TRUE)
  med = max/2
 
  graphData = graphData_full[variable == var, ]
   
  plots[[i]] <-  ggplot() + 
        geom_polygon( data = graphData, aes(x=long, y=lat, group=group, fill=value) ) + 
        coord_equal() +
        geom_path(data = graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
        theme_void() +  
        scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                             na.value = "grey70", space = "Lab", midpoint = med, ## play around with this to get the gradient that you want, depending on data values 
                             breaks=round(seq(0, max, by=((max-min)/4))), 
                             limits=c(0,max), labels = scales :: comma) +
        labs(title= paste0("2018: ", var), 
             fill='Percent', caption = '') + 
        theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12)) +
        facet_wrap( ~ quarter)
  
  i=i+1
}
# ----------------------------------------------
# ----------------------------------------------
dt = snis_res_dps
dt_expanded = expand.grid(quarter = unique(dt$quarter), dps = unique(dt$dps), element = unique(dt$element))
dt_full = merge(dt, dt_expanded, all = TRUE, by = c("quarter", "dps", "element"))

graphData_full <- merge(dt_full, coordinates, by.y='id', by.x='dps', allow.cartesian = TRUE)

max = 100
min = 0
med = 90

graphData = graphData_full[element_eng == 'tbhiv_patients_new_relapse', ]

plots[[i]] <-  ggplot() + 
  geom_polygon( data = graphData, aes(x=long, y=lat, group=group, fill=treatment_success_rate) ) + 
  coord_equal() +
  geom_path(data = graphData_full, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = med, ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max), labels = scales :: comma) +
  labs(title= paste0("2018: ", var), 
       fill='Percent', caption = '') + 
  theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12)) +
  facet_wrap( ~ quarter)

i=i+1
# ----------------------------------------------
# ----------------------------------------------
# print out the list of plots into a pdf
pdf(paste0(out_dir, outFile), height=10, width=12)

for(i in seq(length(plots))) { 
  print(plots[[i]])
} 

dev.off()
# ----------------------------------------------


