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
snis_res_dps <- snis_res[, lapply(.SD, sum, na.rm=TRUE), by = .(dps, element, element_eng, quarter, date), .SDcols = sd_cols]

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

snis_reg_dps <-  snis_reg[, .(value = sum(value, na.rm = TRUE)), by = .(quarter, dps, element, element_eng)]
snis_reg_dps$element <- as.character(snis_reg_dps$element)
# ----------------------------------------------

# ----------------------------------------------
# fortify the shapefile to get coordinates and convert it to a data table / standardize dps names for merging
# ----------------------------------------------
coordinates = as.data.table(fortify(drcShape, region='NAME_1'))
coordinates$id <- standardizeDPSNames(coordinates$id)

snis_reg_dps_expanded = expand.grid(quarter = unique(snis_reg_dps$quarter), dps = unique(snis_reg_dps$dps), element = unique(snis_reg_dps$element))
snis_reg_dps_full = merge(snis_reg_dps, snis_reg_dps_expanded, all = TRUE, by = c("quarter", "dps", "element"))

graphData_full <- merge(snis_reg_dps_full, coordinates, by.y='id', by.x='dps', allow.cartesian = TRUE)
# ----------------------------------------------

# ----------------------------------------------
# plot
# ----------------------------------------------
# list of vars to map
vars = c("TB_Nombre traité Mineurs")

# create a list of plots
plots = NULL
i=1

# loop through vars and make plots
for (var in vars) {
  max = max(graphData[element == var, ]$value, na.rm=TRUE)
  min = min(graphData[element == var, ]$value, na.rm=TRUE)
  med = max/2
 
  graphData = graphData_full[element == var, ]
   
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
             fill='Count', caption = '') + 
        theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12)) +
        facet_wrap( ~ quarter)
  
  i=i+1
}

# print out the list of plots into a pdf
pdf(paste0(out_dir, outFile), height=10, width=12)

for(i in seq(length(plots))) { 
  print(plots[[i]])
} 

dev.off()



