# ----------------------------------------------
# Irena Chen
#
# 3/1/2018
# ### Map FPM SDAs to Sicoin $$ by municipality
# ----------------------------------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(reshape)
library(scales)
library(ggrepel)
library(dplyr)
# ----------------------------------------------

##load the data: 
gtmBudgets <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_fpm_pudr.csv", 
                                  fileEncoding = "latin1"))

sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                   ,fileEncoding="latin1"))


##change the start dates from factors to dates: 

sicoin_data <- sicoin_data[source=="gf"]

sicoin_data$start_date <- as.Date(sicoin_data$start_date,"%Y-%m-%d")
gtmBudgets$start_date <- as.Date(gtmBudgets$start_date,"%Y-%m-%d")
sicoin_data$year <- year(sicoin_data$start_date)

gtmBudgets <- gtmBudgets[!(data_source=="pudr"&year>2015)]

##sum up budget (as "variable") by year, disease, and data source 
byVars = names(gtmBudgets)[names(gtmBudgets)%in%c('year', 'disease', 'code','gf_module', 'gf_intervention')]
gtmBudgets = gtmBudgets[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))
                          , disbursement=sum(na.omit(disbursement))), by=byVars]


##this is the interventions data set: 
gtmBudgets<- gtmBudgets[with(gtmBudgets, order(disease,year,code, gf_module, gf_intervention, budget)), ]
gtmBudgets[, int_fraction := budget/sum(budget), by=c("disease","year", "gf_module")]


##just do for 2013 SICOIN: 
gtm_subset <- gtmBudgets[year==2013&disease=="malaria"]


sicoin_subset <- sicoin_data[year==2013&disease=="malaria"]
sicoin_subset$code <- NULL
sicoin_subset$gf_intervention <- NULL

##sum up budget (as "variable") by year, disease, and data source 
byVars = names(sicoin_subset)[names(sicoin_subset)%in%c('year', 'disease','gf_module', 'adm1', 'adm2', 'loc_name')]
sicoin_subset = sicoin_subset[, list(budget=sum(na.omit(budget)), 
                                     disbursement=sum(na.omit(disbursement))), by=byVars]


##this is the interventions data set: 
sicoin_subset <- sicoin_subset [with(sicoin_subset, order(disease,year,gf_module, adm1, adm2, loc_name)), ]
sicoin_subset [, muni_fraction := budget/sum(budget), by=c("disease","year")]

setwd('J:/Project/Evaluation/GF/mapping/gtm/')



# ----------------------------------------------
# load the shapefile
shapeData = shapefile('J:/Project/Evaluation/GF/mapping/gtm/GTM_munis_only.shp')

## load the admin1 shape with the projection: 
adminData = shapefile('J:/Project/Evaluation/GF/mapping/gtm/gtm_region.shp')

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
# use IDs instead of names
coordinates = data.table(fortify(shapeData, region='Codigo'))
admin_coords <- data.table(fortify(adminData, region='ID_1'))
coordinates$id <- as.numeric(coordinates$id)

# merge on municipality names
names = data.table(shapeData@data)
admin_names <- data.table(adminData@data)
coord_and_names = merge(coordinates, names, by.x='id', by.y='Codigo', allow.cartesian=TRUE)
admin_dataset = merge(admin_coords, admin_names, by.x = 'id', by.y='ID_1', allow.cartesian=TRUE)








pdf("J:/Project/Evaluation/GF/resource_tracking/multi_country/sicoin_fpm_graphs.pdf", height=9, width=12)
invisible(lapply(gos_nat_plots, print))
dev.off()





