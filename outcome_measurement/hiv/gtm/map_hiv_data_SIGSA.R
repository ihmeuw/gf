# ----------------------------------------------
# Naomi Provost
# September 6, 2018
# Master code file for GTM HIV data cleaning 
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(readxl)
library(stats)
library(stringr)
library(rlang)
library(zoo)
library(raster)
library(rgeos)
library(ggplot2)
library(maptools)
library(RColorBrewer)

# ----------------------------------------------
#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#------------ Prep Data------------------------
#### Prep Data
# define main directory
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/SIGSA/')
prep_dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
outFile = paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA_hiv_test_2ndRound.pdf')
mapping_dir = paste0(root, '/Project/Evaluation/GF/mapping/gtm/')


dt = readRDS(paste0(prep_dir, "hiv_sigsa_data_prepped.rds"))

dt$attended_clinic = 1

mapping_data = dt[,.(date, municipality, hospital_department, sex, sexual_orientation, risk_condition_eng, attended_clinic,  completed_hiv_screening_test, hiv_screening_result)]

mapping_data$completedTest = ifelse(mapping_data$completed_hiv_screening_test == "Yes", 1, 0)
mapping_data$rejectedTest = ifelse(mapping_data$completed_hiv_screening_test == "No", 1, 0)
mapping_data$isPosTest = ifelse(mapping_data$hiv_screening_result == "Reactivo", 1, 0)
mapping_data$isNegTest = ifelse(mapping_data$hiv_screening_result == "No Reactivo", 1, 0)
mapping_data$completedTest = ifelse(mapping_data$isPosTest == 1, 1, mapping_data$completedTest)

mapping_data$hiv_screening_result = NULL
mapping_data$completed_hiv_screening_test = NULL

mapping_data[, attended_clinic := sum(attended_clinic, na.rm = TRUE), by=c('date', 'municipality', 'hospital_department', 'sex', 'sexual_orientation', 'risk_condition_eng')]
mapping_data[, completedTest := sum(completedTest, na.rm = TRUE), by=c('date', 'municipality', 'hospital_department', 'sex', 'sexual_orientation', 'risk_condition_eng')]
mapping_data[, rejectedTest := sum(rejectedTest, na.rm = TRUE), by=c('date', 'municipality', 'hospital_department', 'sex', 'sexual_orientation', 'risk_condition_eng')]
mapping_data[, isNegTest := sum(isNegTest, na.rm = TRUE), by=c('date', 'municipality', 'hospital_department', 'sex', 'sexual_orientation', 'risk_condition_eng')]
mapping_data[, isPosTest := sum(isPosTest, na.rm = TRUE), by=c('date', 'municipality', 'hospital_department', 'sex', 'sexual_orientation', 'risk_condition_eng')]

mapping_data[, percentTestdate:= (sum(completedTest, na.rm = TRUE) / sum(attended_clinic, na.rm = TRUE)), by=c('date')]
mapping_data[, percentTestrisk:= (sum(completedTest, na.rm = TRUE) / sum(attended_clinic, na.rm = TRUE)), by=c('date', 'risk_condition_eng')]
mapping_data[, percentTestlgbt:= (sum(completedTest, na.rm = TRUE) / sum(attended_clinic, na.rm = TRUE)), by=c('date', 'sexual_orientation')]

mapping_data[, percentPosdate := (sum(isPosTest, na.rm = TRUE) / sum(completedTest, na.rm = TRUE)), by=c('date')]
mapping_data[, percentPosrisk := (sum(isPosTest, na.rm = TRUE) / sum(completedTest, na.rm = TRUE)), by=c('date', 'risk_condition_eng')]
mapping_data[, percentPoslgbt := (sum(isPosTest, na.rm = TRUE) / sum(completedTest, na.rm = TRUE)), by=c('date', 'sexual_orientation')]


mapping_data = unique(mapping_data)

mapping_long = melt(mapping_data, id.vars = c('date', 'municipality', 'hospital_department', 'sex', 'sexual_orientation', 'risk_condition_eng'))

mapping_long$isMSM = ifelse(mapping_long$sex == "Masculino" & (mapping_long$sexual_orientation == 'Homosexual' | mapping_long$sexual_orientation == 'Bisexual'), 1, 0)


#### R Code for making Maps pretty ####
ratio_colors <- brewer.pal(8, 'Spectral')
results_colors <- brewer.pal(6, 'Blues')
sup_colors <- brewer.pal(6, 'Reds')
ladies <- brewer.pal(11, 'RdYlBu')
gents <- brewer.pal(9, 'Purples') # lavender for males

# red colors for bar graph
bar_colors <- c('Not Suppressed'='#de2d26', 'Suppressed'='#fc9272')

# color palettes I made
graph_colors <- c('#bd0026', '#fecc5c', '#74c476','#3182bd', '#8856a7')
tri_sex <- c('#bd0026', '#74c476', '#3182bd')
wrap_colors <- c('#3182bd', '#fecc5c', '#bd0026', '#74c476', '#8856a7', '#f768a1')
sex_colors <- c('#bd0026', '#3182bd', '#74c476', '#8856a7') # colors by sex plus one for facilities
single_red <- '#bd0026'

# breaks for log transformation legends (this is to set more intuitive breaks if you log the colors):
breaks <- c (1, 20, 400, 8100)


#### graphData: merging Admin 1- Departments shapefile to dataset ####
shapeData = shapefile(paste0(mapping_dir,'GTM_adm1.shp'))
coordinates = as.data.table(fortify(shapeData, region='NAME_1'))
coordinates$id <- toupper(coordinates$id)

# mapping_data$hospital_department = sub("GUATEMALA", "GUATEMALA", mapping_data$hospital_department)
# mapping_data[grepl("GUATEMALA", hospital_department), hospital_department := "GUATEMALA"]
# mapping_data[grepl("PETÉN", hospital_department), hospital_department := "PETÉN"]
# mapping_data[grepl("IXCÁN", hospital_department), hospital_department := "QUICHÉ"]
# mapping_data[grepl("QUETZALTENANGO", hospital_department), hospital_department := "QUEZALTENANGO"]
# 
# graphData <- merge(coordinates, mapping_data, by.x='id', by.y='hospital_department', all=TRUE, allow.cartesian=TRUE)
# graphData$group = as.character(graphData$group)

mapping_long$hospital_department = sub("GUATEMALA", "GUATEMALA", mapping_long$hospital_department)
mapping_long[grepl("GUATEMALA", hospital_department), hospital_department := "GUATEMALA"]
mapping_long[grepl("PETÉN", hospital_department), hospital_department := "PETÉN"]
mapping_long[grepl("IXCÁN", hospital_department), hospital_department := "QUICHÉ"]
mapping_long[grepl("QUETZALTENANGO", hospital_department), hospital_department := "QUEZALTENANGO"]

graphData <- merge(coordinates, mapping_long, by.x='id', by.y='hospital_department', all=TRUE, allow.cartesian=TRUE)
graphData$group = as.character(graphData$group)

# #### Let's make time trends for municipality ####
# list_of_plots = NULL
# i=1
# for(o in unique(und1$org_unit)) {
#   # facility name
#   name <- unique(und1[org_unit==o]$org_unit)
#   
#   # list of plots
#   list_of_plots[[i]] <-  ggplot(und1[org_unit==o], aes(x=date, y=value, color=case, group=case)) +
#     geom_point() +
#     geom_line(alpha=0.5) +
#     facet_wrap(~group, scales='free_y') +
#     theme_bw() + labs(title=name, x='Date', y='Count', color='Case')
#   
#   i=i+1
#   
# }
# pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/vresults_suppressed.pdf', height=6, width=9)
# 
# for(i in seq(length(list_of_plots))) {
#   print(list_of_plots[[i]])
# }
# dev.off()
# 
# ####Let's make some graphs ####

# TOTAL POPULATION
mapping_long = mapping_long[year(date) != "2014"]
dt_total_pop = mapping_long[, sum(value, na.rm = TRUE), by=.(date, variable)]
pa0 = ggplot(dt_total_pop[variable == "attended_clinic" | variable == "completedTest"], aes(y=V1, x=date, color = variable)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  labs(title='Number of Patients over Time', 
       y='Count', x='') + 
  theme_bw()  


dt_total_percent = unique(mapping_long[variable == "percentTestdate", .(date, variable, value)])
pa1 = ggplot(dt_total_percent, aes(y=value, x=date)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  labs(title='Percent(%) of Patients who Completed HIV Tests', 
       y='Percent (%)', x='') + 
  theme_bw()  

dt_total_percent_pos = unique(mapping_long[variable == "percentPosdate", .(date, variable, value)])
pa2 = ggplot(dt_total_percent_pos, aes(y=value, x=date)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  labs(title='Percent (%) of Patients who tested Positive(+) for HIV', 
       y='Count', x='') + 
  theme_bw()  

# BY KVP
dt_risk_group = mapping_long[, sum(value, na.rm = TRUE), by=.(date, variable, risk_condition_eng)]
pb0 = ggplot(dt_risk_group[variable == "attended_clinic" | variable == "completedTest"], aes(y=V1, x=date, color = variable)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  facet_wrap(~risk_condition_eng,  scales='free_y')+
  labs(title='Number of Patients over Time by KVP', 
       y='Count', x='', color = "") + 
  theme_bw()  

dt_risk_percent = unique(mapping_long[variable == "percentTestrisk", .(date, variable, value, risk_condition_eng)])
pb1 = ggplot(dt_risk_percent, aes(y=value, x=date)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  facet_wrap(~risk_condition_eng,  scales='free_y')+
  labs(title='Percent(%) of Patients who Completed HIV Tests', 
       y='Percent (%)', x='') + 
  theme_bw()  

dt_risk_percent_pos = unique(mapping_long[variable == "percentPosrisk", .(date, variable, value, risk_condition_eng)])
pb2 = ggplot(dt_risk_percent_pos, aes(y=value, x=date)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  facet_wrap(~risk_condition_eng,  scales='free_y')+
  labs(title='Percent (%) of Patients who tested Positive(+) for HIV', 
       y='Percent (%)', x='') + 
  theme_bw()  


## BY LGBT STATUS

dt_lgbt_status = mapping_long[, sum(value, na.rm = TRUE), by=.(date, variable, sexual_orientation)]
pc0 = ggplot(dt_lgbt_status[variable == "attended_clinic" | variable == "completedTest"], aes(y=V1, x=date, color = variable)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  facet_wrap(~sexual_orientation,  scales='free_y')+
  labs(title='Number of Patients over Time by LGBT Status', 
       y='Count', x='', color = "LGBT Status") + 
  theme_bw() 


dt_lgbt_percent = unique(mapping_long[variable == "percentTestlgbt", .(date, variable, value, sexual_orientation)])
pc1 = ggplot(dt_lgbt_percent, aes(y=value, x=date)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  facet_wrap(~sexual_orientation,  scales='free_y')+
  labs(title='Percent(%) of Patients who Completed HIV Tests', 
       y='Percent (%)', x='', color = "LGBT Status") + 
  theme_bw()  

dt_lgbt_percent_pos = unique(mapping_long[variable == "percentPoslgbt", .(date, variable, value, sexual_orientation)])
pc2 = ggplot(dt_lgbt_percent_pos, aes(y=value, x=date)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  facet_wrap(~sexual_orientation,  scales='free_y')+
  labs(title='Percent (%) of Patients who tested Positive(+) for HIV', 
       y='Percent (%)', x='', color = "LGBT Status") + 
  theme_bw()  

pdf(outFile, height=5.5, width=7)
pa0
pa1
pa2
pb0
pb1
pb2
pc0
pc1
pc2
dev.off()

# 
# #LETS MAKE SOME GRAPHS
# dt_lgbt_status = graph_1[, sum(value, na.rm = TRUE), by=.(date, variable, sexual_orientation)]
# p21 <- (ggplot() + geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=test_by_department)) + 
#           coord_equal() + ##so the two shapefiles have the same proportions 
#           geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
#           facet_wrap(~year) +
#           scale_fill_gradientn(colors=results_colors) +
#           theme_void()+
#           theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
#   labs(title= "Annual Completed HIV test by Department", fill=paste0('Number of tests done')) 
