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
outFile = paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA_hiv_test.pdf')
mapping_dir = paste0(root, '/Project/Evaluation/GF/mapping/gtm/')

# translation file
translate_data = fread(paste0(dir, "translation_of_HIV_variables.csv"), encoding = 'Latin-1')

#read in file
dt <-data.table(read_excel(paste0(dir,"Solicitud 0593-2018/Solicitud 0593-2018 SIGSA SIDA 1.2 años 2014 al 2017.xlsx")))

total_data = dt

# rename columns
new_names = c("year", "month", "hospital_department", "hospital_health_district", "health_service", "service_type", "municipality", "date", "identifier","birth_department",
              "birth_municipality", "birth_day", "birth_month", "birth_year", "nationality", "sex", "residence_department", "residence_municipality",
              "sexual_orientation", "town", "community_linguistics", "risk_condition", "reason_for_visit", "pregnancy_stage", "pre_orientaiton_test", "did_test",
              "completed_hiv_screening_test", "hiv_screening_result", "hiv_confirm_test", "hiv_confirm_test_result",
              "sifilis_treponemal_test", "sifilis_treponemal_test_result", "sifilis_non_treponemal_test", "sifilis_dilution_test",
              "hepB_screening_test", "hepB_screening_result", "recieved_post_rest_results", "reference")

names(total_data) = new_names

# Drop columns of personal identifying information
total_data$birth_municipality = NULL
total_data$birth_department = NULL
total_data$identifier = NULL

# Remove first and last 3 rows 
total_data = total_data[-(1:3), , drop = FALSE] 
total_data = total_data[1:(nrow(total_data) - 3),, drop = FALSE]

# Create date variable
total_data$YearMonth <- as.yearmon(paste(total_data$year, " ", total_data$month), "%Y %m")
total_data$date <- as.Date(with(total_data, paste(year, month, "1",sep="-")), "%Y-%m-%d")

# Translate data that we wil be using
reason_eng = translate_data[1:14]
risk_eng = translate_data[16:24]
setnames(risk_eng, old = c("reason_for_visit", "reason_for_visit_eng"), new = c("risk_condition", "risk_condition_eng"))
preg_eng = translate_data[26:31]
setnames(preg_eng, old = c("reason_for_visit", "reason_for_visit_eng"), new = c("pregnancy_stage", "pregnancy_stage_eng"))

total_data = merge(total_data, reason_eng, by = "reason_for_visit")
total_data = merge(total_data, risk_eng, by = "risk_condition")
total_data = merge(total_data, preg_eng, by = "pregnancy_stage")

# Prep for mapping
total_data$completed_hiv_screening_test = gsub("Si", "Yes", total_data$completed_hiv_screening_test)
total_data$completed_hiv_screening_test = gsub("Reactivo", "Yes", total_data$completed_hiv_screening_test)
total_data$completed_hiv_screening_test = gsub("-", "No", total_data$completed_hiv_screening_test)
total_data$hiv_screening_result = gsub("-", "Inconclusive", total_data$hiv_screening_result)

# Write csv to folderpath
write.csv(total_data, paste0(prep_dir, "hiv_sigsa_data_prepped.csv"), row.names = FALSE)

#### Let's Graph These ####
#Dropping 2014 for analysis becuase data does not seem accurate

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

mapping_data = total_data[year != "2014"]

# Prep data for map
shapeData = shapefile(paste0(mapping_dir,'GTM_adm1.shp'))
coordinates = as.data.table(fortify(shapeData, region='NAME_1'))
coordinates$id <- toupper(coordinates$id)

mapping_data$hospital_department = sub("GUATEMALA", "GUATEMALA", mapping_data$hospital_department)
mapping_data[grepl("GUATEMALA", hospital_department), hospital_department := "GUATEMALA"]
mapping_data[grepl("PETÉN", hospital_department), hospital_department := "PETÉN"]
mapping_data[grepl("IXCÁN", hospital_department), hospital_department := "QUICHÉ"]
mapping_data[grepl("QUETZALTENANGO", hospital_department), hospital_department := "QUEZALTENANGO"]

mapping_data$completed_hiv_screening_test = ifelse(mapping_data$completed_hiv_screening_test == "Yes", 1, 0)
mapping_data$hiv_screening_result = ifelse(mapping_data$hiv_screening_result == "Reactivo", 1, 0)


mapping_data[, test_by_department := sum(completed_hiv_screening_test), by = c("year", "hospital_department")]
mapping_data[, test_by_department_positive := sum(hiv_screening_result), by = c("year", "hospital_department")]

data_to_map = unique(mapping_data[,c("hospital_department", 
                                     "test_by_department",
                                     "test_by_department_positive",
                                     "year")])
graphData <- merge(coordinates, data_to_map, by.x='id', by.y='hospital_department', all=TRUE, allow.cartesian=TRUE)
graphData$group = as.character(graphData$group)

ratio_colors <- brewer.pal(8, 'Spectral')
           
# Graph desired things
mapping_data[, test_by_date := sum(completed_hiv_screening_test), by = "date"]
mapping_data[, test_by_date_postive := sum(hiv_screening_result), by = "date"]

mapping_data[, test_by_orientation := sum(completed_hiv_screening_test), by = c("date", "sexual_orientation")]
mapping_data[, test_by_orientation_positive := sum(hiv_screening_result), by = c("date", "sexual_orientation")]

mapping_data[, test_by_risk := sum(completed_hiv_screening_test), by = c("date", "risk_condition_eng")]
mapping_data[, test_by_risk_positive := sum(hiv_screening_result), by = c("date", "risk_condition_eng")]

mapping_data[, test_by_reason := sum(completed_hiv_screening_test), by = c( "date", "reason_for_visit_eng")]
mapping_data[, test_by_reason_positive := sum(hiv_screening_result), by = c( "date", "reason_for_visit_eng")]

mapping_data_sub = unique(mapping_data[,c("date", "reason_for_visit_eng", "sexual_orientation", "risk_condition_eng",  "test_by_date", "test_by_date_postive", "test_by_orientation", "test_by_orientation_positive",
                                          "test_by_risk", "test_by_risk_positive", "test_by_reason", "test_by_reason_positive")])

#### Make graphs of data ####
p0 = ggplot(unique(mapping_data_sub), aes(y=test_by_date, x=date)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  #geom_text(hjust=1, vjust=0) + 
  labs(title='Completed HIV Tests over Time', 
       y='Number of Completed HIV test', x='') + 
  theme_bw()

p1 = ggplot(unique(mapping_data_sub), aes(y=test_by_date_postive, x=date)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  #geom_text(hjust=1, vjust=0) + 
  labs(title='Positive HIV Tests over Time', 
       y='Amount of positive HIV test', x='') + 
  theme_bw()

p2 = ggplot(unique(mapping_data_sub), aes(y=test_by_date_postive / test_by_date * 100, x=date)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  #geom_text(hjust=1, vjust=0) + 
  labs(title='Percenatage (%) of HIV Tests over Time', 
       y='Percent (%) positive HIV test', x='') + 
  theme_bw()

p3 = ggplot(unique(mapping_data_sub[sexual_orientation != "Heterosexual" & sexual_orientation != "-" ]), aes(y=test_by_orientation, x=date, colour=sexual_orientation)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  labs(title='Completed HIV test by at-risk Sexual Orientation over Time', 
       y='Amount of completed HIV test', x='') + 
  theme_bw()

p4 = ggplot(unique(mapping_data_sub[sexual_orientation != "Heterosexual" & sexual_orientation != "-" ]), aes(y=test_by_orientation_positive, x=date, colour=sexual_orientation)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  labs(title='Positive HIV test by at-risk Sexual Orientation over Time', 
       y='Amount of positive HIV test', x='') + 
  theme_bw()

p5 = ggplot(unique(mapping_data_sub[sexual_orientation != "Heterosexual" & sexual_orientation != "-" ]), aes(y=test_by_orientation_positive / test_by_orientation * 100, x=date, colour=sexual_orientation)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  labs(title='Percentage of Postive HIV test by at-risk Sexual Orientation over Time', 
       y='Percent (%) positive HIV test', x='') + 
  theme_bw()

p6 = ggplot(unique(mapping_data_sub), aes(y=test_by_risk, x=date, colour=risk_condition_eng)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  labs(title='Completed HIV test by Risk Group', 
       y='Amount of completed HIV test', x='') + 
  theme_bw()

p7 = ggplot(unique(mapping_data_sub), aes(y=test_by_risk_positive, x=date, colour=risk_condition_eng)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  labs(title='Positive HIV test by Risk Group', 
       y='Amount of positive HIV test', x='') + 
  theme_bw()

p8 = ggplot(unique(mapping_data_sub), aes(y=test_by_risk_positive / test_by_risk * 100, x=date, colour=risk_condition_eng)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  labs(title='Percentage of Positive HIV test by Risk Group', 
       y='Percent (%) of positive HIV test', x='') + 
  theme_bw()


p9 = ggplot(unique(mapping_data_sub[reason_for_visit_eng != "Own Initiative" & reason_for_visit_eng != "Other"]), aes(y=test_by_reason, x=date, colour=reason_for_visit_eng)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  labs(title='Completed HIV test by Visit Reason', 
       y='Amount of completed HIV test', x='') + 
  theme_bw()

p10 = ggplot(unique(mapping_data_sub[reason_for_visit_eng != "Own Initiative" & reason_for_visit_eng != "Other"]), aes(y=test_by_reason_positive, x=date, colour=reason_for_visit_eng)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  labs(title='Positive HIV test by Visit Reason', 
       y='Amount of positive HIV test', x='') + 
  theme_bw()

p11 = ggplot(unique(mapping_data_sub[reason_for_visit_eng != "Own Initiative" & reason_for_visit_eng != "Other" & reason_for_visit_eng != "Blood donor"]), aes(y=test_by_reason_positive / test_by_reason * 100, x=date, colour=reason_for_visit_eng)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  labs(title='Percentage of Postive HIV test by Visit Reason', 
       y='Percent (%) of positive HIV test', x='') + 
  theme_bw()

#2015 Maps
p12 = (ggplot() + geom_polygon(data=graphData[year == 2015], aes(x=long, y=lat, group=group, fill=as.numeric(test_by_department))) + 
         coord_equal() + ##so the two shapefiles have the same proportions 
         geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
         scale_fill_gradientn(colors=results_colors) +
         theme_void()+
         theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "2015 Completed HIV test by Department", fill=paste0('Amount of tests done'))

p13 = (ggplot() + geom_polygon(data=graphData[year == 2015], aes(x=long, y=lat, group=group, fill=as.numeric(test_by_department_positive))) + 
         coord_equal() + ##so the two shapefiles have the same proportions 
         geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
         scale_fill_gradientn(colors=results_colors) +
         theme_void()+
         theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "2015 Positive HIV test by Department", fill=paste0('Amount of HIV+'))


p14 = (ggplot() + geom_polygon(data=graphData[year == 2015], aes(x=long, y=lat, group=group, fill=test_by_department_positive/ test_by_department * 100)) + 
         coord_equal() + ##so the two shapefiles have the same proportions 
         geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
         scale_fill_gradientn(colors=results_colors) +
         theme_void()+
         theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "2015 Percent(%) Positive HIV Test", fill=paste0('Percent (%) HIV+')) 

# 2016 Maps
p15 = (ggplot() + geom_polygon(data=graphData[year == 2016], aes(x=long, y=lat, group=group, fill=as.numeric(test_by_department))) + 
           coord_equal() + ##so the two shapefiles have the same proportions 
           geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
         scale_fill_gradientn(colors=results_colors) +
         theme_void()+
           theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "2016 Completed HIV test by Department", fill=paste0('Amount of tests done'))

p16 = (ggplot() + geom_polygon(data=graphData[year == 2016], aes(x=long, y=lat, group=group, fill=as.numeric(test_by_department_positive))) + 
         coord_equal() + ##so the two shapefiles have the same proportions 
         geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
         scale_fill_gradientn(colors=results_colors) +
         theme_void()+
         theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "2016 Positive HIV test by Department", fill=paste0('Amount of HIV+'))

p17 = (ggplot() + geom_polygon(data=graphData[year == 2016], aes(x=long, y=lat, group=group, fill=test_by_department_positive/ test_by_department * 100)) + 
          coord_equal() + ##so the two shapefiles have the same proportions 
          geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
         scale_fill_gradientn(colors=results_colors) +
         theme_void()+
          theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "2016 Percent(%) Positive HIV Test", fill=paste0('Percent (%) HIV+')) 

#2017 Maps
p18 <- (ggplot() + geom_polygon(data=graphData[year == 2017], aes(x=long, y=lat, group=group, fill=test_by_department)) + 
          coord_equal() + ##so the two shapefiles have the same proportions 
          geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
          scale_fill_gradientn(colors=results_colors) +
          theme_void()+
          theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "2017 Completed HIV test by Department", fill=paste0('Amount of tests done')) 

p19 <- (ggplot() + geom_polygon(data=graphData[year == 2017], aes(x=long, y=lat, group=group, fill=test_by_department_positive)) + 
          coord_equal() + ##so the two shapefiles have the same proportions 
          geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
          scale_fill_gradientn(colors=results_colors) + 
          theme_void()+
          theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "2017 Positive HIV test by Department", fill=paste0('Amount of HIV+')) 

p20 <- (ggplot() + geom_polygon(data=graphData[year == 2017], aes(x=long, y=lat, group=group, fill=test_by_department_positive/ test_by_department * 100)) + 
          coord_equal() + 
          geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
          scale_fill_gradientn(colors=results_colors) +
          theme_void()+
          theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
         labs(title= "2017 Percent(%) Positive HIV Test", fill=paste0('Percent (%) HIV+')) 

#ALL YEAR MAPS Maps
p21 <- (ggplot() + geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=test_by_department)) + 
          coord_equal() + ##so the two shapefiles have the same proportions 
          geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
          facet_wrap(~year) +
          scale_fill_gradientn(colors=results_colors) +
          theme_void()+
          theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "Annual Completed HIV test by Department", fill=paste0('Amount of tests done')) 

p22 <- (ggplot() + geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=test_by_department_positive)) + 
          coord_equal() + ##so the two shapefiles have the same proportions 
          geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
          facet_wrap(~year) +
          scale_fill_gradientn(colors=results_colors) + 
          theme_void()+
          theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "Annual Positive HIV test by Department", fill=paste0('Amount of HIV+')) 

p23 <- (ggplot() + geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=test_by_department_positive/ test_by_department * 100)) + 
          coord_equal() + 
          geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
          facet_wrap(~year) +
          scale_fill_gradientn(colors=results_colors) +
          theme_void()+
          theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "Annual Percent(%) Positive HIV Test", fill=paste0('Percent (%) HIV+')) 


#outFile = "/homes/ninip/SIGSA_results.pdf"
pdf(outFile, height=5.5, width=7)
p0
p1
p2
p3
p4
p5
p6
p7
p8
p9
p10
p11
# p12
# p13
# p14
# p15
# p16
# p17
# p18
# p19
# p20
p21
p22
p23
dev.off()

