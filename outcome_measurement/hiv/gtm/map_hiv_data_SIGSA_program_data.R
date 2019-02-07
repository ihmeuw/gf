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

#------------ Load Functions------------------------

fix_diacritics <- function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')
  return(chartr(replace_me, replace_with, x))
  
}

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

#### Prep Data
# ----------------------------------------------
#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# define main directory
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/SIGSA/')
prep_dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
outFile = paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA/SIGSA_hiv_test_Facility.pdf')
mapping_dir = paste0(root, '/Project/Evaluation/GF/mapping/gtm/')

# Read in file
dt = readRDS(paste0(prep_dir, "hiv_sigsa_data_prepped_PD_testing.rds"))

dt$Status = ifelse(dt$Status == "P.VIH","Tested", ifelse(dt$Status == "VIH+", "HIV+",dt$Status))

dt$DAS = fix_diacritics(dt$DAS)
dt$DISTRITO = trimws(dt$DISTRITO)
dt$DISTRITO = ifelse(dt$DISTRITO == "ASI",'ASOCIACION DE SALUD INTEGRAL', dt$DISTRITO )

# Clinca del Adolenscente has DAS in DISTRITO column, and servicio is empty
dt$SERVICIO = ifelse(dt$DAS == "CLINICA DEL ADOLESCENTE", dt$DAS, dt$SERVICIO)
dt$DAS = ifelse(dt$DAS == "CLINICA DEL ADOLESCENTE", dt$DISTRITO, dt$DAS)

# We don't have the locations of these yet
mapping_UAI = dt[DAS == "UNIDADES DE ATENCION INTEGRAL"]
mapping_data = dt[DAS != "UNIDADES DE ATENCION INTEGRAL"]

#### graphData: merging Admin 1- Departments shapefile to dataset ####
shapeData = shapefile(paste0(mapping_dir,'GTM_adm1.shp'))
coordinates = as.data.table(fortify(shapeData, region='NAME_1'))
coordinates$id <- toupper(coordinates$id)
coordinates$id = fix_diacritics(coordinates$id)

mapping_data$year = year(mapping_data$Date)

mapping_data$DAS = toupper(mapping_data$DAS)
mapping_data$DAS = sub("GUATEMALA", "GUATEMALA", mapping_data$DAS)
mapping_data[grepl("GUATEMALA", DAS), DAS := "GUATEMALA"]
mapping_data[grepl("PETEN", DAS), DAS := "PETEN"]
mapping_data[grepl("IXCAN", DAS), DAS := "QUICHE"]
mapping_data[grepl("QUETZALTENANGO", DAS), DAS := "QUEZALTENANGO"]
mapping_data = mapping_data[,value := sum(value, na.rm = TRUE),.(year, Group, Status, DAS)]
mapping_data = unique(mapping_data[Status == "Tested" | Status == "HIV+",.(year, Group, Status, DAS, value)])

mappingData <- merge(coordinates, mapping_data, by.x='id', by.y='DAS', all=TRUE, allow.cartesian=TRUE)


graphing_dt = unique(dt[, sum(value, na.rm = TRUE), by=.(Date, Group, Status)])
graphing_dt$Status <- factor(graphing_dt$Status, levels=c("PRE", "ACEP", "POST", "Tested", "CONF.","HIV+" ))

pB = ggplot(unique(graphing_dt[Status == "Tested" | Status == "HIV+"]), aes(y=V1, x=Date, color = Group)) + 
  geom_line()+
  geom_point(size=.1, color='grey45') + 
  labs(title='', y='Count', x='Tested (n = 36,779), HIV+ (n = 1,739)') + 
  facet_wrap(~Status, scales='free_y')+
  theme_bw() 


map_num_MTS_tested = unique(mappingData[Status == "Tested" & Group == "MTS"])
mapMTSTest <- (ggplot() + geom_polygon(data=map_num_MTS_tested, aes(x=long, y=lat, group=group, fill=value)) +
          coord_equal() + ##so the two shapefiles have the same proportions
          geom_path(data=shapeData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
          facet_wrap(~year) +
          scale_fill_gradientn(colors=results_colors) +
          theme_void()+
          theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "Annual Completed HIV test for Female Sex Workers", fill=paste0('Number of tests done'))


map_num_MTS_pos = unique(mappingData[Status == "HIV+" & Group == "MTS"])
mapMTSPos <- (ggplot() + geom_polygon(data=map_num_MTS_pos, aes(x=long, y=lat, group=group, fill=value)) +
                 coord_equal() + ##so the two shapefiles have the same proportions
                 geom_path(data=shapeData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
                 facet_wrap(~year) +
                 scale_fill_gradientn(colors=results_colors) +
                 geom_text(aes(label = value, x = Longitude, y = Latitude)) +
                 theme_void()+
                 theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "Annual HIV+ test for Female Sex Workers", fill=paste0('Number of HIV+ tests'))

map_num_HSH_tested = unique(mappingData[Status == "Tested" & Group == "HSH"])
mapHSHTest <- (ggplot() + geom_polygon(data=map_num_HSH_tested, aes(x=long, y=lat, group=group, fill=value)) +
                 coord_equal() + ##so the two shapefiles have the same proportions
                 geom_path(data=shapeData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
                 facet_wrap(~year) +
                 scale_fill_gradientn(colors=results_colors) +
                 theme_void()+
                 theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "Annual Completed HIV test for Men who have sex with Men", fill=paste0('Number of tests done'))


map_num_HSH_pos = unique(mappingData[Status == "HIV+" & Group == "HSH"])
mapHSHPos <- (ggplot() + geom_polygon(data=map_num_HSH_pos, aes(x=long, y=lat, group=group, fill=value)) +
                coord_equal() + ##so the two shapefiles have the same proportions
                geom_path(data=shapeData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
                facet_wrap(~year) +
                scale_fill_gradientn(colors=results_colors) +
                theme_void()+
                theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "Annual HIV+ test for Men who have sex with Men", fill=paste0('Number of HIV+ tests'))

map_num_TRANS_tested = unique(mappingData[Status == "Tested" & Group == "TRANS"])
mapTRANSTest <- (ggplot() + geom_polygon(data=map_num_TRANS_tested, aes(x=long, y=lat, group=group, fill=value)) +
                 coord_equal() + ##so the two shapefiles have the same proportions
                 geom_path(data=shapeData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
                 facet_wrap(~year) +
                 scale_fill_gradientn(colors=results_colors) +
                 theme_void()+
                 theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "Annual Completed HIV test for Transgender people", fill=paste0('Number of tests done'))


map_num_TRANS_pos = unique(mappingData[Status == "HIV+" & Group == "TRANS"])
mapTRANSPos <- (ggplot() + geom_polygon(data=map_num_TRANS_pos, aes(x=long, y=lat, group=group, fill=value)) +
                coord_equal() + ##so the two shapefiles have the same proportions
                geom_path(data=shapeData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
                facet_wrap(~year) +
                scale_fill_gradientn(colors=results_colors) +
                theme_void()+
                theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
  labs(title= "Annual HIV+ test for Transgender people", fill=paste0('Number of HIV+ tests'))


pdf(outFile, height=5.5, width=7)
pB

mapHSHTest
mapHSHPos

mapMTSTest
mapMTSPos

mapTRANSTest
mapTRANSPos

dev.off()



list_of_plots = NULL
i=1
#dt_lgbt_status = mapping_long[, sum(value, na.rm = TRUE), by=.(date, variable, sexual_orientation, risk_condition_eng)]
#pc0 = ggplot(dt_lgbt_status[variable == "attended_clinic" | variable == "completedTest"], aes(y=V1, x=date, color = variable)) + 

# for(s in unique(map_num_tested$Group)) {
#   # look up district name
#   name <- unique(map_num_tested[Group==s]$Group)
#   
#   # make your graph
#   
#   list_of_plots[[i]] <-  (ggplot() + geom_polygon(data=map_num_tested[Group == s], aes(x=long, y=lat, group=group, fill=V1)) +
#                             coord_equal() + ##so the two shapefiles have the same proportions
#                             geom_path(data=map_num_tested, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
#                             facet_wrap(~year(Date)) +
#                             scale_fill_gradientn(colors=results_colors) +
#                             theme_void()+
#                             theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
#     labs(title= "Annual Completed HIV test by Department", fill=paste0('Number of tests done'))
#   
#   i=i+1
#   
# }



pdf("/home/j/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA_hiv_test_facility.pdf", height=6, width=9)
for(i in seq(length(list_of_plots))) {
  print(list_of_plots[[i]])
}
dev.off()






# ggplot(dt_lgbt_status[(variable == "completedTest" | variable == "isPosTest") & risk_condition_eng== s], aes(y=as.integer(V1), x=date, color = variable)) +
#   geom_point() +
#   geom_line(alpha=0.5) +
#   facet_wrap(~sexual_orientation, scales='free_y') +
#   theme_bw() + labs(title=name, x='Date', y='Count', color='Risk_Condition')
# 
