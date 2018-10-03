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
# define main directory
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/SIGSA/')
prep_dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
outFile = paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA/SIGSA_hiv_test_Facility.pdf')
mapping_dir = paste0(root, '/Project/Evaluation/GF/mapping/gtm/')

# Read in file
dt = readRDS(paste0(prep_dir, "hiv_sigsa_data_prepped_PD_testing.rds"))
dt[,DISTRITO := gsub('·', "", DISTRITO)]
dt[,DISTRITO := gsub('\\.', "", DISTRITO)]
dt[, DISTRITO:= iconv(DISTRITO)]
dt[ , DISTRITO:= str_squish( DISTRITO)]

dt$DISTRITO = fix_diacritics(dt$DISTRITO)
dt$DISTRITO = toupper(trimws(dt$DISTRITO))
dt[,DISTRITO := gsub("CS DE", "",DISTRITO)]
dt[,DISTRITO := gsub("CS", "",DISTRITO)]
dt[,DISTRITO := gsub("CAP", "",DISTRITO)]
dt[,DISTRITO := gsub("UAI DE", "",DISTRITO)]
dt[,DISTRITO := gsub("UAI", "",DISTRITO)]

dt$Status = ifelse(dt$Status == "P.VIH","Tested", ifelse(dt$Status == "VIH+", "HIV+",dt$Status))

#### graphData: merging Admin 1- Departments shapefile to dataset ####
shapeData = shapefile(paste0(mapping_dir,'GTM_adm1.shp'))
coordinates = as.data.table(fortify(shapeData, region='NAME_1'))
coordinates$id <- toupper(coordinates$id)
coordinates$id = fix_diacritics(coordinates$id)


mapping_data = dt[, sum(value, na.rm = TRUE), by=.(Date, Group, Status, DAS)]
mapping_data$DAS = fix_diacritics(mapping_data$DAS)
mapping_data$DAS = toupper(mapping_data$DAS)
mapping_data$DAS = sub("GUATEMALA", "GUATEMALA", mapping_data$DAS)
mapping_data[grepl("GUATEMALA", DAS), DAS := "GUATEMALA"]
mapping_data[grepl("PETEN", DAS), DAS := "PETEN"]
mapping_data[grepl("IXCAN", DAS), DAS := "QUICHE"]
mapping_data[grepl("QUETZALTENANGO", DAS), DAS := "QUEZALTENANGO"]

mappingData <- merge(coordinates, mapping_data, by.x='id', by.y='DAS', all=TRUE, allow.cartesian=TRUE)
graphData$group = as.character(graphData$group)




graphing_dt = unique(dt[, sum(value, na.rm = TRUE), by=.(Date, Group, Status)])
graphing_dt$Status <- factor(mapping_dt$Status, levels=c("PRE", "ACEP", "POST", "Tested", "CONF.","HIV+" ))

pB = ggplot(unique(graphing_dt[Status == "Tested" | Status == "HIV+"]), aes(y=V1, x=Date, color = Group)) + 
  geom_line()+
  geom_point(size=.1, color='grey45') + 
  labs(title='', y='Count', x='Tested (n = 36,779), HIV+ (n = 1,739)') + 
  facet_wrap(~Status, scales='free_y')+
  theme_bw() 

pdf(outFile, height=5.5, width=7)
pB
dev.off()

