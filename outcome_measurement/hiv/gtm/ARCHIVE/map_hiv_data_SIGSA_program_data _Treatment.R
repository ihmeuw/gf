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
dt = readRDS(paste0(prep_dir, "hiv_sigsa_data_prepped_PD_treatment.rds"))

# Translated varaibles
trans_dt = read.csv(paste0(prep_dir, "treatment_translation_variables.csv"), fileEncoding = "latin1") 
trans_dt$Treatment = fix_diacritics(trans_dt$Treatment)
trans_dt$Treatment = toupper(trans_dt$Treatment)
trans_dt = as.data.table(trans_dt)

dt$Treatment = fix_diacritics(dt$Treatment)
dt$Treatment = toupper(dt$Treatment)
dt$Group = toupper(dt$Group)
dt$Group = fix_diacritics(dt$Group)

#Create sex groups
dt$sex = dt$Group
dt[grepl("MU", Group), sex := "Female"]
dt[grepl("HOM", Group), sex := "Male"]
dt[grepl("NINAS", Group), sex := "Female"]
dt[grepl("NINOS", Group), sex := "Male"]
dt[grepl("MTS", Group), sex := "Female"]
dt[grepl("HSH", Group), sex := "Male"]
dt[grepl("TRAN", Group), sex := "Transgender"]
dt[grepl("EMBA", Group), sex := "Female"]
dt[grepl("EMBA", Treatment), sex := "Female"]
dt[grepl("DE", sex), sex := "Not_Specified"]

dt$KVP = "Not Specified"
dt[grepl("MTS", Group), KVP := "Female Sex Workers"]
dt[grepl("HSH", Group), KVP := "Men who have sex with Men"]
dt[grepl("TRAN", Group), KVP := "Transgender"]
dt[grepl("EMBA", Group), KVP := "Pregnant Women"]
dt[grepl("NIN", Group), KVP := "Children - other"]
dt$KVP = ifelse(dt$KVP == "Not Specified" & grepl("EMBA", dt$Group), "Pregnant Women", 
                ifelse(dt$KVP == "Not Specified" & dt$sex == "Male", "Male - other", 
                       ifelse(dt$KVP == "Not Specified" & dt$sex == "Female", "Female - other",
                              ifelse(dt$Group == "EMBARAZADAS MTS", "Female Sex Workers",dt$KVP))))


dt$AGE = dt$Group
dt[grepl("0 A 6", Group), AGE := "0-6 M"]
dt[grepl("7 A 12", Group), AGE := "6-12 M"]
dt[grepl("< DE UN ANO", Group), AGE := "0-1 Y"]
dt[grepl("1 A 2", Group), AGE := "1-2 Y"]
dt[grepl("3 A 4", Group), AGE := "3-4 Y"]
dt[grepl("5 A 9", Group), AGE := "5-9 Y"]
dt[grepl("10 A 14", Group), AGE := "10-4 Y"]
dt[grepl("15 A 18", Group), AGE := "15-18 Y"]
dt[grepl("15 A 24", Group), AGE := "15-24 Y"]
dt[grepl("19 A 24", Group), AGE := "19-24 Y"]
dt[grepl("25 A 49", Group), AGE := "25-49Y"]
dt[grepl("50", Group), AGE := "50+ Y"]
dt[grepl("NIN", AGE), AGE := "< 15 Y"]


dt_total = merge(dt, trans_dt, by = "Treatment")

#testing = unique(dt_total[,.(Facility, year(Date))])



dt_mapping = dt_total[Keep == 1,.(Treatment_Translation, Date, KVP, value)]
dt_mapping[, value := sum(value),.(Treatment_Translation, Date, KVP)]
dt_mapping = as.data.table(unique(na.omit(dt_mapping)))


list_of_plots = NULL
i=1

for(s in unique(dt_mapping$Treatment_Translation)) {
  # look up district name
  name <- unique(dt_mapping[Treatment_Translation==s]$Treatment_Translation)
  
  # make your graph
  
  list_of_plots[[i]] <-  ggplot(dt_mapping[Treatment_Translation == s], aes(y=as.integer(value), x=Date, color = KVP)) +
    geom_point() +
    geom_line(alpha=0.5) +
    labs(caption= paste0("N = ", sum(as.integer(dt_mapping[Treatment_Translation == s]$value), na.rm = TRUE))) +
    theme_bw() + labs(title=name, x='Date', y='Count', color='')
  
  
  i=i+1
  
}

pdf("/home/j/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA_hiv_Treatment_vars_2.pdf", height=6, width=9)

for(i in seq(length(list_of_plots))) {
  print(list_of_plots[[i]])
}
dev.off()
