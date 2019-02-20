# ----------------------------------------------
# Naomi Provost
# October 12, 2018
# Master code file for GTM HIV data graphing
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
library(plyr)

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


# ----------------------------------------------
#----------------------------------
# Set the directory to download the data
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

#------------ Prep Data------------------------
#### Prep Data
# define main directory
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/SIGSA/')
prep_dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro/prepped_data/')
mapping_dir = paste0(root, '/Project/Evaluation/GF/mapping/gtm/')

# Read in file (this is prepped with SIGSA and SIGPRO level )
dt = readRDS(paste0(prep_dir, "hiv_patientlvl_combined.rds"))

# remove repeat positive tests for patients, only keep the first postivie test and all negative tests before it
total_dt = dt

# change name for risk condition to merge SIGSA and SIGPRO
total_dt$risk_condition_eng = ifelse(total_dt$risk_condition_eng == 'TRANS TRABAJADORAS SEXUALES', "Sex Worker", total_dt$risk_condition_eng)

#Make binaries to make calculations easier
total_dt$isPosTest = ifelse(total_dt$hiv_screening_result == "REACTIVO", 1, 0) #Check to make sure these are capturing everything in new data! 
total_dt$completedTest = ifelse(total_dt$completed_hiv_screening_test == "SI", 1, 0)

#EKL - at this point we only have through Feb. 2018. 
# change all dates to the first of the month to merge dates
total_dt$date = as.Date(paste0(year(total_dt$date), '-', month(total_dt$date), "-01"))

# add Trans as a sex identity because it's already listed that way in SIGPRO, and we can group them together
total_dt$sex = ifelse(total_dt$isTrans == 1, "Trans", total_dt$sex)
total_dt$sex = ifelse(total_dt$sex == "T", "Trans",
                    ifelse(total_dt$sex == "M", "Male",
                           ifelse(total_dt$sex == "F", "Female", total_dt$sex)))

# create a binary for sex worker to for graphing
total_dt$isSexWorker = ifelse(total_dt$risk_condition_eng == "Sex Worker", 1, 0)

# calculate postivie test
total_dt[, completedTest := sum(completedTest, na.rm = TRUE), by=.(date, risk_condition_eng,  sex, isMSM, isSexWorker, hospital_department)]
total_dt[, isPosTest := sum(isPosTest, na.rm = TRUE), by=.(date, risk_condition_eng, sex, isMSM, isSexWorker, hospital_department)]


#creating new variable in case I need change something for testing
dt_map = total_dt
dt_map = dt_map[,.(date, completedTest, isPosTest, risk_condition_eng, sex, isMSM, isSexWorker)]
dt_map = unique(dt_map)

#Drop out one case where year is 2020- need to move this to a data cleaning file! 
dt_map = dt_map[year(date)<2019]
  
# melt to make long
mapping_long = melt(dt_map, id.vars = c('date', 'risk_condition_eng', "sex", "isMSM", "isSexWorker"))
#mapping_long [is.na(isMSM), isMSM := 3]
mapping_long$isMSM = ifelse(mapping_long$isMSM == 1, "Men who have sex with Men", ifelse(mapping_long$isMSM == 0, "Not Men who have Sex with Men", mapping_long$isMSM))
mapping_long = mapping_long[year(date) != 2014] 

find_pos_ratio = function(dt_total, var_name){
  #function finds positive ratios
  
  #remove 2014 because test ratio is 100% since so few tests are done
  dt_total = dt_total[year(date) != 2014]
  
  complete_test_vals = dt_total[variable == "completedTest"]
  complete_test_vals$variable = NULL
  complete_test_vals = rename(complete_test_vals, c("V1" = "completedTest")) 
  
  pos_test_vals = dt_total[variable == "isPosTest"]
  pos_test_vals$variable = NULL
  pos_test_vals = rename(pos_test_vals, c("V1" = "isPosTest")) 
  
  dt_merged = merge(complete_test_vals, pos_test_vals, by = var_name)
  return(dt_merged[,posRatio := isPosTest / completedTest * 100, by = var_name])
}



# TOTAL POPULATION
dt_total_pop = mapping_long[, sum(value, na.rm = TRUE), by=.(date, variable)]
pa0 = ggplot(dt_total_pop[variable == "completedTest"], aes(y=V1, x=date, color = variable)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  labs(title='Number of HIV Tests done over Time', 
       y='Count', x='') + 
  theme_bw()  

#pos Test Ratio
ratio_total_pop = find_pos_ratio(dt_total_pop, "date")
pb0 = ggplot(ratio_total_pop, aes(y=posRatio, x=date)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  labs(title='Percent (%) of Patients who tested HIV+', 
       y='Percent (%)', x='') + 
  theme_bw() 


# By Sex
dt_by_sex = unique(mapping_long[, sum(value, na.rm = TRUE), by=.(date, variable, sex)])
pa1 = ggplot(dt_by_sex[variable == "completedTest"], aes(y=V1, x=date)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  facet_wrap(~sex,  scales='free_y') +
  labs(title='Number of HIV Tests done over Time by Gender Identity', 
       y='Count', x='',  color='Gender Identity') + 
  theme_bw()  

ratio_by_sex = find_pos_ratio(dt_by_sex, c("date", "sex"))
pb1 = ggplot(ratio_by_sex, aes(y=posRatio, x=date)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  facet_wrap(~sex) +
  labs(title='Percent (%) of Patients who tested HIV+ by Gender Identity', 
       y='Percent (%)', x='') + 
  theme_bw() 

# By MSM to compare just Men
dt_by_MSM_male = na.omit(mapping_long[, sum(value, na.rm = TRUE), by=.(date, variable, isMSM, sex)])
  pa3 = ggplot(dt_by_MSM_male[variable == "completedTest"], aes(y=V1, x=date, color = sex)) + 
    geom_line() +
    geom_point(size=1, color='grey45') + 
    facet_wrap(~isMSM,  scales='free_y') +
    labs(title='Number of HIV Tests done over Time by MSM status', 
       y='Count', x='',  color='Gender Identity') + 
    theme_bw()  

ratio_by_MSM_sex = find_pos_ratio(dt_by_MSM_male, c("date", "isMSM", "sex"))
  pb3 = ggplot(ratio_by_MSM_sex, aes(y=posRatio, x=date, color = sex)) + 
  geom_line() +
  geom_point(size=1, color='grey45') + 
  facet_wrap(~isMSM) +
  labs(title='Percent (%) of people who tested HIV+ by MSM status', 
       y='Percent (%)', x='',  color='Gender Identity') + 
  theme_bw() 
  
  
#by SexWorker
  mapping_long$variable = as.character(mapping_long$variable)
  dt_by_sexW = mapping_long[ isSexWorker == 1, sum(value, na.rm = TRUE), by=.(date, variable, sex)]
  
  pa4 = ggplot(dt_by_sexW[variable == "completedTest"], aes(y=V1, x=date, color = sex)) + 
    geom_line() +
    geom_point(size=1, color='grey45') + 
  labs(title='Number of HIV Tests done over Time by Commercial Sex Workers', 
       y='Count', x='',  color='Gender Identity') + 
    theme_bw() 
  
  ratio_by_sex_worker = find_pos_ratio(dt_by_sexW, c("date", "sex"))
  pb4 = ggplot(ratio_by_sex_worker, aes(y=posRatio, x=date, color = sex)) + 
    geom_line() +
    geom_point(size=1, color='grey45') + 
    labs(title='Percent (%) of Commercial Sex Workers who tested HIV+ by Gender Identity', 
         y='Percent (%)', x='',  color='Gender Identity') + 
    theme_bw() 
  
  
  outFile = paste0(root, '/Project/Evaluation/GF/outcome_measurement/gtm/HIV/sigsa_sigpro/visualizations/SIGSA_SIGRPO_testing_comp_new.pdf')
  
  pdf(outFile, height=5.5, width=7)
  pa0
  pb0
  pa1
  pb1
  #pa2
  #pb2
  pa3
  pb3
  pa4
  pb4
  dev.off()


# municipality data is saved here: J:\Project\Evaluation\GF\mapping\gtm\GUATEMALA-municip.shp
  
  # Let's create maps
  shapeData = shapefile(paste0(mapping_dir,'GTM_adm1.shp'))
  coordinates = as.data.table(fortify(shapeData, region='NAME_1'))
  coordinates$id <- toupper(coordinates$id)
  
  
  #ALL YEAR MAPS Maps
  p21 <- (ggplot() + geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=test_by_department)) + 
            coord_equal() + ##so the two shapefiles have the same proportions 
            geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
            facet_wrap(~year) +
            scale_fill_gradientn(colors=results_colors) +
            theme_void()+
            theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +
    labs(title= "Annual Completed HIV test by Department", fill=paste0('Number of tests done')) 



# EXTRA FIGURES --> Don't need them
# 
# 
# 
# list_of_plots = NULL
# i=1
# 
# dt_risk_cond = mapping_long[, sum(value, na.rm = TRUE), by=.(date, variable, risk_condition_eng, sex)]
# 
# for(s in unique(dt_risk_cond$risk_condition_eng)) {
#   # look up district name
#   name <- unique(mapping_long[risk_condition_eng==s]$risk_condition_eng)
#   
#   # make your graph
#   
#   list_of_plots[[i]] <-  ggplot(dt_risk_cond[risk_condition_eng== s], aes(y=as.integer(V1), x=date, color = sex)) +
#     geom_point() +
#     geom_line(alpha=0.5) +
#     facet_wrap(~variable, scales='free_y') +
#     theme_bw() + labs(title=name, x='Date', y='Count', color='Sex')
#   
#   
#   i=i+1
#   
# }
# 
# pdf("/home/j/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA_SIGPRO_combined_test_Complete.pdf", height=6, width=9)
# 
# for(i in seq(length(list_of_plots))) {
#   print(list_of_plots[[i]])
# }
# dev.off()
# 
# 
# list_of_plots = NULL
# i=1
# 
# dt_risk_cond = mapping_long[, sum(value, na.rm = TRUE), by=.(date, variable, risk_condition_eng, sex)]
# 
# for(s in unique(dt_risk_cond$risk_condition_eng)) {
#   # look up district name
#   name <- unique(mapping_long[risk_condition_eng==s]$risk_condition_eng)
#   
#   # make your graph
#   
#   list_of_plots[[i]] <-  ggplot(dt_risk_cond[(variable == "completed_test") & risk_condition_eng== s], aes(y=as.integer(V1), x=date, color = variable)) +
#     geom_point() +
#     geom_line(alpha=0.5) +
#     facet_wrap(~sex, scales='free_y') +
#     theme_bw() + labs(title=name, x='Date', y='Count', color='Risk_Condition')
#   
#   
#   i=i+1
#   
# }
# 
# pdf("/home/j/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/SIGSA_SIGPRO_combined.pdf", height=6, width=9)
# 
# for(i in seq(length(list_of_plots))) {
#   print(list_of_plots[[i]])
# }
# dev.off()


