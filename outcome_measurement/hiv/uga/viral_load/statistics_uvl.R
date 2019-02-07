# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/25/2018
# Descriptive statistics for the Uganda Viral Load Dashboard
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)

# --------------------
# detect if on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# ----------------------------------------------
# Files and directories

# set input/output directory
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')

# upload the data with month, year, sex
dt = readRDS(paste0(dir, "/prepped_data/sex_data.rds"))

# when working locally
# dt = readRDS("C:/Users/ccarelli/Documents/sex_data.rds")

# add a year variable
dt[ , year:=year(date)]

#------------------------------------
# merge in the regions
# missing districts will be missing regions

regions = fread(paste0(root, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
regions = regions[ ,.(region = region10_name, district = dist112_name)]
regions = regions[!duplicated(district)]
dt = merge(dt, regions, by='district', all.x=T)

#-----------------------------------------------
# 2018 table

# subset to 2018
tab = dt[year(date)==2018]

# facilities reporting
tab[ ,.(length(unique(facility))), by=region][order(region)]
tab[ ,.(length(unique(facility)))]

#-----------------------
# viral load tests performed
tab[sex=='Male',.(sum(samples_tested)), by=region][order(region)]
tab[sex=='Male',.(sum(samples_tested))]

tab[sex=='Female',.(sum(samples_tested)), by=region][order(region)]
tab[sex=='Female',.(sum(samples_tested))]

tab[,.(sum(samples_tested)), by=region][order(region)]
tab[,.(sum(samples_tested))]

tab[ ,.(sum(samples_tested)), by=region][order(region)]
#-----------------------
# suppression ratios
sup = dt[year==2018, .(valid_results=sum(valid_results), suppressed=sum(suppressed)), 
         by=.(region, sex)]
sup[ , ratio:=round(100*(suppressed/valid_results), 1)]

sup[sex=='Female', ratio, by=region][order(region)]
sup[sex=='Male', ratio, by=region][order(region)]

sup2 = dt[year==2018, .(valid_results=sum(valid_results), suppressed=sum(suppressed)), by=region]
sup2[ , ratio:=round(100*(suppressed/valid_results), 1)]
sup2[ , ratio, by=region][order(region)]

dt[year==2018 & sex=='Male', .(100*sum(suppressed)/sum(valid_results))]
dt[year==2018 & sex=='Female', .(100*sum(suppressed)/sum(valid_results))]
dt[year==2018 , .(100*sum(suppressed)/sum(valid_results))]

#-------------------------------
# TEXT

#------------------------------
# intro paragraph

# tests performed
dt[ ,length(unique(facility)), by=year][order(year)]
dt[ ,sum(samples_tested), by=year][order(year)]

# viral suppression
total = dt[ ,.(valid_results=sum(valid_results), suppressed=sum(suppressed)), by=.(year, sex)]
total[ ,ratio:=100*(suppressed/valid_results)]
total[ ,ratio, by=.(year, sex)][order(year)]


dist = dt[year==2018,.(valid_results=sum(valid_results), suppressed=sum(suppressed)), by=district]
dist[ ,ratio:=100*(suppressed/valid_results)]
dist[ ,min(ratio)]
dist[ ,max(ratio)]


#---------------------
# tests performed

dt[year==2018, sum(samples_tested), by=sex]
dt[year==2017 & month(date)!=12, sum(samples_tested), by=sex]

x = dt[year==2018, sum(samples_tested)]
y = dt[year==2017 & month(date)!=12, sum(samples_tested)]
x - y

range = dt[year==2018, sum(samples_tested), by=district]
range[order(V1)]
#------------------------
# mean tests per month

motests = dt[ ,.(patients_received=sum(patients_received)), by=.(date, year, sex)]
motests[year==2018, mean(patients_received), by=sex]

motests2 = dt[year==2018 & sex=='Female',.(fpts=sum(patients_received)), by=.(date, district)]
motests3 = dt[year==2018 ,.(pts=sum(patients_received)), by=.(date, district)]
total = merge(motests2, motests3, by=c('date', 'district'))
total[ ,ratio:=(100*fpts/pts)]

total_new = total[ ,mean(ratio), by=district]
total_new[ ,range(V1)]


#---------------------
# viral suppression

dt[year==2014, .(sum(suppressed)/sum(valid_results))]



