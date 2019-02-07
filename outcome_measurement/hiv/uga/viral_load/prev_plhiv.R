# Calculate PLHIV and prevalence
# Caitlin O'Brien-Carelli
# Prep UVL data for analysis
# 12/31/2018
#
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(stringr) 
library(plyr)
library(data.table)
# --------------------

# --------------------
# detect if operating on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working directory
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/lbd_prev')
setwd(dir)

#--------------------------------------------------------
# import the prevalence estimates
dt = fread(paste0(dir, '/uganda_adm2_prev.csv'), stringsAsFactors = FALSE)

# fix the names
dt[ADM1_NAME=='Luwero', ADM1_NAME:='Luweero']
dt[ADM1_NAME=='Sembabule', ADM1_NAME:='SSembabule']

# read in alternate geographies
geo = fread(paste0(root, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
geo = geo[ ,.(region10_name, dist53_name, dist56_name, dist70_name, dist80_name, dist112_name)]

#--------------------------------------------------------
# match as many districts as possible to create regions
# start with admin 1 - 56 districts

adm1 = unique(dt$ADM1_NAME)
test = geo$dist70_name
test2 = geo$dist56_name

# 51 of 56 are in the data - 5 missing 
adm1[!adm1 %in% test]
adm1[!adm1 %in% test2]

#----------------------------------------
# create a list of regions
regions = geo[ ,.(ADM1_NAME=dist70_name), by=region10_name]
regions = regions[!duplicated(ADM1_NAME)]

# merge in the regions
dt = merge(dt, regions, by='ADM1_NAME', all.x=T )

# add regions for the districts that failed to merge
dt[ADM1_NAME=='Kibale', region10_name:='Western']
dt[ADM1_NAME=='Lake Albert', region10_name:='Western']
dt[ADM1_NAME=='SSembabule', region10_name:='Central_1']
dt[ADM1_NAME=='Lake Victoria', region10_name:='Lake Victoria']

#----------------------------------------
# calculate PLHIV

peep = dt[ ,.(plhiv=sum(Mean_PLHIV)), by=.(region=region10_name, year)]
peep = peep[year==2016 | year==2017]

# export one csv per year and copy into the table
x = peep[year==2016][order(region)]
write.csv(x, paste0(dir, '/plhiv.csv'))

# total plhiv all regions
peep[ ,sum(plhiv), by=year]

#------------------------------------------
# 2017 prevalence by district

prev = dt[year==2016 | year==2017 ,.(pop=sum(pop), plhiv=sum(Mean_PLHIV)), 
          by=.(region=region10_name, year)]

prev[ , prevalence:=round(100*(plhiv/pop), 1)]

prev[year==2016][order(region)]
prev[year==2017][order(region)]

# total hiv prevalence
prev2 = dt[ ,.(pop=sum(pop), plhiv=sum(Mean_PLHIV)),  by=year]
prev2[ , prevalence:=round(100*(plhiv/pop), 1)]
prev2[year==2016 | year==2017]

prev2[order(year)]


# district level
pr = dt[year==2017 ,.(pop=sum(pop), plhiv=sum(Mean_PLHIV)), 
          by=.(district=ADM2_NAME, region=region10_name , year)]

pr[ , prevalence:=round(100*(plhiv/pop), 1)]
pr[order(prevalence, decreasing=F)]


reg = dt[ ,.(pop=sum(pop), plhiv=sum(Mean_PLHIV)),  by=.(year, region=region10_name)]
reg[ , prevalence:=round(100*(plhiv/pop), 1)]
reg = reg[year==2000 | year==2017]

# regional rates of change
reg = dcast(reg, region~year, value.var='prevalence')
setnames(reg, c('region', 'y2000', 'y2017'))
reg[ ,roc:=y2000 - y2017]
reg[order(roc, decreasing=T)]


