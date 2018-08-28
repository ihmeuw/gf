# Extract data from figures
# Reverse engineer graphs to get the underlying data
# Used for 'confirmed cases' map in the September TERg slides

# 8/24/2018
#-----------------------

rm(list=ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plyr)
library(devtools)
library(digitize)
library(xlsx)

# --------------------
# set the working directory 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/extract_points')

#--------------------------------------
# upload the graph and digitize 

n <- '010'
dt <- digitize(paste0(dir, '/opd_confirmed_district_malaria cases-', n , ".jpg"))

# enter 0, 33, and then y axis estimates
# then click all of the data points from january 2017 on 
#-----------------------------------------------
# create a data table and save it 

dt <- data.table(dt)
dt[ , x:= round(x, digits=0)]
dt[ , y:=round(y, digits=0)]
dt$x[15] <- 32
dt
saveRDS(dt, paste0(dir, '/data_tables/district_data_', n, '.rds'))


#-----------------------------------------------
# upload each file and assign it a number

setwd(paste0(dir, '/data_tables/'))

# list existing files
files <- list.files('./', recursive=TRUE)
length(files)

# loop over existing files
i = 1
for(f in files) {
  
  #Load the RDs file
  dt = readRDS(f)
  
 if (length(dt$x) > 15) {
   print("Oh no! You clicked too many points!")
   print(f)
 }
  
  dt <- data.table(dt)
  # for early data tables, round y
  dt[ , y:=round(y, digits=0)]
  
  # extract meta data from file name
  meta_data <- strsplit(f, '_')[[1]]
  meta_data <- meta_data[[3]]
  meta_data <- strsplit(meta_data, '\\.')[[1]]
  meta_data <- meta_data[[1]]
  
  # assign the number to the district
  dt[ ,id:=meta_data]
  
  # reset the column names
  setnames(dt, c('x', 'y'), c('date', 'count'))
  
  # reset the years
  dt[date==30 | date==31 | date==32, year:='2018']
  dt[!(date==30 | date==31 | date==32), year:='2017'] 
  
  # reset the months
  dt[date==18 | date==30, month:='01']
  dt[date==19 | date==31, month:='02']
  dt[date==20 | date==32, month:='03']
  dt[date==21, month:='04']
  dt[date==22, month:='05']
  dt[date==23, month:='06']
  dt[date==24, month:='07']
  dt[date==25, month:='08']
  dt[date==26, month:='09']
  dt[date==27, month:='10']
  dt[date==28, month:='11']
  dt[date==29, month:='12']

  # create a date variable
  dt[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

  # append to the full data 
  if(i==1) full_data = dt
  if(i>1) full_data = rbind(full_data, dt)
  i = i+1
  
  
}

# view the final product of full_data
str(full_data)

# ----------------------------------------------
# merge in the names of the districts 

districts <- read.xlsx(paste0(dir, '/districts.xlsx'), 1)
cases <- merge(full_data, districts, by='id', all.x=TRUE)

#-------------------
# save the data table 

saveRDS(cases, paste0(dir, '/confirmed_malaria_cases.rds'))

#--------------------



