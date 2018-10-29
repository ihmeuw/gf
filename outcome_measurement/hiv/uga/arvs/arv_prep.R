# ARV stockouts by facility - data prep
# Caitlin O'Brien-Carelli
# 10/29/2018

# ----------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)


# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

#dir = "C:/Users/ccarelli/Downloads/"

setwd(dir)
# ----------------------
# read in the files 

i = 1
files = list.files('./', recursive=TRUE)

for (f in files) {
  arv_data = read.csv(paste0(dir, f))
  arv_data = data.table(arv_data)
  
  # create useful variable names
  setnames(arv_data, c( 'Health.facility', 'District', 'IP', 'Stock.outs', 'X'), 
           c('facility', 'district', 'ip', 'test_kits', 'arvs'))
  
  # delete the first row of labels
  arv_data = arv_data[-1]
  
  # create a week variable
  week = strsplit(f, '\\s')[[1]][2]
  if (substr(week, 3, 3)!=')') week = substr(week, 2, 3)
  if (substr(week, 3, 3)==')') week = substr(week, 2, 2)
  arv_data[ , week:=week]
  
  # add a year variable
  arv_data[ , year:=strsplit(f, '/')[[1]][1]]
  
  if(i==1) full_data = arv_data
  if(i>1) full_data = rbind(full_data, arv_data)
  i = i+1
  
}

