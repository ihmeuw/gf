# ----------------------------------------------
# Caitlin O'Brien- Carelli
# Format IGSS data sets 

# ----------------------------------------------

#-----------------------------------------------
# Install packages 
# ----------------------------------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(openxlsx)
library(ggplot2)
library(Hmisc)
library(stringr)
#---------------------------------------
# Set up directories 
#----------------------------------------

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')
setwd(paste0(dir, 'sigpro/'))

# to output prepped files
out_dir = paste0(dir, 'prepped/')

#-----------------------------------------

setwd(paste0(dir, 'sigpro/february_transfer_2019/'))

# read in the data 

# list existing files
files = list.files('./', recursive=TRUE)
length(files)
files

# read in the testing and linkage to care data 
vct = data.table(read.csv(paste0(dir, "sigpro/february_transfer_2019/sigpro_f4_JanNov2018 - PB_TVC.csv")))
link = data.table(read.csv(paste0(dir, "sigpro/february_transfer_2019/sigpro_f4_JanNov2018 - Vinculacion.csv")))

vct[ , c('X', 'direccion'):=NULL]
link[ , X:=NULL]


names(vct)


