# Prep the COD DHIS2 PNLS data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/12/2018
#
# Create a subset of the PNLS data for analysis by PATH DRC

# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(xlsx)
library(stringr) # to extract meta data from file names
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#-------------------------------------------
#  subset the pnls data set to only the relevant elements for analysis

# import the pnls data 

# pnls <- readRDS(paste0(dir, 'prepped_data/pnls.rds'))
#  
# # create a list of only the relevant tb_hiv elements
# 
# tb_hiv_points <- c('Hq0aVz1Ki7a', 'C7g1YuoEdic', 't59kPKkqfh3', 'DAHLzUAHbWz',
#      'jhO2beP2wjq', 'qZrNqLxkbAr', 'dk139mVl29G', 'HfEoVMqB0mn', 'EKbCkr59haQ',
#       'YbZCVgZ5aXV', 'c02a6hzCki9', 'FfUE4PcD643', 'I9aio3A8GpO', 'btRcpQRScJq',
#       'VIOWIkL7otc', 'GfWijj9JBMz', 'YEDYWpuL8Os', 'xClRm6CHUSK',
#       'HXYmoLhhXML', 'MUwdwEuzt9L', 'Gv1UQdMw5wL')
# 
# # # create a data table that is a subset of the pnls data 
# tb_hiv <- pnls[element_id %in% tb_hiv_points]
# 
# # save the subset of pnls as a RDS file
# saveRDS(tb_hiv, paste0(dir, 'tb_hiv.rds'))

# import the raw data for cleaning
tb_hiv <- readRDS(paste0(dir, 'tb_hiv.rds'))

# drop unnecessary columns
tb_hiv[ ,data_set_id:=NULL]
#-------------------------------------------

#-------------------------------
# PLHIV enrolled in care
enrolled <- c('I9aio3A8GpO', 'btRcpQRScJq')
screened <- c('Hq0aVz1Ki7a', 'C7g1YuoEdic', 't59kPKkqfh3', 'DAHLzUAHbWz', 'jhO2beP2wjq', 'qZrNqLxkbAr')
detected <- c('dk139mVl29G', 'HfEoVMqB0mn', 'EKbCkr59haQ', 'YbZCVgZ5aXV', 'c02a6hzCki9', 'FfUE4PcD643')
started_tx <- c('VIOWIkL7otc', 'GfWijj9JBMz','YEDYWpuL8Os','xClRm6CHUSK', 'HXYmoLhhXML','MUwdwEuzt9L')

tb_hiv[element_id %in% enrolled,  indicator:='enrolled']
tb_hiv[element_id %in% screened,  indicator:='screened']
tb_hiv[element_id %in% detected,  indicator:='detected']
tb_hiv[element_id %in% started_tx,  indicator:='started_tx']
tb_hiv[element_id=='Gv1UQdMw5wL',  indicator:='test_kit']


# -----------------------
# rearrange the variables into an intuitive order

tb_hiv <- tb_hiv[ ,.(element, org_unit, category, dps, mtk, date, value, facility_level=level,
                     indicator, element_eng, data_set, element_id, org_unit_id, coordinates)]

#------------------------------------
# fix english translations

# 1 - HIV+ persons enrolled in HIV care
tb_hiv[element_id=='btRcpQRScJq', element_eng:='PLHIV enrolled in HIV care, by age']
tb_hiv[element_id=='I9aio3A8GpO', element_eng:='PLHIV enrolled in HIV care, by age and sex']

# 2 - PLHIV enrolled in care screened for signs and symptoms of TB
tb_hiv[element_id=='C7g1YuoEdic', element_eng:='PLHIV screened for TB, by age']
tb_hiv[element_id=='DAHLzUAHbWz', element_eng:='PLHIV screened for TB, Other']
tb_hiv[element_id=='Hq0aVz1Ki7a', element_eng:='PLHIV screened for TB, by age and sex']
tb_hiv[element_id=='jhO2beP2wjq', element_eng:='PLHIV screened for TB - Lactating women']
tb_hiv[element_id=='qZrNqLxkbAr', element_eng:='PLHIV screened for TB - Male partners']
tb_hiv[element_id=='t59kPKkqfh3', element_eng:='PLHIV screened for TB - Pregnant women']

# 3 - TB cases detected among PLHIV enrolled in care
tb_hiv[element_id=='c02a6hzCki9', element_eng:='TB cases detected among PLHIV - Pregnant women']
tb_hiv[element_id=='dk139mVl29G', element_eng:='TB cases detected among PLHIV, by age and sex']
tb_hiv[element_id=='EKbCkr59haQ', element_eng:='TB cases detected among PLHIV - Other']
tb_hiv[element_id=='FfUE4PcD643', element_eng:='TB cases detected among PLHIV - Male partners']
tb_hiv[element_id=='HfEoVMqB0mn', element_eng:='TB cases detected among PLHIV, by age']
tb_hiv[element_id=='YbZCVgZ5aXV', element_eng:='TB cases detected among PLHIV - Lactating women']

tb_hiv[indicator=='started_tx', .(unique(element_eng), unique(element_id))]

# 4 - PLHIV who started TB treatment 
tb_hiv[element_id=='GfWijj9JBMz', element_eng:='PLHIV started TB treatment, by age']
tb_hiv[element_id=='HXYmoLhhXML', element_eng:='PLHIV started TB treatment - Pregnant women']
tb_hiv[element_id=='MUwdwEuzt9L', element_eng:='PLHIV started TB treatment - Male partners']
tb_hiv[element_id=='VIOWIkL7otc', element_eng:='PLHIV started TB treatment, by age and sex']
tb_hiv[element_id=='xClRm6CHUSK', element_eng:='PLHIV started TB treatment - Lactating women']
tb_hiv[element_id=='YEDYWpuL8Os', element_eng:='PLHIV started TB treatment - Other']

# 5 - Test kits
tb_hiv[indicator=='test_kit', .(unique(element_eng), unique(element))]

#----------------------------------------------
# save the data set

# save the data set as a RDS
saveRDS(tb_hiv, paste0(dir, 'path_drc/tb_hiv.rds'))

# save the data set as a CSV
# the CSV is typically too long to download accurately
# write.csv(tb_hiv, paste0(dir, 'tb_hiv.csv'))

#-----------------------------------------
