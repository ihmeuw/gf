#----------------------------------------
# Audrey Batzel
# 6/28/2019
# consistency checks and fixes on combined sigsa/sigpro data
#----------------------------------------

#-----------------------
# Install packages 
# ----------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(RColorBrewer)
library(gridExtra)
# ----------------------

#----------------------------------------
# Set up directories 
#----------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')

# input
combined_data = paste0(dir, "prepped/combined_sigsa_sigpro_updated_12_03_19.rds")

# output
outFile = paste0(dir, 'prepped/combined_sigsa_sipro_corrected_for_graphing_updated_12_03_19.rds')
#----------------------------------------

#----------------------------------------
# read in combined data file:
#----------------------------------------
dt = readRDS(combined_data)
#----------------------------------------

#----------------------------------------
# set up for graphing / additional prep
#----------------------------------------
dt[, trans := ifelse(gender == "Trans", TRUE, FALSE) ]
# dt[pop == 'trans', pop := NA]

dt[, hiv_test_comp := as.numeric(hiv_test)]
dt[, hiv_confirmatoryTest_comp := as.numeric(hiv_confirmatoryTest)]
dt[, hiv_positive := ifelse(hiv_testResult == 'reactive', TRUE, FALSE) ]
dt[, hiv_confirmatoryTest_positive := ifelse(hiv_confirmatoryTestResult == '1', TRUE, FALSE) ]
#----------------------------------------

#----------------------------------------
# data checks / consistency checks:
#----------------------------------------
# in updated version of the data, where date is NA is handled earlier in the process:
# # data variable checks:
# dt[ is.na(date), .N ]
# dt[ is.na(date), .N, by = .(set) ]
# dt[ is.na(date) & hiv_test == 1, .N ]
# dt[ is.na(date) & hiv_test == 1 & hiv_positive == TRUE, .N ]

# hiv testing consistency checks:
# where hiv_test is missing:
dt[ is.na(hiv_test), .N ]
dt[ is.na(hiv_test) & !is.na(hiv_positive), .N, by = .(hiv_testResult)]
dt[ is.na(hiv_test) & hiv_positive == TRUE, .N]
  # where hiv test was originally NA and hiv_testResult was nonreactive, set hiv_test_comp to 1/TRUE
  dt[ is.na(hiv_test) & hiv_testResult == "nonreactive", hiv_test_comp := 1]
  dt[ is.na(hiv_test) & hiv_testResult == "test_not_done", hiv_test_comp := 0]

# where hiv_test was not completed
dt[ hiv_test == 0, .N, by = .(hiv_testResult)]
  # where hiv test was originally 0 but hiv_testResult indicated otherwise, set hiv_test_comp to be 1/TRUE
  dt[ hiv_test == 0 & hiv_testResult %in% c('reactive', 'nonreactive', 'indeterminate'), hiv_test_comp := 1 ] 

# hiv confirmatory testing consistency checks:
dt[, unique(hiv_confirmatoryTest), by = .(set)] # only in sigsa data

dt[is.na(hiv_test), .N, by = .(hiv_confirmatoryTest, hiv_confirmatoryTestResult)]
  # Fix inconsistencies:
  dt[is.na(hiv_test) & hiv_confirmatoryTest == 1 & hiv_confirmatoryTestResult == 1, hiv_positive := TRUE]
  dt[is.na(hiv_test) & hiv_confirmatoryTest == 1 & hiv_confirmatoryTestResult == 1, hiv_test_comp := 1]
  
dt[hiv_test == 0, .N, by = .(hiv_confirmatoryTest, hiv_confirmatoryTestResult)]
  # Fix inconsistencies:
  dt[hiv_test == 0 & hiv_confirmatoryTest == 1 & hiv_confirmatoryTestResult == 1, hiv_positive := TRUE]
  dt[hiv_test == 0 & hiv_confirmatoryTest == 1 & hiv_confirmatoryTestResult == 1, hiv_test_comp := 1]

dt[ hiv_confirmatoryTest == 0 , .N, by = .(hiv_confirmatoryTestResult)]
dt[ is.na(hiv_confirmatoryTest), .N, by = .(hiv_confirmatoryTestResult)]
  # Fix inconsistencies:
  dt[ (hiv_confirmatoryTest == 0 | is.na(hiv_confirmatoryTest)) & hiv_confirmatoryTestResult == 1, hiv_positive := TRUE]
  dt[ (hiv_confirmatoryTest == 0 | is.na(hiv_confirmatoryTest)) & hiv_confirmatoryTestResult == 1, hiv_test_comp := 1]

# where confirmatory test result is negative, set hiv positive to FALSE
dt[ hiv_confirmatoryTestResult == 0 & hiv_positive == TRUE, .N]
dt[ hiv_confirmatoryTestResult == 0 & hiv_positive == TRUE, hiv_positive := FALSE]

dt[, c('hiv_test', 'hiv_testResult', 'hiv_confirmatoryTest', 'hiv_confirmatoryTestResult'):=NULL]

# remove missing data for analyses
dt = dt[!is.na(hiv_test_comp)]
dt = dt[ hiv_test_comp != 0 ] # now the data is all of the data for tests completed

dt[ hiv_test_comp == 1, .N ]
dt[ hiv_positive == TRUE, .N]
(dt[ hiv_positive == TRUE, .N] / dt[ hiv_test_comp == 1, .N ]) * 100

# confirmatory test completion:
dt[ hiv_positive == FALSE, .N, by = .(hiv_confirmatoryTest_comp)]
dt[ hiv_positive == TRUE, .N, by = .(hiv_confirmatoryTest_comp)]

# some dates in sigpro incorrectly (typo likely?) listed as 2020 - change to 2018
dt[set == 'sigpro' & date == '2020-10-06', date := as.Date('2018-10-06') ]
dt[set == 'sigpro' & date == '2020-10-23', date := as.Date('2018-10-23') ]
dt[set == 'sigpro' & month_date == '2020-10-01', month_date := as.Date('2018-10-01')]

saveRDS(dt, outFile)
