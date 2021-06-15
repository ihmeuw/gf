#########################################################
# Audrey Batzel
# 5/24/21
# sum IHME/PATH 2S data to country level and combine with EHG data

# clear
rm(list=ls())

# Set up
library(ggplot2)
library(data.table)
library(readxl)
library(scales)
library(grid)
library(lattice)
library(RColorBrewer)
library(stringr)

# -------------------------------------------------------------------
# Files and directories
setwd('C:/local/gf/')

user=as.character(Sys.info()[7])

# input file
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/")
inFile = paste0(box, '2s_data/CORRECTED_prepped_2s_data_all_countries.csv')
inFile2 = paste0(box, '2s_data/ehg_2s_data_extension.xlsx')
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# read in and sum to appropriate levels
# -------------------------------------------------------------------
ihme_2s = as.data.table(read.csv(inFile))
ehg_2s = as.data.table(read_xlsx(inFile2))

ihme_2s = ihme_2s[version == 'approved_budget', .(budget = sum(budget, na.rm = TRUE)), by = .(loc_name, cycle, coding_2s)]
ihme_2s[ loc_name == 'UGA', `Country`:= 'Uganda']
ihme_2s[ loc_name == 'DRC', `Country`:= 'DRC']
ihme_2s[ loc_name == 'GTM', `Country`:= 'Guatemala']
ihme_2s[ loc_name == 'SEN', `Country`:= 'Senegal']

ehg_2s[ `Grant period` == '2018-20', `Grant cycle`:= 'NFM2']
ehg_2s[ `Grant period` == '2021-23', `Grant cycle`:= 'NFM3']

ihme_2s[ coding_2s == 'Supporting', `2S` := 'Support']
ihme_2s[ coding_2s == 'Strengthening', `2S` := 'Strengthening']
setnames(ihme_2s, 'cycle', 'Grant cycle')
setnames(ihme_2s, 'budget', 'Budget')

all_data = rbindlist(list(ihme_2s, ehg_2s), use.names = TRUE, fill = TRUE)
all_data = all_data[, c('Country', 'Grant cycle', '2S', 'Budget')]
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# save data
# -------------------------------------------------------------------
write.csv(all_data, paste0(box, '2s_data/combined_ihme_ehg_2sData_forExtension.csv'), row.names = FALSE)
write.csv(all_data, 'C:/Users/abatzel/Box Sync/2S analysis/1_source data/combined_ihme_ehg_2sData_forExtension.csv', row.names = FALSE)
# -------------------------------------------------------------------

