# ----------------------------------------------
# Audrey Batzel
#
# 5/16/19 
# final prep for MI (used to be in prep_for_MI.R)
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
rm(list=ls())
library(data.table)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ---------------------------------------------- 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/")
out_dir = paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLP_analysis/" )

# input files
inFile = "PNLP_dt_forMI_updated_6_10_19.rds"
outFile = "percent_missing_over_time_6_24_19.pdf"
# ----------------------------------------------   

# ----------------------------------------------
# Read in the data
# ---------------------------------------------- 
dt = readRDS( paste0(dir, inFile) ) 
# ----------------------------------------------  

# ----------------------------------------------
# percent missing overtime cases
# ---------------------------------------------- 
id_vars = c('id', 'dps', 'health_zone', 'date', 'donor', 'operational_support_partner', 'population', 'year')
dt_long = melt.data.table(dt, id.vars = id_vars, variable.factor = FALSE)

cases = dt_long[grepl(variable, pattern = "newCasesMalaria")]
cases[, num_missing := sum(is.na(value)), by = "date"]
cases[, total_values := .N, by = "date"]

missing_cases = unique(cases[, .(percent_missing = num_missing/total_values), by = "date"])
missing_cases[, percent_missing := percent_missing * 100]

g1 = ggplot(missing_cases, aes(x = date, y = percent_missing)) + geom_point() + geom_line() + 
  ggtitle("Percent of data missing for cases of malaria in DRC PNLP data by date") + xlab("Date (month and year)") + ylab("Percent missing") +
  theme_bw() + ylim(0, 65)  + labs(caption = "Source: Programme National de Lutte contre le Paludisme (PNLP)") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), legend.text =element_text(size=14),
        plot.title = element_text(size=18), plot.caption = element_text(size=14)) 


g1
# ----------------------------------------------

# ----------------------------------------------
# percent missing over time total
# ---------------------------------------------- 
# remove any back-casted variables
all_na = dt_long[ , .(all_missing = all(is.na(value))), by = c("year", "variable")]
all_na = merge(dt_long, all_na, all = TRUE, by = c("year", "variable"))

dt = all_na[ all_missing == FALSE, ]

dt[, num_missing := sum(is.na(value)), by = "date"]
dt[, total_values := .N, by = "date"]

missing_all = unique(dt[, .(percent_missing = num_missing/total_values), by = "date"])
missing_all[, percent_missing := percent_missing * 100]

g2 = ggplot(missing_all, aes(x = date, y = percent_missing)) + geom_point() + geom_line() + 
  ggtitle("Percent of all data missing in DRC PNLP data by date") + xlab("Date (month and year)") + ylab("Percent missing") +
  theme_bw() + ylim(0, 65) + labs(caption = "Source: Programme National de Lutte contre le Paludisme (PNLP)") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), legend.text =element_text(size=14),
        plot.title = element_text(size=18), plot.caption = element_text(size=14)) 

g2
# ----------------------------------------------   

# ----------------------------------------------
# percent missing total
# ---------------------------------------------- 
(sum(is.na(dt$value)) / nrow(dt)) * 100
# ----------------------------------------------  

# ----------------------------------------------
# percent missing over time total for variables used in malaria model
# ---------------------------------------------- 
# inds = c(inds, "LLIN", "ASAQreceived", "SP", "AL", "SSCACT", "SSCRDT", "simpleConfMalariaTreated", "severeMalariaTreated", "presumedMalariaTreated", "RDT", "ANC")
# inds = c('simpleConfMalariaTreated','newCasesMalariaSimpleConf', 'newCasesMalariaSevere',
#          'ANC','SSCfevers','suspectedMalaria', 'malariaDeaths', 'totalDeathsAllDiseases', 'SP')
# 
# inds = unique(dt_long$variable)
# inds_to_keep = inds[grepl(inds, pattern = "ASAQ")]
# inds_to_keep = inds[grepl(inds, pattern = "ASAQ")]
# ---------------------------------------------- 

pdf(paste0(out_dir, outFile), height = 9, width = 12)
print(g1)
print(g2)
dev.off()


