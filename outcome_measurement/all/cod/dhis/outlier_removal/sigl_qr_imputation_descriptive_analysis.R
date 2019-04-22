setwd('C:/local/gf/')
# Audrey Batzel

# ----------------------------------------------

# --------------------
# Set up R / install packages
## -------------------
rm(list=ls())
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
# --------------------

# ----------------------------------------------
# files and directories
# ----------------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory and input/output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

inFile = 'prepped/sigl_quantreg_imputation_results.rds'

# load data
dt = readRDS(paste0(dir, inFile))
# ----------------------------------------------

# ----------------------------------------------
# exclude facilities that never report a given drug:
  # exclude org_unit_id - drug where all values are NA by facility and drug, across time and variable
dt[, completely_missing:=all(is.na(value)), by=c('org_unit_id','drug')]
dt = dt[completely_missing==FALSE]

# Make a histogram of the values of org_units not imputed
# NOTE:  make this histogram before setting the value where got_imputed=="yes" back to NA
if( nrow(dt[got_imputed=="yes" & is.na(value)]) != 0) stop("Imputed values need to be filled in first")

plot <- ggplot(dt[ skipped_qr == "yes" & !is.na(value) & value < 10000 & grepl(drug,pattern = "ASAQ"), ], aes(x=value)) + 
  geom_histogram(color="black", fill="white", binwidth = 50) + theme_bw() + facet_wrap( ~ variable) #+ xlim(0, 2500) + ylim(0, 200000)
plot
# ----------------------------------------------

# ----------------------------------------------
# fac_comp = unique(dt[org_unit_id %in% ids, .(org_unit_id, level) ])
# fac_comp = fac_comp[ ,.(num_of_type = .N), by= 'level']
# fac_comp[ , percent := ( num_of_type/length(dt[org_unit_id %in% ids, unique(org_unit_id)])) *100]
# 
# pie <- ggplot(fac_comp, aes(x="", y=percent, fill=level))+
#        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + theme_minimal()
# pie 
# ----------------------------------------------

# ----------------------------------------------
# compare the percent of missing data before and after QR imputation/ see how much data is still missing after QR imputation
dt_copy <- copy(dt)
dt_copy[ got_imputed== "yes", value := NA ]

number_imputed = dt_copy[ got_imputed=="yes", .(number_imputed = .N), by = c("drug", "variable")]
total_rows = dt_copy[, .(total_rows = .N), by = c("drug", "variable")]

dt2 = merge(total_rows, number_imputed, all = TRUE, by = c("drug", "variable"))
dt2[, percent_imputed := ((number_imputed/total_rows) * 100)]

missing_before = dt_copy[ is.na(value), .(missing_before = .N), by = c("drug", "variable")]
missing_after =  dt_copy[ is.na(value) & is.na(got_imputed), .(missing_after = .N), by = c("drug", "variable")]

dt2 = merge(dt2, missing_before, all = TRUE, by = c("drug", "variable"))
dt2 = merge(dt2, missing_after, all = TRUE, by = c("drug", "variable"))
dt2[, percent_missing_before := ((missing_before/total_rows) * 100)]
dt2[, percent_missing_after := ((missing_after/total_rows) * 100)]

p<-ggplot(dt2, aes(x=variable, y=percent_missing_after, color=drug)) +
  geom_bar(stat="identity", fill = "white", position=position_dodge()) + theme_bw()
p

dt2_copy = copy(dt2)
dt2_copy = dt2_copy[!grepl(drug, pattern = "RH")]
dt2_copy[drug=="SP", drug:= "SP_na"]
dt2_copy[drug=="ASAQ_supp_200mg", drug:= "ASAQ_supp200mg"]
dt2_copy[drug=="ASAQ_supp_400mg", drug:= "ASAQ_supp400mg"]
dt2_copy[, c("drug", "dose") := tstrsplit(drug, "_")]

p <- ggplot(dt2_copy, aes(x=variable, y=percent_missing_after, fill=dose)) +
     geom_bar(stat="identity", position=position_dodge()) + theme_bw() + facet_wrap(~ drug) +
     ggtitle("Percent of data still missing after QR imputation by drug and variable")
p

