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

# set directories
data_path <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/outliers_removed/')
catalogue_path <- paste0(root, "/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/catalogues/")

# input files
data <-"sigl_drugs_rec_prepped_outliers_labeled"
cat <- "data_elements_cod.csv"
imputed_data = paste0(data_path, 'sigl_quantreg_imputation_results.rds')

# output files 
prepped_drug_data <- "drugs_consumed_lost_available_update_3_6_19.rds"
prepped_for_qr = "sigl_for_qr.rds"
post_qr = "sigl_quantreg_imputation_results.rds"
drugs_dist = "drugs_dist_update_3_28_19.rds"


# load data
dt = readRDS(paste0(dir, inFile))
# ----------------------------------------------

# ----------------------------------------------
# -------------------------
# load in imputed data:
dt = readRDS(paste0(data_path, post_qr))
dt[, date := as.Date(date, origin = "1970-01-01")]

# # Figure out how much of the missingness could be from a facility not stocking certain drugs:
# # total missing values = 6566509
# missing = dt[is.na(value), .(num_missing= .N), by = c("org_unit_id", "drug")] # this will be across all time/all variables of a drug...
# # right now all missing would be = 39 NAs
# sum(missing[num_missing == 39, num_missing]) / sum(missing$num_missing) # 62 % of all missing is due to facilities with no data at all (whole time series) for the drug (both avail and consumed)
# # Then, the same, excluded the variable "lost"
# missing = dt[is.na(value), .(num_missing= .N), by = c("org_unit_id", "drug", "variable")] 
# missing[ variable == "lost", sum(num_missing)] # = 2654006, so 40% of total missing is from the "lost" variable
# missing = dt[is.na(value) & variable != "lost", .(num_missing= .N), by = c("org_unit_id", "drug")] 
# sum(missing[num_missing == 26, num_missing]) # 2752906 = the number missing the full time series for drugs not including the lost variable
# # 2752906 + 2654006 = 5406912 missing due to facility missing all data for the drug or the lost variable missing
# # 5406912 / 6566509 * 100 = 82% of the missing data that is still missing could potentially be account for other ways...
# # 6566509 - 5406912 = 1159597 data points still missing

# now, exclude org_unit_id - drug where all values are NA by facility and drug, across time and specific variable (realized I could probably have used all(is.na(x))...) 
# missing = dt[is.na(value), .(num_missing= .N), by = c("org_unit_id", "drug")] 
# exclude_from_rect = unique( missing[num_missing == 39, .(org_unit_id, drug)] )
# dt_for_calc = anti_join(dt, exclude_from_rect, by = c("org_unit_id", "drug"))
# dt_for_calc = as.data.table(dt_for_calc)
dt[, completely_missing:=all(is.na(value)), by=c('org_unit_id','drug')]
dt = dt[completely_missing==FALSE]

# verification of results step - compare median of observed values by id vars to the median of imputed values
# should be similar, so note any cases that are really different
check1= dt[is.na(got_imputed) & skipped_qr == "no", .(med_observed = median(value, na.rm = TRUE)), by = .(org_unit_id, level, drug, variable)]
check2= dt[got_imputed == "yes" & skipped_qr == "no", .(med_imputed = median(value, na.rm = TRUE)), by = .(org_unit_id,level,  drug, variable)]
check3= dt[skipped_qr == "no", .(med_fitted = median(fitted_value, na.rm = TRUE)), by = .(org_unit_id, level, drug, variable)]

dt[ got_imputed== "yes", value := NA ]
# want to try changing minimum # of values needed to run QR from 3 to 4..
# get the number of values that were present before QR imputation
check4 = dt[!is.na(value) & skipped_qr == "no", .N, by = .(org_unit_id, level, drug, variable) ]

check = merge(check1, check2, all = TRUE)
check = merge(check, check3, all = TRUE)
check = merge(check, check4, all = TRUE)

check[, med_imputed := round(med_imputed, digits = 2)]
check[, abs_diff := abs(med_observed - med_imputed) ]

dt = merge(dt, check, all = TRUE, by = c('org_unit_id', 'drug', 'variable'))

# make figures that show an example of good fit and bad fit
o = 'kVjIG3QXgoX'
d = 'RH_150mg+75mg'

ex = ggplot( dt[ org_unit_id == o & drug == d, ], aes(x=date, y=value)) +
  geom_point(size = 2) +
  geom_line(aes(x=date, y=fitted_value), alpha = 0.5) +
  geom_point(data= dt[  org_unit_id == o & drug == d & got_imputed == "yes" ], aes(x=date, y = fitted_value), color='#d73027', alpha=0.8) +
  facet_wrap(~ variable, scales = "free") + 
  theme_bw()
ex

# set fitted values back to value where imputation was true
dt[ got_imputed == "yes", value := fitted_value]
# ---------------------------------------------------
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

