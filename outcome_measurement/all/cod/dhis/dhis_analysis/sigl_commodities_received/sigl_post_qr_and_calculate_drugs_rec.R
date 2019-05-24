setwd('C:/local/gf/')
# Audrey Batzel

# 4/18/19 split this off into it's own file because it was getting too overwhelming/complex
# this file is for manipulating the results of QR on SIGL drugs available, consumed, and lost data, to include:
  # - removing outliers
  # - fixing bad QR results (where fitted value is negative, replace with 0)
  # - replace > 99.5 percentile with missing where QR was skipped (some of these look to be outliers, but since
  #     QR didn't run on them, they won't be caught with the other outlier method.)
  # - handle values still missing with median imputation

# Then aggregate to hz level and calculate drugs/commodities received

# 4/23/19 updated with post qr and outliers labeled data
# ----------------------------------------------

# --------------------
# Set up R / install packages
# -------------------
rm(list=ls())
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(readxl)
# -------------------

# ----------------------------------------------
# Files and directories
# ----------------------------------------------
# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set directories
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# input files
data = "prepped/sigl/sigl_drugs_prepped_outliers_labeled.rds"
pre_qr = "prepped/sigl/sigl_for_qr.rds"
more_outliers = 'outliers/sigl/sigl_more_outliers_to_remove.xlsx'

# output file
outData = 'prepped/sigl/sigl_prepped_drugs_received.rds'
# ----------------------------------------------

# ----------------------------------------------
# load data
# ----------------------------------------------
dt = readRDS(paste0(dir, data)) 
outliers = as.data.table(read_xlsx(paste0(dir, more_outliers)))
outliers[,date := as.Date(date)]
# ----------------------------------------------

# ----------------------------------------------
# Further data prep to be able to calculate drugs/commodities received
# ----------------------------------------------
# Impute missing values with fitted value where got_imputed == "yes" and number_of_values is greater than or equal to 5
dt[got_imputed == "yes" & number_of_values >= 5, value := fitted_value]
dt[got_imputed == "yes" & number_of_values < 5, got_imputed := "no"]

# remove manually identified outliers
dt = merge(dt, outliers, all = TRUE, by = c('org_unit_id', 'date', 'drug', 'variable', 'value'))
dt[outlier.y == TRUE, outlier.x:= TRUE]
setnames(dt, 'outlier.x', 'outlier')
dt[,outlier.y := NULL]

# Replace outlier values with fitted values
dt[outlier == TRUE, value := fitted_value]

# Fix QR results where fitted value is negative - replace with 0
dt[fitted_value < 0 & got_imputed == "yes", value := 0]
  # there are 31 cases where value is still less than 0 because the orig value was an outlier and we replaced it with fitted
  dt[fitted_value < 0 & outlier == TRUE, value := 0]
  if( nrow(dt[value < 0, ]) != 0 ) stop ("You have negative values in the data set - fix this!")

# Replace > 99.5 percentile with missing where QR was skipped (some of these look to be outliers, but since
# QR didn't run on them, they won't be caught with the other outlier method.)
dt[skipped_qr == "yes" & value > limit, outlier := TRUE ]
dt[skipped_qr == "yes" & value > limit, value := NA ]

# Use median imputation to fill in missing where QR was skipped and/or reset to missing when min to run QR was raised to 5
dt[ , median_by_id_vars := median(value, na.rm = TRUE), by = .(org_unit_id, drug, variable)]
dt[ is.na(value), value := median_by_id_vars]

# check number still missing:
dt[is.na(value), .N] # 609752

# where lost is na, set it to be 0
dt[variable == "lost" & is.na(value), value := 0]

# check number still missing:
dt[is.na(value), .N] # 46657  # I think based on what we've done so far these will only be cases where one variable is completely missing but another
                             # is actually present in the data
# ----------------------------------------------

# ----------------------------------------------
# make figures of outliers where QR was skipped 
# ----------------------------------------------
# Set of histograms for where QR was skipped, by level/drug/variable
loop = unique(dt[,.(level, drug, variable)])
loop = loop[!is.na(level),]

list_of_plots = NULL
i=1

for( index in 1:nrow(loop)){
  l = loop[ index, level ]
  d = loop[ index, drug ]
  v = loop[ index, variable ]
  
  list_of_plots[[i]] = ggplot(dt[skipped_qr == "yes" & level == l & drug == d & variable == v, ], aes(x=value)) + 
    geom_histogram() + 
    geom_vline(xintercept = dt[skipped_qr == "yes" & level == l & drug == d & variable == v, unique(limit) ], color = 'red', size = 1.5, linetype = "dashed") +
    theme_bw() + 
    labs(title =paste("Histogram for:", l, d, v, "for original values in the data where QR was not run \n(less than 5 data points and or 0 variance)"),
         subtitle = "Red line shows cut off of 99.5 percentile for this limit-drug-variable combination")
  i = i + 1
}

pdf(paste0(dir, "outliers/sigl/histograms_by_leveldrugvariable_QRskipped.pdf"), height=6, width=10)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 
dev.off()

# subset to the health facilities and elements that contain outliers where QR skipped
dt[ , combine := paste0(org_unit_id, drug)]
out_orgs = dt[skipped_qr == "yes" & outlier == TRUE, unique(combine)]
out = dt[combine %in% out_orgs]

# drop the unique identifier
out[ , combine := NULL]
dt[ , combine := NULL]

# checked emerging trends and not relevant here

# create a list of plots
list_of_plots = NULL
i=1

for (d in unique(out$drug)) {
  for (o in unique(out[drug==d]$org_unit_id)) {
    # title states drug variable, facility
    title = paste0(d, ' in ', out[org_unit_id==o, unique(org_unit)])
    
    # create the plot
    list_of_plots[[i]] = ggplot(out[drug==d & org_unit_id==o], aes(x=date, y=orig_value)) +
      geom_point(size = 3) +
      geom_line(aes(x=date, y=median_by_id_vars), color = 'black') +
      geom_line(aes(x=date, y=limit), color = 'red') +
      geom_point(data = out[drug==d & org_unit_id==o & outlier==TRUE & skipped_qr == "yes"], color='#d73027', size=3, alpha=0.8) +
      geom_point(data = out[drug==d & org_unit_id==o & outlier==TRUE & skipped_qr == "yes"], aes(x=date, y=median_by_id_vars), 
                 color='#4575b4', size=3, alpha=0.8) +
      facet_wrap(~variable, scales = "free") +
      labs(title=title, x='Date', y='Count',
           color='Age', subtitle= "Outliers where QR was not run (<3 data points) and imputation with median (black line) \nRed line shows 99.5 percentile by level, drug, and variable, which was used to ID outliers \nRed points are outliers; black points are other original values") +
      theme_bw()
    
    i=i+1
  }}

pdf(paste0(dir, qr_skipped_outliers), height=6, width=10)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# Calculate drugs/commodities received
# ----------------------------------------------
# Aggregate to hz level
dt_hz = dt[, .(value = sum(value, na.rm = TRUE)), by = .(dps, health_zone, date, drug, variable)] 

# Formula/calculate:
# received(n) = available(n) + consumed(n) + lost(n) - available(n-1)
# cast wide
# create formula for cast
all_vars <- colnames(dt_hz)
vars_for_cast <- all_vars[!all_vars %in% c("variable", "value")]
f <- as.formula(paste(paste(vars_for_cast, collapse = " + "), "~ variable"))
# cast variable wide so we can add/subtract vars
dt_for_calc_wide <- dcast.data.table(dt_hz, f, value.var = "value")

# identify where previous date (by month) is missing in the data,
# by unique identifiers
calc <- setorderv(dt_for_calc_wide, c("health_zone", "drug", "date"))
calc <- calc[, previous_date := (date - months(1))]
calc <- calc[, actual_previous_date := data.table::shift(date, 1L, type="lag"), by=c('health_zone', 'drug')] #by=c('org_unit_id', 'drug', 'category')]
calc <- calc[, include_in_calculation := ifelse(previous_date == actual_previous_date, TRUE, FALSE), ]

# calculate received, only for dates where the previous month exists
# use shift to get the value for the previous date of the available variable
# calc[, received2 := (available + consumed + lost - (data.table::shift(available))), by = c('health_zone', 'drug')]
# calc[, received1 := (available + (data.table::shift(consumed)) + (data.table::shift(lost)) - (data.table::shift(available))), by = c('health_zone', 'drug')]
calc[, received := (data.table::shift(available, type='lead')) - available + consumed + lost, by = c('health_zone', 'drug')] # USE THIS FORMULA

#--------------------
# calc[ received < 0, .N, by = 'date']

# violin plot of received by date
calc$date = as.character(calc$date)
p <- ggplot(calc, aes(x=date, y=received)) + 
  geom_violin() + theme_bw() + geom_boxplot(width = 0.1)
p

plot = ggplot(calc, aes(x=received)) + 
  geom_histogram(binwidth = 1000) + facet_wrap( ~date, scales = "free_y")
plot

# where received is negative, set to 0
calc[ received < 0 , received := 0]

# save a copy of the data to be used in the results chain model:
calc[, c('previous_date','actual_previous_date', 'include_in_calculation'):= NULL]
saveRDS(calc, paste0(dir, outData))
# ----------------------------------------------

# ----------------------------------------------
# graphs / checks 
# ----------------------------------------------
check = dt[ value > limit, ]
check = check[value > 5000, ]

plot <- ggplot(calc[received < 100000 & received > -50000,], aes(x=received)) + 
        geom_histogram(color="black", fill="white", binwidth = 500) + 
        theme_bw() + labs(subtitle = "Note: bounded this by -50,000 and 100,000 so it was easier to see \nThere were 35 values below -50,000 and 17 above 100,000")
plot

pdf(paste0(dir, "outliers/sigl/histogram_drugs_recieved.pdf"), height=6, width=10)
print(plot)
dev.off()

calc[received < -50000, .N] 
# -------------------------
calc_long = calc[, .(dps, health_zone, date, drug, available, consumed, lost, received)]
calc_long = melt.data.table(calc_long, id.vars = c("dps", "health_zone", "date", "drug"))
calc_long[, variable := as.character(variable)]

plot_list = NULL
hzs = unique(calc$health_zone)

i = 1
for (h in hzs){
  plot_list[[i]] = ggplot(calc_long[health_zone == h & grepl(drug, pattern = "ASAQ_") & !grepl(drug, pattern = "supp|inj"),], aes(x=date, y=value, color = variable)) + 
    geom_point() + geom_line() +
    theme_bw() + ggtitle(paste0("health zone = ", h)) +
    facet_wrap( ~ drug, scales = "free")
  i = i + 1
}

# save a subset of this data for David to work on the formula:
save_subset = calc_long[health_zone == hzs[3] & drug == "ASAQ_14yrsAndOlder",]
save = dcast.data.table(save_subset, dps + health_zone + date + drug ~ variable )
saveRDS(save, paste0(dir, "outliers/sigl/subset_of_data_adi_ASAQ14+.rds"))

pdf(paste0(dir, "outliers/sigl/time_series_drugs_received_hz_level_(cont).pdf"), height=6, width=10)
for(i in 222:(length(plot_list))) { 
  print(plot_list[[i]])
}
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# time series figures for manual outlier removal
# ----------------------------------------------
loop_through = unique(dt[value > 50000 & value > limit, .(drug, org_unit_id, variable, date, value, level)])
plot_list = NULL
i = 1
for (row in 1:nrow(loop_through)){
  d = loop_through[row, drug]
  o = loop_through[row, org_unit_id]
  v = loop_through[row, variable]
  date = loop_through[row, date]
  value = loop_through[row, value]
  level = loop_through[row, level]
  if( is.na(level) )  level = "no level reported"
  plot_list[[i]] = ggplot(dt[org_unit_id == o & drug == d & variable == v,], aes(x=date, y=value, color = variable)) + 
    geom_point() + geom_line() +
    theme_bw() + ggtitle(paste(o, "(", level, ")", date, d, v, value))
  i = i + 1
}
pdf(paste0(dir, "outliers/sigl/sigl_additional_outlier_screening_manual.pdf"), height=6, width=10)
for(i in 1:(length(plot_list))) { 
  print(plot_list[[i]])
}
dev.off()

d = "SP"
o = "xEgqynT6g7G"
v = "available"
g = ggplot(dt[org_unit_id == o & drug == d & variable == v,], aes(x=date, y=value, color = variable)) + 
  geom_point() + geom_line() +
  theme_bw() + ggtitle(paste(o, d, v))
print(g)

h = "kenge"
g = ggplot(calc_long[health_zone == h & drug == d ,], aes(x=date, y=value, color = variable)) + 
  geom_point() + geom_line() +
  theme_bw() + ggtitle(paste0("health zone = ", h))
print(g)
# ----------------------------------------------

# ----------------------------------------------
# compare with some hz's in pnlp
# ----------------------------------------------
dir_pnlp = paste0('J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')
# pnlp = "archive/imputedData_run2_agg_hz.rds"
pnlp = "archive/imputedData_run2_agg_dps.rds"
source('./core/standardizeDPSNames.R')
source('./core/standardizeHZNames.R')

dt_pnlp <- readRDS(paste0(dir_pnlp, pnlp))
setnames(dt_pnlp, "mean", "value")

dt_pnlp <- dt_pnlp[indicator %in% c("ASAQreceived")]
dt_pnlp$date <- as.Date(dt_pnlp$date)
dt_pnlp[, drug:= paste0(indicator, "_", subpopulation)]
dt_pnlp$source = "PNLP"
dt_pnlp[, drug := gsub("received", "", drug)]
setnames(dt_pnlp, "value", "received")

dt_snis = calc[ , drug := gsub("12to59mos", "1to5yrs", drug)]
dt_snis$source = "SNIS (calculated)"
vars = unique(dt_pnlp$drug)
dt_snis = dt_snis[drug %in% vars, ]

dt_snis = dt_snis[, .(received = sum(received, na.rm = TRUE)), by = c("dps", "date", "drug", "source")]

cols = names(dt_snis)[names(dt_snis) %in% names(dt_pnlp)]
dt_snis = dt_snis[, cols, with = FALSE]
dt_pnlp = dt_pnlp[, cols, with = FALSE]

dt_compare <- rbindlist(list(dt_snis, dt_pnlp), use.names=TRUE)
dt_compare$dps = standardizeDPSNames(dt_compare$dps)
dt_compare$health_zone = standardizeHZNames(dt_compare$health_zone)
dps = unique(dt_compare$dps)
hzs = unique(dt_compare$health_zone)

plot_list_hz = NULL
plot_list_dps = NULL
i = 1
# for (x in hzs){
for (x in dps){
  # plot_list_hz[[i]] <- ggplot(dt_compare[health_zone == x], aes(x=date, y=received, color=source)) + 
  plot_list_dps[[i]] <- ggplot(dt_compare[dps == x], aes(x=date, y=received, color=source)) + 
    geom_point() + geom_line() +
    ggtitle(paste0("Time series showing PNLP and calculated variable using SIGL in ", x)) +
    ylab("Drugs/doses received") + xlab("Date") + theme_bw() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
          legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14), legend.position = "bottom") +
    facet_wrap( ~drug, scales = "free") +
    scale_y_continuous( label= scales :: comma ) + 
    guides(color = guide_legend(title= "Data Source:"))
  i = i + 1
}
# pdf(paste0(dir, "outliers/sigl/time_series_drugs_received_hz_level_pnlp_comparison.pdf"), height=6, width=10)
# for(i in seq(length(plot_list_hz))) {
#   print(plot_list_hz[[i]])
# }
# dev.off()

pdf(paste0(dir, "outliers/sigl/time_series_drugs_received_dps_level_pnlp_comparison.pdf"), height=6, width=10)
for(i in seq(length(plot_list_dps))) {
  print(plot_list_dps[[i]])
}
dev.off()
# ----------------------------------------------

