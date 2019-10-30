# ----------------------------------------------
# Audrey Batzel
# 
# 07/16/19
# Analyze PQR products
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(scales)
library(treemap)
library(treemapify)
library(grid)
library(dplyr)
library(grid)
library(gridExtra)
library(stringr)
library(ggrepel)
library(openxlsx)
# --------------------

# ----------------------------------------------
# Files and directories
# ----------------------------------------------
# switch for cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/vfm/')

# input files
inFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr_with_sept_download_data.rds')

subset_commodities = paste0(dir, 'unit_cost_data/prepped_data/subset_commodities.csv')

# output files 
regr_results_ratio_costs_by_category_log = paste0(dir, 'visualizations/PQR/regr_results_ratio_costs_by_category_log.pdf')
regr_results_ratio_costs_by_categoryAndCountry_log = paste0(dir, 'visualizations/PQR/regr_results_ratio_costs_by_categoryAndCountry_log.pdf')
regr_results_ratio_costs_by_procurement_mechanism = paste0(dir, 'visualizations/PQR/regr_results_ratio_costs_by_procurementMechanism_log.pdf')

frequency_table_supplier =  paste0(dir, 'unit_cost_data/prepped_data/frequency_table_supplier_product_category.csv')
bar_plots = paste0(dir, 'visualizations/PQR/bar_plot_number_of_orders_procurement_mechanism_by_category.pdf')
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data
# ----------------------------------------------
data = readRDS(inFile)

# subset to where intl ref price and unit cost are both not missing
dt = data[!is.na(po_international_reference_price) & !is.na(unit_cost_usd)]

freq_table = dt[,.N, by = .(supplier, product_category)]
setorderv(freq_table, c('product_category', 'supplier'))
write.csv(freq_table, frequency_table_supplier)
# ----------------------------------------------

# ----------------------------------------------
# regression by procurement mechanism across all countries and product categories
# ----------------------------------------------
pdf(regr_results_ratio_costs_by_procurement_mechanism, height = 9, width = 12)
data_subset = copy(dt)
# get the log of the ratio of unit cost to reference price
data_subset[, log_ratio := log(unit_cost_over_ref_price)]

fit = glm(log_ratio ~ purchase_order_date + supplier, data = data_subset, family = gaussian())

dt2 = unique(dt[,.(supplier)])
dt2 = as.data.table(dt2)
dt2[, purchase_order_date := mean(data_subset$purchase_order_date)]
dt2[, purchase_order_date := as.Date(purchase_order_date)]

dt2[, unit_cost_over_ref_price := predict(fit, newdata = dt2)]
dt2[, std_error := predict(fit, newdata=dt2, se.fit = TRUE)$se.fit]  
dt2[, lower:= unit_cost_over_ref_price - (1.96*std_error)]
dt2[, upper:= unit_cost_over_ref_price + (1.96*std_error)]

dt2[, unit_cost_over_ref_price := exp(unit_cost_over_ref_price)]
dt2[, lower := exp(lower)]
dt2[, upper := exp(upper)]

g = ggplot(dt2[!is.na(supplier)], aes(x = supplier, y = unit_cost_over_ref_price, color = supplier)) +
  geom_point(shape = 1, size = 3) + 
  theme_bw() + geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
  theme(strip.text = element_blank()) +
  labs(y = 'Ratio of unit cost to reference price', x = '', 
       title = paste0('Prediction intervals from regression results\n     ratio_unit_cost_to_ref_price ~ purchase_order_date + procurement_mechanism')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 14)) + guides(color = FALSE)
print(g)

data_subset[, supplier := as.factor(supplier)]
data_subset[ unit_cost_over_ref_price>=10, unit_cost_over_ref_price := 10 ]

print(ggplot(data_subset, aes(x = unit_cost_over_ref_price, color = supplier)) + geom_density() + theme_bw() + 
        theme(legend.position = 'bottom', text = element_text(size = 14)) +
        geom_vline(xintercept = 1, linetype = 'dashed', color = 'red') +
        labs(y = 'Density', x = 'Ratio unit cost:reference price', 
             caption = 'Note: values over 10 are set to 10 for the purpose of visualization \n10 represents >=10',
             title = paste0('Distribution of ratio of unit cost to reference price \nby procurement mechanism')))

freq_table[ supplier == 'PPM, through Partnership for Supply Chain Management (PFSCM)', supplier := 'PPM, through PFSCM']
freq_table[ supplier == 'Partnership for Supply Chain Management Inc (PFSCM)', supplier := 'PFSCM']

dev.off()

pdf(bar_plots, height = 9, width = 15)
print(ggplot(freq_table, aes(x = supplier, y = N, fill = product_category)) + theme_bw() + 
        geom_bar(stat="identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 14), legend.position = 'bottom') +
        labs(y = 'Number of orders', x = 'Procurement Mechanism', 
             title = '', fill = 'Product category') +
        coord_flip())

print(ggplot(freq_table[!is.na(supplier)], aes(x = supplier, y = N, fill = product_category)) + theme_bw() + 
        geom_bar(stat="identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 14), legend.position = 'bottom') +
        labs(y = 'Number of orders', x = 'Procurement Mechanism', 
             title = '', fill = 'Product category') +
        coord_flip())
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# subset to 'first line' products based on country guidelines
# ----------------------------------------------
subset_by = as.data.table(read.csv(subset_commodities))
subset_by = subset_by[ keep == TRUE, ]

subset = merge(dt, subset_by, by = c('iso3codecountry', 'product_category', 'product_name_en', 'description'))
subset = subset[, c('X', 'keep') := NULL]
subset[, product_name_en := as.factor(product_name_en)]
dt = copy(subset)

dt[product_category == 'diagnostic_test_hiv' & grepl(description, pattern = 'etermin', ignore.case = TRUE), product_name_en := 'Determine']
dt[product_category == 'diagnostic_test_hiv' & grepl(description, pattern = 'gold', ignore.case = TRUE), product_name_en := 'Uni-Gold']
dt[product_category == 'diagnostic_test_hiv' & grepl(description, pattern = 'stat', ignore.case = TRUE), product_name_en := 'Stat-Pak']
dt[product_category == 'diagnostic_test_hiv' & grepl(description, pattern = 'oraq', ignore.case = TRUE), product_name_en := 'OraQuick']
dt[product_category == 'diagnostic_test_hiv' & grepl(description, pattern = 'immunocomb', ignore.case = TRUE), product_name_en := 'ImmunoComb']
# ----------------------------------------------

# ----------------------------------------------
# 1.	Cost vs reference price stratified by categorical variables (procurement mechanism mainly, 
# please explore for other interesting options). Is there a procurement mechanism (or stratum) that consistently
# outperforms the reference price
#   a.	If you have time, try running the regression 
#   `difference_from_reference_cost ~ time + factor(procurement_mechanism)*factor(product_category)` 
# where the outcome variable is defined as unit_cost - reference_price. You'll get one coefficient per 
# procurement mechanism and category that tells you whether that combination is significantly different 
# from the first procurement mechanism/category (alphabetically). You can then make a data frame of the 
# unique values of the explanatory variables and predict in it to get estimates/CI of the mean cost differences.
# ----------------------------------------------
ggplot(dt, aes(x = diff_from_ref_cost)) + geom_histogram(binwidth = 0.5) + theme_bw()
summary(dt$diff_from_ref_cost)
plot(dt$purchase_order_date, dt$diff_from_ref_cost)
# dt = dt[ purchase_order_date >= '2015-01-01', ]
# ----------------------------------------------

# ----------------------------------------------
# loop through product categories across all countries 
# ----------------------------------------------
pdf(regr_results_ratio_costs_by_category_log, height = 9, width = 12)
for (cat in unique(dt$product_category)){
  # subset to each category
  data_subset = dt[product_category == cat, ]
  # get the log of the ratio of unit cost to reference price
  data_subset[, log_ratio := log(unit_cost_over_ref_price)]
  
  if(nrow(data_subset)<=1) next
  
  if (length(unique(data_subset$product_name_en))==1) form = as.formula('log_ratio ~ purchase_order_date')
  if (length(unique(data_subset$product_name_en))>1) form = as.formula('log_ratio ~ purchase_order_date + product_name_en')
  
  fit = glm(form, data = data_subset, family = gaussian())
  
  dt2 = unique(dt[product_category == cat,.(product_category, product_name_en)])
  dt2 = as.data.table(dt2)
  dt2[, purchase_order_date := mean(data_subset$purchase_order_date)]
  dt2[, purchase_order_date := as.Date(purchase_order_date)]
  
  dt2[, unit_cost_over_ref_price := predict(fit, newdata = dt2)]
  dt2[, std_error := predict(fit, newdata=dt2, se.fit = TRUE)$se.fit]  
  dt2[, lower:= unit_cost_over_ref_price - (1.96*std_error)]
  dt2[, upper:= unit_cost_over_ref_price + (1.96*std_error)]
  
  dt2[, unit_cost_over_ref_price := exp(unit_cost_over_ref_price)]
  dt2[, lower := exp(lower)]
  dt2[, upper := exp(upper)]
  
  g = ggplot(dt2, aes(x = product_name_en, y = unit_cost_over_ref_price, color = product_name_en)) +
    geom_point(shape = 1, size = 3) + 
    theme_bw() + geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
    theme(strip.text = element_blank()) +
    labs(y = 'Ratio of unit cost to reference price', x = '', title = paste0(cat, ': prediction intervals from regression results\n     ratio_unit_cost_to_ref_price ~ purchase_order_date + product_name_en')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 14)) + guides(color = FALSE)
  print(g)
  
  data_subset[, product_name_en := as.factor(product_name_en)]
  data_subset[ unit_cost_over_ref_price>=10, unit_cost_over_ref_price := 10 ]
  
  print(ggplot(data_subset, aes(x = unit_cost_over_ref_price, color = product_name_en)) + geom_density() + theme_bw() + 
    theme(legend.position = 'bottom', text = element_text(size = 14)) +
    geom_vline(xintercept = 1, linetype = 'dashed', color = 'red') +
    labs(y = 'Density', x = 'Ratio unit cost:reference price', color = 'Product:',
         caption = 'Note: values over 10 are set to 10 for the purpose of visualization; 10 represents >=10',
         title = paste0(cat, ': distribution of ratio of unit cost to reference price by product')))
}
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# loop through product categories AND country individually
# ----------------------------------------------
pdf(regr_results_ratio_costs_by_categoryAndCountry_log, height = 9, width = 12)
for (cat in unique(dt$product_category)){
  for (country in unique(dt$country_name)){
  # subset to each category
  data_subset = dt[product_category == cat & country_name == country, ]
  # get the log of the ratio of unit cost to reference price
  data_subset[, log_ratio := log(unit_cost_over_ref_price)]
  
  if(nrow(data_subset)<=1) next

  if (length(unique(data_subset$product_name_en))==1) form = as.formula('log_ratio ~ purchase_order_date')
  if (length(unique(data_subset$product_name_en))>1) form = as.formula('log_ratio ~ purchase_order_date + product_name_en')
 
  fit = glm(form, data = data_subset, family = gaussian())

  dt2 = unique(dt[product_category == cat & country_name == country,.(product_category, product_name_en)])
  dt2 = as.data.table(dt2)
  dt2[, purchase_order_date := mean(data_subset$purchase_order_date)]
  dt2[, purchase_order_date := as.Date(purchase_order_date)]

  dt2[, unit_cost_over_ref_price := predict(fit, newdata = dt2)]
  dt2[, std_error := predict(fit, newdata=dt2, se.fit = TRUE)$se.fit]
  dt2[, lower:= unit_cost_over_ref_price - (1.96*std_error)]
  dt2[, upper:= unit_cost_over_ref_price + (1.96*std_error)]

  dt2[, unit_cost_over_ref_price := exp(unit_cost_over_ref_price)]
  dt2[, lower := exp(lower)]
  dt2[, upper := exp(upper)]

  g = ggplot(dt2, aes(x = product_name_en, y = unit_cost_over_ref_price, color = product_name_en)) +
    geom_point(shape = 1, size = 3) + 
    theme_bw() + geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
    theme(strip.text = element_blank()) +
    labs(y = 'Ratio of unit cost to reference price', x = '', title = paste0(country, ', ', cat, ': prediction intervals from regression results\n     ratio_unit_cost_to_ref_price ~ purchase_order_date + product_name_en')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 14)) + guides(color = FALSE)
  print(g)
  
  data_subset[, product_name_en := as.factor(product_name_en)]
  data_subset[ unit_cost_over_ref_price>=10, unit_cost_over_ref_price := 10 ]
  
  print(ggplot(data_subset, aes(x = unit_cost_over_ref_price, color = product_name_en)) + geom_density() + theme_bw() + 
          theme(legend.position = 'bottom', text = element_text(size = 14)) +
          geom_vline(xintercept = 1, linetype = 'dashed', color = 'red') +
          labs(y = 'Density', x = 'Ratio unit cost:reference price', color = 'Product:',
               caption = 'Note: values over 10 are set to 10 for the purpose of visualization; 10 represents >=10',
               title = paste0(country, ', ', cat, ': distribution of ratio of unit cost to reference price by product')))

}}
dev.off()
# ----------------------------------------------
