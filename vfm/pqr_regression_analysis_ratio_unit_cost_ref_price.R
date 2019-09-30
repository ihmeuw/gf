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

# input file
dir = paste0(j, '/Project/Evaluation/GF/vfm/')
inFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr.rds')
inFile_new_pqr = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr_updated_09_2019.rds')
# inFile_codebook = paste0(dir, 'unit_cost_data/prepped_data/commodity_codebook.csv')
subset_commodities = paste0(dir, 'unit_cost_data/prepped_data/subset_commodities.csv')

# output files for figures
# for primary analyses
scatterplots_on_all_data = paste0(dir, 'visualizations/scatterplots_unit_cost_vs_ref_price_and_volume.pdf')
compare_ref_prices_with_size = paste0(dir, 'visualizations/ts_compare_ref_price_unit_cost_with_size_by_volume_(subset_to_most_common).pdf')
compare_ref_prices_volume_on_x = paste0(dir, 'visualizations/compare_ref_price_unit_cost_with_volume_on_x_(subset_to_most_common).pdf')
scatterplot_unit_cost_vs_intl_ref_price = paste0(dir, 'visualizations/scatterplot_unit_cost_vs_intl_ref_price_(subset_to_most_common).pdf')
hist_ref_price_by_product_category = paste0(dir, 'visualizations/hist_diff_unit_cost_ref_price_by_product_category.pdf')
regr_results_subtract_costs_by_category = paste0(dir, 'visualizations/regr_results_diff_unit_cost_ref_price_by_category.pdf')
regr_results_divide_costs_by_category = paste0(dir, 'visualizations/regr_results_ratio_unit_cost_ref_price_by_category.pdf')
regr_results_divide_costs_by_category_log = paste0(dir, 'visualizations/regr_results_ratio_unit_cost_ref_price_by_category_log.pdf')
ratio_unit_cost_to_ref_price_over_time = paste0(dir, 'visualizations/ts_compare_ratio_unit_cost_ref_price_over_time.pdf')

#subset of DRC and UGANDA
ts_ratio_unit_cost_ref_price_uga_drc = paste0(dir, 'visualizations/PQR/ts_compare_ratio_unit_cost_ref_price_over_time_(uga_drc).pdf')
ts_compare_unit_cost_ref_price_uga_drc = paste0(dir, 'visualizations/PQR/ts_compare_ref_price_unit_cost_with_size_by_volume_(uga_drc).pdf')
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data
# ----------------------------------------------
data = readRDS(inFile)
data = data[iso3codecountry %in% c('COD', 'GTM', 'SEN', 'UGA')]

# David said we don't care about specific brand of bednet, so make it so those will sum over product_name here:
data[product_category == 'bednet', product_name_en := 'bednet' ]

# make a separate "product category" for first and second line TB drugs:
first_line = c("Rifampicin" , "Pyrazinamide", "Ethambutol+Isoniazid+Pyrazinamide+Rifampicin (RHZE", "Isoniazid", 
               "Ethambutol+Isoniazid+Rifampicin - FDC", "Isoniazid+Pyrazinamide+Rifampicin - FDC", "Isoniazid+Rifampicin - FDC", 
               "Ethambutol+Isoniazid - FDC", "Ethambutol")
second_line = c("Ofloxacin", "Levofloxacin", "Moxifloxacin", "Cycloserine", "Protionamide", "Amikacin", "Ethionamide", "Kanamycin",
                "Capreomycin", "Linezolid", "Bedaquiline", "Meropenem", "Clofazimine", "Amoxicillin+Clavulanate - FDC", "Streptomycin", "PAS Sodium")
other = c("Water for injection")

data[ product_name_en %in% first_line, sub_product_category := "first_line"]
data[ product_name_en %in% second_line, sub_product_category := "second_line"]
data[ product_name_en %in% other, sub_product_category := "other"]

data[ product_category == 'anti_tb_medicine', product_category := paste(product_category, sub_product_category, sep = "_") ]

# adjust IRPs and unit costs that are wrong:
data[unit_cost_usd == 0, unit_cost_usd := NA]
data[po_international_reference_price == 0, po_international_reference_price := NA ]

data[, diff_from_ref_cost := unit_cost_usd - po_international_reference_price]
data[, unit_cost_over_ref_price := unit_cost_usd / po_international_reference_price]
data[, purchase_order_year := year(purchase_order_date)]

# save hiv drc testing data
# drc_hiv_tests = data[iso3codecountry == 'COD' & product_category== 'diagnostic_test_hiv']
# setorderv(drc_hiv_tests, c('purchase_order_date'))
# write.xlsx(drc_hiv_tests, paste0(dir, 'unit_cost_data/prepped_data/drc_hiv_diagnostic_tests.xlsx'))
# drc_hiv_tests = drc_hiv_tests[ purchase_order_date >= "2017-01-01" & product_name_en == 'HIV RDT and EIA']

# combine data sources
data2 = readRDS(inFile_new_pqr)


dt = data[!is.na(po_international_reference_price) & !is.na(unit_cost_usd)]
# ----------------------------------------------

# ----------------------------------------------
# subset to 'first line' products based on country guidelines
# ----------------------------------------------
subset_by = as.data.table(read.csv(subset_commodities))
subset_by = subset_by[ keep == TRUE, ]

subset = merge(dt, subset_by, by = c('iso3codecountry', 'product_category', 'product_name_en', 'description'))
subset = subset[, c('X', 'keep') := NULL]
subset[, product_name_en := as.factor(product_name_en)]
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

# dt[, purchase_order_year := year(purchase_order_date)]
# dt[, purchase_order_month := month(purchase_order_date)]
# dt[, purchase_order_date := as.Date(paste(purchase_order_year, "01", "01", sep = "-"))]
# # dt = dt[ purchase_order_date >= '2015-01-01', ]

#fit1 = glm(diff_from_ref_cost ~ purchase_order_date + factor(product_category), data = dt, family = gaussian())
#fit2 = glm(diff_from_ref_cost ~ purchase_order_date + factor(product_category) * factor(iso3codecountry), data = dt, family = gaussian())
fit = glm(diff_from_ref_cost ~ purchase_order_date + factor(product_category) * factor(supplier), data = dt, family = gaussian())

summary(fit)
# ----------------------------------------------

# ----------------------------------------------
# loop through product categories across all countries 
# ----------------------------------------------
pdf(regr_results_divide_costs_by_category_log, height = 9, width = 12)
for (cat in unique(dt$product_category)[1:6]){
  # subset to each category
  data_subset = dt[product_category == cat, ]
  # get the log of the ratio of unit cost to reference price
  data_subset[, log_ratio := log(unit_cost_over_ref_price)]
  
  fit = glm(log_ratio ~ purchase_order_date + product_name_en, data = data_subset, family = gaussian())
  
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
  
  g = ggplot(dt2, aes(x = product_name_en, y = unit_cost_over_ref_price)) +
    geom_bar(stat = 'identity') + theme_bw() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
    theme(strip.text = element_blank()) +
    labs(y = 'Ratio of unit cost to reference price', x = '', title = paste0(cat, ': prediction intervals from regression results\n     ratio_unit_cost_to_ref_price ~ purchase_order_date + product_name_en')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(g)
  
}
dev.off()

# make a bar graph showing the coefficients and CIs
model_coeff = data.table(var= names(fit$coefficients), coeff = fit$coefficients)
ci = confint(fit)
ci = data.table(var = rownames(ci), lower = ci[,1], upper = ci[,2])
result = merge(model_coeff, ci, by = 'var')
result = result[!is.na(coeff)]

plot_max = max(result$upper)
plot_min = min(result$lower)
result[, var := gsub('factor', '', var)]
result[, var := gsub('supplier', '', var)]
result[, var := gsub('product_category', '', var)]
result[, var := gsub('iso3codecountry', '', var)]
result[, var := gsub('\\()', '', var)]
result[, var := gsub('_', ' ', var)]
result[, var := gsub(':', ' : ', var)]
result[, var_wrap := str_wrap(var, width = 1)]

result[c(1:5, 11:15), facet_by := 1]
result[c(6:10, 16:20), facet_by := 2]

g1 = ggplot(result[1:10,], aes(x = var_wrap, y = coeff)) +
  geom_bar(stat = 'identity') + theme_bw() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
  facet_wrap(~ facet_by, scales = 'free_x') +
  ylim(plot_min,plot_max) +
  theme(strip.text = element_blank()) + xlab('') +
  labs( title = 'Results of diff_from_ref_cost ~ purchase_order_date + factor(product_category) * factor(supplier)',
        subtitle = 'Error bars show 95% C.I.') +
  theme(text = element_text( size = 15 ))

g2 = ggplot(result[11:20,], aes(x = var_wrap, y = coeff)) +
  geom_bar(stat = 'identity') + theme_bw() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
  facet_wrap(~ facet_by, scales = 'free_x') +
  ylim(plot_min,plot_max) +
  theme(strip.text = element_blank()) + xlab('') +
  theme(text = element_text( size = 15 ))

g3 = ggplot(result[21:27,], aes(x = var_wrap, y = coeff)) +
  geom_bar(stat = 'identity') + theme_bw() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
  facet_wrap(~ facet_by, scales = 'free_x') +
  ylim(plot_min,plot_max) +
  theme(strip.text = element_blank()) + xlab('') +
  theme(text = element_text( size = 15 ))

grid.arrange(g1, g2, nrow = 2)
grid.arrange(g1, g2, g3, nrow = 3)
# ----------------------------------------------