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
drc_hiv_tests = data[iso3codecountry == 'COD' & product_category== 'diagnostic_test_hiv']
setorderv(drc_hiv_tests, c('purchase_order_date'))
write.xlsx(drc_hiv_tests, paste0(dir, 'unit_cost_data/prepped_data/drc_hiv_diagnostic_tests.xlsx'))

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
# plots - Compare unit cost to reference price (ratio) over time
# ----------------------------------------------
pdf(ratio_unit_cost_to_ref_price_over_time, height = 9, width = 12)
# pdf(ts_ratio_unit_cost_ref_price_uga_drc, height = 9, width = 12)
for (cat in unique(subset$product_category)) {
  
  colors1 = brewer.pal(8, 'Set2')
  colors = c(colors1, "#6EB5FF")
  
  names(colors) = unique(subset[product_category == cat, product_name_en])
  
  for (x in unique(subset$iso3codecountry)){
  # for (x in c('COD', 'UGA')){    
    country = unique(subset[product_category == cat & iso3codecountry == x, country_name])
    plot_subtitle = subset[product_category == cat & iso3codecountry == x, unique(sub)]
    if(plot_subtitle == 'none') plot_subtitle = ''
      
    print(ggplot(subset[product_category == cat & iso3codecountry == x, ], aes(x=purchase_order_date, y=unit_cost_over_ref_price, color=product_name_en, size = total_units_in_order)) +
      geom_point() +
      theme_bw()  +
      scale_color_manual(name = 'Product Name', values = colors) +
      geom_hline(aes(yintercept = 1), color = 'red') +
      theme(text = element_text(size=14)) +
      # facet_wrap( ~description, scales = "free") +
      scale_size_continuous(labels = comma) +
      labs(title = paste0(country, ", ", cat, ": ratio of unit cost to reference price over time"),
           subtitle = plot_subtitle,
           x='Purchase order date', y='Unit Cost / International Reference Price', size = 'Volume Purchased',
           caption = 'Red line shows a ratio of 1 (cost equal to reference price).'))
      # annotate(geom= 'text', y = 1, x = min(subset[product_category == cat & iso3codecountry == x, purchase_order_date]), 
      #          hjust = -0.05, vjust = -0.5, label = 'Ratio of 1 (cost equal to reference price)'))
  }
}
dev.off()
# ----------------------------------------------


# # ----------------------------------------------
# # subset data to only "most common" commodities 
# # ----------------------------------------------
# subset_with_codebook = as.data.table(read.csv(inFile_codebook, stringsAsFactors = FALSE))
# 
# products_to_include_in_analysis = unique(subset_with_codebook[ included_in_analysis == TRUE, .(product_name_en, description) ])
# subset = merge(data, products_to_include_in_analysis, by = c('product_name_en', 'description'))
# 
# # get a subset of the products NOT included in 'subset'
# # subset_less_common = anti_join(data, subset, by = c('product_name_en', 'description'))
# # ----------------------------------------------

# ----------------------------------------------
# Graphs comparing unit costs and international reference prices
# ----------------------------------------------
subset[, country_name := as.factor(country_name)]

colors = brewer.pal(4, 'Set1')
names(colors) = levels(subset$country_name)

pdf(compare_ref_prices_with_size, height = 9, width = 12)

# subset = subset[grepl(product_name_en, pattern = 'artesu|lumefan', ignore.case=TRUE) & iso3codecountry %in% c('COD', 'UGA'), ]
# pdf(ts_compare_unit_cost_ref_price_uga_drc, height = 9, width = 12)

for ( cat in unique(subset$product_category) ){ # loop through category first so they're grouped by category
  for ( p in unique(subset[product_category == cat, product_name_en]) ){
    print(ggplot(subset[product_name_en == p, ], aes(x=purchase_order_date, y=unit_cost_usd, color=country_name, size = total_units_in_order)) +
      geom_point() +
      scale_color_manual(name = 'Country Name', values = colors) +
      geom_line(aes(x=purchase_order_date, y=po_international_reference_price), color = 'darkgrey', size = 1 ) +
      geom_point(aes(x=purchase_order_date, y=po_international_reference_price), color = 'darkgrey', size = 2 ) +
      theme_bw() +
      theme(text = element_text(size=14)) +
      facet_wrap( ~description, scales = "free") +
      scale_size_continuous(labels = comma) +
      labs(title = paste0("Comparison of unit costs and international reference prices for ", p),
           subtitle = '(Gray points represent the international reference price over time)',
           x='Purchase order date', y='Unit Cost (USD)', size = 'Volume Purchased'))
  }
}
dev.off()

pdf(compare_ref_prices_volume_on_x, height = 9, width = 12)
for ( cat in unique(subset$product_category) ){ # loop through category first so they're grouped by category
  for ( p in unique(subset[product_category == cat, product_name_en]) ){
    print(ggplot(subset[product_name_en == p, ], aes(x=total_units_in_order, y=unit_cost_usd, color=country_name)) +
      geom_point(size = 4.5) +
      scale_color_manual(name = 'Country Name', values = colors) +
      geom_point(aes(x=total_units_in_order, y=po_international_reference_price), color = 'black', size = 3 ) +
      theme_bw() +
      theme(text = element_text(size=14)) +
      facet_wrap( ~description, scales = "free") +
      scale_x_continuous(labels = comma) +
      theme(legend.position = 'bottom') +
      labs(title = paste0(cat, ": Comparison of unit costs and international reference prices for ", p),
           subtitle = '(Black points represent the international reference price)',
           x='Units Purchased', y='Unit Cost (USD)'))
  }
}
dev.off()

pdf(scatterplot_unit_cost_vs_intl_ref_price, height = 9, width = 12)
for ( cat in unique(subset$product_category) ){ # loop through category first so they're grouped by category
  for ( p in unique(subset[product_category == cat, product_name_en]) ){
    print(ggplot(subset[product_name_en == p, ], aes(x=po_international_reference_price, y=unit_cost_usd, color=country_name, size=total_units_in_order)) +
      geom_point() + geom_abline() +
      scale_color_manual(name = 'Country Name', values = colors) +
      theme_bw() +
      theme(text = element_text(size=14)) +
      facet_wrap( ~description) +
      scale_size_continuous(labels = comma) +
      labs(title = paste0(cat, ": Comparison of unit costs and international reference prices for ", p),
           x='International Reference Price', y='Unit Cost (USD)', size = 'Volume Purchased') +
      ylim(0, NA) + xlim(0, NA))
  }
}
dev.off()
# ----------------------------------------------

# # ----------------------------------------------
# # Scatterplots and histograms for full data
# # ----------------------------------------------
# # histograms
# ggplot(dt, aes(x = po_international_reference_price)) + 
#   geom_histogram() +
#   theme_bw() +
#   theme(text = element_text(size=14)) +
#   labs(title = paste0("Distribution of international reference prices by product category"), 
#        x='International Reference Price', y='Count') +
#   facet_wrap(~product_category, scales = 'free')
# 
# ggplot(dt, aes(x = diff_from_ref_cost)) + 
#   geom_histogram() +
#   theme_bw() +
#   theme(text = element_text(size=14)) +
#   labs(title = paste0("Distribution of difference between reference price and unit cost\nby product category"), 
#        x='International Reference Price - Unit Cost USD', y='Count') +
#   facet_wrap(~product_category, scales = 'free')
# 
# ggplot(dt, aes(x = log(unit_cost_over_ref_price))) + 
#   geom_histogram() +
#   theme_bw() +
#   theme(text = element_text(size=14)) +
#   labs(title = paste0("Distribution of (unit cost / reference price) by product category"), 
#        x='Unit Cost USD / International Reference Price', y='Count') +
#   facet_wrap(~product_category, scales = 'free')
# 
# # scatterplots
# colors = brewer.pal(4, 'Set1')
# names(colors) = levels(data$country_name)
# 
# max = max( c(max(dt$unit_cost_usd), max(dt$po_international_reference_price)) )+0.5
# 
# pdf(scatterplots_on_all_data, height = 9, width = 11)
# # ref price vs. unit cost - ALL 
# ggplot(dt[ ], aes(x=po_international_reference_price, y=unit_cost_usd, color=country_name)) +
#   geom_point(size = 1.8) + geom_abline() + 
#   scale_color_manual(name = 'Country Name', values = colors) +
#   theme_bw() +
#   theme(text = element_text(size=14)) +
#   # facet_wrap( ~purchase_order_year) +
#   scale_size_continuous(labels = comma) +
#   labs(title = paste0("Comparison of unit costs and international reference prices"), 
#        x='International Reference Price', y='Unit Cost (USD)') +
#   ylim(0, max) + xlim(0, max)
# # ref price vs. unit cost - zoomed in to 1 by 1
# ggplot(dt[], aes(x=po_international_reference_price, y=unit_cost_usd, color=country_name)) +
#   geom_point(size = 1.8) + geom_abline() + 
#   scale_color_manual(name = 'Country Name', values = colors) +
#   theme_bw() +
#   theme(text = element_text(size=14)) +
#   # facet_wrap( ~purchase_order_year) +
#   scale_size_continuous(labels = comma) +
#   labs(title = paste0("Comparison of unit costs and international reference prices"), 
#        x='International Reference Price', y='Unit Cost (USD)') +
#   ylim(0, 1) + xlim(0, 1)
# # ref price vs. unit cost - zoomed in to 0.25 by 0.25
# ggplot(dt[ ], aes(x=po_international_reference_price, y=unit_cost_usd, color=country_name)) +
#   geom_point(size = 1.8) + geom_abline() + 
#   scale_color_manual(name = 'Country Name', values = colors) +
#   theme_bw() +
#   theme(text = element_text(size=14)) +
#   # facet_wrap( ~purchase_order_year) +
#   scale_size_continuous(labels = comma) +
#   labs(title = paste0("Comparison of unit costs and international reference prices"), 
#        x='International Reference Price', y='Unit Cost (USD)') +
#   ylim(0, 0.25) + xlim(0, 0.25)
# 
# # facet wrap by product category
# ggplot(dt[ ], aes(x=po_international_reference_price, y=unit_cost_usd, color=country_name)) +
#   geom_point(size = 1.8) + geom_abline() + 
#   scale_color_manual(name = 'Country Name', values = colors) +
#   theme_bw() +
#   theme(text = element_text(size=14)) +
#   facet_wrap( ~product_category, scales = 'free') +
#   scale_size_continuous(labels = comma) +
#   labs(title = paste0("Comparison of unit costs and international reference prices"), 
#        x='International Reference Price', y='Unit Cost (USD)') +
#   ylim(0, NA) + xlim(0, NA)
# 
# 
# # scatterplots unit cost vs. volume purchased
# dt[, total_units_in_order_millions := total_units_in_order/1000000 ]
# ggplot(dt[ ], aes(x=total_units_in_order_millions, y=unit_cost_usd, color=country_name)) +
#   geom_point(size = 1.8) + 
#   scale_color_manual(name = 'Country Name', values = colors) +
#   theme_bw() +
#   theme(text = element_text(size=14)) +
#   facet_wrap( ~product_category, scales = 'free') +
#   scale_size_continuous(labels = comma) +
#   labs(title = paste0("Comparison of unit cost and number of units purchased by product category"), 
#        x='Number of units purchased (in millions)', y='Unit Cost (USD)') +
#   ylim(0, NA) + xlim(0, NA)
# 
# dev.off()
# 
# ggplot(dt[ ], aes(x=total_units_in_order_millions, y=unit_cost_usd, color=product_category)) +
#   geom_point(size = 1.8) + 
#   # scale_color_manual(name = 'Country Name', values = colors) +
#   theme_bw() +
#   theme(text = element_text(size=14)) +
#   facet_grid( product_category ~country_name, scales = 'free') +
#   scale_size_continuous(labels = comma) +
#   labs(title = paste0("Comparison of unit cost and number of units purchased by product category"), 
#        x='Number of units purchased (in millions)', y='Unit Cost (USD)') +
#   ylim(0, NA) + xlim(0, NA)
# 
# # ----------------------------------------------

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

# ggplot(dt, aes(x = diff_from_ref_cost)) + geom_histogram(binwidth = 0.1) + theme_bw()
# summary(dt$diff_from_ref_cost)
# plot(dt$purchase_order_date, dt$diff_from_ref_cost)
# 
# dt[, purchase_order_year := year(purchase_order_date)]
# dt[, purchase_order_month := month(purchase_order_date)]
# dt[, purchase_order_date := as.Date(paste(purchase_order_year, "01", "01", sep = "-"))]
# # dt = dt[ purchase_order_date >= '2015-01-01', ]
# 
# plot(dt$purchase_order_date, dt$diff_from_ref_cost)
# 
# fit1 = glm(diff_from_ref_cost ~ purchase_order_date + factor(product_category), data = dt, family = gaussian())
# 
# fit2 = glm(diff_from_ref_cost ~ purchase_order_date + factor(product_category) * factor(iso3codecountry), data = dt, family = gaussian())
# fit3 = glm(diff_from_ref_cost ~ factor(product_category) * factor(supplier), data = dt, family = gaussian())
# 
# fit = copy(fit1)
# summary(fit)
# ----------------------------------------------
# pdf(regr_results_subtract_costs_by_category, height = 9, width = 12)
# for (cat in unique(dt$product_category)[1:6]){
#   data_subset = dt[product_category == cat, ]
#   fit = glm(diff_from_ref_cost ~ purchase_order_date + product_name_en, data = data_subset, family = gaussian())
#   
#   dt2 = unique(dt[product_category == cat,.(product_category, product_name_en)])
#   dt2 = as.data.table(dt2)
#   dt2[, purchase_order_date := '2014-01-01']
#   dt2[, purchase_order_date := as.Date(purchase_order_date)]
#   
#   dt2[, diff_from_ref_cost := predict(fit, newdata = dt2)]
#   dt2[, std_error := predict(fit, newdata=dt2, se.fit = TRUE)$se.fit]
#   dt2[, lower:= diff_from_ref_cost - (1.96*std_error)]
#   dt2[, upper:= diff_from_ref_cost + (1.96*std_error)]
#   
#   g = ggplot(dt2, aes(x = product_name_en, y = diff_from_ref_cost)) +
#     geom_bar(stat = 'identity') + theme_bw() +
#     geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
#     theme(strip.text = element_blank()) + 
#     labs(y = 'Difference between unit cost and reference price', x = '', title = paste0(cat, ': prediction intervals from regression results\n     diff_from_ref_cost ~ purchase_order_date + product_name_en')) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   print(g)
# }
# dev.off()
# 
# pdf(regr_results_divide_costs_by_category_log, height = 9, width = 12)
# for (cat in unique(dt$product_category)[1:6]){
#   data_subset = dt[product_category == cat, ]
#   data_subset[, log_ratio := log(unit_cost_over_ref_price)]
#   fit = glm(log_ratio ~ purchase_order_date + product_name_en, data = data_subset, family = gaussian())
#   
#   dt2 = unique(dt[product_category == cat,.(product_category, product_name_en)])
#   dt2 = as.data.table(dt2)
#   dt2[, purchase_order_date := '2014-01-01']
#   dt2[, purchase_order_date := as.Date(purchase_order_date)]
# 
#   dt2[, unit_cost_over_ref_price := predict(fit, newdata = dt2)]
#   dt2[, unit_cost_over_ref_price := exp(unit_cost_over_ref_price)]
#   dt2[, std_error := predict(fit, newdata=dt2, se.fit = TRUE)$se.fit]
#   dt2[, lower:= unit_cost_over_ref_price - (1.96*std_error)]
#   dt2[, upper:= unit_cost_over_ref_price + (1.96*std_error)]
#   
#   g = ggplot(dt2, aes(x = product_name_en, y = unit_cost_over_ref_price)) +
#     geom_bar(stat = 'identity') + theme_bw() +
#     geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
#     theme(strip.text = element_blank()) + 
#     labs(y = 'Ratio of unit cost to reference price', x = '', title = paste0(cat, ': prediction intervals from regression results\n     ratio_unit_cost_to_ref_price ~ purchase_order_date + product_name_en')) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   print(g)
# }
# dev.off()

# # make a bar graph showing the coefficients and CIs
# model_coeff = data.table(var= names(fit$coefficients), coeff = fit$coefficients)
# ci = confint(fit)
# ci = data.table(var = rownames(ci), lower = ci[,1], upper = ci[,2])
# result = merge(model_coeff, ci, by = 'var')
# result = result[!is.na(coeff)]
# 
# plot_max = max(result$upper)
# plot_min = min(result$lower)
# result[, var := gsub('factor', '', var)]
# result[, var := gsub('supplier', '', var)]
# result[, var := gsub('product_category', '', var)]
# result[, var := gsub('iso3codecountry', '', var)]
# result[, var := gsub('\\()', '', var)]
# result[, var := gsub('_', ' ', var)]
# result[, var := gsub(':', ' : ', var)]
# result[, var_wrap := str_wrap(var, width = 1)]

# result[c(1:5, 11:15), facet_by := 1]
# result[c(6:10, 16:20), facet_by := 2]
#
# g1 = ggplot(result[1:10,], aes(x = var_wrap, y = coeff)) + 
#   geom_bar(stat = 'identity') + theme_bw() +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) + 
#   facet_wrap(~ facet_by, scales = 'free_x') + 
#   ylim(plot_min,plot_max) + 
#   theme(strip.text = element_blank()) + xlab('') +
#   labs( title = 'Results of diff_from_ref_cost ~ purchase_order_date + factor(product_category) * factor(supplier)',
#         subtitle = 'Error bars show 95% C.I.') +
#   theme(text = element_text( size = 15 )) 
# 
# g2 = ggplot(result[11:20,], aes(x = var_wrap, y = coeff)) + 
#   geom_bar(stat = 'identity') + theme_bw() +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) + 
#   facet_wrap(~ facet_by, scales = 'free_x') + 
#   ylim(plot_min,plot_max) +
#   theme(strip.text = element_blank()) + xlab('') +
#   theme(text = element_text( size = 15 )) 
# 
# g3 = ggplot(result[21:27,], aes(x = var_wrap, y = coeff)) + 
#   geom_bar(stat = 'identity') + theme_bw() +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) + 
#   facet_wrap(~ facet_by, scales = 'free_x') + 
#   ylim(plot_min,plot_max) +
#   theme(strip.text = element_blank()) + xlab('') +
#   theme(text = element_text( size = 15 )) 
# 
# grid.arrange(g1, g2, nrow = 2)
# grid.arrange(g1, g2, g3, nrow = 3)
# ----------------------------------------------

# ----------------------------------------------
# Other/Off-hand analyses quickly
# # ----------------------------------------------
# # Histogram of international reference price by product category and country
# # ----------------------------------------------
# pdf(hist_ref_price_by_product_category, height = 9, width = 11)
# for ( p in unique(data$product_category) ){
#   print(ggplot(data[product_category == p, ], aes(x=unit_cost_usd - po_international_reference_price)) + geom_histogram() + facet_grid(iso3codecountry~product_category))
# }
# dev.off()
# # ----------------------------------------------
# 
# # ----------------------------------------------
# # Data checks David asked for:
# # ----------------------------------------------
# # 4.	Are we sure about the ITN costs on page 10? I believe the budget says 1$ and the program plans for $2.50, but the graphs are between 3.50 and 5.
# check = data[product_category == 'bednet_irs', ]
# check[pack_cost_usd != unit_cost_usd]
# check[, .(min = min(unit_cost_usd), max = max(unit_cost_usd)), by = 'product_name_en']
# # 6.	How often are there two different reference prices for the same commodity at the same time?
# check = data[, .(num_of_irps = length(unique(po_international_reference_price))), by = .(product_name_en, description, purchase_order_date)]
# # ----------------------------------------------
