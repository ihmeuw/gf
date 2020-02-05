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
inFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr_with_sept_download_data.rds')

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
# dt = data[!is.na(po_international_reference_price) & !is.na(unit_cost_usd)]
# ----------------------------------------------

# ----------------------------------------------
# unit cost - focused analysis - calculate percent change in unit cost over time.
# ----------------------------------------------
# calculate mean unit cost across countries by product for first half of time series and second half, weighted by purchase volume
data = data[!unit_cost_usd == Inf,]
data = data[!is.na(unit_cost_usd),]

data = data[purchase_order_date >= '2017-01-01',]
# use product_description, nb_units_in_pack instead of description, product_pack
data[, t:=ifelse(purchase_order_date >= '2018-04-01', 'after', 'before')] #this is the halfway point in the data
dt = data[, .(weightedMean = weighted.mean(unit_cost_usd, total_units_in_order, na.rm = TRUE)), by = .(t, product_name_en, nb_units_in_pack, product_description, product_category)]
avg_unit_cost = dcast.data.table(dt, product_category + product_name_en + nb_units_in_pack + product_description ~ t, value.var = 'weightedMean')

# calculate percent change
avg_unit_cost[, percent_change := round(((after - before)/before)*100, 2) ]
avg_unit_cost = avg_unit_cost[, .(product_category, product_name_en, nb_units_in_pack, product_description, before=round(before, 2), after=round(after, 2), percent_change)]
vol = data[, .(total_volume_purchased= sum(total_units_in_order)), .(product_category, product_name_en, nb_units_in_pack, product_description)]
avg_unit_cost = merge(avg_unit_cost, vol, by = c('product_category', 'product_name_en', 'nb_units_in_pack', 'product_description'))

avg_unit_cost_subset = avg_unit_cost[!is.na(percent_change), ]
#hist(avg_unit_cost_subset[percent_change <= 100, percent_change])
avg_unit_cost_subset[ percent_change >= 5, change:='increase']
avg_unit_cost_subset[ percent_change <= -5, change:='decrease']
avg_unit_cost_subset[ is.na(change), change:='no_change']
avg_unit_cost_subset[, .N, by = .(change)]

prod_cat = avg_unit_cost_subset[, .N, by = .(product_category, change)]
prod_cat = dcast.data.table(prod_cat, product_category ~ change)
prod_cat[ is.na(increase), increase := 0]
prod_cat[ is.na(decrease), decrease := 0]
prod_cat[ is.na(no_change), no_change := 0]
prod_cat_tot = avg_unit_cost_subset[, .N, by = .(product_category)]
prod_cat = merge(prod_cat, prod_cat_tot)
prod_cat[, percent_that_decreased := decrease/N * 100]
prod_cat[, percent_decrease_or_no_change := (decrease+no_change)/N * 100]
# ----------------------------------------------

# ----------------------------------------------
# save hiv drc testing data
# drc_hiv_tests = data[iso3codecountry == 'COD' & product_category== 'diagnostic_test_hiv']
# setorderv(drc_hiv_tests, c('purchase_order_date'))
# write.xlsx(drc_hiv_tests, paste0(dir, 'unit_cost_data/prepped_data/drc_hiv_diagnostic_tests.xlsx'))
# drc_hiv_tests = drc_hiv_tests[ purchase_order_date >= "2017-01-01" & product_name_en == 'HIV RDT and EIA']

# dt[, purchase_order_year_factor := as.factor(purchase_order_year)]
# pdf(paste0(dir, 'visualizations/comp_intlRefPrice_medianUnitCost.pdf'), height = 9, width = 11)
# ggplot( dt, aes(x=po_international_reference_price, y = po_median_unit_cost, color = purchase_order_year_factor)) +
#   geom_point(size = 3) + theme_bw() +
#   theme(text = element_text(size = 14)) + geom_abline()
# 
# ggplot( dt[po_international_reference_price <= 2,], aes(x=po_international_reference_price, y = po_median_unit_cost, color = purchase_order_year_factor)) +
#   geom_point(size = 3) + theme_bw() +
#   theme(text = element_text(size = 14)) + geom_abline()
# 
# ggplot( dt[po_international_reference_price <= .5,], aes(x=po_international_reference_price, y = po_median_unit_cost, color = purchase_order_year_factor)) +
#   geom_point(size = 3) + theme_bw() +
#   theme(text = element_text(size = 14)) + geom_abline()
# dev.off()
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
