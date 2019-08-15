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
# --------------------

# ----------------------------------------------
# Files and directories
# ----------------------------------------------
# switch for cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')

# input file
dir = paste0(j, '/Project/Evaluation/GF/vfm/')
inFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr.rds')

# output files for figures
# for primary analyses
compare_ref_prices_with_size = paste0(dir, 'visualizations/compare_ref_price_unit_cost_with_size_by_volume_(subset_to_most_common).pdf')
compare_ref_prices_volume_on_x = paste0(dir, 'visualizations/compare_ref_price_unit_cost_with_volume_on_x_(subset_to_most_common).pdf')
scatterplot_unit_cost_vs_intl_ref_price = paste0(dir, 'visualizations/scatterplot_unit_cost_vs_intl_ref_price_(subset_to_most_common).pdf')
hist_ref_price_by_product_category = paste0(dir, 'visualizations/hist_diff_unit_cost_ref_price_by_product_category.pdf')

# for narrowing down commodities
treemap_purchasing_volume_by_disease_country =paste0(dir, 'visualizations/treemap_purchasing_volume_by_disease_country.pdf')
treemap_number_of_orders_by_disease_country =paste0(dir, 'visualizations/treemap_number_of_orders_by_disease_country.pdf')
treemap_overall_spend_by_disease_country =paste0(dir, 'visualizations/treemap_overall_spend_by_disease_country.pdf')

# for narrowing down commodities, by category
treemap_purchasing_volume_by_country_category =paste0(dir, 'visualizations/treemap_purchasing_volume_by_country_category.pdf')
treemap_number_of_orders_by_country_category =paste0(dir, 'visualizations/treemap_number_of_orders_by_country_category.pdf')
treemap_overall_spend_by_country_category =paste0(dir, 'visualizations/treemap_overall_spend_by_country_category.pdf')
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data
# ----------------------------------------------
# load
data = readRDS(inFile)
data = data[iso3codecountry %in% c('COD', 'GTM', 'SEN', 'UGA')]

# IRP checks
unique(data[po_international_reference_price < (0.05*unit_cost_usd), .(po_international_reference_price, unit_cost_usd)])

tot = data[, .(total_rows = .N), by = .(product_name_en, product_category)]
na = data[is.na(po_international_reference_price)|po_international_reference_price == 0|(po_international_reference_price > (0.05*unit_cost_usd)), 
          .(irp_wrong_or_missing = .N), 
          by = .(product_name_en, product_category)]

check = merge(tot, na,by = c('product_name_en', 'product_category'), all = TRUE)
check = check[ !product_category %in% c('bednet', 'diagnostic_test_tb', 'diagnostic_test_other', 'irs')]
sum(check$irp_wrong_or_missing, na.rm = TRUE) / sum(check$total_rows) # = 14.7% NA outside of the above categories
check[total_rows == irp_wrong_or_missing, all_irp_wrong_or_missing := TRUE]
# ----------------------------------------------

# ----------------------------------------------
# Narrow down to the most common products by country / disease to about 10 per country / disease
# Try it using purchasing volume, overall spend, and total number of orders
# ----------------------------------------------
# Calculate purchasing volume, overall spend, and number of orders from 2015 on
# first, David said we don't care about specific brand of bednet, so make it so those will sum over product_name here:
dt = data[product_category == 'bednet', product_name_en := 'bednet' ]

# then, sum by country, product_category and product_name
dt = dt[purchase_order_date >= '2015-01-01',.(purchasing_volume = sum(total_units_in_order), 
                                                overall_spend = sum(total_cost_order), 
                                                total_number_of_orders = .N ),
          by = .(product_name_en, product_category, product_disease, iso3codecountry)]

dt[is.na(product_disease), product_disease := 'other']
dt[, product_disease := as.factor(product_disease)]
dt[, overall_spend_in_millions := overall_spend/1000000]

# Subset dt to the top 10 commodities per country/disease:
subset = dt[product_disease != 'other']
# subset_by_vol = setorder(subset, -purchasing_volume)[, head(.SD, 5L), keyby =c('iso3codecountry', 'product_disease')]
# subset_by_orders = setorder(subset, -total_number_of_orders)[, head(.SD, 5L), keyby =c('iso3codecountry', 'product_disease')]
# subset_by_spend = setorder(subset, -overall_spend)[, head(.SD, 5L), keyby =c('iso3codecountry', 'product_disease')]
subset_by_vol = setorderv(subset, c('iso3codecountry', 'product_disease', 'purchasing_volume'), c(1, 1, -1))[, head(product_name_en, 7L), c('iso3codecountry', 'product_category')]
subset_by_spend = setorderv(subset, c('iso3codecountry', 'product_disease', 'overall_spend'), c(1, 1, -1))[, head(product_name_en, 7L), c('iso3codecountry', 'product_category')]
# subset_by_orders = setorderv(subset, c('iso3codecountry', 'product_disease', 'total_number_of_orders'), c(1, 1, -1))[, head(product_name_en, 7L), c('iso3codecountry', 'product_category')]

setnames(subset_by_vol, 'V1', 'product_name_en')
setnames(subset_by_spend, 'V1', 'product_name_en')
# setnames(subset_by_orders, 'V1', 'product_name_en')

subset_by_vol[, by_vol_purchased := TRUE] 
subset_by_spend[, by_tot_spend := TRUE] 
# subset_by_orders[, by_num_orders := TRUE] 

# subset = merge(subset_by_vol, subset_by_orders, all = TRUE, by = c('iso3codecountry', 'product_disease', 'product_name_en'))
subset = merge(subset_by_vol, subset_by_spend, all = TRUE, by = c('iso3codecountry', 'product_category', 'product_name_en'))
subset = merge(subset, data, by = c('iso3codecountry', 'product_disease', 'product_name_en'), all.x = TRUE)

dt = dt[!product_category %in% c('irs', 'diagnostic_test_other')]
dt[, product_category := as.factor(product_category)]
# Make a color palette so colors are the same for disease across different plots (even if some 'levels' are removed - for ex, SEN does not have 'other')
colors = brewer.pal(8, 'Set2')
names(colors) = levels(dt$product_category)
fillScale = scale_fill_manual(name = 'product_category', values = colors)

# # Treemap (one per country) showing purchasing volume by disease and product with overall spend as the gradient
# pdf(treemap_purchasing_volume_by_disease_country, height = 10, width = 15)
# for ( x in unique(dt$iso3codecountry) ){
#   g = ggplot(dt[ iso3codecountry == x, ], aes(area = purchasing_volume, fill = product_category, subgroup = product_category, subgroup2 = product_name_en)) + 
#     geom_treemap(aes(alpha = overall_spend_in_millions)) +
#     scale_alpha_continuous(range = c(0.2, 1)) + 
#     geom_treemap_subgroup_border() +
#     geom_treemap_text(aes(label = product_name_en), fontface = 'italic', place = 'top', 
#                       reflow = TRUE, padding.x = unit(3, 'mm'), padding.y = unit(3, 'mm')) +
#     geom_treemap_subgroup2_border(colour = "white", size = 2) +
#     geom_treemap_subgroup_border(colour = "white", size = 10) +
#     fillScale +
#     ggtitle(paste0('Purchasing volume of different commodities by disease ', x, ', 2015 - 2018')) +
#     guides(fill = guide_legend(title="Disease"), alpha = guide_legend(title = 'Overall Spend (in millions)')) + 
#     theme( plot.title = element_text(size = 24), legend.title = element_text(size = 18), legend.text = element_text(size = 16)) 
#   print(g)
# }
# dev.off()
# 
# # Treemap (one per country) showing number of orders by disease and product with overall spend as the gradient
# pdf(treemap_number_of_orders_by_disease_country, height = 10, width = 15)
# for ( x in unique(dt$iso3codecountry) ){
#   g = ggplot(dt[ iso3codecountry == x, ], aes(area = total_number_of_orders, fill = product_category, subgroup = product_category, subgroup2 = product_name_en)) + 
#     geom_treemap(aes(alpha = overall_spend_in_millions)) +
#     scale_alpha_continuous(range = c(0.2, 1)) + 
#     geom_treemap_subgroup_border() +
#     geom_treemap_text(aes(label = product_name_en), fontface = 'italic', place = 'top', 
#                       reflow = TRUE, padding.x = unit(3, 'mm'), padding.y = unit(3, 'mm')) +
#     geom_treemap_subgroup2_border(colour = "white", size = 2) +
#     geom_treemap_subgroup_border(colour = "white", size = 10) +
#     fillScale +
#     ggtitle(paste0('Total number of orders by commodity and disease in ', x, ', 2015 - 2018')) +
#     guides(fill = guide_legend(title="Disease"), alpha = guide_legend(title = 'Overall Spend (in millions)')) + 
#     theme( plot.title = element_text(size = 24), legend.title = element_text(size = 18), legend.text = element_text(size = 16)) 
#   print(g)
# }
# dev.off()
# 
# # Treemap (one per country) showing overall spend by disease and product [***should this have a gradient?]
# pdf(treemap_overall_spend_by_disease_country, height = 10, width = 15)
# for ( x in unique(dt$iso3codecountry) ){
#   g = ggplot(dt[ iso3codecountry == x, ], aes(area = overall_spend, fill = product_category, subgroup = product_category, subgroup2 = product_name_en)) + 
#     geom_treemap(alpha = 0.75) +
#     scale_alpha_continuous(range = c(0.2, 1)) + 
#     geom_treemap_subgroup_border() +
#     geom_treemap_text(aes(label = product_name_en), fontface = 'italic', place = 'top', 
#                       reflow = TRUE, padding.x = unit(3, 'mm'), padding.y = unit(3, 'mm')) +
#     geom_treemap_subgroup2_border(colour = "white", size = 2) +
#     geom_treemap_subgroup_border(colour = "white", size = 10) +
#     fillScale +
#     ggtitle(paste0('Overall spend by commodity and disease in ', x, ', 2015 - 2018')) +
#     guides(fill = guide_legend(title="Disease")) + 
#     theme( plot.title = element_text(size = 24), legend.title = element_text(size = 18), legend.text = element_text(size = 16)) 
#   print(g)
# }
# dev.off()

dt[, iso3codecountry := as.factor(iso3codecountry)]
# Make a color palette so colors are the same for disease across different plots (even if some 'levels' are removed - for ex, SEN does not have 'other')
colors = brewer.pal(4, 'Set2')
names(colors) = levels(dt$iso3codecountry)
fillScale = scale_fill_manual(name = 'iso3codecountry', values = colors)

pdf(treemap_purchasing_volume_by_country_category, height = 10, width = 15)
for ( x in unique(dt$product_category) ){
  g = ggplot(dt[ product_category == x, ], aes(area = purchasing_volume, fill = iso3codecountry, subgroup = iso3codecountry, subgroup2 = product_name_en)) + 
    geom_treemap(aes(alpha = overall_spend_in_millions)) +
    scale_alpha_continuous(range = c(0.2, 1)) + 
    geom_treemap_subgroup_border() +
    geom_treemap_text(aes(label = product_name_en), fontface = 'italic', place = 'top', 
                      reflow = TRUE, padding.x = unit(3, 'mm'), padding.y = unit(3, 'mm')) +
    geom_treemap_subgroup2_border(colour = "white", size = 2) +
    geom_treemap_subgroup_border(colour = "white", size = 10) +
    fillScale +
    ggtitle(paste0(x, ': purchasing volume of different commodities by country, 2015 - 2018')) +
    guides(fill = guide_legend(title="Disease"), alpha = guide_legend(title = 'Overall Spend (in millions)')) + 
    theme( plot.title = element_text(size = 24), legend.title = element_text(size = 18), legend.text = element_text(size = 16)) 
  print(g)
}
dev.off()

pdf(treemap_number_of_orders_by_country_category, height = 10, width = 15)
for ( x in unique(dt$product_category) ){
  g = ggplot(dt[ product_category == x, ], aes(area = total_number_of_orders, fill = iso3codecountry, subgroup = iso3codecountry, subgroup2 = product_name_en)) + 
    geom_treemap(aes(alpha = overall_spend_in_millions)) +
    scale_alpha_continuous(range = c(0.2, 1)) + 
    geom_treemap_subgroup_border() +
    geom_treemap_text(aes(label = product_name_en), fontface = 'italic', place = 'top', 
                      reflow = TRUE, padding.x = unit(3, 'mm'), padding.y = unit(3, 'mm')) +
    geom_treemap_subgroup2_border(colour = "white", size = 2) +
    geom_treemap_subgroup_border(colour = "white", size = 10) +
    fillScale +
    ggtitle(paste0(x, ': total # orders of different commodities by country, 2015 - 2018')) +
    guides(fill = guide_legend(title="Disease"), alpha = guide_legend(title = 'Overall Spend (in millions)')) + 
    theme( plot.title = element_text(size = 24), legend.title = element_text(size = 18), legend.text = element_text(size = 16)) 
  print(g)
}
dev.off()

pdf(treemap_overall_spend_by_country_category, height = 10, width = 15)
for ( x in unique(dt$product_category) ){
  g = ggplot(dt[ product_category == x, ], aes(area = overall_spend, fill = iso3codecountry, subgroup = iso3codecountry, subgroup2 = product_name_en)) + 
    geom_treemap(alpha = 0.75) +
    scale_alpha_continuous(range = c(0.2, 1)) + 
    geom_treemap_subgroup_border() +
    geom_treemap_text(aes(label = product_name_en), fontface = 'italic', place = 'top', 
                      reflow = TRUE, padding.x = unit(3, 'mm'), padding.y = unit(3, 'mm')) +
    geom_treemap_subgroup2_border(colour = "white", size = 2) +
    geom_treemap_subgroup_border(colour = "white", size = 10) +
    fillScale +
    ggtitle(paste0(x, ': overall spend on different commodities by country, 2015 - 2018')) +
    guides(fill = guide_legend(title="Disease")) + 
    theme( plot.title = element_text(size = 24), legend.title = element_text(size = 18), legend.text = element_text(size = 16)) 
  print(g)
}
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# Graphs comparing unit costs and international reference prices
# ----------------------------------------------
pdf(compare_ref_prices_with_size, height = 9, width = 11)
for ( p in unique(data$product_name_en) ){
  print(ggplot(data[product_name_en == p, ], aes(x=purchase_order_date, y=unit_cost_usd, color=country_name, size = nb_tests_units)) +
    geom_point() +
    geom_line(aes(x=purchase_order_date, y=po_international_reference_price), color = 'darkgrey', size = 1.5 ) +
    geom_point(aes(x=purchase_order_date, y=po_international_reference_price), color = 'darkgrey', size = 3 ) +
    theme_bw() +
    theme(text = element_text(size=14)) +
    facet_wrap( ~description, scales = "free") +
    scale_size_continuous(labels = comma) +
    labs(title = paste0("Comparison of unit costs and international reference prices for ", p), 
         subtitle = '(Gray points represent the international reference price over time)',
         x='Date', y='Unit Cost (USD)', color="Country Name", size = 'Volume Purchased'))
}
dev.off()

pdf(compare_ref_prices_volume_on_x, height = 9, width = 11)
for ( p in unique(data$product_name_en) ){
  print(ggplot(data[product_name_en == p, ], aes(x=nb_tests_units, y=unit_cost_usd, color=country_name)) +
          geom_point(size = 4.5) +
          geom_line(aes(x=nb_tests_units, y=po_international_reference_price), color = 'darkgrey', size = 1.5 ) +
          geom_point(aes(x=nb_tests_units, y=po_international_reference_price), color = 'darkgrey', size = 3 ) +
          theme_bw() +
          theme(text = element_text(size=14)) +
          facet_wrap( ~description, scales = "free") +
          scale_x_continuous(labels = comma) +
          theme(legend.position = 'bottom') + 
          labs(title = paste0("Comparison of unit costs and international reference prices for ", p), 
               subtitle = '(Gray points represent the international reference price over time)',
               x='Units Purchased', y='Unit Cost (USD)', color="Country Name"))
}
dev.off()

pdf(scatterplot_unit_cost_vs_intl_ref_price, height = 9, width = 11)
for ( p in unique(data$product_name_en) ){
    print(ggplot(data[product_name_en == p, ], aes(x=po_international_reference_price, y=unit_cost_usd, color=country_name, size=nb_tests_units)) +
            geom_point() + geom_abline() + 
            theme_bw() +
            theme(text = element_text(size=14)) +
            facet_wrap( ~description) +
            scale_size_continuous(labels = comma) +
            labs(title = paste0("Comparison of unit costs and international reference prices for ", p), 
                 x='International Reference Price', y='Unit Cost (USD)', color="Country Name", size = 'Volume Purchased'))
}
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# Histogram of international reference price by product category and country
# ----------------------------------------------
pdf(hist_ref_price_by_product_category, height = 9, width = 11)
for ( p in unique(data$product_category) ){
  print(ggplot(data[product_category == p, ], aes(x=unit_cost_usd - po_international_reference_price)) + geom_histogram() + facet_grid(iso3codecountry~product_category))
}
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# Data checks David asked for:
# ----------------------------------------------
# 4.	Are we sure about the ITN costs on page 10? I believe the budget says 1$ and the program plans for $2.50, but the graphs are between 3.50 and 5.
check = data[product_category == 'bednet_irs', ]
check[pack_cost_usd != unit_cost_usd]
check[, .(min = min(unit_cost_usd), max = max(unit_cost_usd)), by = 'product_name_en']
# 6.	How often are there two different reference prices for the same commodity at the same time?
check = data[, .(num_of_irps = length(unique(po_international_reference_price))), by = .(product_name_en, description, purchase_order_date)]
# ----------------------------------------------
