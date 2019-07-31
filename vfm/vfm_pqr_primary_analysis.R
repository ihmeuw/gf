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
# --------------------

# ----------------------------------------------
# Files and directories
# ----------------------------------------------
# switch for cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')

# input file
dir = paste0(j, '/Project/Evaluation/GF/vfm/')
inFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr.rds')

# output files
compare_ref_prices = paste0(dir, 'visualizations/compare_ref_price_unit_cost.pdf')
compare_ref_prices_with_size = paste0(dir, 'visualizations/compare_ref_price_unit_cost_with_size_by_volume.pdf')
compare_ref_prices_volume_on_x = paste0(dir, 'visualizations/compare_ref_price_unit_cost_with_volume_on_x.pdf')
scatterplot_unit_cost_vs_intl_ref_price = paste0(dir, 'visualizations/scatterplot_unit_cost_vs_intl_ref_price.pdf')
hist_ref_price_by_product_category = paste0(dir, 'visualizations/hist_diff_unit_cost_ref_price_by_product_category.pdf')
# outFile = paste0(dir, 'visualizations/.pdf')
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data
# ----------------------------------------------
# load
data = readRDS(inFile)
data = data[iso3codecountry %in% c('COD', 'GTM', 'SEN', 'UGA')]

# remove repetitive vars
data[, strength := NULL]
data[, product_pack_usd := NULL]
# ----------------------------------------------

# ----------------------------------------------
# graphs comparing unit costs and reference prices
# ----------------------------------------------

# commodity_missing_irp = c()
# for ( p in unique(data$product_name_en) ){
#   if(all(is.na(data[product_name_en == p, po_international_reference_price]))) commodity_missing_irp = c(commodity_missing_irp,p)
# }

# one per commodity, y=unit cost, x=date and a horizontal line for the reference price, color by countries
# pdf(compare_ref_prices, height = 9, width = 11)
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

pdf(hist_ref_price_by_product_category, height = 9, width = 11)
for ( p in unique(data$product_category) ){
  print(ggplot(data[product_category == p, ], aes(x=unit_cost_usd - po_international_reference_price)) + geom_histogram() + facet_grid(iso3codecountry~product_category))
}
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# data checks David asked for:
# ----------------------------------------------
# 4.	Are we sure about the ITN costs on page 10? I believe the budget says 1$ and the program plans for $2.50, but the graphs are between 3.50 and 5.
check = data[product_category == 'bednet_irs', ]
check[pack_cost_usd != unit_cost_usd]
check[, .(min = min(unit_cost_usd), max = max(unit_cost_usd)), by = 'product_name_en']
# 6.	How often are there two different reference prices for the same commodity at the same time?
check = data[, .(num_of_irps = length(unique(po_international_reference_price))), by = .(product_name_en, description, purchase_order_date)]
# ----------------------------------------------

# ----------------------------------------------
# 2.	Use 2018+ purchasing volume and overall spend to narrow down the long list of commodities to about 10 per country/disease.
    # a.	Circulate some graphs that show how this was narrowed down
# ----------------------------------------------
# data[, purchase_year := year(purchase_order_date)]
# data[, purchase_mo := month(purchase_order_date)]
# data[, purchase_order_date := as.yearmon(paste(purchase_year, purchase_mo, sep = "-"))]
# data[, purchase_order_date := as.Date(purchase_order_date)]
# data[, c('purchase_year', 'purchase_mo'):= NULL]
data[nb_tests_units != pack_quantity * nb_of_suom_in_pack]

setnames(data, 'nb_tests_units', 'total_units_in_order')
setnames(data, 'pack_quantity', 'nb_packs_ordered')
setnames(data, 'nb_of_suom_in_pack', 'nb_units_in_pack')

data[total_units_in_order != nb_packs_ordered * nb_units_in_pack]
data[is.na(nb_packs_ordered), nb_packs_ordered := as.integer(total_units_in_order/nb_units_in_pack)]
data[ , total_cost_order := ifelse(( !is.na(nb_packs_ordered)& !is.na(pack_cost_usd) ), nb_packs_ordered * pack_cost_usd, total_product_cost_usd)]

data[, unit_cost_usd := pack_cost_usd / nb_units_in_pack]
data[, expected_cost_by_reference := po_international_reference_price * total_units_in_order]
data[, cost_above_reference := total_cost_order - expected_cost_by_reference]

dt = data[,.( purchasing_volume = sum(), overall_spend = sum(total_cost), total_number_of_orders = .N )
          , by = .(product_name_en, product_disease, iso3codecountry)]

# ----------------------------------------------

# ----------------------------------------------
# 3.	Primary analysis For those "big spend" commodities, make a series of graphs comparing the prices incurred to reference prices over time. 
    # a.	I recommend you include all four countries on the same graph
    # b.	Once ready, circulate and present at an RT meeting
# ----------------------------------------------

# ----------------------------------------------



