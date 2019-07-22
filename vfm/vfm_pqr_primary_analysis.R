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
# one per commodity, y=unit cost, x=date and a horizontal line for the reference price, color by countries
commodity_missing_irp = c()
for ( p in unique(data$product_name_en) ){
  if(all(is.na(data[product_name_en == p, po_international_reference_price]))) commodity_missing_irp = c(commodity_missing_irp,p)
}

pdf(compare_ref_prices, height = 9, width = 11)
for ( p in unique(data$product_name_en) ){
  
  if(all(is.na(data[product_name_en == p, po_international_reference_price]))) commodity_missing_irp = c(commodity_missing_irp,p)
  
  print(ggplot(data[product_name_en == p, ], aes(x=purchase_order_date, y=unit_cost_usd, color=country_name)) +
    geom_point(size = 5) +
    geom_line(aes(x=purchase_order_date, y=po_international_reference_price), color = 'darkgrey', size = 1.5 ) +
    geom_point(aes(x=purchase_order_date, y=po_international_reference_price), color = 'darkgrey', size = 3 ) +
    theme_bw() +
    theme(text = element_text(size=18), legend.position = 'bottom') +
    facet_wrap( ~description, scales = "free") +
    labs(title = paste0("Comparison of unit costs and international reference prices for \n", p, " by type and country"),
          x='Date', y='Unit Cost (USD)', color="Country Name"))
}
dev.off()
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



