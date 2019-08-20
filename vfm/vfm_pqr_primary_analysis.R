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
# --------------------

# ----------------------------------------------
# Files and directories
# ----------------------------------------------
# switch for cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')

# input file
dir = paste0(j, '/Project/Evaluation/GF/vfm/')
inFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr.rds')
outFile_codebook = paste0(dir, 'unit_cost_data/prepped_data/commodity_codebook.csv')

# output files for figures
# for primary analyses
compare_ref_prices_with_size = paste0(dir, 'visualizations/compare_ref_price_unit_cost_with_size_by_volume_(subset_to_most_common).pdf')
compare_ref_prices_volume_on_x = paste0(dir, 'visualizations/compare_ref_price_unit_cost_with_volume_on_x_(subset_to_most_common).pdf')
scatterplot_unit_cost_vs_intl_ref_price = paste0(dir, 'visualizations/scatterplot_unit_cost_vs_intl_ref_price_(subset_to_most_common).pdf')
hist_ref_price_by_product_category = paste0(dir, 'visualizations/hist_diff_unit_cost_ref_price_by_product_category.pdf')
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data
# ----------------------------------------------
data = readRDS(inFile)
data = data[iso3codecountry %in% c('COD', 'GTM', 'SEN', 'UGA')]
# ----------------------------------------------

# ----------------------------------------------
# Narrow down to the most common products by country / disease to about 10 per country / disease
# Try it using purchasing volume, overall spend, and total number of orders
# ----------------------------------------------
# David said we don't care about specific brand of bednet, so make it so those will sum over product_name here:
dt = data[product_category == 'bednet', product_name_en := 'bednet' ]

# make a separate "product category" for first and second line TB drugs:
first_line = c("Rifampicin" , "Pyrazinamide", "Ethambutol+Isoniazid+Pyrazinamide+Rifampicin (RHZE", "Isoniazid", 
               "Ethambutol+Isoniazid+Rifampicin - FDC", "Isoniazid+Pyrazinamide+Rifampicin - FDC", "Isoniazid+Rifampicin - FDC", 
               "Ethambutol+Isoniazid - FDC", "Ethambutol")
second_line = c("Ofloxacin", "Levofloxacin", "Moxifloxacin", "Cycloserine", "Protionamide", "Amikacin", "Ethionamide", "Kanamycin",
                "Capreomycin", "Linezolid", "Bedaquiline", "Meropenem", "Clofazimine", "Amoxicillin+Clavulanate - FDC", "Streptomycin", "PAS Sodium")
other = c("Water for injection")

dt = dt[ product_name_en %in% first_line, sub_product_category := "first_line"]
dt = dt[ product_name_en %in% second_line, sub_product_category := "second_line"]
dt = dt[ product_name_en %in% other, sub_product_category := "other"]

dt[ product_category == 'anti_tb_medicine', product_category := paste(product_category, sub_product_category, sep = "_") ]
# ----------------------------------------------

# ----------------------------------------------
# subset data to only "most common" commodities 
# ----------------------------------------------
subset_with_codebook = read.csv(outFile_codebook) # make manual changes to "included_in_analysis" column based on treemaps and then read that back in

products_to_include_in_analysis = subset_with_codebook[ included_in_anlaysis == TRUE, product_name_en ]
subset = data[ product_name_en %in% products_to_include_in_analysis, ]

subset_less_common = data[ !product_name_en %in% products_to_include_in_analysis, ]
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
