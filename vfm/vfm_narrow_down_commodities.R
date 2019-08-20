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

# for narrowing down commodities, by country 
treemap_purchasing_volume_by_disease_country =paste0(dir, 'visualizations/treemap_purchasing_volume_by_disease_country.pdf')
treemap_number_of_orders_by_disease_country =paste0(dir, 'visualizations/treemap_number_of_orders_by_disease_country.pdf')
treemap_overall_spend_by_disease_country =paste0(dir, 'visualizations/treemap_overall_spend_by_disease_country.pdf')

# for narrowing down commodities, by product category
treemap_purchasing_volume_by_country_category_desc =paste0(dir, 'visualizations/treemap_purchasing_volume_by_country_category_description.pdf')
treemap_number_of_orders_by_country_category =paste0(dir, 'visualizations/treemap_number_of_orders_by_country_category.pdf')
treemap_overall_spend_by_country_category =paste0(dir, 'visualizations/treemap_overall_spend_by_country_category.pdf')

# irp_value_check
irp_value_check = paste0(dir, 'visualizations/ts_hist_check_irp_values.pdf')
irp_value_check_subset_commodities = paste0(dir, 'visualizations/ts_hist_check_irp_values_subset_most_common_commodities.pdf')
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

# # make a code book of all commodities, whether or not will use them in analysis, and whether or not they have a ref price
# codebook = unique(dt[, .(product_name_en, product_category, po_international_reference_price, description)])
# codebook = codebook[, has_irp := !all(is.na(po_international_reference_price)), by = .(product_name_en, product_category, description)]
# codebook = unique(codebook[,.(product_name_en, product_category, description, has_irp)])
# codebook[, included_in_analysis := TRUE]
# write.csv(codebook, outFile_codebook)

# Calculate purchasing volume, overall spend, and number of orders from 2015 on
# sum by country, product_category and product_name
dt = dt[purchase_order_date >= '2015-01-01',
        .(purchasing_volume = sum(total_units_in_order, na.rm = TRUE), 
          overall_spend = sum(total_cost_order, na.rm = TRUE), 
          total_number_of_orders = .N, na.rm = TRUE ),
          by = .(product_name_en, product_category, product_disease, description, iso3codecountry)]

dt[is.na(product_disease), product_disease := 'other']
dt[, product_disease := as.factor(product_disease)]
dt[, overall_spend_in_millions := overall_spend/1000000]

# # Subset dt to the top 10 commodities per country/disease:
# subset = dt[product_disease != 'other']
# # subset_by_vol = setorder(subset, -purchasing_volume)[, head(.SD, 5L), keyby =c('iso3codecountry', 'product_disease')]
# # subset_by_orders = setorder(subset, -total_number_of_orders)[, head(.SD, 5L), keyby =c('iso3codecountry', 'product_disease')]
# # subset_by_spend = setorder(subset, -overall_spend)[, head(.SD, 5L), keyby =c('iso3codecountry', 'product_disease')]
# subset_by_vol = setorderv(subset, c('iso3codecountry', 'product_disease', 'purchasing_volume'), c(1, 1, -1))[, head(product_name_en, 7L), c('iso3codecountry', 'product_category')]
# subset_by_spend = setorderv(subset, c('iso3codecountry', 'product_disease', 'overall_spend'), c(1, 1, -1))[, head(product_name_en, 7L), c('iso3codecountry', 'product_category')]
# # subset_by_orders = setorderv(subset, c('iso3codecountry', 'product_disease', 'total_number_of_orders'), c(1, 1, -1))[, head(product_name_en, 7L), c('iso3codecountry', 'product_category')]
# 
# setnames(subset_by_vol, 'V1', 'product_name_en')
# setnames(subset_by_spend, 'V1', 'product_name_en')
# # setnames(subset_by_orders, 'V1', 'product_name_en')
# 
# subset_by_vol[, by_vol_purchased := TRUE] 
# subset_by_spend[, by_tot_spend := TRUE] 
# # subset_by_orders[, by_num_orders := TRUE] 
# 
# # subset = merge(subset_by_vol, subset_by_orders, all = TRUE, by = c('iso3codecountry', 'product_disease', 'product_name_en'))
# subset = merge(subset_by_vol, subset_by_spend, all = TRUE, by = c('iso3codecountry', 'product_category', 'product_name_en'))
# subset = merge(subset, data, by = c('iso3codecountry', 'product_disease', 'product_name_en'), all.x = TRUE)
# 
# dt = dt[!product_category %in% c('irs', 'diagnostic_test_other')]
# dt[, product_category := as.factor(product_category)]
# # Make a color palette so colors are the same for disease across different plots (even if some 'levels' are removed - for ex, SEN does not have 'other')
# colors = brewer.pal(8, 'Set2')
# names(colors) = levels(dt$product_category)
# fillScale = scale_fill_manual(name = 'product_category', values = colors)
# 
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

# REMAKE TREEMAPS - so there is a page for each product category and the first subgroup is by country. 
# This can show us within a given product category, which specific products are most common within and among countries 
# Note: This was orgiinally done before TB drug commodities were reclassified into first- and second-line drugs. 

# Make a color palette so colors are the same for disease across different plots (even if some 'levels' are removed - for ex, SEN does not have 'other')
dt[, iso3codecountry := as.factor(iso3codecountry)]
dt[, product_name_en := as.factor(product_name_en)]
dt[, description := as.factor(description)]

colors1 = brewer.pal(8, 'Set1')
colors2 = brewer.pal(8, 'Set2')
colors3 = brewer.pal(8, 'Set3')
colors = c(colors1, colors2, colors3)

uniq = unique(dt[, .(iso3codecountry, product_category)])
setorder(uniq, product_category)

pdf(treemap_purchasing_volume_by_country_category_desc, height = 10, width = 15)

for ( row in 1:nrow(uniq) ){
  x = uniq[row, product_category]
  y = uniq[row, iso3codecountry]
  
  names(colors) = unique(dt[ product_category == x, product_name_en])
  fillScale = scale_fill_manual(name = 'product_name_en', values = colors)

  g = ggplot(dt[ product_category == x & iso3codecountry == y, ], aes(area = purchasing_volume, fill = product_name_en, subgroup = product_name_en, subgroup2 = description)) + 
    geom_treemap(aes(alpha = 0.75)) +
    scale_alpha_continuous(range = c(0.2, 1)) + 
    geom_treemap_subgroup_border() +
    geom_treemap_text(aes(label = description), fontface = 'italic', place = 'top', padding.x = unit(3, 'mm'), padding.y = unit(3, 'mm')) +
    geom_treemap_subgroup2_border(colour = "white", size = 2) +
    geom_treemap_subgroup_border(colour = "white", size = 10) +
    fillScale +
    ggtitle(paste0(y, ", ", x, ': relative purchasing volume of different commodities, 2015 - 2018')) +
    guides(fill = guide_legend(title="Product")) + 
    theme( plot.title = element_text(size = 24), legend.title = element_text(size = 18), legend.text = element_text(size = 16)) 
  print(g)
  
  g2 = ggplot(dt[ product_category == x & iso3codecountry == y, ], aes(area = overall_spend, fill = product_name_en, subgroup = product_name_en, subgroup2 = description)) +
    geom_treemap(alpha = 0.75) +
    scale_alpha_continuous(range = c(0.2, 1)) +
    geom_treemap_subgroup_border() +
    geom_treemap_text(aes(label = description), fontface = 'italic', place = 'top',
                      reflow = TRUE, padding.x = unit(3, 'mm'), padding.y = unit(3, 'mm')) +
    geom_treemap_subgroup2_border(colour = "white", size = 2) +
    geom_treemap_subgroup_border(colour = "white", size = 10) +
    fillScale +
    ggtitle(paste0(y, ", ",x, ': overall spend on different commodities by country, 2015 - 2018')) +
    guides(fill = guide_legend(title="Product")) +
    theme( plot.title = element_text(size = 24), legend.title = element_text(size = 18), legend.text = element_text(size = 16))
  print(g2)  
}
dev.off()


dt = dt[purchase_order_date >= '2015-01-01',.(purchasing_volume = sum(total_units_in_order), 
                                              overall_spend = sum(total_cost_order), 
                                              total_number_of_orders = .N ),
        by = .(product_name_en, product_category, product_disease, iso3codecountry)]
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
    guides(fill = guide_legend(title="Country"), alpha = guide_legend(title = 'Overall Spend (in millions)')) + 
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
    guides(fill = guide_legend(title="Country"), alpha = guide_legend(title = 'Overall Spend (in millions)')) +
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
    guides(fill = guide_legend(title="Country")) +
    theme( plot.title = element_text(size = 24), legend.title = element_text(size = 18), legend.text = element_text(size = 16))
  print(g)
}
dev.off()
# ----------------------------------------------


# ----------------------------------------------
# IRP checks - make histograms of IRP by commodity, and time series to show if there are duplicates by date/commodity but with different IRPs. 
# ----------------------------------------------
data[, purchase_order_year := as.numeric(year(purchase_order_date))]
data[, purchase_order_month := as.numeric(month(purchase_order_date))]
check_irp = data[, unique(po_international_reference_price), by = .(product_name_en, purchase_order_year, purchase_order_month, description)]
check_irp[, order_date := as.yearmon(paste(check_irp$purchase_order_year, check_irp$purchase_order_month), "%Y%m")]
check_irp[, order_date := as.Date(order_date)]

check_irp[, more_than_one_irp := FALSE]
dups = check_irp[duplicated(check_irp[, .(product_name_en, description, order_date)]),]
for( row in 1:nrow(dups) ){
  product = dups[row, product_name_en]
  date = dups[row, order_date]
  check_irp[product_name_en == product & order_date == date, more_than_one_irp := TRUE]
  if ( check_irp[product_name_en == product & order_date == date & !is.na(V1), .N] <= 1 ) {
    check_irp[product_name_en == product & order_date == date, more_than_one_irp := FALSE]
  }
}
check_irp[, more_than_one_irp:=as.factor(more_than_one_irp)]
colors = c('cadetblue3', 'coral2')
names(colors) = levels(check_irp$more_than_one_irp)

check_irp[, description := as.character(description)]
check_irp[, product_name_en := as.character(product_name_en)]

pdf(irp_value_check_subset_commodities, height = 8, width = 11)
for( cat in unique(data$product_category)){
  for( x in unique(data[product_category == cat, product_name_en]) ){
    g1 = ggplot( check_irp[ product_name_en == x, ], aes(x = order_date, y = V1) ) +
      geom_point(size = 3) + theme_bw() + theme(text = element_text(size = 16)) + 
      labs( title = paste0("Time series of international reference prices for ", x), x= 'Order date (year, month)', y = 'International reference price (USD per unit)') +
      ylim(0, NA) +
      facet_wrap(~description, scales = 'free_x', labeller = label_wrap_gen(multi_line = TRUE))
    print(g1)
    for( y in unique(data[product_name_en == x, description])){
      if( length(unique(data[ product_name_en == x & description == y, po_international_reference_price]))==1 ) next
      g2 = ggplot( data[ product_name_en == x & description == y, ], aes(x = po_international_reference_price) ) + geom_histogram() + 
        theme_bw() + theme(text = element_text(size = 16)) + 
        labs( title = paste0("International reference prices for ", x, " ", y), x= 'International reference price (USD per unit)')
      print(g2)
    }
  }}
dev.off()
# ----------------------------------------------