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
inFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr.rds')
inFile_new_pqr = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr_updated_09_2019.rds')

# output files 
outFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr_with_sept_download_data.rds')
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data
# ----------------------------------------------
data = readRDS(inFile)
data = data[iso3codecountry %in% c('COD', 'GTM', 'SEN', 'UGA')]
data = data[, .(country_name, iso3codecountry, grant_name, grant_id, grant_start_date, grant_end_date, 
                purchase_order_date, scheduled_delivery_date, actual_delivery_date,
                product_name_en, product_category, product_pack, description, product_description,
                nb_packs_ordered, nb_units_in_pack, total_units_in_order,
                total_product_cost_usd, total_cost_order, pack_cost_usd, unit_cost_usd,
                po_median_unit_cost, po_international_reference_price, 
                supplier, pr_name, pr_type_name, primary_key, data_source = 'april_download')]

# add in new data from the most recent download.
data2 = readRDS(inFile_new_pqr)
data2 = data2[iso3codecountry %in% c('COD', 'GTM', 'SEN', 'UGA')]
data2 = data2[, .(country_name, iso3codecountry, grant_name, 
                  purchase_order_date, scheduled_delivery_date, actual_delivery_date,
                  'product_name_en'=product_name, product_category, product_pack, description, product_description, 
                  nb_packs_ordered, nb_units_in_pack, total_units_in_order,
                  total_product_cost_usd, total_cost_order, pack_cost_usd, unit_cost_usd,
                  supplier, supplier_agent_manufacturer_intermediatry, primary_key, data_source = 'sept_download')]
# ----------------------------------------------

# ----------------------------------------------
# combine data sources
# ----------------------------------------------
# get the orders in the new data that were not in the old data - note some of these are not just after the last date in the
# old data... the oldest new order is from 2014?! so, add in the new orders using priamry key!
rm_from_data2 = unique(data$primary_key)
add_data = data2[ !primary_key %in% rm_from_data2 ]

# add the datasets together with rbind() to get all orders
data = rbindlist(list(data, add_data), use.names = TRUE, fill = TRUE)

# use primary key to add back in the this variable? - it seems to be a more complete version of the supplier/procurement mechanism variable!
data[, supplier_agent_manufacturer_intermediatry := NULL]
supplier_data = unique(data2[, .(primary_key, supplier_agent_manufacturer_intermediatry)])
data = merge(data, supplier_data, by = 'primary_key', all = TRUE)
# ----------------------------------------------

# ----------------------------------------------
# set reference price in the new data where it exists previously
# ----------------------------------------------
# standardize product names:
data[ product_name_en == "Ethambutol+Isoniazid+Pyrazinamide+Rifampicin (RHZE", product_name_en := "Ethambutol+Isoniazid+Pyrazinamide+Rifampicin (RHZE)"]
# the new orders don't have reference price - if there is an order without reference price, lookup if there is a
# reference price for the same product/description/pack size from a previous order - if there is, take the most recent
# reference price to when that order was purchased (max date that is less than the given order's date) and make that the
# reference price.  Otherwise, leave it NA. 
counter = 1
for (key in unique(data[data_source == 'sept_download', primary_key])) {
  date = data[ primary_key == key, purchase_order_date]
  prod = data[ primary_key == key, product_name_en]
  dose = data[ primary_key == key, description]
  pack = data[ primary_key == key, nb_units_in_pack]
  
  all_potential_data = data[ product_name_en == prod & description == dose & nb_units_in_pack == pack, ]

  if(nrow(all_potential_data)==1) next 
  
  if(all(is.na(all_potential_data$po_international_reference_price))) next

  max_date = all_potential_data[ !is.na(po_international_reference_price), max(purchase_order_date)]
  set_irp = all_potential_data[ purchase_order_date == max_date, po_international_reference_price]
  if(length(set_irp) > 1) {
    #print(set_irp)
    set_irp = set_irp[!is.na(set_irp)]
    #print(set_irp)
  }
  data[primary_key == key, po_international_reference_price := set_irp]
  print(counter)
  counter = counter + 1
}
# ----------------------------------------------

# ----------------------------------------------
# other set up for analysis
# ----------------------------------------------
# David said we don't care about specific brand of bednet, so make it so those will sum over product_name here:
data[product_category == 'bednet', product_name_en := 'bednet' ]

# make a separate "product category" for first and second line TB drugs:
first_line = c("Rifampicin" , "Pyrazinamide", "Ethambutol+Isoniazid+Pyrazinamide+Rifampicin (RHZE)", "Isoniazid", 
               "Ethambutol+Isoniazid+Rifampicin - FDC", "Isoniazid+Pyrazinamide+Rifampicin - FDC", "Isoniazid+Rifampicin - FDC", 
               "Ethambutol+Isoniazid - FDC", "Ethambutol")
second_line = c("Ofloxacin", "Levofloxacin", "Moxifloxacin", "Cycloserine", "Protionamide", "Amikacin", "Ethionamide", "Kanamycin",
                "Capreomycin", "Linezolid", "Bedaquiline", "Meropenem", "Clofazimine", "Amoxicillin+Clavulanate - FDC", "Streptomycin", "PAS Sodium", "Delamanid")
other = c("Water for injection")

data[ product_name_en %in% first_line, sub_product_category := "first_line"]
data[ product_name_en %in% second_line, sub_product_category := "second_line"]
data[ product_name_en %in% other, sub_product_category := "other"]

data[ product_category  == 'anti_tb_medicine', product_category := paste(product_category, sub_product_category, sep = "_") ]

# adjust IRPs and unit costs that are wrong:
data[unit_cost_usd == 0, unit_cost_usd := NA]
data[po_international_reference_price == 0, po_international_reference_price := NA ]

data[, diff_from_ref_cost := unit_cost_usd - po_international_reference_price]
data[, unit_cost_over_ref_price := unit_cost_usd / po_international_reference_price]
data[, purchase_order_year := year(purchase_order_date)]
# ----------------------------------------------

# ----------------------------------------------
# save data 
# ----------------------------------------------
saveRDS(data, outFile)
# ----------------------------------------------