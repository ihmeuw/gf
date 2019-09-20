
# ----------------------------------------------
# Audrey Batzel
# 
# 08/30/19
# Analyze PQR products in comparison to their budgeted costs
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(readxl)
# --------------------

# ----------------------------------------------
# Files and directories
# ----------------------------------------------
# switch for cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/vfm/')

# input file
inFile_pqr = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr.rds')
inFile_budget = paste0(dir, 'unit_cost_data/gf_budgets/guatemala_tb_hand_extraction.xlsx')

# output files
outFile = paste0(dir, 'visualizations/PQR/comparison_of_budgeted_cost_and_actual_cost_gtm_tb.pdf')
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data budget data
# ----------------------------------------------
budget = as.data.table(read_excel(inFile_budget))

products = c('NA', 'NA', 'Cycloserine', 'Ethionamide', 'Levofloxacin', 'Kanamycin', 'Capreomycin', 'Moxifloxacin', 'PAS Sodium', 'Linezolid', 'Clofazimine', 'Cilastatin-Inipenem', 'Genexpert tests', 'Genexpert equipment', 'Genexpert equipment')
doses = c('NA', 'NA', '250mg Capsule', '250mg tab', '500mg tab', '1g/4ml inj', '1g', '400mg tab', '9.2g sachet', '600mg tab', '100mg capsule', '1g', 'Genexpert tests', '4 modulos', '2 modulos')
pack_size = c('NA', 'NA', '100', '100', '100', '10', '10', '100', '166', '20', '100', '10', '50', 'NA', 'NA')

budget[, product_name_en := products]       
budget[, product_dose := doses]    
budget[, nb_in_pack := pack_size]    

setnames(budget, 'Y1 Unit Cost USD', '2016_pack_cost')
setnames(budget, 'Y2 Unit Cost USD', '2017_pack_cost')
setnames(budget, 'Y3 Unit Cost USD', '2018_pack_cost')

setnames(budget, 'Y1 Cantidad', '2016_budgeted_packQuantity')
setnames(budget, 'Y2 Cantidad', '2017_budgeted_packQuantity')
setnames(budget, 'Y3 Cantidad', '2018_budgeted_packQuantity')

subset = names(budget)[grepl(names(budget), pattern = '_')]
budget = budget[, subset, with = FALSE]
# dt = copy(budget)
# dt[ , tot_cost_2016 := `2016_pack_cost` * `2016_budgeted_packQuantity`]
# dt[ , tot_cost_2017 := `2017_pack_cost` * `2017_budgeted_packQuantity`]
# dt[ , tot_cost_2018 := `2018_pack_cost` * `2018_budgeted_packQuantity`]
# 
# sum(dt$tot_cost_2016, na.rm=TRUE) + sum(dt$tot_cost_2017, na.rm=TRUE) + sum(dt$tot_cost_2018, na.rm=TRUE)

budget = budget[product_name_en != "NA"]

# dt = copy(budget)
# dt[ , tot_cost_2016 := `2016_pack_cost` * `2016_budgeted_packQuantity`]
# dt[ , tot_cost_2017 := `2017_pack_cost` * `2017_budgeted_packQuantity`]
# dt[ , tot_cost_2018 := `2018_pack_cost` * `2018_budgeted_packQuantity`]
# 
# sum(dt$tot_cost_2016, na.rm=TRUE) + sum(dt$tot_cost_2017, na.rm=TRUE) + sum(dt$tot_cost_2018, na.rm=TRUE)

budget = melt.data.table(budget, id.vars = c('product_name_en', 'product_dose', 'nb_in_pack'))
budget[, c('year', 'unit_ordered', 'measure') := tstrsplit(variable, "_")]
budget[, measure := paste(unit_ordered, measure, sep = "_")]
budget[, c('unit_ordered', 'variable') := NULL]

budget = dcast.data.table(budget, product_name_en + product_dose + nb_in_pack + year ~ measure)

budget[, nb_in_pack := as.numeric(nb_in_pack)]
budget[, year := as.numeric(year)]

budget[, budgeted_unit_cost := pack_cost/nb_in_pack ]
budget[, budgeted_unit_cost := round(budgeted_unit_cost, digits = 2) ]
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data pqr data
# ----------------------------------------------
data = readRDS(inFile_pqr)
data = data[iso3codecountry == 'GTM' & grant_name == 'GTM-T-MSPAS',]

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

setnames(data, 'total_units_in_order', 'nb_units_ordered')
setnames(data, 'total_cost_order', 'order_cost')

data = data[, .(grant_name, country_name, purchase_order_year, purchase_order_date, product_category, product_name_en, description, 
                product_pack, nb_units_in_pack, unit_cost_usd, nb_packs_ordered, nb_units_ordered, order_cost)]

add_to_data = data.table(product_name_en='Genexpert tests', purchase_order_year= 2018, nb_units_in_pack= 50, unit_cost_usd= 9.98, nb_units_ordered= 5000, order_cost= 49900)
data = rbindlist(list(data, add_to_data), use.names = TRUE, fill = TRUE)
# ----------------------------------------------

# ----------------------------------------------
# merge data
# ----------------------------------------------
merged = merge(data, budget, by.x = c('product_name_en', 'purchase_order_year', 'nb_units_in_pack'), by.y = c('product_name_en', 'year', 'nb_in_pack'))

merged = merged[,.(product_name_en, purchase_order_year, nb_units_in_pack, budgeted_unit_cost, unit_cost_usd, nb_units_ordered, order_cost)]

merged[, diff_cost := unit_cost_usd - budgeted_unit_cost]
merged[, total_diff_of_order := nb_units_ordered * diff_cost]
sum(merged$total_diff_of_order)
merged[, budgeted_cost_full_order := budgeted_unit_cost * nb_units_ordered]
sum(merged$budgeted_cost_full_order) - sum(merged$order_cost)
# ----------------------------------------------

# ----------------------------------------------
# make figure comparing budgeted cost to order unit cost
# ----------------------------------------------
# Note: not pictured - $17,000 was budgeted for a Cepheid GeneXpert GX-IV-4 module Instrument in 2016, and one was purchased for $17,000 on 02/14/19

pdf(outFile, height = 9, width = 12)
ggplot(merged, aes(x=budgeted_unit_cost, y=unit_cost_usd, color=product_name_en)) +
  geom_point(size = 5) +
  geom_abline() + 
  theme_bw()  +
  theme(text = element_text(size=16)) +
  labs(title = 'Comparison of budgeted and actual costs for the GTM-T-MSPAS grant',
       x='Budgeted Unit Cost (USD)', y='Order Unit Cost (USD)', color = 'Product Name') +
  theme(legend.position = 'bottom')

ggplot(merged, aes(x=budgeted_unit_cost, y=unit_cost_usd, color=product_name_en)) +
  geom_point(size = 5) +
  geom_abline() + 
  theme_bw()  +
  theme(text = element_text(size=16)) +
  facet_wrap( ~purchase_order_year, scales = "free") +
  labs(title = 'Comparison of budgeted and actual costs for the GTM-T-MSPAS grant',
       x='Budgeted Unit Cost (USD)', y='Order Unit Cost (USD)', color = 'Product Name') +
  theme(legend.position = 'bottom')

ggplot(merged, aes(x=budgeted_unit_cost, y=unit_cost_usd, color=product_name_en)) +
  geom_point(size = 5) +
  geom_abline() + 
  theme_bw()  +
  theme(text = element_text(size=16)) +
  facet_wrap( ~purchase_order_year) +
  labs(title = 'Comparison of budgeted and actual costs for the GTM-T-MSPAS grant',
       x='Budgeted Unit Cost (USD)', y='Order Unit Cost (USD)', color = 'Product Name') +
  theme(legend.position = 'bottom')

# same graphs but multiplying unit costs by volume to get total cost
ggplot(merged, aes(x=budgeted_cost_full_order, y=order_cost, color=product_name_en)) +
  geom_point(size = 5) +
  geom_abline() + 
  theme_bw()  +
  theme(text = element_text(size=16)) +
  labs(title = 'Comparison of budgeted and actual costs for the GTM-T-MSPAS grant',
       x='Total Budgeted Cost (USD)', y='Total Order Cost (USD)', color = 'Product Name') +
  theme(legend.position = 'bottom') +
  scale_y_continuous(label = scales::comma) + scale_x_continuous(label = scales::comma)

ggplot(merged, aes(x=budgeted_cost_full_order, y=order_cost, color=product_name_en)) +
  geom_point(size = 5) +
  geom_abline() + 
  theme_bw()  +
  theme(text = element_text(size=16)) +
  facet_wrap( ~purchase_order_year, scales = "free") +
  labs(title = 'Comparison of budgeted and actual costs for the GTM-T-MSPAS grant',
       x='Total Budgeted Cost (USD)', y='Total Order Cost (USD)', color = 'Product Name') +
  theme(legend.position = 'bottom') +
  scale_y_continuous(label = scales::comma) + scale_x_continuous(label = scales::comma)

ggplot(merged, aes(x=budgeted_cost_full_order, y=order_cost, color=product_name_en)) +
  geom_point(size = 5) +
  geom_abline() + 
  theme_bw()  +
  theme(text = element_text(size=16)) +
  facet_wrap( ~purchase_order_year) +
  labs(title = 'Comparison of budgeted and actual costs for the GTM-T-MSPAS grant',
       x='Total Budgeted Cost (USD)', y='Total Order Cost (USD)', color = 'Product Name') +
  theme(legend.position = 'bottom') +
  scale_y_continuous(label = scales::comma) + scale_x_continuous(label = scales::comma)
dev.off()
# ----------------------------------------------
