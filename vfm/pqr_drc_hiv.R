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
inFile_new = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr_updated_09_2019.rds')

# output files
outFile_appendix_table = paste0(dir, 'unit_cost_data/prepped_data/drc_hiv_diagnostic_tests_current_grants.xlsx')
output_figures_for_slide = paste0(dir, 'visualizations/DRC_HIV_tests_procurement.pdf')
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data
# ----------------------------------------------
data = readRDS(inFile_new)
# adjust  unit costs that are wrong:
data[unit_cost_usd == 0, unit_cost_usd := NA]
# get hiv drc testing data
setnames(data, 'product_name', 'product_name_en') # just to be consistent with old data
dt = data[iso3codecountry == 'COD' & product_name_en == 'HIV RDT and EIA', ]
dt = dt[ grant_name == 'COD-C-CORDAID' | purchase_order_date >= '2018-01-01', ] # only CORDAID data.

dt[grepl(description, pattern = 'Determine'), product_name := 'Determine (first line)']
dt[grepl(description, pattern = 'VIKIA'), product_name := 'VIKIA (second line)']
dt[grepl(description, pattern = 'Uni'), product_name := 'Uni-Gold (third line)']
# ----------------------------------------------

# ----------------------------------------------
# make table
# ----------------------------------------------
table = dt[, .(country_name, grant_name, purchase_order_date, product_name, total_product_cost_usd, total_units_in_order, unit_cost_usd, nb_packs_ordered, product_pack_usd, nb_units_in_pack, scheduled_delivery_date, actual_delivery_date, supplier)]
setnames(table, 'total_product_cost_usd', 'total_order_cost')

write.xlsx(table, outFile_appendix_table)
# ----------------------------------------------

# ----------------------------------------------
# aggregate by grant and test type
# ----------------------------------------------
sum_cols = c('total_units_in_order', 'total_order_cost')
sums = table[, lapply(.SD, sum), .SDcols = sum_cols, by = c('grant_name', 'product_name')]

avg_cols = c('unit_cost_usd')
table[, lapply(.SD, mean), .SDcols = avg_cols, by = c('grant_name', 'product_name')]
# ----------------------------------------------

# ----------------------------------------------
# make figures
# ----------------------------------------------
dt = data[iso3codecountry == 'COD' & product_name_en == 'HIV RDT and EIA', ]

dt[grepl(description, pattern = 'Determine'), product_name := 'Determine (first line)']
dt[grepl(description, pattern = 'VIKIA'), product_name := 'VIKIA (second line)']
dt[grepl(description, pattern = 'Uni'), product_name := 'Uni-Gold (third line)']

dt[, volume_purchased := sum(total_units_in_order), by = .(purchase_order_date, unit_cost_usd, product_name)]

pdf( output_figures_for_slide, height = 9, width = 12 )
g1 = ggplot(table, aes(x=purchase_order_date, y=unit_cost_usd, color=product_name)) +
  geom_point(aes(size = total_units_in_order)) +
  scale_size_continuous(range = c(2, 8), labels = comma) +
  theme_bw() +
  theme(text = element_text(size=14)) +
  labs(title = "HIV tests procured under the CORDAID-C grant over time",
       x='Purchase order date', y='Unit Cost (USD)', size = 'Volume Purchased', color = 'HIV Test')

print(g1)

g1_v2 = ggplot(table, aes(x=purchase_order_date, y=unit_cost_usd, color=product_name)) +
  geom_point(aes(size = total_units_in_order), position = position_jitter()) +
  scale_size_continuous(range = c(2, 8), labels = comma) +
  theme_bw() +
  theme(text = element_text(size=14)) +
  labs(title = "HIV tests procured under the CORDAID-C grant over time",
       x='Purchase order date', y='Unit Cost (USD)', size = 'Volume Purchased', color = 'HIV Test')

print(g1_v2)

g2 = ggplot(dt[!is.na(product_name),], aes(x=purchase_order_date, y=unit_cost_usd, color=product_name)) +
      geom_point(aes(size = volume_purchased)) +
      scale_size_continuous(range = c(2, 8), labels = comma) +
      theme_bw() +
      theme(text = element_text(size=14)) +
      labs(title = "HIV tests procured over time in DRC",
           x='Purchase order date', y='Unit Cost (USD)', size = 'Volume Purchased', color = 'HIV Test')

print(g2)

g2_v2 = ggplot(dt[!is.na(product_name),], aes(x=purchase_order_date, y=unit_cost_usd, color=product_name)) +
  geom_point(aes(size = volume_purchased), position = position_jitter(h = .03)) +
  scale_size_continuous(range = c(2, 8), labels = comma) +
  theme_bw() +
  theme(text = element_text(size=14)) +
  labs(title = "HIV tests procured over time in DRC",
       x='Purchase order date', y='Unit Cost (USD)', size = 'Volume Purchased', color = 'HIV Test')

print(g2_v2)

table = table[, .(tot_units = sum(total_units_in_order), tot_cost = sum(total_order_cost)),
                       by = .(purchase_order_date, grant_name, product_name)]
table[, mean_unit_cost:= tot_cost/tot_units]

g3 = ggplot(table, aes(x=product_name, y=mean_unit_cost, fill = product_name)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  theme(text = element_text(size=16)) +
  labs(title = "Average unit costs of HIV tests procured in DRC by test type, CORDAID-C",
       x='HIV Test Type', y='Mean Unit Cost (USD)') + 
  guides(fill = FALSE)

print(g3)

table[, units_in_mill := tot_units / 1000000]

g3_v2 = ggplot(table, aes(x=product_name, y=units_in_mill, fill = product_name)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  theme(text = element_text(size=22)) +
  labs(title = "Total units of HIV tests procured in DRC by test type, CORDAID-C",
       x='HIV test type', y='Total Units Procured (in millions)') + 
  guides(fill = FALSE)

print(g3_v2)

dev.off()
# ----------------------------------------------



