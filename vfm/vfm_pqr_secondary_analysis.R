# ----------------------------------------------
# Audrey Batzel
# 08/12/19
# Secondary analyses of PQR products
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

# output files
time_series_products_over_time_by_category_country = paste0(dir, 'visualizations/time_series_products_over_time_by_category_country.pdf')
time_series_products_over_time_by_category_country_subset2015on = paste0(dir, 'visualizations/time_series_products_over_time_by_category_country_2015on.pdf')
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data
# ----------------------------------------------
# load
data = readRDS(inFile)
data = data[iso3codecountry %in% c('COD', 'GTM', 'SEN', 'UGA')]
# ----------------------------------------------

# ----------------------------------------------
# SECONDARY ANALYSES
# ----------------------------------------------

# ----------------------------------------------
# 2.	Cost vs budgeted price. Do some countries and product categories budget poorly?
# a.	You will need to get this info from the detailed budget files, which hasn't been systematically extracted as far as I know. 
# It may be easier to just hand-extract the budgeted unit costs that you need, so just do whatever is fastest. Please only focus on 
# current/ongoing grants and the most recent year of unit costs for each commodity.
# ----------------------------------------------

# ----------------------------------------------
# 3.	Delays between dates. Do some product categories experience greater delays?
# a.	Maybe provide median/20th and 80th percentile to summarize the delays.
# ----------------------------------------------
data[, delivery_delay := purchase_order_date - actual_delivery_date]
data[, delivery_delay := as.numeric(delivery_delay)]
data[, delivery_delay := abs(delivery_delay)]
data[, purchase_order_year := year(purchase_order_date)]

ggplot(data, aes(delivery_delay)) + geom_histogram() + facet_grid(product_category ~ iso3codecountry)
fit2 = glm( delivery_delay ~ factor(product_category), data = data, family = poisson())
summary(fit2)
# fit3 = glm( delivery_delay ~ factor(product_category) + purchase_order_year, data = data, family = poisson())
# anova(fit2, fit3, test = "Chisq")
# fit4 = glm( delivery_delay ~ factor(product_category):factor(iso3codecountry) + purchase_order_year, data = data, family = poisson())
# anova(fit3, fit4, test = "Chisq")
# summary(fit4)

summary = data[, .(delay_20th = quantile(delivery_delay, .2), 
                   delay_med = median(delivery_delay), 
                   delay_80th = quantile(delivery_delay, .8)), 
               by = .(purchase_order_year, product_name_en, product_category, product_disease, iso3codecountry)]
# ----------------------------------------------
# 4.	Shifting preferences in commodities. Are there high-volume commodities that have at some point been replaced by a different 
# version of the same thing? 
#   a.	You may not have enough information just in the data to do this. If you give Jen a list of drug names she can tell you which 
# ones might be good subjects for this analysis.
# ----------------------------------------------
# Calculate purchasing volume, overall spend, and number of orders - ALSO by purchase order date (year and month) for time series graphs
data[, purchase_order_year := year(purchase_order_date)]
data[, purchase_order_month := month(purchase_order_date)]

dt = data[,.(purchasing_volume = sum(total_units_in_order), 
             overall_spend = sum(total_cost_order), 
             total_number_of_orders = .N ),
          by = .(purchase_order_year, product_name_en, product_category, product_disease, iso3codecountry)]

dt[, purchase_order_date := as.Date(paste(purchase_order_year, "01", "01", sep = "-"))]
dt[, purchasing_volume_in_millions := purchasing_volume / 1000000]

pdf(time_series_products_over_time_by_category_country, height = 9, width = 11)
for (country in unique(dt$iso3codecountry)){
  for (type in unique(dt$product_category)){
    g=ggplot(dt[iso3codecountry == country & product_category == type, ], aes(x = purchase_order_date, y = purchasing_volume_in_millions, color = product_name_en)) +
      geom_point() + geom_line() +
      theme_bw() + 
      labs(title = paste0("Products used over time in ", country, " for ", type), x = 'Purchase Order Year', y = "Purchasing Volume (in millions of units)",
           color = "Product Name") + 
      theme(text = element_text(size = 16))
    print(g) }}
dev.off()

# same as above, subset to only 2015
dt = data[purchase_order_date >= '2015-01-01',.(purchasing_volume = sum(total_units_in_order), 
                                                overall_spend = sum(total_cost_order), 
                                                total_number_of_orders = .N ),
          by = .(purchase_order_year, purchase_order_month, product_name_en, product_category, product_disease, iso3codecountry)]

dt[, purchase_order_date := as.Date(paste(purchase_order_year, purchase_order_month, "01", sep = "-"))]
dt[, purchasing_volume_in_millions := purchasing_volume / 1000000]
dt[product_category == 'diagnostic_test', product_category := paste(product_category, product_disease, sep= '_')]

pdf(time_series_products_over_time_by_category_country_subset2015on, height = 9, width = 11)
for (country in unique(dt$iso3codecountry)){
  for (type in unique(dt$product_category)){
    g=ggplot(dt[iso3codecountry == country & product_category == type, ], aes(x = purchase_order_date, y = purchasing_volume_in_millions, color = product_name_en)) +
      geom_point() + geom_line() +
      theme_bw() + 
      labs(title = paste0("Products used over time in ", country, " for ", type), x = 'Purchase Order Date (year - month)', y = "Purchasing Volume (in millions of units)",
           color = "Product Name") + 
      theme(text = element_text(size = 16))
    print(g) }}
dev.off()
# ----------------------------------------------




