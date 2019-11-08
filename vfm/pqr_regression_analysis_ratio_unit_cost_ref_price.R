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
inFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr_with_sept_download_data.rds')
# inFile = paste0(dir, 'unit_cost_data/prepped_data/prepped_full_pqr.rds')
subset_commodities = paste0(dir, 'unit_cost_data/prepped_data/subset_commodities.csv')

# output files 
regr_results_ratio_costs_by_category_log = paste0(dir, 'visualizations/PQR/regr_results_ratio_costs_by_category_log.pdf')
regr_results_ratio_costs_by_categoryAndCountry_log = paste0(dir, 'visualizations/PQR/regr_results_ratio_costs_by_categoryAndCountry_log.pdf')
regr_results_ratio_costs_by_procurement_mechanism = paste0(dir, 'visualizations/PQR/regr_results_ratio_costs_by_procurementMechanism_log_updated_10_31.pdf')
regr_results_ratio_costs_by_procurement_mechanism_AND_category =  paste0(dir, 'visualizations/PQR/regr_results_ratio_costs_by_procurementMechanism_log_byProductCategory.pdf')
regr_results_ratio_costs_by_procurement_mechanism_AND_category_withDeliveryTime = paste0(dir, 'visualizations/PQR/regr_results_ratio_costs_by_procurementMechanism_log_byProductCategory_withDeliveryTime.pdf')

scatterplot_ratio_vs_volume = paste0(dir, 'visualizations/PQR/scatterplot_ratio_vs_volume.pdf')
ts_mean_median_ratio_by_procurement_mech = paste0(dir, 'visualizations/PQR/ts_mean_median_ratio_by_procurement_mech.pdf')

frequency_table_procurement_mech =  paste0(dir, 'unit_cost_data/prepped_data/frequency_table_procurement_mech_product_category.csv')
bar_plots = paste0(dir, 'visualizations/PQR/bar_plot_number_of_orders_procurement_mechanism_by_category.pdf')

findings_1a = paste0(dir, 'unit_cost_data/prepped_data/results_productCat.xlsx')
findings_1b = paste0(dir, 'unit_cost_data/prepped_data/results_productCat_country.xlsx')
findings_2a = paste0(dir, 'unit_cost_data/prepped_data/results_procureMech.xlsx')
findings_2b = paste0(dir, 'unit_cost_data/prepped_data/results_productCat_procureMech.xlsx')
# ----------------------------------------------

# ----------------------------------------------
# Load/set up data
# ----------------------------------------------
data = readRDS(inFile)

# subset to where intl ref price and unit cost are both not missing
dt = data[!is.na(po_international_reference_price) & !is.na(unit_cost_usd)]
setnames(dt, 'supplier_agent_manufacturer_intermediatry', 'procurement_mech')
dt[is.na(procurement_mech), procurement_mech := supplier]
dt[procurement_mech == 'Other n/s', .N]
dt[procurement_mech == 'United Nations Population Fund (UNFPA)', procurement_mech := 'UN Population Fund']
dt[ procurement_mech == 'PPM, through Partnership for Supply Chain Management (PFSCM)', procurement_mech := 'PPM, through PFSCM']
dt[ procurement_mech == 'Partnership for Supply Chain Management Inc (PFSCM)', procurement_mech := 'PFSCM']

freq_table = dt[,.N, by = .(procurement_mech, product_category)]
setorderv(freq_table, c('procurement_mech', 'product_category'))
# write.csv(freq_table, frequency_table_procurement_mech)
# ----------------------------------------------

# ----------------------------------------------
# set colors for procurement mechanisms
# ----------------------------------------------
pm = unique(dt$procurement_mech)
colors1 = brewer.pal(8, 'Set2')
colors2 = brewer.pal(8, 'Set1')
colors = c(colors1, colors2)
colors = colors[!colors %in% c('#FFFF33')]
names(colors) = pm
# ----------------------------------------------

# ----------------------------------------------
# time series figures by procurement mechanism
# ----------------------------------------------
# first make a date variable by month/year
dt[, month := month(purchase_order_date)]
dt[, year := year(purchase_order_date)]
dt[, date_for_figures := as.Date(paste0(year,'-', month, '-01'))]

# then get the mean/median of ratio of unit cost to ref price, as well as sum of units purchased for those orders,
# at a given time point across products within product category by procurement mech
graph_dt = dt[, .(mean_ratio = mean(unit_cost_over_ref_price),
                  median_ratio = median(unit_cost_over_ref_price),
                  sum_units_purchased = sum(total_units_in_order)),
                by = .(date_for_figures, procurement_mech, product_category)]

pdf(ts_mean_median_ratio_by_procurement_mech, height = 9, width = 12)
# loop through product category to make figures:
for ( cat in unique(graph_dt$product_category) ){
  pm =  dt[product_category == cat, (unique(procurement_mech))]
  
  if ( length(pm) > 4 ){
    pm2 = pm[5:length(pm)]
    pm = pm[!pm %in% pm2]
  }
  
  # graph orders as individual points - **could try faceting by product? or procurement mech?
  print(ggplot(dt[product_category == cat & unit_cost_over_ref_price < 8 , ], aes(x=purchase_order_date, y=unit_cost_over_ref_price, color=procurement_mech, size = total_units_in_order/1000000)) +
          geom_point() +
          theme_bw() +
          geom_hline(yintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
          theme(text = element_text(size=16)) +
          scale_color_manual(name = 'Procurement Mechanism', values = colors) +
          labs(title = paste0("Ratio of unit cost to reference price over time for ", cat, " products, by \nprocurement mechanism"),
               x='Purchase order date', y='Ratio of unit cost to reference price', color = 'Procurement Mechanism',
               size = 'Volume Purchased (in millions)'))
  
  # graph orders as individual points - **could try faceting by product? or procurement mech?
  print(ggplot(dt[product_category == cat & unit_cost_over_ref_price < 8 & procurement_mech %in% pm, ], aes(x=purchase_order_date, y=unit_cost_over_ref_price, color=procurement_mech, size = total_units_in_order/1000000)) +
          geom_point() +
          theme_bw() +
          geom_hline(yintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
          theme(text = element_text(size=16)) +
          guides(color = FALSE) +
          scale_color_manual(name = '', values = colors) +
          theme(legend.position = 'bottom') +
          labs(title = paste0("Ratio of unit cost to reference price over time for ", cat, " products, by \nprocurement mechanism"),
               x='Purchase order date', y='Ratio of unit cost to reference price', color = 'Procurement Mechanism',
                 size = 'Volume Purchased (in millions)') +
          facet_wrap(~procurement_mech, scales = 'free'))
  
  if(exists('pm2')){
  print(ggplot(dt[product_category == cat & unit_cost_over_ref_price < 8 & procurement_mech %in% pm2, ], aes(x=purchase_order_date, y=unit_cost_over_ref_price, color=procurement_mech, size = total_units_in_order/1000000)) +
          geom_point() +
          theme_bw() +
          geom_hline(yintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
          theme(text = element_text(size=16)) +
          guides(color = FALSE) +
          theme(legend.position = 'bottom') +
          scale_color_manual(name = '', values = colors) +
          labs(title = paste0("Ratio of unit cost to reference price over time for ", cat, " products, by \nprocurement mechanism"),
               x='Purchase order date', y='Ratio of unit cost to reference price', color = 'Procurement Mechanism',
               size = 'Volume Purchased (in millions)') +
          facet_wrap(~procurement_mech, scales = 'free'))
  }
  rm(pm)
  rm(pm2)
  # graph mean
  print(ggplot(graph_dt[product_category == cat, ], aes(x=date_for_figures, y=mean_ratio, color=procurement_mech)) +
          geom_point(aes(size = sum_units_purchased/1000000)) + geom_line(size = 1) + theme_bw() +
          geom_hline(yintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
          theme(text = element_text(size=16)) +
          scale_color_manual(name = 'Procurement Mechanism', values = colors) +
          labs(title = paste0("Mean of ratio of unit cost to reference price over time for ", cat, " products, by \nprocurement mechanism"),
               x='Purchase order date (year, month)', y='Mean of ratio of unit cost to reference price', size = 'Volume purchased (in millions)'))
  
  # graph median
  print(ggplot(graph_dt[product_category == cat, ], aes(x=date_for_figures, y=median_ratio, color=procurement_mech)) +
          geom_point(aes(size = sum_units_purchased/1000000)) + geom_line(size = 1) + theme_bw() +
          geom_hline(yintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
          theme(text = element_text(size=16)) +
          scale_color_manual(name = 'Procurement Mechanism', values = colors) +
          labs(title = paste0("Median of ratio of unit cost to reference price over time for ", cat, " products, by \nprocurement mechanism"),
               x='Purchase order date (year, month)', y='Mean of ratio of unit cost to reference price', size = 'Volume purchased (in millions)'))
}
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# regression by procurement mechanism across all countries and product categories
# ----------------------------------------------
findings_table_2a = data.table()

pdf(regr_results_ratio_costs_by_procurement_mechanism, height = 9, width = 12)
data_subset = copy(dt)
# get the log of the ratio of unit cost to reference price
data_subset[, log_ratio := log(unit_cost_over_ref_price)]
data_subset = data_subset[procurement_mech != 'Medical Export Group (MEG)']
fit = glm(log_ratio ~ purchase_order_date + procurement_mech, data = data_subset, family = gaussian())

dt2 = unique(data_subset[,.(procurement_mech)])
dt2 = as.data.table(dt2)
dt2[, purchase_order_date := mean(data_subset$purchase_order_date)]
dt2[, purchase_order_date := as.Date(purchase_order_date)]

dt2[, unit_cost_over_ref_price := predict(fit, newdata = dt2)]
dt2[, std_error := predict(fit, newdata=dt2, se.fit = TRUE)$se.fit]  
dt2[, lower:= unit_cost_over_ref_price - (1.96*std_error)]
dt2[, upper:= unit_cost_over_ref_price + (1.96*std_error)]

dt2[, unit_cost_over_ref_price := exp(unit_cost_over_ref_price)]
dt2[, lower := exp(lower)]
dt2[, upper := exp(upper)]

data_subset[, procurement_mech := as.factor(procurement_mech)]

findings_table_2a = copy(dt2)
findings_table_2a[lower > 1, result:='costs higher than reference price']
findings_table_2a[upper < 1, result:='costs lower than reference price']
findings_table_2a[lower < 1 & upper >1, result:='not significant']
findings_table_2a = findings_table_2a[, .(procurement_mech, result)]

g = ggplot(dt2[!is.na(procurement_mech)], aes(x = procurement_mech, y = unit_cost_over_ref_price, color = procurement_mech)) +
  geom_point(shape = 1, size = 5) + 
  theme_bw() + geom_hline(yintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2, size = 1 ) +
  theme(strip.text = element_blank()) +
  scale_color_manual(name = '', values = colors) +
  labs(y = 'Ratio of unit cost to reference price', x = 'Procurement Mechanism', 
       title = paste0('Prediction intervals from regression results\n     ratio_unit_cost_to_ref_price ~ purchase_order_date + procurement_mechanism')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 16)) + guides(color = FALSE)
print(g)

data_subset[ unit_cost_over_ref_price>=10, unit_cost_over_ref_price := 10 ]

print(ggplot(data_subset[procurement_mech %in% pm[1:7], ], aes(x = unit_cost_over_ref_price, color = procurement_mech)) + geom_density(size = 1) + theme_bw() + 
        theme(legend.position = 'bottom', text = element_text(size = 16)) +
        scale_color_manual(name = '', values = colors) +
        geom_vline(xintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
        labs(y = 'Density', x = 'Ratio unit cost:reference price', 
             caption = 'Note: values over 10 are set to 10 for the purpose of visualization \n10 represents >=10',
             title = paste0('Distribution of ratio of unit cost to reference price by procurement mechanism')))

print(ggplot(data_subset[procurement_mech %in% pm[8:length(pm)],], aes(x = unit_cost_over_ref_price, color = procurement_mech)) + geom_density(size = 1) + theme_bw() + 
        theme(legend.position = 'bottom', text = element_text(size = 16)) +
        geom_vline(xintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
        scale_color_manual(name = '', values = colors) +
        labs(y = 'Density', x = 'Ratio unit cost:reference price', 
             caption = 'Note: values over 10 are set to 10 for the purpose of visualization \n10 represents >=10',
             title = paste0('Distribution of ratio of unit cost to reference price by procurement mechanism')))
dev.off()

write.xlsx(findings_table_2a, findings_2a)
# ----------------------------------------------

# ----------------------------------------------
# inverted bar plot showing break down of product categories by procurement mech
# ----------------------------------------------
freq_table[ procurement_mech == 'PPM, through Partnership for Supply Chain Management (PFSCM)', procurement_mech := 'PPM, through PFSCM']
freq_table[ procurement_mech == 'Partnership for Supply Chain Management Inc (PFSCM)', procurement_mech := 'PFSCM']
freq_table[, total_by_pm := sum(N), by = 'procurement_mech']
setorderv(freq_table, c('total_by_pm'))
pm = unique(freq_table$procurement_mech)

pdf(bar_plots, height = 9, width = 15)

print(ggplot(freq_table, aes(x = procurement_mech, y = N, fill = product_category)) + theme_bw() + 
        geom_bar(stat="identity") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 16), legend.position = 'bottom') +
        labs(y = 'Number of orders', x = 'Procurement Mechanism', 
             title = '', fill = 'Product category') + scale_x_discrete(limits = pm) +
        coord_flip() )

dev.off()
# ----------------------------------------------

# ----------------------------------------------
# regression of procurement mechanism BY CATEGORY across all countries and products, controlling for date and volume
pm = unique(dt$procurement_mech)
colors = colors[1: length(pm)]

dt[, delivery_time := as.integer(scheduled_delivery_date - purchase_order_date)]

findings_table_2b = data.table()

pdf(regr_results_ratio_costs_by_procurement_mechanism_AND_category, height = 9, width = 12)
# pdf(regr_results_ratio_costs_by_procurement_mechanism_AND_category_withDeliveryTime, height = 9, width = 12)

for (cat in unique(dt$product_category)){
  # subset to each category
  data_subset = dt[product_category == cat, ]
  # get the log of the ratio of unit cost to reference price
  data_subset[, log_ratio := log(unit_cost_over_ref_price)]

  # if the subset only has one observation, need to skip
  if(nrow(data_subset)<=1) next
  # set the formula based on whether or not there is more than one procurement mechanism per product category (there should be, but just in case, since the regr will break 
  # if you try to inlcude procurement mechanism and there is only one) 
  if (length(unique(data_subset$procurement_mech))==1) form = as.formula('log_ratio ~ purchase_order_date + total_units_in_order')
  if (length(unique(data_subset$procurement_mech))>1) form = as.formula('log_ratio ~ purchase_order_date + total_units_in_order + procurement_mech')
  
  # run the regression with the formula set above. 
  fit = glm(form, data = data_subset, family = gaussian())
  
  dt2 = unique(data_subset[,.(product_category, procurement_mech)])
  dt2 = as.data.table(dt2)
  # date and volume (total units in order) need to be in the data table in order to predict, so take the mean to get an estimate
  dt2[, purchase_order_date := mean(data_subset$purchase_order_date)]
  dt2[, purchase_order_date := as.Date(purchase_order_date)]
  dt2[, total_units_in_order := mean(data_subset$total_units_in_order)]
  # dt2[, delivery_time := mean(data_subset$delivery_time)]
  
  dt2[, unit_cost_over_ref_price := predict(fit, newdata = dt2)]
  dt2[, std_error := predict(fit, newdata=dt2, se.fit = TRUE)$se.fit]  
  dt2[, lower:= unit_cost_over_ref_price - (1.96*std_error)]
  dt2[, upper:= unit_cost_over_ref_price + (1.96*std_error)]
  
  dt2[, unit_cost_over_ref_price := exp(unit_cost_over_ref_price)]
  dt2[, lower := exp(lower)]
  dt2[, upper := exp(upper)]
  
  data_subset[, procurement_mech := as.factor(procurement_mech)]
  
  # add to the findings table
  add_to_findings = copy(dt2)
  add_to_findings[lower > 1, result:='costs higher than reference price']
  add_to_findings[upper < 1, result:='costs lower than reference price']
  add_to_findings[lower < 1 & upper >1, result:='not significant']
  add_to_findings[, product_category := cat]
  add_to_findings = add_to_findings[, .(product_category, procurement_mech, result)]
  findings_table_2b = rbind(findings_table_2b, add_to_findings)
  
  # add n = x to the labels for the figures
  for( p in unique(dt2$procurement_mech)){
    n = data_subset[ procurement_mech == p, .N ]
    dt2[procurement_mech == p, procurement_mech_n := paste0(procurement_mech, "\n (n = ", n, ")")] 
  }
  
  # make plot showing predictions and prediction intervals
  g = ggplot(dt2, aes(x = procurement_mech_n, y = unit_cost_over_ref_price, color = procurement_mech)) +
    geom_point(shape = 1, size = 5) + 
    theme_bw() + geom_hline(yintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2, size = 1 ) +
    theme(strip.text = element_blank()) +
    scale_color_manual(name = '', values = colors) +
    labs(y = 'Ratio of unit cost to reference price', x = 'Supplier / Procurement mechanism', 
         title = paste0(cat, ': Prediction intervals from regression results'),
         subtitle = 'ratio_unit_cost_to_ref_price ~ purchase_order_date + total_units_in_order + procurement_mechanism') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 16)) + guides(color = FALSE)
  print(g)
  
  data_subset[ unit_cost_over_ref_price>=10, unit_cost_over_ref_price := 10 ]
  
  # if there are more than 5 procurement mechanisms for a given category, split the density plot into two density plots
  pm = unique(data_subset$procurement_mech)
  if (length(pm)>5){
    pm = unique(data_subset$procurement_mech)
    pm2 = pm[round((length(pm)/2)):length(pm)]
    pm = pm[!pm %in% pm2]
  }
  
  # make density plots
  print(ggplot(data_subset[ procurement_mech %in% pm, ], aes(x = unit_cost_over_ref_price, color = procurement_mech)) + geom_density(size = 1) + theme_bw() + 
          theme(legend.position = 'bottom', text = element_text(size = 16)) +
          scale_color_manual(name = '', values = colors) +
          geom_vline(xintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
          labs(y = 'Density', x = 'Ratio unit cost:reference price', 
               caption = 'Note: values over 10 are set to 10 for the purpose of visualization \n10 represents >=10',
               title = paste0(cat, ': Distribution of ratio of unit cost to reference price by procurement mechanism')))
  
  if (exists('pm2')){
    print(ggplot(data_subset[procurement_mech %in% pm2,], aes(x = unit_cost_over_ref_price, color = procurement_mech)) + geom_density(size = 1) + theme_bw() + 
            theme(legend.position = 'bottom', text = element_text(size = 16)) +
            geom_vline(xintercept = 1, linetype = 'dashed', color = 'red', size = 1) +
            scale_color_manual(name = '', values = colors) +
            labs(y = 'Density', x = 'Ratio unit cost:reference price', 
                 caption = 'Note: values over 10 are set to 10 for the purpose of visualization \n10 represents >=10',
                 title = paste0(cat, ': Distribution of ratio of unit cost to reference price by procurement mechanism')))
    rm(pm2)
  }
  
  hist(data_subset$total_units_in_order)
  hist(log(data_subset$total_units_in_order))
  
  # make scatterplot showing volume purchased by ratio of cost to reference price
  print(ggplot(dt[product_category == cat, ], aes(x = log(total_units_in_order), y = unit_cost_over_ref_price, color = procurement_mech)) + 
          theme_bw() + 
          theme(legend.position = 'bottom', text = element_text(size = 16)) +
          geom_point(size = 3) +
          scale_color_manual(name = '', values = colors) +
          geom_smooth( aes(x = log(total_units_in_order), y = unit_cost_over_ref_price), inherit.aes = FALSE, method='lm') +
          labs(y = 'Ratio of unit cost to reference price', x = 'Volume purchased (total units in order, log space)', 
               color = 'Procurement Mechanism:',
               title = paste0(cat, ': Relationship between the number of units purchased and the ratio of cost to reference price')))

  # # make scatterplot showing delivery time by ratio of cost to reference price
  # print(ggplot(data_subset, aes(x = delivery_time, y = unit_cost_over_ref_price, color = procurement_mech)) + 
  #         theme_bw() + 
  #         theme(legend.position = 'bottom', text = element_text(size = 16)) +
  #         geom_point() +
  #         scale_color_manual(name = '', values = colors) + 
  #         geom_smooth(aes(x = delivery_time, y = unit_cost_over_ref_price), method='lm', inherit.aes = FALSE) +
  #         labs(y = 'Ratio of unit cost:reference price', x = 'Time between order date and delivery date (days)', 
  #              title = paste0(cat, ': Relationship between the time for delivery \nand the ratio of cost to reference price')))
}
dev.off()

write.xlsx(findings_table_2a, findings_2a)
# ----------------------------------------------

# ----------------------------------------------
# Scatterplots with all data
# ----------------------------------------------
pdf(scatterplot_ratio_vs_volume, height = 9, width = 11)
# scatterplot by volume, all data:
print(ggplot(dt, aes(x = log(total_units_in_order), y = unit_cost_over_ref_price)) + 
        theme_bw() + 
        theme(legend.position = 'bottom', text = element_text(size = 16)) +
        geom_point() +
        geom_smooth(method='lm') +
        labs(y = 'Ratio of unit cost:reference price', x = 'Volume purchased (total units in order, log space)', 
             title = paste0('Relationship between the number of units purchased and the ratio of cost to reference price')))

for(p in unique(dt$procurement_mech)){
  print(ggplot(dt[procurement_mech == p,], aes(x = log(total_units_in_order), y = unit_cost_over_ref_price)) + 
          theme_bw() + 
          theme(legend.position = 'bottom', text = element_text(size = 16)) +
          geom_point() +
          geom_smooth(method='lm') +
          labs(y = 'Ratio of unit cost:reference price', x = 'Volume purchased (total units in order, log space)', 
               title = paste0(p, ': Relationship between the number of units \npurchased and the ratio of cost to reference price')))
}

# scatterplot by delivery time, all data:
print(ggplot(dt, aes(x = delivery_time, y = unit_cost_over_ref_price)) + 
        theme_bw() + 
        theme(legend.position = 'bottom', text = element_text(size = 16)) +
        geom_point() +
        geom_smooth(method='lm') +
        labs(y = 'Ratio of unit cost:reference price', x = 'Time between order date and delivery date (days)', 
             title = paste0('Relationship between the time for delivery and the ratio of cost to reference price')))
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# subset to 'first line' products based on country guidelines
# ----------------------------------------------
subset_by = as.data.table(read.csv(subset_commodities))
subset_by = subset_by[ keep == TRUE, ]

subset = merge(dt, subset_by, by = c('iso3codecountry', 'product_category', 'product_name_en', 'description'))
subset = subset[, c('X', 'keep') := NULL]
subset[, product_name_en := as.factor(product_name_en)]
dt = copy(subset)

dt[product_category == 'diagnostic_test_hiv' & grepl(description, pattern = 'etermin', ignore.case = TRUE), product_name_en := 'Determine']
dt[product_category == 'diagnostic_test_hiv' & grepl(description, pattern = 'gold', ignore.case = TRUE), product_name_en := 'Uni-Gold']
dt[product_category == 'diagnostic_test_hiv' & grepl(description, pattern = 'stat', ignore.case = TRUE), product_name_en := 'Stat-Pak']
dt[product_category == 'diagnostic_test_hiv' & grepl(description, pattern = 'oraq', ignore.case = TRUE), product_name_en := 'OraQuick']
dt[product_category == 'diagnostic_test_hiv' & grepl(description, pattern = 'immunocomb', ignore.case = TRUE), product_name_en := 'ImmunoComb']

n_for_regr = dt[, .N, by = c('iso3codecountry', 'product_category', 'product_name_en')]
# ----------------------------------------------

# ----------------------------------------------
# 1.	Cost vs reference price stratified by categorical variables (procurement mechanism mainly, 
# please explore for other interesting options). Is there a procurement mechanism (or stratum) that consistently
# outperforms the reference price
#   a.	If you have time, try running the regression 
#   `difference_from_reference_cost ~ time + factor(procurement_mechanism)*factor(product_category)` 
# where the outcome variable is defined as unit_cost - reference_price. You'll get one coefficient per 
# procurement mechanism and category that tells you whether that combination is significantly different 
# from the first procurement mechanism/category (alphabetically). You can then make a data frame of the 
# unique values of the explanatory variables and predict in it to get estimates/CI of the mean cost differences.
# ----------------------------------------------
ggplot(dt, aes(x = diff_from_ref_cost)) + geom_histogram(binwidth = 0.5) + theme_bw()
summary(dt$diff_from_ref_cost)
plot(dt$purchase_order_date, dt$diff_from_ref_cost)
# dt = dt[ purchase_order_date >= '2015-01-01', ]
# ----------------------------------------------

# ----------------------------------------------
# loop through product categories across all countries 
# ----------------------------------------------
pdf(regr_results_ratio_costs_by_category_log, height = 9, width = 12)
for (cat in unique(dt$product_category)){
  # subset to each category
  data_subset = dt[product_category == cat, ]
  # get the log of the ratio of unit cost to reference price
  data_subset[, log_ratio := log(unit_cost_over_ref_price)]
  
  if(nrow(data_subset)<=1) next
  
  if (length(unique(data_subset$product_name_en))==1) form = as.formula('log_ratio ~ purchase_order_date')
  if (length(unique(data_subset$product_name_en))>1) form = as.formula('log_ratio ~ purchase_order_date + product_name_en')
  
  fit = glm(form, data = data_subset, family = gaussian())
  
  dt2 = unique(dt[product_category == cat,.(product_category, product_name_en)])
  dt2 = as.data.table(dt2)
  dt2[, purchase_order_date := mean(data_subset$purchase_order_date)]
  dt2[, purchase_order_date := as.Date(purchase_order_date)]
  
  dt2[, unit_cost_over_ref_price := predict(fit, newdata = dt2)]
  dt2[, std_error := predict(fit, newdata=dt2, se.fit = TRUE)$se.fit]  
  dt2[, lower:= unit_cost_over_ref_price - (1.96*std_error)]
  dt2[, upper:= unit_cost_over_ref_price + (1.96*std_error)]
  
  dt2[, unit_cost_over_ref_price := exp(unit_cost_over_ref_price)]
  dt2[, lower := exp(lower)]
  dt2[, upper := exp(upper)]
  
  g = ggplot(dt2, aes(x = product_name_en, y = unit_cost_over_ref_price, color = product_name_en)) +
    geom_point(shape = 1, size = 3) + 
    theme_bw() + geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
    theme(strip.text = element_blank()) +
    labs(y = 'Ratio of unit cost to reference price', x = '', title = paste0(cat, ': prediction intervals from regression results\n     ratio_unit_cost_to_ref_price ~ purchase_order_date + product_name_en')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 16)) + guides(color = FALSE)
  print(g)
  
  data_subset[, product_name_en := as.factor(product_name_en)]
  data_subset[ unit_cost_over_ref_price>=10, unit_cost_over_ref_price := 10 ]
  
  print(ggplot(data_subset, aes(x = unit_cost_over_ref_price, color = product_name_en)) + geom_density(size = 1) + theme_bw() + 
    theme(legend.position = 'bottom', text = element_text(size = 16)) +
    geom_vline(xintercept = 1, linetype = 'dashed', color = 'red') +
    labs(y = 'Density', x = 'Ratio unit cost:reference price', color = 'Product:',
         caption = 'Note: values over 10 are set to 10 for the purpose of visualization; 10 represents >=10',
         title = paste0(cat, ': distribution of ratio of unit cost to reference price by product')))
}
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# loop through product categories AND country individually
# ----------------------------------------------
pdf(regr_results_ratio_costs_by_categoryAndCountry_log, height = 9, width = 12)
for (cat in unique(dt$product_category)){
  for (country in unique(dt$country_name)){
  # subset to each category
  data_subset = dt[product_category == cat & country_name == country, ]
  # get the log of the ratio of unit cost to reference price
  data_subset[, log_ratio := log(unit_cost_over_ref_price)]
  
  if(nrow(data_subset)<=1) next

  if (length(unique(data_subset$product_name_en))==1) form = as.formula('log_ratio ~ purchase_order_date')
  if (length(unique(data_subset$product_name_en))>1) form = as.formula('log_ratio ~ purchase_order_date + product_name_en')
 
  fit = glm(form, data = data_subset, family = gaussian())

  dt2 = unique(dt[product_category == cat & country_name == country,.(product_category, product_name_en)])
  dt2 = as.data.table(dt2)
  dt2[, purchase_order_date := mean(data_subset$purchase_order_date)]
  dt2[, purchase_order_date := as.Date(purchase_order_date)]

  dt2[, unit_cost_over_ref_price := predict(fit, newdata = dt2)]
  dt2[, std_error := predict(fit, newdata=dt2, se.fit = TRUE)$se.fit]
  dt2[, lower:= unit_cost_over_ref_price - (1.96*std_error)]
  dt2[, upper:= unit_cost_over_ref_price + (1.96*std_error)]

  dt2[, unit_cost_over_ref_price := exp(unit_cost_over_ref_price)]
  dt2[, lower := exp(lower)]
  dt2[, upper := exp(upper)]

  g = ggplot(dt2, aes(x = product_name_en, y = unit_cost_over_ref_price, color = product_name_en)) +
    geom_point(shape = 1, size = 3) + 
    theme_bw() + geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2 ) +
    theme(strip.text = element_blank()) +
    labs(y = 'Ratio of unit cost to reference price', x = '', title = paste0(country, ', ', cat, ': prediction intervals from regression results\n     ratio_unit_cost_to_ref_price ~ purchase_order_date + product_name_en')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 16)) + guides(color = FALSE)
  print(g)
  
  data_subset[, product_name_en := as.factor(product_name_en)]
  data_subset[ unit_cost_over_ref_price>=10, unit_cost_over_ref_price := 10 ]
  
  print(ggplot(data_subset, aes(x = unit_cost_over_ref_price, color = product_name_en)) + geom_density(size = 1) + theme_bw() + 
          theme(legend.position = 'bottom', text = element_text(size = 16)) +
          geom_vline(xintercept = 1, linetype = 'dashed', color = 'red') +
          labs(y = 'Density', x = 'Ratio unit cost:reference price', color = 'Product:',
               caption = 'Note: values over 10 are set to 10 for the purpose of visualization; 10 represents >=10',
               title = paste0(country, ', ', cat, ': distribution of ratio of unit cost to reference price by product')))

}}
dev.off()
# ----------------------------------------------
