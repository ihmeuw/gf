# Trying to assess completeness of PQR data 
# Track expenditure for certain categories of drugs. 
rm(list=ls())
library(data.table) 
expenditure = readRDS("C:/Users/abatzel/Box Sync/Global Fund Files/tableau_data/all_cost_categories.rds") #Change to your box account. 
# Use the column "cleaned cost category" and "start date" 
# Also, watch the "pudr_semester_financial" 
expenditure = expenditure[pudr_semester_financial == '1-A', .(expenditure=sum(expenditure, na.rm=T)), by=c('cleaned_cost_category', 'grant', 'grant_period')] #first semester of 2018
expenditure[cleaned_cost_category =='4.1 Antiretroviral medicines', ]

# load pqr data:
pqr = readRDS("J:/Project/Evaluation/GF/vfm/unit_cost_data/prepped_data/prepped_full_pqr_with_sept_download_data.rds")

# example comparison of expenditure data and pqr spending
dt = pqr[grant_name == 'SEN-H-CNLS']
compare = expenditure[ grant == 'SEN-H-CNLS']

comparison_cost_categories = c('4.0 Health Products - Pharmaceutical Products (HPPP)', '4.1 Antiretroviral medicines', '4.2 Anti-tuberculosis medicines', '4.3 Antimalarial medicines', '4.7 Other medicines', 
                               '5.0 Health Products - Non-Pharmaceuticals (HPNP)', '5.1 Insecticide-treated Nets (LLINs/ITNs)', '5.2 Condoms - Male', '5.3 Condoms - Female', '5.4 Rapid Diagnostic Test', '5.8 Other consumables', 
                               '6.0 Health Products - Equipment (HPE)', '6.2 HIV Viral Load analyser/accessories', '6.3 Microscopes', '6.4 TB Molecular Test equipment', '6.6 Other health equipment')

dt[purchase_order_date >= '2018-01-01' & purchase_order_date <= '2018-07-01', sum(total_cost_order), by = .(product_category)]
compare[cleaned_cost_category %in% comparison_cost_categories, ]
# questions - time period and overlap? 



