library(data.table)
dir = 'J:/Project/Evaluation/GF/vfm/unit_cost_data/prepped_data/'
prepped_data = 'prepped_full_pqr_with_sept_download_data.rds'

dt = readRDS(paste0(dir, prepped_data))

# subset to UGA HIV data
dt = dt[iso3codecountry=='UGA']
dt = dt[product_category %in% c('anti_retroviral', 'condom')]
unique(dt[, .(product_category, product_name_en)])

# .	Total amount of condoms procured, when, and total amount paid
condoms = dt[product_category=='condom',]
condoms[, .(number_purchased = sum(total_units_in_order)), by = .(purchase_order_year, product_name_en, pr_name, grant_name)]
condoms[, .(number_purchased = sum(total_units_in_order), total_spent = sum(total_cost_order)), by = .(product_name_en, purchase_order_year, grant_name)]
condoms[, .(number_purchased = sum(total_units_in_order), total_spent = sum(total_cost_order)), by = .(product_name_en, grant_name)]

# .	Total money spent on ARVs for all adult or pediatric
arvs = dt[product_category=='anti_retroviral',]

arvs[pr_name == 'Ministry of Finance, Planning and Economic Development of the Republic of Uganda', pr_name := 'MoFPED']
arvs[pr_name == 'The AIDS Support Organisation (Uganda) Limited', pr_name := 'TASO']
arvs[grepl(grant_name, pattern = 'MoFPED'), pr_name := 'MoFPED']

arvs[grant_name %in% c('UGA-T-MoFPED', 'UGA-H-MoFPED', 'UGA-C-TASO'), .(number_purchased = sum(total_units_in_order), total_spent = sum(total_cost_order), number_of_orders = .N), by = .(purchase_order_year)]
# .	If condoms or lube included, may stratify by PR (TASO versus MoFPED