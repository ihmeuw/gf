# ----------------------------------------------
# Audrey Batzel
# 
# 12/18/19
# Prep PPM reference price data for ARVs
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(stringr)
library(openxlsx)
library(dplyr)
# --------------------

# ----------------------------------------------
# Files and directories
# ----------------------------------------------
# switch for cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/vfm/unit_cost_data/reference_price_data/')

# input files
inFiles_ARVs = list.files(paste0(dir, 'ARVs/converted_files/'))

# output files 
outFile = paste0(dir, '')
# ----------------------------------------------

# ----------------------------------------------
# Load/prep data
# ----------------------------------------------
list_of_dts = list()
for (i in 1:length(inFiles_ARVs)){
  x = inFiles_ARVs[i]
  date = as.Date(str_split_fixed(x, ' ', 2)[1])
  if (date > '2019-02-01') { 
    data = as.data.table(read.xlsx(paste0(dir, 'ARVs/converted_files/', x), sheet = 1))
  } else {
    data = as.data.table(read.xlsx(paste0(dir, 'ARVs/converted_files/', x), sheet = 2))
  }
  
  header_row = which(data[, 1]=='Product Description')
  
  if (date == '2019-12-05') header_row = which(data[, 1]=='All products')
    
  names(data) = as.character(data[header_row, ])

  data = data[-c(1:header_row), ]
  
  if (date == '2019-12-05') {
    keep_cols = names(data)[!grepl(names(data), pattern = 'X[0-9]')]
    data = data[, keep_cols, with = FALSE]
    names(data) = c('product_description', 'reference_price_usd')
  }
  
  names(data) = tolower((names(data)))
  names(data) = gsub('exw,', '', names(data))
  names(data) = gsub('  ', ' ', names(data))
  names(data) = gsub(' ', '_', names(data))
  names(data) = gsub('\\(', '', names(data))
  names(data) = gsub('\\)', '', names(data))
  
  if (date <= '2017-03-01') { 
    data[, product_description := gsub(' + ', '+', product_description, fixed = TRUE)]
    data[, product_description := gsub('\\smg', 'mg', product_description)]
    data[, product_description := gsub('  ', ' ', product_description, fixed = TRUE)]
    data[, product_description := gsub('Atanzavir Sulfate', 'Atanzavir+Sulfate', product_description, fixed = TRUE)]
    
    data[, product_name := unlist(lapply(product_description, function (x) {str_split_fixed(x, ' ', 3)[1]}))]
    data[, dose := unlist(lapply(product_description, function (x) {str_split_fixed(x, ' ', 3)[2]}))]
    data[, description := unlist(lapply(product_description, function (x) {str_split_fixed(x, ' ', 3)[3]}))]
  } else if (date >= '2017-04-01' & date <= '2019-12-31') {
    data[, product_description := gsub('/', '+', product_description, fixed = TRUE)]
    data[, product_description := gsub(' + ', '+', product_description, fixed = TRUE)]
    data[, product_description := gsub('\\smg', 'mg', product_description)]
    data[, product_description := gsub('  ', ' ', product_description, fixed = TRUE)]
    data[, product_description := gsub('Atanzavir Sulfate', 'Atanzavir+Sulfate', product_description, fixed = TRUE)]
    data[, product_description := gsub('mg+ml', 'mg/ml', product_description, fixed = TRUE)]
    
    data[, product_description_copy := product_description]
    data[, product_description_copy := gsub(' - no carton', '', product_description_copy, fixed = TRUE)]
    data[, product_name := unlist(lapply(product_description_copy, function (x) {str_split_fixed(x, ' ', 3)[1]}))]
    data[, dose := unlist(lapply(product_description_copy, function (x) {str_split_fixed(x, ' ', 3)[2]}))]
    data[, description := unlist(lapply(product_description_copy, function (x) {str_split_fixed(x, ' ', 3)[3]}))]
    data[, pack_size := gsub("^.* ", "", description)]
    data[, description := trimws(description)]
    data[, description := unlist(lapply(description, function (x) {gsub(data[description == x, unique(pack_size)], '', x, fixed = TRUE)}))]
    data[, paste_no_carton := ifelse(grepl(product_description, pattern = 'no carton'), TRUE, FALSE)]
    data[, description := ifelse(paste_no_carton == TRUE, paste0(description, ' - no carton'), description)]
    data[, product_description_copy := NULL]
    data[, paste_no_carton := NULL]
  } else {
    stop('Date has not been checked in the prep process - please look at the prep manually to be sure the nuances are correct.')
  }
  
  data[, date:= date]
  
  list_of_dts[[i]] = data
}

dt = rbindlist(list_of_dts, use.names = TRUE, fill = TRUE)
dt[, description := trimws(description)]
dt[, description := tolower(description)]
dt[, description := gsub('  ', ' ', description)]
dt[description == 'tab', description := 'tablet']
dt[description == 'co-blistered tablet', description := 'tablet co-blistered']
dt[description == 'dispersible tablet', description := 'tablet dispersible']
dt[description == 'scored tablet', description := 'tablet scored']
dt[description == 'capsule delayed release 30', description := 'capsule delayed release']
dt[description == '30+60 tablet co-blistered', description := 'tablet co-blistered']
dt[description == '60+30 tablet co-blistered', description := 'tablet co-blistered']
dt[description == 'capsule+oral granules', description := 'capsules of oral pellets']

dt[, pack_size := gsub('ml', '', pack_size)]
dt[pack_size == '30+60', pack_size := '90']
dt[pack_size == '90 (30+60)', pack_size := '90']
dt[pack_size == '60*5', pack_size := '300']
dt[, pack_size := trimws(pack_size)]

dt[, dose := gsub('0\\+', '0mg\\+', dose)]
dt[, dose := trimws(dose)]

dt[product_name == 'Atanzavir+Sulfate+Ritonavir+Lamividuine+Zidovudine', product_name := 'Atazanavir+Ritonavir+Lamivudine+Zidovudine']
dt[dose == '(300mg+100mg)+(150mg+300mg)', dose := '300mg+100mg+150mg+300mg']
dt[, dispersible := ifelse(grepl('dispers', description), TRUE, FALSE)]
dt[, pack_size := as.integer(pack_size)]
dt[dose == '50mg+5ml', dose := '50mg/5ml']
dt[, description:= gsub(' dispersible', '', description)]
# note we need to clean and use description variable since the price varies by capsule vs tablet vs scored tablet
dt[product_name == 'Dolutegravir', description := 'tablet']
dt[, dose := gsub('\\(', '', dose)]
dt[, dose := gsub('\\)', '', dose)]
dt[, dose := gsub(' ', '', dose)]
dt[product_name == 'Lopinavir+Ritonavir' & dose == '40mg+10mg', description := 'capsule' ]

dt[grepl(dose, pattern = 'ml'), description:= 'liquid']
# ----------------------------------------------

# ----------------------------------------------
# combine with PQR data
# ----------------------------------------------
pqr = readRDS("J:/Project/Evaluation/GF/vfm/unit_cost_data/prepped_data/prepped_full_pqr_with_sept_download_data.rds")
pqr_arvs = pqr[product_category == 'anti_retroviral',]

setnames(pqr_arvs, 'product_description', 'dose')
setnames(pqr_arvs, 'nb_units_in_pack', 'pack_size')
setnames(pqr_arvs, 'product_name_en', 'product_name')

pqr_arvs[, dispersible := ifelse(grepl('dispers', description), TRUE, FALSE)]

pqr_arvs[, description := gsub('\\|', '', description)]
pqr_arvs[, description := gsub('  ', ' ', description)]

pqr_arvs[, product_name := gsub(' - FDC', '', product_name)]
pqr_arvs[, product_name := gsub('\\s\\((.*)', '', product_name)]
pqr_arvs[, product_name := gsub(' - Co-blister', '', product_name)]
pqr_arvs[, product_name := gsub(' ', '', product_name)]
pqr_arvs[product_name == 'Dolutegravir+Lamivudine+TenofovirDF', product_name := 'Dolutegravir+Lamivudine+Tenofovir']

setnames(dt, 'date', 'reference_price_date')
setnames(dt, 'reference_price_usd', 'ppm_ref_price')
pqr_arvs = pqr_arvs[purchase_order_date >= min(unique(dt$reference_price_date)), ]
setnames(pqr_arvs, 'description', 'full_description')

pqr_arvs[grepl(full_description, pattern = 'tab'), description:= 'tablet']
pqr_arvs[grepl(full_description, pattern = 'cap'), description:= 'capsule']
pqr_arvs[grepl(full_description, pattern = 'liq'), description:= 'liquid']

#check_pqr = unique(pqr_arvs[, .(product_name, pack_size, dose, dispersible, description)])
#check_dt = unique(dt[, .(product_name, pack_size, dose, dispersible, description)])
dt[duplicated(dt[, c('reference_price_date', 'product_name', 'dose', 'pack_size', 'dispersible', 'description')])]
# Darunavir has two identical entries for 2017-03-01 except for different reference prices - I checked
# and this is also in the original pdf files.  take average for now...
dt[product_name == 'Darunavir' & reference_price_date == '2017-03-01', ppm_ref_price := 67.50]
dt = unique(dt)

merged_dt = data.table()
ordered_dates = unique(dt$reference_price_date)
ordered_dates = sort(ordered_dates)

for (i in 1:length(ordered_dates)) {
  current_date = ordered_dates[i]
  if (i == length(ordered_dates)){ 
    next_date = max(pqr_arvs$purchase_order_date) + 1
  } else {
    next_date = ordered_dates[i+1]
  }
  
  subset_dt = dt[reference_price_date == current_date,]
  subset_pqr = pqr_arvs[purchase_order_date >= current_date & purchase_order_date < next_date, ]
  
  if (nrow(subset_pqr)==0) next
  
  current = merge(subset_pqr, subset_dt, all.x = TRUE, by = c('product_name', 'dose', 'pack_size', 'dispersible', 'description'))
  if ( nrow(current[duplicated(current[,.(primary_key)])]) != 0 ) stop( 'Something went wrong with the merge... check it' )
  
  if (nrow(merged_dt)==0) {
    merged_dt = current
  } else {
    merged_dt = rbind(merged_dt, current) 
  }
  
  if ( nrow(merged_dt[duplicated(merged_dt[,.(primary_key)])]) != 0 ) stop( 'Primary key now duplicated in merged data... check it' )
}

saveRDS(merged_dt, 'J:/Project/Evaluation/GF/vfm/unit_cost_data/prepped_data/prepped_pqr_arvs_with_ppm_ref_prices.rds')
# ----------------------------------------------
