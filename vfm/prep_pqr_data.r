# ----------------------------------------------
# David Phillips
# 
# 12/18/2018
# Prep PQR data for analysis
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(readxl)
library(zoo) # for na.locf
library(stringr)
# --------------------


# ----------------------------------------------
# Files and directories

# switch for cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')

# input file
dir = paste0(j, '/Project/Evaluation/GF/vfm/')
transactionFile = paste0(dir, 'unit_cost_data/download_12.18.18/Transaction List All - External Search.csv')
referenceFileARV = paste0(dir, 'unit_cost_data/download_12.18.18/PQR Public Anti-Retroviral.xlsx')
referenceFileTB = paste0(dir, 'unit_cost_data/download_12.18.18/PQR Public Anti-TB medicine.xlsx')
referenceFileMal = paste0(dir, 'unit_cost_data/download_12.18.18/PQR Public Anti-malaria medicine.xlsx')
referenceFileTests = paste0(dir, 'unit_cost_data/download_12.18.18/PQR Public Diagnostic test.xlsx')

# output files
outFile1 = paste0(dir, 'outputs/pqr_transaction_data_prepped.rdata')
outFile2 = paste0(dir, 'outputs/pqr_data_prepped_with_reference_prices.rdata')
outFile3 = paste0(dir, 'outputs/pqr_data_prepped_with_reference_prices_all_countries.rdata')
# ----------------------------------------------


# -----------------------------------------------------------------------------------
# Load/prep reference prices datasets

# load
arvPrices = data.table(read_excel(referenceFileARV, sheet='Prices', skip=8))
tbPrices = data.table(read_excel(referenceFileTB, sheet='Prices', skip=8))
antimalariaPrices = data.table(read_excel(referenceFileMal, sheet='Prices', skip=8))
testPrices = data.table(read_excel(referenceFileTests, sheet='Prices', skip=10))

# subset variables
arvPrices = arvPrices[, c(1, 5, 10, 11, 12, 14, 15, 16), with=FALSE]
tbPrices = tbPrices[, c(1, 6, 10, 12, 13, 14, 15, 16), with=FALSE]
antimalariaPrices = antimalariaPrices[, c(1, 6, 10, 11, 12, 13, 14, 15), with=FALSE]
testPrices = testPrices[, c(1, 6, 10, 11, 12, 13, 14), with=FALSE]

# set names
varNames = c('product','description','reference_countries','units',
	'reference_quantity', 'mean_reference','median_reference','min_reference')
setnames(arvPrices, varNames)
setnames(tbPrices, varNames)
setnames(antimalariaPrices, varNames)
varNames = c('product','manufacturer', 'reference_countries','reference_quantity', 
	'mean_reference','median_reference','min_reference')
setnames(testPrices, varNames)

# split descriptions from names for test price data
# NEED TO DO

# identify datasets
arvPrices[, type:='arv']
tbPrices[, type:='tb_meds']
antimalariaPrices[, type:='antimalarials']
testPrices[, type:='tests']

# rbind
prices = rbind(arvPrices, tbPrices)
prices = rbind(prices, antimalariaPrices)
prices = rbind(prices, testPrices, fill=TRUE)

# subset observations
prices = prices[!is.na(mean_reference)]

# 'carry down' product name
prices[, product:=na.locf(product)]
# -----------------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Load/prep observed transaction datasets

# load
transactions = fread(transactionFile)

# set names
varNames = c('country', 'grant_number', 'supplier', 
	'manufacturer', 'product', 'description', 'product_pack', 
	'pack_quantity', 'pack_cost', 'total_product_cost', 'order_date', 
	'scheduled_delivery_date', 'actual_delivery_date', 'units_per_pack', 
	'prepaid', 'freight_cost', 'invoice_number', 'purchase_order_number', 
	'invoice_currency', 'primary_key', 'status')
setnames(transactions, varNames)

# format dates
# somebody do this for me
transactions[, year:=as.numeric(str_sub(order_date,1,4))]

# subset rows to only those that can compute unit costs
transactions = transactions[!is.na(pack_cost)]

# compute unit costs from pack costs and pack sizes
transactions[, unit_cost:=pack_cost/units_per_pack]
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Merge references prices to transactions

# harmonize product names
tproducts = unique(transactions$product)
pproducts = unique(prices$product)
tproducts[!tproducts %in% pproducts]
pproducts[!pproducts %in% tproducts]
matches = tproducts[tproducts %in% pproducts]
# CANNOT FIND ANY THAT COULD BE MATCHED BEYOND WHAT ALREADY MATCHES...

# harmonize product descriptions
transactions[, description:=gsub('\\|', '', description)]
transactions[, description:=gsub('  ', ' ', description)]

# drop products from reference prices that don't match
prices = prices[product %in% tproducts]

# merge the product names that do match
test = nrow(prices) == nrow(unique(prices[,c('product','description'),with=F]))
if (test==FALSE) stop('Something is wrong. Product and Description do not 
	uniquely identify rows in the reference price dataset')
data = merge(transactions, prices, by=c('product','description'))
	
# subset to PCE countries
pce = c('Guatemala', 'Congo (Democratic Republic)', 'Uganda', 
	'Cambodia', 'Myanmar', 'Sudan', 'Senegal', 'Mozambique')
data_pce = data[country %in% pce]
# ------------------------------------------------------------------------


# -----------------------------
# Save
saveRDS(transactions, outFile1)
saveRDS(data_pce, outFile2)
saveRDS(data, outFile3)
# -----------------------------
