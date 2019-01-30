# ----------------------------------------------
# David Phillips
# 
# 1/30/2019
# List products in transactions, reference prices, both and neither for secretariat
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
dir = 'C:/local/GF_copy/vfm/'
transactionFile = paste0(dir, 'unit_cost_data/Transaction List All - External Search.csv')
referenceFileARV = paste0(dir, 'unit_cost_data/PQR Public Anti-Retroviral.xlsx')
referenceFileTB = paste0(dir, 'unit_cost_data/PQR Public Anti-TB medicine.xlsx')
referenceFileMal = paste0(dir, 'unit_cost_data/PQR Public Anti-malaria medicine.xlsx')
referenceFileTests = paste0(dir, 'unit_cost_data/PQR Public Diagnostic test.xlsx')

# output files
outFile = paste0(dir, 'outputs/product_lists.csv')
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

# rbind
prices = rbind(arvPrices, tbPrices)
prices = rbind(prices, antimalariaPrices)
prices = rbind(prices, testPrices, fill=TRUE)

# subset observations
prices = prices[!is.na(mean_reference)]
prices = prices[reference_countries!='COUNTRIES']

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

# harmonize product descriptions
transactions[, description:=gsub('\\|', '', description)]
transactions[, description:=gsub('  ', ' ', description)]

# list product names
tproducts = unique(transactions$product)
pproducts = unique(prices$product)
both = pproducts[pproducts %in% tproducts]
missing = tproducts[!tproducts %in% pproducts]

# display numbers
length(tproducts)
length(pproducts)
length(both)
length(missing)

# assemble into dataset
pproducts = c(pproducts, rep(NA, length(tproducts)-length(pproducts)))
both = c(both, rep(NA, length(tproducts)-length(both)))
missing = c(missing, rep(NA, length(tproducts)-length(missing)))
tproducts = tproducts[order(tproducts)]
pproducts = pproducts[order(pproducts)]
both = both[order(both)]
missing = missing[order(missing)]
all_products = data.table('products_in_transactions'=tproducts, 'products_in_reference_prices'=pproducts, 
	'products_in_both'=both, 
	'products_in_transactions_but_not_reference_prices'=missing)
for(v in names(all_products)) all_products[is.na(get(v)), (v):='']
# ------------------------------------------------------------------------


# -----------------------------------------------
# Save
write.csv(all_products, outFile, row.names=FALSE)
# -----------------------------------------------
