#-------------------------------------------------------------------
# AUTHOR: Emily Linebarger / Audrey Batzel
# PURPOSE: Prep PQR data from Global Fund's public-facing Tableau dashboard
#https://public.tableau.com/profile/the.global.fund#!/vizhome/PQRPricelist_English/PriceList
# DATE: Last updated April 2019
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# Set up libraries, directories, files
#-------------------------------------------------------------------
rm(list = ls())
library(data.table)

j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, "/Project/Evaluation/GF/vfm/unit_cost_data/")

inFile = "download_4.4.19/PQR_ExternalReportingView.csv"
inFile_new = "download_9.24.19/PQR_ExternalReportingView.csv"
outFile = "prepped_data/prepped_full_pqr_updated_09_2019.rds"

#-------------------------------------------------------------------
# Read in data
#-------------------------------------------------------------------
pqr_prev = fread(paste0(dir, inFile), stringsAsFactors = FALSE)

pqr_new = fread(paste0(dir, inFile_new), stringsAsFactors = FALSE)

pqr = copy(pqr_new)
#-------------------------------------------------------------------
# Drop columns that aren't needed and rename 
#-------------------------------------------------------------------
#Try to create a small subset of usable data, that just has our countries and a few key variables. 
key_cols = c("Country Name", "ISO3CodeCountry",
             'Invoice Amount (USD)', 'Invoice Creation Date', 'Invoice ID', 'Supplier Invoice Number', 'Invoice Date',
             "Purchase Order Date", "Purchase Order Month Name", "Purchase Order Month", "Purchase Order Quarter", "Purchase Order Week", "Purchase Order Year", "Purchase Order Latest Approval Date", "Purchase Order Original Approval Date",
             "Product Name (EN)", "Product Code", "Product ID", "Product Pack", "Product Category", "Unit", "Product Description", "Description", 
             "Strength", "Strength Dosage Form (EN)", "Suom Name (EN)", "Nb of Suom in Pack", "Nb of Suom in Primary Pack",
             "Pack quantity", "Pack Cost (USD)", "Product pack (USD)", "Total Product Cost (USD)", "Primary Key", "Product Key",
             "#tests/units", "Total Number of Products", "Unit Cost (USD)", 
             "PO Median Unit Cost", "PO International Reference Price",
             "Expected Cost by Median (USD)", "Expected Cost by Reference (USD)", "Cost Above Median (USD)", "Cost above reference (USD)",
             "Grant Name", "Grant ID", "Grant Start Date", "Grant End Date", "Grant Status", "IP Start Date", "IP End Date", 
             "PR Name", "PR Type Name", "PR Sub Type Name", "Supplier",
             "Scheduled Delivery Date", "Scheduled Delivery Month Name", "Scheduled Delivery Month", "Scheduled Delivery Quarter", "Scheduled Delivery Week", "Scheduled Delivery Year", 
             "Actual Delivery Date", "Actual Delivery Month Name", "Actual Delivery Month", "Actual Delivery Quarter", "Actual Delivery Week", "Actual Delivery Year",
             "Total Tariff Cost (USD)", "Total Freight & Insurance Cost (USD)", "Total Handling & Agent Cost (USD)",                              
             "Product Pack Name (EN)", "Secondary Pack Name (EN)",
             "Treatment Dose", "Treatment Frequency")

# # check if the columns in key_cols include all unique ids
# check_ids = pqr[, key_cols, with = FALSE]
# check_dups = check_ids[duplicated(check_ids)]
# check_dups = check_dups[`Country Name`=='Congo (Democratic Republic)']
# check_dups2 = pqr[`Country Name`== 'Congo (Democratic Republic)' & `Product Name (EN)` == 'Permanet 2.0' & `Total Package Quantity`== 508950 &
#                     +                     `Total Number of Products`==13 & `Purchase Order Date`== "11/10/2017 12:00:00 AM" & `Actual Delivery Date`== "12/25/2017 12:00:00 AM"]

# #Key cols - country, disease, product, unit, reference price (pull everything related), and pack size, and 
# #Unit, mean, median, and international reference price. 
# cost_cols = names[grep("cost|avg|median|price", tolower(names))]
# cost_cols = c(cost_cols, c("Supplier/Agent/Manufacturer/Intermediatry", "Manufacturer"))
# 
# all_cols = c(key_cols, cost_cols)
# names[!names%in%all_cols] #See what you haven't categorized yet. 
#
# #See what's going on with the different categories in the data. 
# lc_cols = grep("(LC)", names(subset)) #Drop all of the columns in 'local currency'
# subset = subset[, -lc_cols, with = FALSE]

# country_ids = pqr[`Country/Teritorry` %in% c("Congo (Democratic Republic)", "Senegal", "Uganda", "Guatemala", 
#                                         "Mozambique", "Sudan", "Myanmar", "Cambodia"), unique(`ISO3CodeCountry`) ]#Keep EHG's countries in here as well. 
# 
# pqr = pqr[`ISO3CodeCountry`%in% country_ids, ]

#Remove duplicated column names
dup_names = unique(names(pqr)[duplicated(names(pqr))])
if(length(dup_names) != 0){
  rm_vars = c()
  for (var in dup_names){
    for (i in 2:length(which( colnames(pqr)==var ))){
      rm_vars = c(rm_vars, which( colnames(pqr)==var )[i])
    }
  }
  pqr = pqr[, -rm_vars, with=FALSE]
}
nrow(unique(pqr)) == nrow(pqr) # at this point all rows are still uniquely id'ed
if( length(names(pqr)[duplicated(names(pqr))]) != 0 ) stop( 'You still have duplicates in column names!' ) #Run the duplicate check again - this data table should have 0 rows.

# # assess duplicates in rows:
# dt = pqr[, c(key_cols), with=FALSE] # the data table with vars we want
# nrow(unique(dt)) == nrow(dt) # at this point rows are NOT all uniquely id'ed
# 
# dups = dt[duplicated(dt)] # 174 duplicated rows, but some have multiple duplicates
# # dups2 = dups[duplicated(dups)] are there multiple duplicates
# # dups3 = dups2[duplicated(dups2)]
# 
# # get all duplicates with all rows 
# all_dups = merge(dups, pqr, all.x = TRUE, by = names(dups))
# all_dups = unique(all_dups)
# 
# # example of dups but one that's not caught in the above setting :/
# example = dt[`Product Name (EN)`== 'Permanet 2.0']
# example = example[`Grant Name`=='COD-M-PSI']
# setorderv(example, c("Supplier Invoice Number", "Total Product Cost (USD)"))
# 
# example = merge(example, pqr, all.x = TRUE)
# 
# keep_vars = names(example)[(example[1, ] == example[2, ]) == FALSE]
# keep_vars = keep_vars[!is.na(keep_vars)]
# 
# example_subset = example[, c("Country Name","Product Name (EN)", "Description", "Product Category", "Supplier Invoice Number", 
#                              "Total Product Cost (USD)", "Purchase Order Date")]

# drop duplicates of orders:
# try to eliminate duplicates across languages
pqr = pqr[`Product Category Language`=='en', ]
# unique_ids = c("Country Name", "Grant Name", "Product Name (EN)", "Supplier Invoice Number", "Total Product Cost (USD)", "Purchase Order Date", 
#                "Description", "Product Pack", "Pack quantity", "Scheduled Delivery Date", "Actual Delivery Date", "Product Key", "Primary Key")
# # unique_ids = c("Primary Key")
# # unique_ids = c("Invoice Item ID")
# dt = pqr[, c(unique_ids), with=FALSE]
# 
# # further check dt duplicates
# dups = dt[duplicated(dt)] 
# all_dups = merge(dups, pqr, all.x = TRUE, by= names(dups))
# all_dups = unique(all_dups)
# 
# # merge dt with pqr data subset to the key cols in order to get all columns we want
# subset = merge(dt, pqr[, ..key_cols], by = names(dt))

subset = copy(pqr)

#Reset names. 
old_names = names(subset)
new_names = tolower(old_names)
new_names = gsub("\\(", "", new_names)
new_names = gsub(")", "", new_names)
new_names = gsub("\\*", "", new_names)
new_names = gsub(" ", "_", new_names)
new_names = gsub("/", "_", new_names)
new_names = gsub("#tests", "nb_tests", new_names)
new_names = gsub("&", "and", new_names)
new_names = gsub("__", "_", new_names)

setnames(subset, old_names, new_names)

#-------------------------------------------------------------------
# Format dates correctly 
#-------------------------------------------------------------------
date_vars = new_names[ grepl(new_names, pattern = 'date') ]

for (v in date_vars){
  subset[, (v):=substr(get(v), 1, 11)]
  subset[, (v):=as.Date(get(v), format="%m/%d/%Y")]
}

# remove_vars = new_names[ grepl(new_names, pattern = 'month|year|quarter|week')]
# subset = subset[, -remove_vars, with=FALSE]

#-------------------------------------------------------------------
# Pull a 'grant disease' variable that can be used to flag 
# drug types, along with product_category. 
#-------------------------------------------------------------------
subset[, disease_split:=strsplit(grant_name, "-")]
potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')

for (i in 1:nrow(subset)){
  if (subset$disease_split[[i]][2]%in%potential_diseases){
    subset[i, grant_disease:=sapply(disease_split, "[", 2 )]
  } else if (subset$disease_split[[i]][3]%in%potential_diseases){
    subset[i, grant_disease:=sapply(disease_split, "[", 3 )]
  } else if (subset$disease_split[[i]][4]%in%potential_diseases){
    subset[i, grant_disease:=sapply(disease_split, "[", 4 )]
  }
}

subset[, disease_split:=NULL]

unique(subset[!grant_disease%in%potential_diseases, .(grant_name, grant_disease)]) #Visual check that these all make sense. 

subset[grant_disease=='C', grant_disease:='hiv/tb']
subset[grant_disease=='H', grant_disease:='hiv']
subset[grant_disease=='T', grant_disease:='tb']
subset[grant_disease=='S' | grant_disease=='R', grant_disease:='rssh']
subset[grant_disease=='M', grant_disease:='malaria']
subset[grant_disease=='Z' & grant_name=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 

stopifnot(unique(subset$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))

#-----------------------------------------------------------------------
# Fix the 'category' variable so it can be used to make disease subsets
#-----------------------------------------------------------------------
subset[, product_category := tolower(product_category)]
subset[, product_category := gsub('-', '_', product_category)]
subset[, product_category := gsub('/', '_', product_category)]
subset[, product_category := gsub(' ', '_', product_category)]
subset[product_category %in% c('anti_malaria_medicine', 'bednet_irs'), product_disease := 'malaria']
subset[product_category %in% c('anti_tb_medicine'), product_disease := 'tb']
subset[product_category %in% c('anti_retroviral', 'condom'), product_disease := 'hiv']
subset[product_category == 'diagnostic_test' & grepl("HIV", product_name), product_disease := 'hiv']
subset[product_category == 'diagnostic_test' & grepl("Malaria", product_name), product_disease := 'malaria']
subset[product_category == 'diagnostic_test' & grepl("TB", product_name), product_disease := 'tb']

#-------------------------------------------------------------------
# Flag some variables that are disease-specific THESE NEED TO BE VERIFIED BY A CLINICIAN EKL 6/10/19
#-------------------------------------------------------------------
hiv_prev = c("Female Condom", "Male Latex Condom")
hiv_test = c("HIV & hepatitis/syphilis combined tests", "HIV CD4 testing consumables/test kits", "HIV CD4 testing equipment", "HIV RDT and EIA", "HIV Testing Equipment (other than molecular)", "HIV virological testing consumables/test kits",
             "HIV virological testing equipment", "Microscopes & accessories")
hiv_treat = c("Lamivudine (3TC)", "Zidovudine (AZT or ZDV)","Efavirenz (EFV)", "Nevirapine (NVP)", "Tenofovir (TDF)", "Abacavir (ABC)", "Abacavir+Lamivudine - FDC", "Stavudine (d4T)", "Saquinavir (SQV)", "Ritonavir (RTV)", "Raltegravir",
              "Lamivudine+Nevirapine+Stavudine - FDC", "Lamivudine+Nevirapine+Zidovudine - FDC", "Lamivudine+Stavudine - FDC", "Lamivudine+Tenofovir - FDC", "Lamivudine+Zidovudine - FDC", "Darunavir (TCM)", "Atazanavir+Ritonavir - FDC",
              "Efavirenz+Emtricitabine+Tenofovir - FDC", "Efavirenz+Lamivudine+Tenofovir - FDC", "Emtricitabine+Tenofovir - FDC", "Etravirine (ETV)", "Lopinavir+Ritonavir - FDC", "Dolutegravir (as sodium salt)", "Didanosine (ddI)",
              "Abacavir+Lamivudine+Zidovudine - FDC", "Atazanavir (ATV)", "Dolutegravir + Lamivudine + Tenofovir DF - FDC", "Efavirenz + [Lamivudine+Zidovudine] - Co-blister", "Indinavir (IDV)")

mal_bednet = c("Duranet", "Insecticide-treated net (ITN)", "Long-Lasting Insecticidal Net (LLIN)", "MAGNet", "MiraNet", "Permanet 2.0", "Permanet 3.0", "Netprotect", "Yorkool LN", "Yahe LN", "Royal Sentry",
             "Olyset", "Dawa-Plus 2.0", "SafeNet", "Health Net", "Interceptor")
mal_irs = c("zz-p,p' DDT-WP", "zz-Pirimiphos Methyl CS", "zz-Deltamethrin-WG", "zz-Bendiocarb-WP")
mal_test = c("Malaria RDT: Pan", "Malaria RDT: P.f./P.v", "Malaria RDT: P.f.", "Microscopes & accessories", "Malaria RDT: P.f/Pan", "Malaria Rapid Diagnostic Test")
mal_treat = c("Quinine", "Artesunate", "Artesunate + [Sulfadoxine+Pyrimethamine] - Co-blister", "Artesunate + Amodiaquine - Co-blister", "Artesunate + Amodiaquine - FDC", "Artesunate + Mefloquine - Co-blister", "Artesunate + Mefloquine FDC",
              "Artesunate+Pyronaridine tetraphosphate", "Artemether", "Artemether+Lumefantrine - FDC", "Sulfadoxine+Pyrimethamine - FDC", "Chloroquine", "Chloroquine phosphate",
              "Dihydroartemisinin+Piperaquine - FDC", "Mefloquine", "Quinine-resorcine", "Primaquine")

tb_prev = c()
tb_test = c("TB testing consumables/test kits", "TB molecular diagnostics", "Microscopes & accessories", "TB testing equipment (non-molecular)")
#tb_treat = c("Isoniazid", "Rifampicin", "Pyrazinamide", "Streptomycin", "Ethambutol", "Isoniazid+Pyrazinamide+Rifampicin - FDC", "Isoniazid+Rifampicin - FDC", "Ethambutol+Isoniazid+Rifampicin - FDC",
             #"Amikacin", "Protionamide", "PAS Sodium", "Ofloxacin", "Cycloserine", "Capreomycin", "Bedaquiline", "Amoxicillin+Clavulanate - FDC", "Ethambutol+Isoniazid - FDC", "Ethambutol+Isoniazid+Pyrazinamide+Rifampicin (RHZE",
             #"Ethionamide", "Kanamycin", "Levofloxacin", "Moxifloxacin", "Meropenem", "Linezolid", "Clofazimine")

tb_drugs1 = c("Rifampicin" , "Pyrazinamide", "Ethambutol+Isoniazid+Pyrazinamide+Rifampicin (RHZE)- FDC","Ethambutol+Isoniazid+Pyrazinamide+Rifampicin (RHZE", 
              "Isoniazid", "Ethambutol+Isoniazid+Rifampicin - FDC", "Isoniazid+Pyrazinamide+Rifampicin - FDC", "Isoniazid+Rifampicin - FDC", "Ethambutol+Isoniazid - FDC",
              "Ethambutol", "TB Cat. I+III Patient Kit A")
tb_drugs2 = c("Capreomycin", "Clofazimine", "Cycloserine" , "Kanamycin", "Moxifloxacin",  "Bedaquiline", "Ethionamide", "Levofloxacin", "Linezolid", "Protionamide", "Amikacin", 
              "Streptomycin", "Delamanid", "PAS Sodium", "Ofloxacin", "Imipenem+Cilastatin inj", "Amoxicillin+Clavulanate - FDC", "Meropenem")

other = c("Hepatitis B assays", "Hepatitis C assays", "Syphilis tests", "Water for injection", "Sofosbuvir")

#Check that these variables have captured everything
classified = c(hiv_prev, hiv_test, hiv_treat, mal_bednet, mal_test, mal_treat, mal_irs, tb_prev, tb_test, tb_drugs1, tb_drugs2, other)
unique(subset[!product_name%in%classified, .(product_name)][order(product_name)])

# hiv_subset = subset[product_name_en%in%hiv_prev | product_name_en%in%hiv_test | product_name_en%in%hiv_treat]
# mal_subset = subset[product_name_en%in%mal_prev | product_name_en%in%mal_test | product_name_en%in%mal_treat]
# tb_subset = subset[product_name_en%in%tb_prev | product_name_en%in%tb_test | product_name_en%in%tb_treat]

subset[product_category == 'diagnostic_test', product_category := paste(product_category, product_disease, sep= '_')]
subset[product_category == 'diagnostic_test_NA', product_category := 'diagnostic_test_other']

subset[product_category == 'bednet_irs' & product_name %in% mal_bednet, product_category := 'bednet']
subset[product_category == 'bednet_irs' & product_name %in% mal_irs, product_category := 'irs']
#-------------------------------------------------------------------
# Clean supplier variable
#-------------------------------------------------------------------
subset[ supplier == 'United Nations Population Fund (UNFPA)', supplier := 'UN Population Fund']
subset[ supplier == 'Médecins sans Frontières (MSF)', supplier := 'MSF']
#-------------------------------------------------------------------
# Other data fixes/changes
#-------------------------------------------------------------------
# # remove repetitive vars
# subset[, strength := NULL]
# subset[, product_pack_usd := NULL]

# subset[ supplier == '', supplier:= NA]

remove_vars = names(subset)[ grepl(names(subset), pattern = 'langfilter')]
subset = subset[, -remove_vars, with=FALSE]
remove_vars = names(subset)[ grepl(names(subset), pattern = 'language')]
subset = subset[, -remove_vars, with=FALSE]

setnames(subset, 'country_teritorry', 'country_name')

# subset[nb_tests_units != pack_quantity * nb_of_suom_in_pack]
# setnames(subset, 'nb_tests_units', 'total_units_in_order')

setnames(subset, 'pack_quantity', 'nb_packs_ordered')
setnames(subset, 'nb_of_suom_in_pack', 'nb_units_in_pack')
setnames(subset, 'total_number_of_suom', 'total_units_in_order')

subset[is.na(nb_packs_ordered), nb_packs_ordered := as.integer(total_units_in_order/nb_units_in_pack)]
subset[ , total_cost_order := ifelse(( !is.na(nb_packs_ordered)& !is.na(pack_cost_usd) ), total_units_in_order * unit_cost_usd, total_product_cost_usd)]

# # check these against internally calculated vars
# subset[, unit_cost_usd := pack_cost_usd / nb_units_in_pack]
# subset[, expected_cost_by_reference := po_international_reference_price * total_units_in_order]
# subset[, cost_above_reference := total_cost_order - expected_cost_by_reference]
#-------------------------------------------------------------------
# Save data 
#-------------------------------------------------------------------
saveRDS(subset, paste0(dir, outFile))
# saveRDS(hiv_subset, paste0(dir, "prepped_data/hiv_pqr.rds"))
# saveRDS(mal_subset, paste0(dir, "prepped_data/mal_pqr.rds"))
# saveRDS(tb_subset, paste0(dir, "prepped_data/tb_pqr.rds"))
# saveRDS(subset, paste0(dir, 'pqr_all_countries.rds'))
# write.csv(subset, paste0(dir, 'pqr_all_countries.rds'))
#-------------------------------------------------------------------



#-------------------------------------------------------------------
# AUDREY IGNORE CODE BELOW HERE! 
#-------------------------------------------------------------------
#-------------------------------------------------------------------
# # Make a specialized Guatemala TB grants file 
#-------------------------------------------------------------------
# gtm = subset[country_name=="Guatemala" & grant_disease%in%c('tb', 'hiv/tb')]
# gtm = gtm[order(-grant_start_date)]
# saveRDS(gtm, "J:/Project/Evaluation/GF/impact_evaluation/gtm/prepped_data/pqr_data.rds")
# 
# 
# 
# #RMEI code- drop this somewhere else 
# #-----------------------------------------------------------
# # rmei_vars = c('Volume', "Actual Delivery Month", "Actual Delivery Year", "Other Bednet Size (Invoice)", "Strength", "Pack quantity", "Product Name (EN)", 'Country Name')
# # rmei_interest_vars = c(mal_prev, mal_test, mal_treat)
# # 
# # rmei_locs = c("Belize", "Colombia", "Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama")
# # rmei_data = pqr[`Country Name`%in%rmei_locs, rmei_vars, with=FALSE]
# # rmei_data = rmei_data[`Product Name (EN)`%in%rmei_interest_vars]
# # rmei_data = rmei_data[, .(`Product Name (EN)`, `Country Name`, `Actual Delivery Year`)][order(`Country Name`, `Actual Delivery Year`)]
# # names(rmei_data) = c('product', 'country', 'delivery_year')
# # 
# # mal_nets = c("Duranet", "Insecticide-treated net (ITN)", "Long-Lasting Insecticidal Net (LLIN)", "MAGNet", "MiraNet", "Permanet 2.0", "Permanet 3.0", "Netprotect", "Yorkool LN", "Yahe LN", "Royal Sentry", 
# #              "Olyset", "Dawa-Plus 2.0")
# # irs = "zz-Pirimiphos Methyl CS"
# # 
# # rmei_data[product%in%mal_nets, category:="LLIN"]
# # rmei_data[product%in%irs, category:="IRS"]
# # rmei_data[product%in%mal_treat, category:="Treatment"]
# # rmei_data[product%in%mal_test, category:="Testing"]
# # 
# # write.csv(rmei_data, paste0(dir, "/prepped_data/rmei_key_variables.csv"), row.names = FALSE)
# #--------------------------------------------------------------
# 
# 
# 
# 
# 
