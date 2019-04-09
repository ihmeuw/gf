#-------------------------------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Prep PQR data from Global Fund's public-facing Tableau dashboard
#https://public.tableau.com/profile/the.global.fund#!/vizhome/PQRPricelist_English/PriceList
# DATE: Last updated April 2019
#-------------------------------------------------------------------

#-------------------------------------------------------------------
#Set up libraries, and read data 
#-------------------------------------------------------------------

library(data.table)
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, "/Project/Evaluation/GF/vfm/unit_cost_data")
setwd(dir)

pqr = fread("download_4.4.19/PQR_ExternalReportingView.csv", stringsAsFactors = FALSE)

#-------------------------------------------------------------------
# Drop columns that aren't needed and rename 
#-------------------------------------------------------------------
sort(names(pqr))

#Try to create a small subset of usable data, that just has our countries and a few key variables. 
key_cols = c("Country Name", "Grant Name", "Grant Start Date", "Grant End Date", "IP Start Date", "IP End Date", "Actual Delivery Date", "Actual Delivery Month Name", 
             "Actual Delivery Month", "Actual Delivery Quarter", "Actual Delivery Week", "Actual Delivery Year", "Product Name (EN)", "Product Code", "Strength Dosage Form (EN)", "Strength",
             "Supplier", "Tariff Cost (LC)", "Tariff Cost (USD)", "Total Freight & Insurance Cost (USD)", "Total Handling & Agent Cost (USD)", "Total Package Quantity",                                 
             "Total Product Cost (USD)", "Total Tariff Cost (USD)", "Treatment Dose", "Treatment Frequency", "Unit Cost (USD)", "Unit Cost : Avg Diag",
             "Unit Cost : Avg detail", "Unit Cost : Avg Diag old", "Purchase Order Date", "Purchase Order Month Name", "Purchase Order Month", "Purchase Order Quarter", "Purchase Order Week", "Purchase Order Year", "Purchase Order Latest Approval Date",                        
             "Purchase Order Original Approval Date", "Scheduled Delivery Date", "Scheduled Delivery Month Name", "Scheduled Delivery Month", "Scheduled Delivery Quarter", 
             "Scheduled Delivery Week", "Scheduled Delivery Year", "Product Category")

countries = c("Congo (Democratic Republic)", "Senegal", "Uganda", "Guatemala")

subset = pqr[`Country Name`%in%countries, key_cols, with=FALSE]

old_names = names(subset)
new_names = tolower(old_names)
new_names = gsub("\\(", "", new_names)
new_names = gsub(")", "", new_names)
new_names = gsub(" ", "_", new_names)

setnames(subset, old_names, new_names)

#-------------------------------------------------------------------
# Flag some variables that are disease-specific
#-------------------------------------------------------------------
hiv_prev = c("Female Condom", "Male Latex Condom")
hiv_test = c("HIV & hepatitis/syphilis combined tests", "HIV CD4 testing consumables/test kits", "HIV CD4 testing equipment", "HIV RDT and EIA", "HIV Testing Equipment (other than molecular)", "HIV virological testing consumables/test kits", 
             "HIV virological testing equipment", "Microscopes & accessories")
hiv_treat = c("Lamivudine (3TC)", "Zidovudine (AZT or ZDV)","Efavirenz (EFV)", "Nevirapine (NVP)", "Tenofovir (TDF)", "Abacavir (ABC)", "Abacavir+Lamivudine - FDC", "Stavudine (d4T)", "Saquinavir (SQV)", "Ritonavir (RTV)", "Raltegravir", 
              "Lamivudine+Nevirapine+Stavudine - FDC", "Lamivudine+Nevirapine+Zidovudine - FDC", "Lamivudine+Stavudine - FDC", "Lamivudine+Tenofovir - FDC", "Lamivudine+Zidovudine - FDC", "Darunavir (TCM)", "Atazanavir+Ritonavir - FDC", 
              "Efavirenz+Emtricitabine+Tenofovir - FDC", "Efavirenz+Lamivudine+Tenofovir - FDC", "Emtricitabine+Tenofovir - FDC", "Etravirine (ETV)", "Lopinavir+Ritonavir - FDC", "Dolutegravir (as sodium salt)", "Didanosine (ddI)")

mal_prev = c("Duranet", "Insecticide-treated net (ITN)", "Long-Lasting Insecticidal Net (LLIN)", "MAGNet", "MiraNet", "Permanet 2.0", "Permanet 3.0", "Netprotect", "zz-Pirimiphos Methyl CS", "Yorkool LN", "Yahe LN", "Royal Sentry", 
             "Olyset", "Dawa-Plus 2.0")
mal_test = c("Malaria RDT: Pan", "Malaria RDT: P.f./P.v", "Malaria RDT: P.f.", "Microscopes & accessories", "Malaria RDT: P.f/Pan")
mal_treat = c("Quinine", "Artesunate", "Artesunate + [Sulfadoxine+Pyrimethamine] - Co-blis", "Artesunate + Amodiaquine - Co-blister", "Artesunate + Amodiaquine - FDC", "Artesunate + Mefloquine - Co-blister", "Artesunate + Mefloquine FDC", 
              "Artesunate+Pyronaridine tetraphosphate", "Artemether", "Artemether+Lumefantrine - FDC", "Sulfadoxine+Pyrimethamine - FDC")

tb_prev = c()
tb_test = c("TB testing consumables/test kits", "TB molecular diagnostics", "Microscopes & accessories")
tb_treat = c("Isoniazid", "Rifampicin", "Pyrazinamide", "Streptomycin", "Ethambutol", "Isoniazid+Pyrazinamide+Rifampicin - FDC", "Isoniazid+Rifampicin - FDC", "Ethambutol+Isoniazid+Rifampicin - FDC", 
             "Amikacin", "Protionamide", "PAS Sodium", "Ofloxacin", "Cycloserine", "Capreomycin", "Bedaquiline", "Amoxicillin+Clavulanate - FDC", "Ethambutol+Isoniazid - FDC", "Ethambutol+Isoniazid+Pyrazinamide+Rifampicin (RHZE", 
             "Ethionamide", "Kanamycin", "Levofloxacin", "Moxifloxacin", "Meropenem", "Linezolid", "Clofazimine")
  
#Check that these variables have captured everything 
classified = c(hiv_prev, hiv_test, hiv_treat, mal_prev, mal_test, mal_treat, tb_prev, tb_test, tb_treat)
unique(subset[!product_name_en%in%classified, .(product_name_en)][order(product_name_en)])


hiv_subset = subset[product_name_en%in%hiv_prev | product_name_en%in%hiv_test | product_name_en%in%hiv_treat]
mal_subset = subset[product_name_en%in%mal_prev | product_name_en%in%mal_test | product_name_en%in%mal_treat]
tb_subset = subset[product_name_en%in%tb_prev | product_name_en%in%tb_test | product_name_en%in%tb_treat]

#-------------------------------------------------------------------
# Save data 
#-------------------------------------------------------------------

saveRDS(subset, "prepped_data/basic_pqr.rds")
saveRDS(hiv_subset, "prepped_data/hiv_pqr.rds")
saveRDS(mal_subset, "prepped_data/mal_pqr.rds")
saveRDS(tb_subset, "prepped_data/tb_pqr.rds")







#RMEI code- drop this somewhere else 
#-----------------------------------------------------------
rmei_vars = c('Volume', "Actual Delivery Month", "Actual Delivery Year", "Other Bednet Size (Invoice)", "Strength", "Pack quantity", "Product Name (EN)", 'Country Name')
rmei_interest_vars = c(mal_prev, mal_test, mal_treat)

rmei_locs = c("Belize", "Colombia", "Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama")
rmei_data = pqr[`Country Name`%in%rmei_locs, rmei_vars, with=FALSE]
rmei_data = rmei_data[`Product Name (EN)`%in%rmei_interest_vars]
rmei_data = rmei_data[, .(`Product Name (EN)`, `Country Name`, `Actual Delivery Year`)][order(`Country Name`, `Actual Delivery Year`)]
names(rmei_data) = c('product', 'country', 'delivery_year')

mal_nets = c("Duranet", "Insecticide-treated net (ITN)", "Long-Lasting Insecticidal Net (LLIN)", "MAGNet", "MiraNet", "Permanet 2.0", "Permanet 3.0", "Netprotect", "Yorkool LN", "Yahe LN", "Royal Sentry", 
             "Olyset", "Dawa-Plus 2.0")
irs = "zz-Pirimiphos Methyl CS"

rmei_data[product%in%mal_nets, category:="LLIN"]
rmei_data[product%in%irs, category:="IRS"]
rmei_data[product%in%mal_treat, category:="Treatment"]
rmei_data[product%in%mal_test, category:="Testing"]

write.csv(rmei_data, paste0(dir, "/prepped_data/rmei_key_variables.csv"), row.names = FALSE)
#--------------------------------------------------------------





