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
names = names(pqr)
sort(names(pqr))

#Do we have any more granular information than the 

#Try to create a small subset of usable data, that just has our countries and a few key variables. 
key_cols = c("Country Name", "Grant Name", "Grant Start Date", "Grant End Date", "IP Start Date", "IP End Date", "Actual Delivery Date", "Actual Delivery Month Name", 
             "Actual Delivery Month", "Actual Delivery Quarter", "Actual Delivery Week", "Actual Delivery Year", "Product Name (EN)", "Product Code", "Strength Dosage Form (EN)", "Strength",
             "Supplier", "Tariff Cost (LC)", "Tariff Cost (USD)", "Total Freight & Insurance Cost (USD)", "Total Handling & Agent Cost (USD)", "Total Package Quantity",                                 
             "Total Product Cost (USD)", "Total Tariff Cost (USD)", "Treatment Dose", "Treatment Frequency", "Unit Cost (USD)", "Unit Cost : Avg Diag",
             "Unit Cost : Avg detail", "Unit Cost : Avg Diag old", "Purchase Order Date", "Purchase Order Month Name", "Purchase Order Month", "Purchase Order Quarter", "Purchase Order Week", "Purchase Order Year", "Purchase Order Latest Approval Date",                        
             "Purchase Order Original Approval Date", "Scheduled Delivery Date", "Scheduled Delivery Month Name", "Scheduled Delivery Month", "Scheduled Delivery Quarter", 
             "Scheduled Delivery Week", "Scheduled Delivery Year", "Product Category", "PR Name", "Prtype", "PR Type Code", "PR Type Name", "PR Sub Type Name",
             "PR Sub Type Code", "Sub Continent Name", "Sub National Name", "Suom Name")

#Key cols - country, disease, product, unit, reference price (pull everything related), and pack size, and 
#Unit, mean, median, and international reference price. 
cost_cols = names[grep("cost|avg|median|price", tolower(names))]
cost_cols = c(cost_cols, c("Supplier/Agent/Manufacturer/Intermediatry", "Manufacturer"))

all_cols = c(key_cols, cost_cols)
names[!names%in%all_cols] #See what you haven't categorized yet. 

countries = c("Congo (Democratic Republic)", "Senegal", "Uganda", "Guatemala", "Mozambique", "Sudan", "Myanmar", "Cambodia") #Keep EHG's countries in here as well. 

subset = pqr[`Country Name`%in%countries, c(key_cols, cost_cols), with=FALSE]

#See what's going on with the different categories in the data. 
lc_cols = grep("(LC)", names(subset)) #Drop all of the columns in 'local currency'
subset = subset[, -lc_cols, with = FALSE]

#Review duplicated variable names 
dup_names = names(subset)[duplicated(names(subset))]
dup_names

#An ugly way to do this- just force drop these duplicate names 
subset = subset[, -dup_names, with=FALSE]
names(subset)[duplicated(names(subset))] #Run the duplicate check again - this data table should have 0 rows. 

#Reset names. 
old_names = names(subset)
new_names = tolower(old_names)
new_names = gsub("\\(", "", new_names)
new_names = gsub(")", "", new_names)
new_names = gsub(" ", "_", new_names)

setnames(subset, old_names, new_names)

#-------------------------------------------------------------------
# Format dates correctly 
#-------------------------------------------------------------------
date_vars = c('grant_start_date', 'grant_end_date', 'ip_start_date', 'ip_end_date', 'actual_delivery_date', 'purchase_order_date', 
              'scheduled_delivery_date')
for (v in date_vars){
  subset[, (v):=substr(get(v), 1, 11)]
  subset[, (v):=as.Date(get(v), format="%m/%d/%Y")]
}

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
subset[product_category%in%c("Pruebas de diagnÃ³stico", "Tests de diagnostic", "Diagnostic test"), product_category:="DIAGNOSTIC TESTS"]
subset[product_category%in%c("Mosquitero/IRS", "Bednet/IRS", "Moustiquaire/IRS"), product_category:="INSECTICIDE-TREATED NET"]
subset[product_category%in%c("Anti-Retroviral", "AntirÃ©troviral", "Antirretroviral"), product_category:="ANTI-RETROVIRAL"]
subset[product_category%in%c("PrÃ©servatif", "Condom", "Preservativo"), product_category:="CONDOM"]
subset[product_category%in%c("Anti-malaria medicine", "AntipaludÃ©en", "AntimalÃ¡rico"), product_category:="ANTI-MALARIAL MEDICINE"]
subset[product_category%in%c("Antituberculeux", "Anti-TB medicine", "Antituberculoso"), product_category:="ANTI-TB MEDICINE"]

#-------------------------------------------------------------------
# Flag some variables that are disease-specific THESE NEED TO BE VERIFIED BY A CLINICIAN EKL 6/10/19
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


#-------------------------------------------------------------------
# Make a specialized Guatemala TB grants file 
#-------------------------------------------------------------------
gtm = subset[country_name=="Guatemala" & grant_disease%in%c('tb', 'hiv/tb')]
gtm = gtm[order(-grant_start_date)]
saveRDS(gtm, "J:/Project/Evaluation/GF/impact_evaluation/gtm/prepped_data/pqr_data.rds")



#RMEI code- drop this somewhere else 
#-----------------------------------------------------------
# rmei_vars = c('Volume', "Actual Delivery Month", "Actual Delivery Year", "Other Bednet Size (Invoice)", "Strength", "Pack quantity", "Product Name (EN)", 'Country Name')
# rmei_interest_vars = c(mal_prev, mal_test, mal_treat)
# 
# rmei_locs = c("Belize", "Colombia", "Costa Rica", "Dominican Republic", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama")
# rmei_data = pqr[`Country Name`%in%rmei_locs, rmei_vars, with=FALSE]
# rmei_data = rmei_data[`Product Name (EN)`%in%rmei_interest_vars]
# rmei_data = rmei_data[, .(`Product Name (EN)`, `Country Name`, `Actual Delivery Year`)][order(`Country Name`, `Actual Delivery Year`)]
# names(rmei_data) = c('product', 'country', 'delivery_year')
# 
# mal_nets = c("Duranet", "Insecticide-treated net (ITN)", "Long-Lasting Insecticidal Net (LLIN)", "MAGNet", "MiraNet", "Permanet 2.0", "Permanet 3.0", "Netprotect", "Yorkool LN", "Yahe LN", "Royal Sentry", 
#              "Olyset", "Dawa-Plus 2.0")
# irs = "zz-Pirimiphos Methyl CS"
# 
# rmei_data[product%in%mal_nets, category:="LLIN"]
# rmei_data[product%in%irs, category:="IRS"]
# rmei_data[product%in%mal_treat, category:="Treatment"]
# rmei_data[product%in%mal_test, category:="Testing"]
# 
# write.csv(rmei_data, paste0(dir, "/prepped_data/rmei_key_variables.csv"), row.names = FALSE)
#--------------------------------------------------------------





