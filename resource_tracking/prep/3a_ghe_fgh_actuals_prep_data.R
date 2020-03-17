# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chen
# PURPOSE: Read in FGH data and clean into a presentable format.  
# DATE: Last updated July 2019
# ----------------------------------------------


#------------------
#GHE MALARIA 
#------------------
{

  # ARCHIVE - This was how the data was being pulled before. I copied the data over on 3.14.19 to make sure the starting point was always consistent. EL 7.17.19
  #ghe_malaria = fread(paste0(j, "/Project/IRH/Malaria/processed_data/all_data_vetted.csv"), stringsAsFactors = FALSE)
  
  ghe_malaria = fread(paste0(fgh_ghe_malaria_raw, "ghes_data.csv"))
  ghe_malaria = ghe_malaria[tolower(ihme_loc_id)%in%code_lookup_tables$iso_code]
  
  #Make values numeric 
  ghe_malaria[, value:=gsub(",", "", value)]
  ghe_malaria[, value:=as.numeric(value)]
  
  #----------------------------------------
  #Only keep NHAs, but review data first. 
  #----------------------------------------
  # What data types do we have per country/year? First, make data rectangular so you can see what years are missing. 
  frame = expand.grid(ihme_loc_id=unique(ghe_malaria$ihme_loc_id), year_id=seq(min(ghe_malaria$year_id), max(ghe_malaria$year), by=1))
  ghe_malaria = merge(ghe_malaria, frame, all.y=T, by=c('ihme_loc_id', 'year_id'))
  
  ghe_malaria[is.na(value), .(ihme_loc_id, year_id)][order(ihme_loc_id, year_id)] #Looks like nothing after 2010 is missing. 
  
  #What sources do you have for each country?
  ghe_malaria[, count:=1]
  ghe_malaria[, sum(count), by=c('ihme_loc_id', 'source_type')]
  
  #------------------------------------------
  # Collapse data, set names and save 
  #------------------------------------------
  #stopifnot(nrow(ghe_malaria[is.na(value)])==0)
  ghe_malaria = ghe_malaria[, .(expenditure=sum(value, na.rm=T)), by=c('ihme_loc_id', 'year_id', 'value_code', 'source_type')]
  
  #setnames(ghe_malaria, c('year_id', 'units_new', 'units_year_new', 'ihme_loc_id'), c('year', 'currency', 'currency_year', 'loc_name'))
  setnames(ghe_malaria, c('year_id', 'ihme_loc_id'), c('year', 'loc_name'))
  ghe_malaria[, loc_name:=tolower(loc_name)]
  
  ghe_only = ghe_malaria[value_code == 'fs_malaria_domestic_public']
  saveRDS(ghe_only, paste0(fgh_ghe_malaria_prepped, "ghe_actuals_malaria.rds"))
  write.csv(ghe_only, paste0(fgh_ghe_malaria_prepped, "ghe_actuals_malaria.csv"), row.names=FALSE)
  saveRDS(ghe_only, paste0(fgh_ghe_malaria_prepped, "archive/ghe_actuals_malaria", Sys.Date(), ".rds"))
  
  oop_only = ghe_malaria[value_code == 'fs_malaria_domestic_private_oop']
  saveRDS(oop_only, paste0(fgh_ghe_malaria_prepped, "oop_actuals_malaria.rds"))
  write.csv(oop_only, paste0(fgh_ghe_malaria_prepped, "oop_actuals_malaria.csv"), row.names=FALSE)
  saveRDS(oop_only, paste0(fgh_ghe_malaria_prepped, "archive/oop_actuals_malaria", Sys.Date(), ".rds"))
  
} 

#------------------
#GHE TB
#------------------
{
  
  ghe_tb = fread(paste0(fgh_ghe_tb_raw, "extracted_data_processed.csv"))
  ghe_tb = ghe_tb[tolower(ihme_loc_id)%in%code_lookup_tables$iso_code]
  
  # make data numeric - in older version of this file the column of interest was called "value"
  ghe_tb[, data:=gsub(",", "", data)]
  ghe_tb[, data:=as.numeric(data)]
  
  #----------------------------------------
  #Only keep NHAs, but review data first. 
  #----------------------------------------
  # What data types do we have per country/year? First, make data rectangular so you can see what years are missing. 
  frame = expand.grid(ihme_loc_id=unique(ghe_tb$ihme_loc_id), year_id=seq(min(ghe_tb$year_id), max(ghe_tb$year), by=1))
  ghe_tb = merge(ghe_tb, frame, all.y=T, by=c('ihme_loc_id', 'year_id'))
  
  ghe_tb[is.na(data), .(ihme_loc_id, year_id)][order(ihme_loc_id, year_id)] #Looks like nothing after 2010. 
  
  #What sources do you have for each country?
  ghe_tb[, count:=1]
  ghe_tb[, sum(count), by=c('ihme_loc_id', 'source_type')]
  
  #------------------------------------------
  # Collapse data, set names and save 
  #------------------------------------------
  # stopifnot(nrow(ghe_tb[is.na(value)])==0) #There are several NA values for WHO sources. 
  ghe_tb = ghe_tb[, .(expenditure=sum(data, na.rm=T)), by=c('ihme_loc_id', 'year_id', 'value_code', 'source_type')]
  
  setnames(ghe_tb, c('year_id', 'ihme_loc_id'), c('year', 'loc_name'))
  ghe_tb[, loc_name:=tolower(loc_name)]
  
  ghe_tb = ghe_tb[value_code == 'fs_tb_domestic_public']
  saveRDS(ghe_tb, paste0(fgh_ghe_tb_prepped, "ghe_actuals_tb.rds"))
  write.csv(ghe_tb, paste0(fgh_ghe_tb_prepped, "ghe_actuals_tb.csv"), row.names=FALSE)
  
  
################################
# GHE HIV Actuals
#################################
  
  ghe_hiv = fread(paste0(fgh_ghe_hiv_raw, "hiv_compiled_converted_10.21.19.csv")) # we now have actuals for HIV
  ghe_hiv = ghe_hiv[tolower(ihme_loc_id)%in%code_lookup_tables$iso_code]
  
  # make value column numeric
  ghe_hiv[, value:=gsub(",", "", value)]
  ghe_hiv[, value:=as.numeric(value)]
  
  #----------------------------------------
  # Review data availability 
  #----------------------------------------
  # What data types do we have per country/year? First, make data rectangular so you can see what years are missing. 
  frame = expand.grid(ihme_loc_id=unique(ghe_hiv$ihme_loc_id), year_id=seq(min(ghe_hiv$year_id), max(ghe_hiv$year), by=1))
  ghe_hiv = merge(ghe_hiv, frame, all.y=T, by=c('ihme_loc_id', 'year_id'))
  
  ghe_hiv[is.na(value), .(ihme_loc_id, year_id)][order(ihme_loc_id, year_id)] #  countries and years with missing values
  
  #What sources do you have for each country?
  ghe_hiv[, count:=1]
  ghe_hiv[, sum(count), by=c('ihme_loc_id', 'source_type')]
  
  #------------------------------------------
  # Collapse data, set names and save 
  #------------------------------------------
  # stopifnot(nrow(ghe_tb[is.na(value)])==0) #There are several NA values for WHO sources. 
  ghe_hiv = ghe_hiv[, .(expenditure=sum(value, na.rm=T)), by=c('ihme_loc_id', 'year_id', 'value_code', 'source_type')]
  
  setnames(ghe_hiv, c('year_id', 'ihme_loc_id'), c('year', 'loc_name'))
  ghe_hiv[, loc_name:=tolower(loc_name)]
  
  ghe_hiv = ghe_hiv[value_code == 'fs_hiv_domestic_public']
  saveRDS(ghe_hiv, paste0(fgh_ghe_hiv_prepped, "ghe_actuals_hiv.rds"))
  write.csv(ghe_hiv, paste0(fgh_ghe_hiv_prepped, "ghe_actuals_hiv.csv"), row.names=FALSE)
  
} 
#Do we want to use PCE extracted PNCNS? EL 7.16.19 
#pce_extractions = read.xlsx(paste0(fgh_ghe_malaria_raw, "pce_extractions.xlsx"))

print("Completed step 3A: Prep FGH Actuals.")
