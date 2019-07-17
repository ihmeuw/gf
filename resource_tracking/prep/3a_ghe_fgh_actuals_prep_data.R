# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chen
# PURPOSE: Read in FGH data and clean into a presentable format.  
# DATE: Last updated July 2019
# ----------------------------------------------


#------------------
#GHE MALARIA 
#------------------

# ARCHIVE - This was how the data was being pulled before. I copied the data over on 3.14.19 to make sure the starting point was always consistent. EL 7.17.19
#ghe_malaria = fread(paste0(j, "/Project/IRH/Malaria/processed_data/all_data_vetted.csv"), stringsAsFactors = FALSE)

ghe_malaria = readRDS(paste0(fgh_ghe_malaria_raw, "ghe_actuals_malaria.rds"))
ghe_malaria = ghe_malaria[value_code == 'fs_malaria_domestic_public']
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

ghe_malaria[is.na(value), .(ihme_loc_id, year_id)][order(ihme_loc_id, year_id)] #Looks like nothing after 2010. 

#What sources do you have for each country?
ghe_malaria[, count:=1]
ghe_malaria[, sum(count), by=c('ihme_loc_id', 'source_type')]

#------------------------------------------
# Collapse data, set names and save 
#------------------------------------------
stopifnot(nrow(ghe_malaria[is.na(value)])==0)
ghe_malaria[, .(expenditure=sum(value, na.rm=T)), by=c('ihme_loc_id', 'year_id', 'value_code', 'source_type', 'units_new', 'units_year_new')]

setnames(ghe_malaria, c('year_id', 'units_new', 'units_year_new', 'ihme_loc_id'), c('year', 'currency', 'currency_year', 'loc_name'))
ghe_malaria[, loc_name:=tolower(loc_name)]

saveRDS(ghe_malaria, paste0(fgh_prepped, "ghe_actuals_malaria.rds"))
write.csv(ghe_malaria, paste0(fgh_prepped, "ghe_actuals_malaria.csv"), row.names=FALSE)

#Do we want to use PCE extracted PNCNS? EL 7.16.19 
#pce_extractions = read.xlsx(paste0(fgh_ghe_malaria_raw, "pce_extractions.xlsx"))

print("Completed step 3A: Prep FGH Actuals.")
