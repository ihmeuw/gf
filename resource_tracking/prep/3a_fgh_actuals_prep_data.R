# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chen
# PURPOSE: Read in FGH data and clean into a presentable format.  
# DATE: Last updated March 2019. 
# ----------------------------------------------

# ----------------------------------------------------------------------
# To do list for this code: 

# ---------------------------------------------------------------------

#Archiving here how the most recent raw FGH data was saved - new FGH data through 2018 was received on March 13, 2019. EKL
# fgh_data = read.dta13(paste0(j, "/Project/IRH/DAH/RESEARCH/INTEGRATED DATABASES/DATA/FGH_2018/FGH_EZ_2018.dta"))
# setDT(fgh_data)
# fgh_data = fgh_data[tolower(iso3_rc)%in%code_lookup_tables$iso_code] #Keeping GTM, UGA, COD, and SEN
# saveRDS(fgh_data, paste0(j, "/Project/Evaluation/GF/resource_tracking/_fgh/raw_data/FGH_EZ_2018.rds"))

#This is the previous version of the FGH data, which only had information through 2016. 
# fgh_data <- fread(paste0(j, "Project/Evaluation/GF/resource_tracking/multi_country/gf/ihme_dah_cod_uga_gtm_1990_2016.csv"))

# ----------------------------------------------
# DAH ACTUALS 
# ----------------------------------------------
fgh_data = readRDS(paste0(fgh_raw, "FGH_EZ_2018.rds"))

#Read in the mapping documents 
fgh_mapping <- fread(paste0(mapping_dir, "fgh_mapping.csv"), stringsAsFactors = FALSE)
fgh_mapping = fgh_mapping[disease != "" & code != "" & !is.na(coefficient)] #Only keep the rows we've classified fully. 
fgh_mapping = fgh_mapping[, .(sda_activity, code, coefficient)]

final_mapping <- fread(paste0(mapping_dir, "all_interventions.csv"))
final_mapping = final_mapping[, -c("disease")] #Remove the disease column because we don't need it.

# ----------------------------------------------
# Prep the DAH data
# ----------------------------------------------

setnames(fgh_data, c("source", "iso3_rc"), c("dah_origin","loc_name"))

fgh_data$financing_source <- mapply(get_dah_source_channel, fgh_data$channel)
fgh_data = fgh_data[, -c('channel')] #Remove unnecessary categorical variables 
disease_vars = grep("hiv|mal|tb|swap", names(fgh_data)) #Grab HIV, TB, Malaria, and RSSH variables 
id_vars = grep("year|financing_source|loc_name", names(fgh_data))
keep_cols =c(id_vars, disease_vars)

fgh_data = fgh_data[, .SD, .SDcols=keep_cols]

#Drop 'total' columns 
fgh_data = fgh_data[, -c('hiv_dah_18', 'mal_dah_18', 'tb_dah_18', 'swap_hss_total_dah_18')]

## "melt" the data: 
fghData <-  melt(fgh_data, id=c("year", "financing_source", "loc_name"), variable.name = "sda_activity", value.name="disbursement")
fghData$disbursement <- as.numeric(fghData$disbursement)

##get the disease column: 
fghData[grepl("hiv", sda_orig), disease:='hiv']
fghData[grepl("mal", sda_orig), disease:='malaria']
fghData[grepl("tb", sda_orig), disease:='tb']
fghData[grepl("swap|hss", sda_orig), disease:='rssh']
fgh_data[is.na(disease), disease:='other']

##sum the disbursement by the other variables just to remove any duplicates: 
byVars = c('year', 'disease', 'financing_source','sda_activity', 'loc_name')
fghData = fghData[, disbursement:=sum(na.omit(disbursement)), 
                  by=byVars]
fghData = unique(fghData)

fghData$fin_data_type <- "actual"

#Map this data to global fund modules and interventions. 
fgh_to_codes <- merge(fghData, fgh_mapping, by='sda_activity', all.x = TRUE, allow.cartesian = TRUE)
fgh_to_codes[is.na(coefficient), coefficient:=1] #For things that didn't map, make sure disbursement isn't growing or shrinking 
fgh_mapped <- merge(fgh_to_codes, final_mapping, by='code', all.x = TRUE)

#Apply redistributive coefficients 
fgh_mapped[, disbursement:=disbursement*coefficient]

#Check merge 
#the number of rows of fgh_to_codes and fgh_mapped at this point should be the same. 
stopifnot(nrow(fgh_mapped) == nrow(fgh_to_codes))
# The sum of disbursement from fghData and fgh_mapped should be the same.
stopifnot(fgh_mapped[, sum(disbursement, na.rm=TRUE)] == fghData[, sum(disbursement, na.rm=TRUE)])

#Create variables. 
fgh_mapped$fileName <- "FGH_EZ_2018.rds"
fgh_mapped$fin_data_type <- "actual"
fgh_mapped$lang <- "eng"

# ----------------------------------------------
# export the FGH data 
# ----------------------------------------------

write.csv(fgh_mapped, paste0(fgh_prepped, "prepped_current_fgh.csv"), row.names=FALSE)
saveRDS(fgh_mapped, paste0(fgh_prepped, "prepped_current_fgh.rds"))


# ----------------------------------------------
# GHE ACTUALS (Malaria and HIV, coming from separate files)
# ----------------------------------------------

ghe_malaria = fread(paste0(j, "/Project/IRH/Malaria/processed_data/all_data_vetted.csv"), stringsAsFactors = FALSE)
ghe_malaria = ghe_malaria[tolower(ihme_loc_id)%in%code_lookup_tables$iso_code]
ghe_malaria[, year_diff:=year_end-year_start]
unique(ghe_malaria$year_diff)

ghe_malaria = ghe_malaria[, .(ihme_loc_id, year_id, value_code, value, units, source_type)]

#Convert currencies to USD. 

saveRDS(ghe_malaria, paste0(fgh_prepped, "ghe_actuals_malaria.rds"))
write.csv(ghe_malaria, paste0(fgh_prepped, "ghe_actuals_malaria.csv"), row.names=FALSE)
