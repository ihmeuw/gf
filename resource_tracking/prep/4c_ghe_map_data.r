# ------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Map prepped GHE data to final mappings, split HIV/TB
#          combined grants, and save to final save location. 
# DATE: Last updated May 2019. 
# ------------------------------------------------------------------

# EMILY ADD IN ALL DIFFERENT GHE FILES HERE! 
raw_data = copy(resource_database)

module_map = fread(paste0(mapping_dir, "sicoin_mapping.csv"))
all_interventions = fread(paste0(mapping_dir, "all_interventions.csv"))
all_interventions = all_interventions[, .(module_eng, intervention_eng, code)]
all_interventions[, code:=trimws(code)]
setnames(all_interventions, c('module_eng', 'intervention_eng'), c('gf_module', 'gf_intervention'))

module_map = merge(module_map, all_interventions, by='code', all.x=TRUE)
if (nrow(module_map[is.na(gf_module)])!=0) stop("Some codes not matching to the map!")
module_map = module_map[, .(activity, disease, code, coefficient, gf_module, gf_intervention)]
#-------------------------------------------------------
# Prep mapping data for merge 
#-------------------------------------------------------
#Remove whitespaces, punctuation, and unwanted characters from module and intervention. 
raw_data[, activity:=tolower(activity)] #Lowercase
raw_data[, activity:=fix_diacritics(activity)] #Fix diacritical marks
raw_data[, activity:=gsub("[[:punct:]]", "", activity)] #Remove punctuation
raw_data[, activity:=gsub(" ", "", activity)] # Remove spaces 

#Correct common acronyms in the resource database and the module map. 
raw_data[, activity:=replace_acronyms(activity)]
module_map[, activity:=replace_acronyms(activity)]

#------------------------------------------------------------
# Map to module mapping framework 
#------------------------------------------------------------
# Check for unmapped modules/interventions before mapping
unmapped_mods <- raw_data[!activity%in%module_map$activity]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("activity"), with= FALSE]))
  print(unique(unmapped_mods$file_name)) #For documentation in the comments above. 
  stop("You have unmapped original modules/interventions!")
}

#----------------------------------------------------------------------------
# Merge with module map on module, intervention, and disease to pull in code
#----------------------------------------------------------------------------
#Check that your data will sum correctly before and after redistribution
pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure')]
pre_coeff_check[[1]] = round(pre_coeff_check[[1]])
pre_coeff_check[[2]] = round(pre_coeff_check[[2]])

mergeVars = c('disease', 'activity')

mapped_data <- merge(raw_data, module_map, by=mergeVars, all.x = TRUE, allow.cartesian = TRUE)
dropped_mods <- mapped_data[is.na(mapped_data$gf_module), ]

if(nrow(dropped_mods) >0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_mods[, c("activity", "disease"), with= FALSE]))
  stop("Modules/interventions were dropped!")
}
#-------------------------------------------------------
# Redistribute using mapped coefficient 
# ------------------------------------------------------
remapped_rows = nrow(mapped_data[coefficient != 1])
print(paste0("A total of ", remapped_rows, " rows will be redistributed."))
mapped_data[, budget:=budget*coefficient]
mapped_data[, expenditure:=expenditure*coefficient]

post_coeff_check = mapped_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure')]
post_coeff_check[[1]] = round(post_coeff_check[[1]])
post_coeff_check[[2]] = round(post_coeff_check[[2]])

stopifnot(abs(pre_coeff_check[[1]]-post_coeff_check[[1]]) < 1) #Decision by David Phillips 5/10/19 - it's okay if there is less than one cent difference between the pre- and post-redistribution. 
stopifnot(abs(pre_coeff_check[[2]]-post_coeff_check[[2]]) < 1)

# --------------------------------------------------------
# Add in location variables and year
# --------------------------------------------------------
mapped_data$loc_name = "GTM"
mapped_data$country = "Guatemala"
mapped_data[, year:=year(start_date)]

# --------------------------------------------------------
# Convert currencies to USD 
# --------------------------------------------------------
#Use the FGH team's currency conversion function - see where this is sourced in 'set_up_r' for details on these variables. 
converted_usd = currency_conversion(data = mapped_data, 
                                    col.loc = 'loc_name', 
                                    col.currency.year = 'year', 
                                    currency='LCU', 
                                    col.value=c('budget', 'expenditure'), 
                                    base.year=2018, 
                                    base.unit='USD', 
                                    simplify=T, 
                                    converter.version=3)


#NOTE - The table below was how we were converting before May of 2019. We would merge on the data by year and the convert budget and expenditure. 
# Emily Linebarger, May 2019
# Convert from Quitzal to USD
# conversion_table = data.table("year" = c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"), 
#                               "conversion" = c(7.74022,	7.5532,	7.3301,	7.45875,	7.42153,	8.01039,	7.92282,	7.64965,	7.68406,	7.71407,	7.59794,	7.49704,	7.43533,	7.18309,	7.31697))



#Check to make sure that this didn't replace any budget or expenditure as 0. 
stopifnot(nrow(converted_usd[is.na(budget)])==0 | nrow(converted_usd[is.na(expenditure)])==0)

#One tiny hack after - make loc name lowercase so it's the same as the GF data 
converted_usd[, loc_name:='gtm']

# --------------------------------------------------------
#Validate the columns in final data and the storage types  
# --------------------------------------------------------
converted_usd = converted_usd[order(loc_name, country, department, municipality, disease, start_date, activity, gf_module, gf_intervention, code)]

saveRDS(converted_usd, paste0(sicoin_prepped, "prepped_sicoin_data.rds"))
write.csv(converted_usd, paste0(sicoin_prepped, "prepped_sicoin_data.csv"), row.names=FALSE)
