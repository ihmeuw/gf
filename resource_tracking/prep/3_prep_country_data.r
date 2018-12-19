# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Prep country-level budgets and PUDRs
# DATE: Last updated November 2018. 
# ----------------------------------------------

#-----------------------------------------------
# Prep raw data 
#-----------------------------------------------

# Read in file list 
source(paste0(country_code_dir, "read_filelist_", country, ".R"))
resource_database <- read_fileList()
original_db <- copy(resource_database)

#Make sure all budget and expenditure variables are numeric. 
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure <- as.numeric(resource_database$expenditure)

#Drop any rows that have 0 or NA for budget and expenditure
resource_database = resource_database[!((budget==0| is.na(budget)) & (expenditure == 0 | is.na(expenditure)))]
print(paste0("Dropped ", nrow(original_db) - nrow(resource_database), " rows with NA or 0 for budget and expenditure"))

#Replace NAs as 0s for valid rows of data (where we only have expenditure data but not budget, or vice versa) 
resource_database[is.na(budget) & !is.na(expenditure), budget:=0]
resource_database[!is.na(budget) & is.na(expenditure), expenditure:=0]

check_na_budget <- resource_database[is.na(budget) | is.na(expenditure)]
stopifnot(nrow(check_na_budget)==0)

#check for duplicates, and sum their values if they exist:
dups<-resource_database[duplicated(resource_database) | duplicated(resource_database, fromLast=TRUE)]
print(paste0(nrow(dups), " duplicates found in database; values will be summed"))
byVars = names(resource_database)[!names(resource_database)%in%c('budget', 'expenditure')]
resource_database= resource_database[, list(budget=sum(na.omit(budget)) ,expenditure=sum(na.omit(expenditure))), by=byVars]

#-------------------------------------------------------
# Split HIV/TB combined grants  #EKL still need to verify this function with David; this is from Uganda. 
# ------------------------------------------------------

get_hivtb_split <- function(disease,module){
  x <- disease
 if(disease=="hiv/tb" | disease == "tb/hiv"){
   if(grepl(paste(c("tb", "tuber"), collapse="|"), module)){
    x <- "tb"
  } else { ##otherwise, map it to HIV
    x <- "hiv"
  }
 }
return(x)
}

resource_database$disease <- mapply(get_hivtb_split, resource_database$disease, resource_database$module)


#--------------------------------------------------------
# Adjust module and intervention manually in the raw data 
#-------------------------------------------------------

source(paste0(country_code_dir, "correct_modules_interventions.r"))
resource_database = correct_modules_interventions(resource_database)

#--------------------------------------------------------
# Prep data for merge 
#-------------------------------------------------------
#Remove whitespaces, punctuation, and unwanted characters from module and intervention. 
resource_database <- strip_chars(resource_database, unwanted_array, remove_chars)

#Correct common acronyms in the resource database and the module map. 
resource_database$module <- replace_acronyms(resource_database$module)
resource_database$intervention = replace_acronyms(resource_database$intervention)

module_map$module <- replace_acronyms(module_map$module)
module_map$intervention = replace_acronyms(module_map$intervention)

#------------------------------------------------------------
# Map budgets and PUDRs to module mapping framework 
#------------------------------------------------------------

# Check for unmapped modules/interventions before mapping
gf_concat <- paste0(module_map$module, module_map$intervention)
rt_concat <- paste0(resource_database$module, resource_database$intervention)
unmapped_mods <- resource_database[!rt_concat%in%gf_concat]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention"), with= FALSE]))
  print(unique(unmapped_mods$fileName)) #For documentation in the comments above. 
  stop("You have unmapped original modules/interventions!")
}

# Merge with module map on module, intervention, and disease to pull in code
mapped_country_data <- merge(resource_database, module_map, by=c("module", "intervention", "disease"), all.x=TRUE)

#Merge with intervention using code merged from previous line to pull in gf_module and gf_intervention. 
map_dir <- "J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/"
final_mapping <- load_mapping_list(paste0(map_dir, "intervention_and_indicator_list.xlsx")
                                  , include_rssh_by_disease = FALSE) ##set the boolean to false for just mapping
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
final_mapping = final_mapping[, .(code, gf_module, gf_intervention, abbrev_module, abbrev_intervention)]

# Merge the dataset with gf_module/intervention to the raw file data by code. 
mapped_country_data<- merge(mapped_country_data, final_mapping, by="code", all.x=TRUE) 
dropped_mods <- mapped_country_data[is.na(mapped_country_data$gf_module), ]

if(nrow(dropped_mods) >0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_mods[, c("module", "intervention", "disease", "code"), with= FALSE]))
  stop("Modules/interventions were dropped! - Check Mapping Spreadsheet codes vs intervention tabs")
}

#-----------------------------------------
# Apply redistribution coefficients
# ----------------------------------------
mapped_country_data$budget <- mapped_country_data$budget*mapped_country_data$coefficient
mapped_country_data$expenditure <- mapped_country_data$expenditure*mapped_country_data$coefficient
mapped_country_data$disbursement <- mapped_country_data$disbursement*mapped_country_data$coefficient

#-----------------------------------------
# Add in location variables
# ----------------------------------------
if(country == "cod"){
  mapped_country_data$adm1 <- 171
  mapped_country_data$adm2 <- 171
  mapped_country_data$country <- "Congo (Democratic Republic)"
} else if (country == "gtm"){
  mapped_country_data$adm1 <- 128 
  mapped_country_data$adm2 <- 128 
  mapped_country_data$country <- "Guatemala" 
} else if (country == "uga"){
  mapped_country_data$adm1 <- 190
  mapped_country_data$adm2 <- 190
  mapped_country_data$country <- "Uganda"
}

mapped_country_data$loc_name = country

#Emily still not surewhy we're doing this. 
mapped_country_data$sda_activity <- ifelse(tolower(mapped_country_data$sda_activity) == "all" | mapped_country_data$sda_activity == "0", "Unspecified (Summary budget)", mapped_country_data$sda_activity)

# --------------------------------------------------------
#Validate the columns in final data and the storage types  
# --------------------------------------------------------

desired_cols <- c("abbrev_intervention", "abbrev_module", "adm1", "adm2", "budget", "code", "code_count", "coefficient", "cost_category", "country", "data_source", "disbursement", "disease", 
                  "expenditure", "file_iteration", "fileName", "frequency", "gf_intervention", "gf_module", "grant_number", "grant_period", "intervention", "lang", "loc_name", "module", 
                  "orig_intervention", "orig_module", "period", "prefix", "primary_recipient", "sda_activity", "secondary_recipient", "start_date", "year")
stopifnot(sort(colnames(mapped_country_data)) == desired_cols)   

# ----------------------------------------------
# Write the prepped data as .csvs
# ----------------------------------------------

final_budgets <- mapped_country_data[file_iteration == "final" & data_source == "fpm"]
final_expenditures <- mapped_country_data[file_iteration == "final" & data_source == "pudr"]

write.csv(final_budgets, paste0(export_dir, "final_budgets.csv"), fileEncoding = "latin1", row.names = FALSE)
write.csv(final_expenditures, paste0(export_dir, "final_expenditures.csv"), fileEncoding = "latin1", row.names = FALSE)
write.csv(mapped_country_data, paste0(export_dir, "budget_iterations.csv"), fileEncoding = "latin1", row.names = FALSE)
