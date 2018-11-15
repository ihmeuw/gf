# ----------------------------------------------
# Irena Chen
# May 16th, 2018
# Master code file for GTM FPM/PUDR data cleaning 

source(paste0(code_dir, "gtm_prep/budget_prep/prep_fpm_detailed_budget.R"))
source(paste0(code_dir, "gtm_prep/budget_prep/prep_fpm_summary_budget.R"))
source(paste0(code_dir, "gtm_prep/budget_prep/prep_fpm_other_budget.R"))
source(paste0(code_dir, "gtm_prep/budget_prep/prep_fpm_other_detailed_budget.R"))
source(paste0(code_dir, "gtm_prep/budget_prep/prep_gtm_pudr.R"))

file_list <- read.csv(paste0("J:/Project/Evaluation/GF/resource_tracking/gtm/gtm_budget_filelist.csv"), na.strings=c("","NA"),
                      stringsAsFactors = FALSE) 

# ----------------------------------------------
###### For loop that preps data and aggregates it
# --------------------------------------------

for(i in 1:length(file_list$file_name)){
  folder = "budgets"
  folder = ifelse (file_list$data_source[i] == "fpm" | file_list$data_source[i] == "fpm_iter", folder, "pudrs")
  file_dir = paste0(master_file_dir, file_list$status[i], "/", file_list$grant_name[i], "/", folder, "/")
  
  if(file_list$function_type[i]=="detailed"){ ## fpm detailed budgets 
    tmpData <- prep_fpm_detailed_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_num[i], file_list$disease[i], file_list$period[i], 
                                        file_list$lang[i], file_list$grant_name[i], file_list$primary_recipient[i])
    tmpData$disbursement<- 0 
  } else if (file_list$function_type[i]=="summary"){ ## only summary level data - no municipalities 
    tmpData <- prep_fpm_summary_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                       ymd(file_list$start_date[i]), file_list$qtr_num[i], file_list$disease[i], file_list$period[i], 
                                       file_list$grant_name[i], file_list$primary_recipient[i], file_list$lang[i])
    tmpData$loc_id <- "gtm"
    tmpData$disbursement<- 0 
    
  } else if (file_list$function_type[i]=="detailed_other"){ ## there's an older version of detailed fpm budgets
    tmpData <- prep_other_detailed_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                        ymd(file_list$start_date[i]), file_list$qtr_num[i], file_list$disease[i], file_list$period[i], 
                                        file_list$lang[i], file_list$grant_name[i])
    tmpData$disbursement<- 0 

  } else if (file_list$function_type[i]=="pudr"){ 
    tmpData <- prep_gtm_pudr(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                          ymd(file_list$start_date[i]), file_list$qtr_num[i], file_list$disease[i], file_list$period[i], 
                                          file_list$grant_name[i], file_list$data_source[i], file_list$loc_id[i], file_list$lang[i])

  } else if (file_list$function_type[i]=="other"){
    tmpData <- prep_other_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]),
                                          ymd(file_list$start_date[i]), file_list$qtr_num[i], file_list$disease[i], file_list$period[i], 
                                          file_list$lang[i], file_list$grant_name[i])
    tmpData$disbursement<- 0 
  }
  tmpData$loc_id <- "gtm"
  tmpData$data_source <- file_list$data_source[i]
  tmpData$fileName <- file_list$file_name[i]
  tmpData$grant_period <- file_list$grant_period[i]
  
  if(i==1){
    resource_database = tmpData
  }
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }

  
  print(paste0(i, " ", file_list$function_type[i], " ", file_list$grant_name[i])) ## if the code breaks, you know which file it broke on
}


# ----------------------------------------------
##Add more RT variables and clean up any rows with "junk" data
# ----------------------------------------------

resource_database$adm1 <- 128 ## change if we get department data in the budgets 
resource_database$adm2 <- resource_database$adm1 ## change if we get municipality data in the budgets 
resource_database$start_date <- as.Date(resource_database$start_date)
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure<- as.numeric(resource_database$expenditure)
resource_database$disbursement<- as.numeric(resource_database$disbursement)
## since we only have budget data, include exp and disbursed as 0:  
resource_database$financing_source <- "gf"

resource_database <- resource_database[!grepl("Fondos pendientes de asignar a SR",resource_database$recipient)]

##most of the time, these duplicates are either budget values with NA or 0
##GTM-T-MSPAS and GUA-M-MSPAS has duplicated rows, but these were present in the original budget, so we'll aggregate them together
dups<-resource_database[duplicated(resource_database) | duplicated(resource_database, fromLast=TRUE)]

##sum up to remove duplicates: 
byVars = names(resource_database)[!names(resource_database)%in%c('budget', 'disbursement', 'expenditure')]
cleaned_database= resource_database[, list(budget=sum(na.omit(budget)), 
                                            disbursement=sum(na.omit(disbursement)),expenditure=sum(na.omit(expenditure))), by=byVars]
# ----------------------------------------------
##optional: check for any dropped data 
# ----------------------------------------------
data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number","data_source", "disease")])
data_check2<- as.data.frame(cleaned_database[, sum(budget, na.rm = TRUE),by = c("grant_number","data_source", "disease")])
# ----------------------------------------------
##### Map to the GF Modules and Interventions #####
##run the map_modules_and_interventions.R script first
# ----------------------------------------------

gtmData <- strip_chars(resource_database, unwanted_array, remove_chars)
gtmData[is.na(module), module:=intervention]

## the directory on the J Drive for the intervention list is:
map_dir <- "J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/"
mapping_list <- load_mapping_list(paste0(map_dir, "intervention_and_indicator_list.xlsx"),
                                  include_rssh_by_disease = FALSE)

## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
final_mapping <- copy(mapping_list)
final_mapping$disease <- NULL
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
mapping_list$coefficient <- 1
mapping_list$abbrev_intervention <- NULL
mapping_list$abbrev_module<- NULL
gf_mapping_list <- total_mapping_list(paste0(map_dir,"intervention_and_indicator_list.xlsx"),
                                      mapping_list, unwanted_array, remove_chars)

# ---------------------------------------------------------------------------------------
# Correct any unmapped modules, leaving initials, date, and applicable budget filepath. 
# These should all be written as if they could modify any other file to catch general errors. 
# ---------------------------------------------------------------------------------------

#EKL 10/25/18, official_budgets/03. Presupuesto detallado.xlsx 
gtmData$intervention = ifelse(gtmData$intervention == "detecciondecasosydiagnosticotbmdr", "detecciondecasosydiagnosticotbmr", 
                              gtmData$intervention)
gtmData$intervention = ifelse(gtmData$intervention == "detecciondecasosydiagnostico" & gtmData$module == "paqueteparatbmr", "detecciondecasosydiagnosticotbmr", 
                              gtmData$intervention)
gtmData$module = ifelse(gtmData$intervention == "detecciondecasosydiagnosticotbmr", "paqueteparatbmr", gtmData$module)
gtmData$intervention = ifelse(gtmData$module == "paqueteparatbmr" & gtmData$intervention == "tratamiento", "tratamientotbmr", gtmData$intervention)        
gtmData$intervention = ifelse(gtmData$module == "paqueteparatbmr" & gtmData$intervention == "prestaciondeatencioncomunitariaparatbmdr", 
                              "prestaciondeserviciosdeatenciondelatuberculosisenlacomunidad", gtmData$intervention)

#EKL 10/30/18, official_budgets/03. Presupuesto detallado.xlsx
gtmData$module = ifelse(gtmData$module == "atencionyprevenciondetuberculosis" & gtmData$intervention == "srssrespuestaysistemacomunitrio", 
                        "ssrsrespuestasysistemascomunitarios", 
                        gtmData$module)
gtmData$intervention = ifelse(gtmData$module == "ssrsrespuestasysistemascomunitarios" & gtmData$intervention == "srssrespuestaysistemacomunitrio", 
                              "otrasintervencionespararespuestasysistemascomunitarios", gtmData$intervention)
gtmData$intervention = ifelse(gtmData$module =="atencionyprevenciondetuberculosis" & gtmData$intervention == "implicaratodoslosproveedoresdeatencionatencionyprevenciondetb", 
                              "implicaratodoslosproveedoresdeasistencia", gtmData$intervention)

# ----------------------------------------------
# Use this to check for any unmapped modules/interventions
# ---------------------------------------------

gf_concat <- paste0(gf_mapping_list$module, gf_mapping_list$intervention)
gtm_concat <- paste0(gtmData$module, gtmData$intervention)
unmapped_mods <- gtmData[!gtm_concat%in%gf_concat]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention", "fileName"), with= FALSE]))
  stop("You have unmapped original modules/interventions!")
}

# --------------------------------------------------------------------------
# Correct any modules that are dropping here. Comment initials, date, and filename. 
# --------------------------------------------------------------------------


# ----------------------------------------------
# Merge the datasets on the GF codes to map to framework 
# ----------------------------------------------
gtm_init_mapping <- merge(gtmData, gf_mapping_list, by=c("module", "intervention", "disease"), all.x=TRUE, allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
dropped_gf <- gtm_init_mapping[is.na(gtm_init_mapping$code)]

mappedGtm <- merge(gtm_init_mapping, final_mapping, by="code", all.x=TRUE) 

if(sum(is.na(mappedGtm$gf_module)) > 0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_gf[, c("module", "intervention", "disease"), with= FALSE]))
  stop("Modules/interventions were dropped! - Check Mapping Spreadsheet codes vs intervention tabs")
}

mappedGtm$budget <- mappedGtm$budget*mappedGtm$coefficient
mappedGtm$expenditure <- mappedGtm$expenditure*mappedGtm$coefficient
mappedGtm$disbursement <- mappedGtm$disbursement*mappedGtm$coefficient

mappedGtm$sda_activity <- ifelse(tolower(mappedGtm$sda_activity) == "all" | mappedGtm$sda_activity == "0", "Unspecified (Summary budget)", mappedGtm$sda_activity)

mappedGtm$year <- year(mappedGtm$start_date)


# ----------------------------------------------
##output dataset to the correct folder as a csv: 
# ----------------------------------------------

#stopifnot(sort(colnames(mappedGtm)) == colnames_desired)

write.csv(mappedGtm, paste0("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_budget_data.csv"), row.names = FALSE,
          fileEncoding = "latin1")







