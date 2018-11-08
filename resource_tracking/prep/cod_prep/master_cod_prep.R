# ----------------------------------------------

# Irena Chen
## May 15, 2018
# Master code file for DRC data prep

##set up some variables: 
# ----------------------------------------------
loc_name <- 'cod' ##use the ISO3 country code for DRC
implementer <- "CAGF"
# ----------------------------------------------
###### source the functions that we need 
# ----------------------------------------------

source(paste0(code_dir, "cod_prep/prep_detailed_budget.R"))
source(paste0(code_dir, "cod_prep/prep_summary_budget.R"))
source(paste0(code_dir, "cod_prep/prep_cod_pudr.R"))
source(paste0(code_dir, "cod_prep/prep_old_module_budget.R"))
source(paste0(code_dir, "cod_prep/prep_cod_rejected.R"))
source(paste0(code_dir, "cod_prep/prep_old_detailed_budget.R"))

# ----------------------------------------------
###### For loop that preps data and aggregates it
# ----------------------------------------------
for(i in 1:length(file_list$file_name)){
  
  if(file_list$type[i]=="summary"){
    tmpData <- prep_summary_budget(file_dir, as.character(file_list$file_name[i]),
                                  file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i], 
                                  file_list$disease[i], file_list$loc_id[i], file_list$period[i]
                                  , file_list$grant[i], implementer, file_list$source[i], file_list$lang[i])
    tmpData$year <- year(tmpData$start_date)
    tmpData$data_source <- file_list$data_source[i]
   } else if (file_list$type[i]=="detailed"){
    tmpData <- prep_detailed_budget(file_dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i],
                                        file_list$disease[i], file_list$period[i],  file_list$lang[i], file_list$grant[i], loc_name, file_list$source[i])
    tmpData$year <- year(tmpData$start_date)
    tmpData$data_source <- file_list$data_source[i]
  } else if(file_list$type[i]=="module"){
    tmpData <- prep_old_module_budget(file_dir, as.character(file_list$file_name[i]),
                                   file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i], 
                                   file_list$disease[i], file_list$loc_id[i], file_list$period[i]
                                   , file_list$grant[i], implementer, file_list$source[i], file_list$lang[i])
    tmpData$year <- year(tmpData$start_date)
    tmpData$data_source <- file_list$data_source[i]
  } else if(file_list$type[i]=="rejected"){
    tmpData <- prep_cod_rejected(paste0(file_dir, file_list$file_name[i]))
    tmpData$data_source <- "iterated_fpm"
    
  }  else if (file_list$type[i]=="pudr"){ ##has expenditure data 
    tmpData <- prep_pudr_cod(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                             file_list$start_date[i], file_list$disease[i], file_list$period[i], 
                             file_list$grant[i], file_list$sr[i],file_list$data_source[i], file_list$lang[i], loc_name)
  tmpData$data_source <- "pudr"
  } else if (file_list$type[i]=="old_detailed"){
    tmpData <- prep_old_detailed_budget(file_dir, file_list$file_name[i], file_list$sheet[i], file_list$start_date[i], file_list$qtr_number[i],
                                    file_list$disease[i], file_list$period[i],  file_list$lang[i], file_list$grant[i], loc_name, file_list$source[i],
                                    file_list$pr[i])
    tmpData$year <- year(tmpData$start_date)
    tmpData$data_source <- file_list$data_source[i]
  }
  tmpData$financing_source <- "gf"
  tmpData$fileName = file_list$file_name[i]
  tmpData$grant_period = file_list$grant_period[i]
  if(i==1){
    resource_database = tmpData
  } 
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  
  print(paste0(i, " ", file_list$function_type[i], " ", file_list$grant_name[i])) ## if the code breaks, you know which file it broke on
}

# ---------------------------------------------
########## Modify the prepped data variables as necessary ########
# ---------------------------------------------

## since we only have budget and exp, set disbursed as 0:  
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure <- as.numeric(resource_database$expenditure)
resource_database$disbursement <- 0 

resource_database <- resource_database[!(module%in%c("6", "4"))]

##check for duplicates:

dups<-resource_database[duplicated(resource_database) | duplicated(resource_database, fromLast=TRUE)]

##most of the time, these duplicates are either budget values with NA or 0
##COD-M-PSI has duplicated rows for program management, but these were present in the original budget, so we'll aggregate them together


##sum up to remove duplicates: 
byVars = names(resource_database)[!names(resource_database)%in%c('budget', 'disbursement', 'expenditure')]
resource_database= resource_database[, list(budget=sum(na.omit(budget)), 
                                            disbursement=sum(na.omit(disbursement)),expenditure=sum(na.omit(expenditure))), by=byVars]

# ----------------------------------------------
######## Optional: do a data check for dropped values ########
# ----------------------------------------------
# data_check<- resource_database[, sum(budget, na.rm = TRUE),by = c("grant_number","data_source","year", "disease")]

# ----------------------------------------------
##### Load the mapping files  #####
# ----------------------------------------------
codData <- strip_chars(resource_database, unwanted_array, remove_chars)

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


# ----------------------------------------------
# Correct unmapped modules- initial, date, and filepath 
# ----------------------------------------------

#Irena Chen, before October 2018 
codData[module == "systcmesdesanteresiliantsetperennesstrategiesnationalesdesante"  & intervention == "strategiessanitairesnationalesalignementaveclesplansmaladiespecifiquesgouvernancedusecteurdelasanteetfinancement", module := 'ssrsestrategiasnacionalesdesalud']
codData[module == 'ssrsestrategiasnacionalesdesalud' & intervention == "strategiessanitairesnationalesalignementaveclesplansmaladiespecifiquesgouvernancedusecteurdelasanteetfinancement", intervention := 'estrategiasnacionalesensaludalineamientoconplanesespecificosdeenfermedadesgobernanzayfinanciamientoenelsectorsalud']

#EKL 10/25/18, official_budgets/1c.COD-M-SANRU_Budget_IL1_20.08.2018.xlsx
codData$intervention = ifelse(codData$module == "lutteantivectorielle" & codData$intervention == "ieccccpriseencharge", "iecccc", codData$intervention)

#EKL 11/2/18, pudrs/Copy of LFA_Review_COD-T-MOH_Progress Report_30Jun2018_Sent_02102018-Brk....xlsx
codData$module = ifelse(codData$module == "humanresourcesforhealthhrhincludingcommunityhealthworkers" & codData$intervention == "retentionandscaleupofhealthworkersincludingforcommunityhealthworkers",
                        "humanresourcesforhealthincludingcommunityhealthworkers", codData$module)

#EKL 11/2/18, pudrs/Copy of LFA Review_COD-H-MOH_Progress  Report_30Jun2018_07092018 ok_Sent....xlsb.xlsx
codData$module = ifelse(codData$module == "comprehensivepreventionprogramsfortgs", "comprehensivepreventionprogramsfortransgenderpeople", codData$module)
codData$intervention = ifelse(codData$module == "comprehensivepreventionprogramsfortransgenderpeople" & codData$intervention == "hivtestingservicesfortgs", 
                              "hivtestingservicesfortransgenderpeople", codData$intervention)
codData$intervention = ifelse(codData$module == "comprehensivepreventionprogramsforpeoplewhoinjectdrugspwidandtheirpartners" & codData$intervention == "diagnosisandtreatmentofstisandothersexualhealthservicesforpwid", 
                              "diagnosisandtreatmentofsexuallytransmittedinfectionsandothersexualhealthservicesforpeoplewhoinjectdrugs", codData$intervention)
codData$intervention = ifelse(codData$module == "comprehensivepreventionprogramsforpeoplewhoinjectdrugspwidandtheirpartners" & codData$intervention == "behavioralinterventionsforpwid", 
                              "behavioralinterventionsforpeoplewhoinjectdrugs", codData$intervention)
codData$intervention = ifelse(codData$module == "comprehensivepreventionprogramsforpeoplewhoinjectdrugspwidandtheirpartners" & codData$intervention == "hivtestingservicesforpwid", 
                              "hivtestingservicesforpeoplewhoinjectdrugs", codData$intervention)
codData$module = ifelse(codData$module == "comprehensivepreventionprogramsforpeoplewhoinjectdrugspwidandtheirpartners", "comprehensivepreventionprogramsforpeoplewhoinjectdrugsandtheirpartners", 
                        codData$module)


# ----------------------------------------------
########### USE THIS TO CHECK FOR UNMAPPED MODULE/INTERVENTIONS ##########
# ----------------------------------------------
gf_concat <- paste0(gf_mapping_list$module, gf_mapping_list$intervention)
cod_concat <- paste0(codData$module, codData$intervention)
unmapped_mods <- codData[!cod_concat%in%gf_concat]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention", "fileName"), with= FALSE]))
  stop("You have unmapped original modules/interventions!")
}

# ----------------------------------------------
########### map the RT data to the GF modular framework ##########
# ----------------------------------------------

cod_init_mapping <- merge(codData, gf_mapping_list, by=c("module", "intervention", "disease"), all.x=TRUE, allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
dropped_gf <- cod_init_mapping[is.na(cod_init_mapping$code)]

if(nrow(dropped_gf)>0){
  print(unique(dropped_gf[, c("module", "intervention", "disease", "fileName"), with= FALSE]))
  stop("Modules/interventions were dropped!")
}

## merge the dataset with the codes and coefficients to the Modular Framework
mappedCod <- merge(cod_init_mapping, final_mapping, by="code", all.x=TRUE) 

if(sum(is.na(mappedCod$gf_module)) > 0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  stop("Modules/interventions were dropped! - Check Mapping Spreadsheet codes vs intervention tabs")
}

mappedCod$budget <- mappedCod$budget*mappedCod$coefficient
mappedCod$expenditure <- mappedCod$expenditure*mappedCod$coefficient
mappedCod$disbursement <- mappedCod$disbursement*mappedCod$coefficient


##change this when we get geo locations for DRC: 
mappedCod$adm1 <- 171
mappedCod$adm2 <- 171
mappedCod$country <- "Congo (Democratic Republic)"
mappedCod$loc_name = 'cod'

mappedCod$sda_activity <- ifelse(tolower(mappedCod$sda_activity) == "all" | mappedCod$sda_activity == "0", "Unspecified (Summary budget)", mappedCod$sda_activity)

# ----------------------------------------------
## write as csv 
# ----------------------------------------------
write.csv(mappedCod, paste0(country_output_dir, "prepped_budget_data.csv"), fileEncoding = "latin1", row.names = FALSE)



