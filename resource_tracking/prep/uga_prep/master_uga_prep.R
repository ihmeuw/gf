# ----------------------------------------------

# Irena Chen
# Master code file for UGA data prep
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------

# ----------------------------------------------
###### source the functions that we need 
# ----------------------------------------------
source(paste0(code_dir, "uga_prep/prep_detailed_uga_budget.R"))
source(paste0(code_dir, "uga_prep/prep_summary_uga_budget.R"))
source(paste0(code_dir, "uga_prep/prep_pudr_uga.R"))

file_list <- read.csv(paste0("J:/Project/Evaluation/GF/resource_tracking/uga/grants/uga_budget_filelist.csv"), na.strings=c("","NA"),
                      stringsAsFactors = FALSE)

# ---------------------------------------------
########## Set up variables and load the prep files ########
# ---------------------------------------------
cashText <- " Cash Outflow" ##we'll need this to grab the right columns from the budgets 
loc_name <- 'uga' ##change this when we can get subnational data 
source <- "gf" ## denotes the type of data (e.g. government expenditures, Global fund, etc.)

setDT(file_list)
file_list = file_list[iteration == "final"] #Emily, figure out a more elegant way to do this, but for right now just want the final version of each file. 
duplicates = file_list[duplicated(file_list, by = c("grant", "start_date", "data_source")), ]
file_list$iteration <- NULL

# ---------------------------------------------
########## Run the for loop that preps data ########
# ---------------------------------------------

for(i in 1:length(file_list$file_name)){ 
  folder = "budgets"
  folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
  file_dir = paste0(master_file_dir, file_list$status[i], "/", file_list$grant[i], "/", folder, "/")

  if(file_list$type[i]=="detailed"){##most detailed level of budgets 
    tmpData <- prep_detailed_uga_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$qtr_number[i],
                                       cashText, file_list$grant[i], 
                                        file_list$disease[i], file_list$period[i],file_list$data_source[i])
  } else if (file_list$type[i]=="summary"){ ##not much detail, only high level SDAs: 
    tmpData <- prep_summary_uga_budget(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                                       file_list$start_date[i], file_list$qtr_number[i], 
                                       cashText, file_list$grant[i], 
                                       file_list$disease[i], file_list$period[i], file_list$recipient[i], 
                                       file_list$data_source[i])
    tmpData$disbursement <- 0 
  ##LFA data cleaning: 
  } else if (file_list$type[i]=="pudr"){ ##has expenditure data 
    tmpData <- prep_pudr_uga(file_dir, file_list$file_name[i], as.character(file_list$sheet[i]), 
                             file_list$start_date[i], file_list$disease[i], file_list$period[i], 
                             file_list$grant[i], file_list$recipient[i],file_list$data_source[i])
  }
  tmpData$fileName = file_list$file_name[i]
  tmpData$grant_period = file_list$grant_period[i]
  
  if(i==1){
    resource_database = tmpData 
  } 
  if(i>1){
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }
  tmpData$start_date <- as.Date(tmpData$start_date,  "%Y-%m-%d")

  print(paste0(i, " ", file_list$data_source[i], " ", file_list$grant[i])) ## if the code breaks, you know which file it broke on
}


# ---------------------------------------------
########## Modify the prepped data variables as necessary ########
# ---------------------------------------------

##make sure to change the financial data variables to be "numeric" 
resource_database$budget <- as.numeric(resource_database$budget)
resource_database$expenditure <- as.numeric(resource_database$expenditure)
resource_database$disbursement <- as.numeric(resource_database$disbursement)
resource_database[is.na(budget), budget := 0]
resource_database[is.na(expenditure), expenditure := 0]

##assign loc_name and source: 
resource_database$loc_name <- loc_name
resource_database$financing_source <- source

## optional: do a check on data to make sure values aren't dropped: 
# data_check1<- as.data.frame(resource_database[, sum(budget, na.rm = TRUE), by = c("grant_number", "data_source","disease")])

## we have some junk "modules" that should be dropped:
toMatch <- c("0", "Please sel", "6", "4")
cleaned_database <- resource_database[!grepl(paste(toMatch, collapse="|"), resource_database$module),]

dups<-cleaned_database[duplicated(cleaned_database) | duplicated(cleaned_database, fromLast=TRUE)]

##most of the time, these duplicates are either budget values with NA or 0
##UGA-T-MoFPED has duplicated rows for a treatment category
## but these were present in the original budget, so we'll aggregate them together


##sum up to remove duplicates: 
byVars = names(cleaned_database)[!names(cleaned_database)%in%c('budget', 'disbursement', 'expenditure')]
cleaned_database <- cleaned_database [, list(budget=sum(na.omit(budget)), 
                                            disbursement=sum(na.omit(disbursement)),expenditure=sum(na.omit(expenditure))), by=byVars]

# ---------------------------------------------
########## We'll split the TB/HIV grants as follows: ########
########## If the module explicitly says TB, then assign as TB ########
########## Otherwise, default it to HIV ########
# ---------------------------------------------
## split hiv/tb into hiv or tb (for module/intervention mapping purposes): 
get_hivtb_split <- function(disease,module){
  x <- disease
 if(disease=="hiv/tb"){
   if(grepl(paste(c("tb", "tuber"), collapse="|"), module)){ 
    x <- "tb"
  } else { ##otherwise, map it to HIV
    x <- "hiv"
  }
 }
return(x)
}

cleaned_database$disease <- mapply(get_hivtb_split, cleaned_database$disease, cleaned_database$module)



# ---------------------------------------------
########## Strip special characters from the SDA descriptions ########
# ---------------------------------------------
##list of punctions to remove: 
sda_remove_chars <- c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                  , "[[:punct:]]", "\"", ",")

##get rid of punctuation and accents in the SDA activities: 
cleaned_database$sda_activity <-gsub(paste(sda_remove_chars, collapse="|"), "",cleaned_database$sda_activity)
cleaned_database$sda_activity <-tolower(cleaned_database$sda_activity)



# ----------------------------------------------
##### Map to the GF Modules and Interventions #####
# ----------------------------------------------

## on the J Drive: map_dir <- "J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/"
map_dir <- "where the intervention_and_indicator_list.xlsx file lives"
map_dir = "J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/"
mapping_list <- load_mapping_list(paste0(map_dir, "intervention_and_indicator_list.xlsx")
                                  , include_rssh_by_disease = FALSE) ##set the boolean to false for just mapping

## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
final_mapping <- copy(mapping_list)
final_mapping$disease <- NULL ## we will be joining on code 
setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
mapping_list$coefficient <- 1
mapping_list$abbrev_intervention <- NULL
mapping_list$abbrev_module <- NULL


##this loads the list of modules/interventions with their assigned codes
gf_mapping_list <- total_mapping_list(paste0(map_dir, "intervention_and_indicator_list.xlsx"),
                                      mapping_list, unwanted_array, remove_chars)

##strip all of the special characters, white space, etc. from the RT database
cleaned_database <- strip_chars(cleaned_database, unwanted_array, remove_chars)


## we have some junk "modules" that should be dropped:
ugaData  <- cleaned_database[!grepl("pleasesel", cleaned_database$module),]
ugaData$module = ifelse(ugaData$module == "tbhivc", "tbhiv", ugaData$module)
ugaData = ugaData[module != "module" & module != "servicedeliveryarea"]
ugaData$intervention = ifelse(ugaData$intervention == "supportiveenvironmentotherspecifycssmonitoringevaluationevidencebuilding",
                              "supportiveenvironmentmonitoringdrugresistance", ugaData$intervention)
ugaData$intervention = ifelse(ugaData$intervention == "informationsystemoperationalresearch" & ugaData$module == 'malhealthsystemsstrengthening',
                              "informationsystem", ugaData$intervention)

#-----------------------------------------------------------------------
# Manually adjust unmapped modules- comment initials, date, and file name
#-----------------------------------------------------------------------
# EKL 11/21/18, LFA reviewed UGA-T-MOFPED PUDR PE 31Dec2015_final.xlsx
ugaData$intervention = ifelse(ugaData$module == "tbhivcommunitytbcaredelivery" & ugaData$intervention == "all", "communitytbhivcaredelivery", ugaData$intervention)
ugaData$module = ifelse(ugaData$module == "tbhivcommunitytbcaredelivery" & ugaData$intervention == "communitytbhivcaredelivery", "tbhiv", ugaData$module)

ugaData$intervention = ifelse(ugaData$module == "tbcareandpreventiontreatment" & ugaData$intervention == "all", "treatment", ugaData$intervention)
ugaData$module = ifelse(ugaData$module == "tbcareandpreventiontreatment" & ugaData$intervention == "treatment", "tbcareandprevention", ugaData$module)

ugaData$intervention = ifelse(ugaData$module == "tbhivtbhivcollaborativeinterventions" & ugaData$intervention == "all", "tbhivcollaborativeinterventions", ugaData$intervention)
ugaData$module = ifelse(ugaData$module == "tbhivtbhivcollaborativeinterventions" & ugaData$intervention == "tbhivcollaborativeinterventions", "tbhiv", ugaData$module)

ugaData$intervention = ifelse(ugaData$module == "tbhivcollaborativeactivitieswithotherprogramsandsectors" & ugaData$intervention == "all", "collaborativeactivitieswithotherprogramsandsectors", ugaData$intervention)
ugaData$module = ifelse(ugaData$module == "tbhivcollaborativeactivitieswithotherprogramsandsectors" & ugaData$intervention == "collaborativeactivitieswithotherprogramsandsectors", "tbhiv", ugaData$module)

ugaData$intervention = ifelse(ugaData$module == "mdrtbcasedetectionanddiagnosismdrtb" & ugaData$intervention == "all", "casedetectionanddiagnosismdrtb", ugaData$intervention)
ugaData$intervention = ifelse(ugaData$module == "mdrtbpreventionformdrtb" & ugaData$intervention == "all", "preventionformdrtb", ugaData$intervention)
ugaData$module = ifelse(ugaData$module == "mdrtbtreatmentmdrtb" & ugaData$intervention == "all", "treatmentmdrtb", ugaData$module)
ugaData$intervention = ifelse(ugaData$module == "mdrtbcommunitytbcaredelivery" & ugaData$intervention == "all", "communitymdrtbcaredelivery", ugaData$intervention)
ugaData$intervention = ifelse(ugaData$module == "mdrtbkeyaffectedpopulations" & ugaData$intervention == "all", "keyaffectedpopulations", ugaData$intervention)

ugaData$module = ifelse(ugaData$module %in% c("mdrtbcasedetectionanddiagnosismdrtb", "mdrtbpreventionformdrtb", "mdrtbcommunitytbcaredelivery", "mdrtbkeyaffectedpopulations"), "mdrtb", ugaData$module)

ugaData$intervention = ifelse(ugaData$module == "healthinformationsystemsandmeroutinereporting" & ugaData$intervention == "all", "routinereporting", ugaData$intervention)
ugaData$intervention = ifelse(ugaData$module == "healthinformationsystemsandmeanalysisreviewandtransparency" & ugaData$intervention == "all", "analysisreviewandtransparency", ugaData$intervention)
ugaData$intervention = ifelse(ugaData$module == "healthinformationsystemsandmesurveys" & ugaData$intervention == "all", "surveys", ugaData$intervention)

ugaData$module = ifelse(ugaData$module %in% c("healthinformationsystemsandmeroutinereporting", "healthinformationsystemsandmeanalysisreviewandtransparency", "healthinformationsystemsandmesurveys"), "healthinformationsystemsandme", ugaData$module)


ugaData$intervention = ifelse(ugaData$module == "communitysystemsstrengtheningcommunitybasedmonitoringforaccountability" & ugaData$intervention == "all", "communitybasedmonitoringforaccountability", ugaData$intervention)
ugaData$intervention = ifelse(ugaData$module == "communitysystemsstrengtheningsocialmobilizationbuildingcommunitylinkagescollaborationandcoordination" & ugaData$intervention == "all", "socialmobilizationbuildingcommunitylinkagescollaborationandcoordination", ugaData$intervention)
ugaData$module = ifelse(ugaData$module %in% c("communitysystemsstrengtheningcommunitybasedmonitoringforaccountability", "communitysystemsstrengtheningsocialmobilizationbuildingcommunitylinkagescollaborationandcoordination"), "communitysystemsstrengthening", ugaData$module)

# ----------------------------------------------
########### USE THIS TO CHECK FOR ANY UNMAPPED MODULE/INTERVENTIONS ###########
# ----------------------------------------------
gf_concat <- paste0(gf_mapping_list$module, gf_mapping_list$intervention)
uga_concat <- paste0(ugaData$module, ugaData$intervention)
unmapped_mods <- ugaData[!uga_concat%in%gf_concat]
unmapped_mods<- unique(unmapped_mods, by = c("module", "intervention", "sda_activity"))

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention")]))
  stop(paste0("You have unmapped original modules/interventions in ", country))
}

# ----------------------------------------------
########### Map the RT dataset to the GF Modular Framework ###########
# ----------------------------------------------
##merge the RT dataset and the initial mapping list to assign codes: 
uga_init_mapping <- merge(ugaData, gf_mapping_list, by=c("module", "intervention", "disease"), 
                          all.x=TRUE,allow.cartesian = TRUE)

##use this to check if any modules/interventions were dropped:
dropped_gf <- uga_init_mapping[is.na(uga_init_mapping$code)]

if(nrow(dropped_gf)>0){
  stop(paste0("Modules/interventions were dropped in", country))
}

##finally, merge the RT dataset to the GF framework list by code: 
mappedUga <- merge(uga_init_mapping, final_mapping, by="code", all.x=TRUE) 

if(sum(is.na(mappedUga$gf_module)) > 0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  stop("Modules/interventions were dropped! - Check Mapping Spreadsheet codes vs intervention tabs")
}



##if any of the modules are "split", this performs the $$ split across modules 
mappedUga$budget <- mappedUga$budget*mappedUga$coefficient
mappedUga$expenditure <- mappedUga$expenditure*mappedUga$coefficient
mappedUga$disbursement <- mappedUga$disbursement*mappedUga$coefficient

##change this when we get subnational locations for UGA: 
mappedUga$adm1 <- 190
mappedUga$adm2 <- 190
mappedUga$country <- "Uganda"
mappedUga$lang <- "eng"
mappedUga$sda_activity <- ifelse(tolower(mappedUga$sda_activity) == "all" | mappedUga$sda_activity == "0", "Unspecified (Summary budget)", mappedUga$sda_activity)

# ----------------------------------------------
###Check that nothing got dropped: ### 
# ----------------------------------------------
data_check1 <- ugaData[, sum(budget, na.rm = TRUE),by = c( "module","intervention","disease")]
data_check2 <-mappedUga[, sum(budget, na.rm = TRUE),by = c("module", "intervention","disease")]

# ----------------------------------------------
#####write csv to correct folder ############
# ----------------------------------------------
export_dir <- "where you want the prepped dataset to live" 
export_dir <- "J:/Project/Evaluation/GF/resource_tracking/uga/prepped/"

write.csv(mappedUga, paste0(export_dir, "prepped_budget_data.csv"), row.names = FALSE,
          fileEncoding = "latin1")

