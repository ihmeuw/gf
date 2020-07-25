# --------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: script that cleans PUDR Indicators Performance Framework Data for analyses
# DATE: Last updated October 2019 
# NOTE: this version assumes that all country data is stored in same file
# --------------------------------------

#----------------------------------------------
# TO-DO list for this code: 
# - Need to reshape wide based off of the most "final" indicator (PR, LFA, GF)
# - systematically test for strange data formats. 
#----------------------------------------------

#  Read in data, and correct names 
dt = readRDS(paste0(prepped_dir, "all_prepped_data.rds"))
#Make sure that nothing is a factor. 
names(dt) = gsub("%", "pct", names(dt)) #EMILY THIS SHOULD BE CORRECTED IN PREP CODE

#--------------------------------------------------
# Remove unnecessary rows - #EMILY this may be a data extraction issue. Mostly they are hidden rows in GTM PU/DRs
#--------------------------------------------------
dt <- dt[indicator!="[Impact Indicator Name]"]
dt <- dt[indicator!="[Outcome Indicator Name]"]
dt <- dt[indicator!="0"]

#---------------------------------------------
# clean special characters, blanks, and NAs # EMILY- how are you systematically testing for this? 
#---------------------------------------------
numVars = names(dt)[grepl("_value|_year|_n|_d|_pct|_achievement_ratio", names(dt))]
numVars = numVars[!numVars%in%c('start_date_programmatic', 'end_date_programmatic', 'report_due_date', 'loc_name', 'file_name', 'lfa_verified')]

for (var in numVars) { 
  if (verbose){print(var)}
  #Do an overall check - how many rows have at least one digit for this value (numeric information?) Is it the same at the end? 
  start_numeric = nrow(dt[grepl("[[:digit:]]", get(var))])
  # REMOVE NAs
  dt[get(var)=="N/A" | get(var)=="ND" | get(var)==" " | get(var)=="No disaggregation" | get(var)=="No disagregation" | get(var)=="qu " | get(var)=="i" | get(var)=="The PR did not report any result" | get(var)=="Sin dato" | get(var)=="Not reported" | get(var)=="Pendiente de recibir la info del PNS" | get(var)==".", (var):=NA]
  start_nas = nrow(dt[is.na(get(var))]) #Make sure that this numeric conversion doesn't accidentally introduce any NAs after this point.
  
  # LOOK FOR "E-" AND CONVERT SCIENTIFIC NOTATION
  dt[grepl("E\\-", get(var)), neg_sci_notation:=TRUE]
  dt[grepl("E\\+", get(var)), pos_sci_notation:=TRUE]
  if (nrow(dt[neg_sci_notation==TRUE | pos_sci_notation==TRUE])>0){
    if (verbose){
      print("These are the values that will be converted from scientific notation.")
      print(dt[pos_sci_notation==TRUE | neg_sci_notation, unique(get(var))])
    }
    dt[neg_sci_notation==TRUE, c('value', 'rounding'):=tstrsplit(get(var), "E\\-", fixed=TRUE)]
    dt[pos_sci_notation==TRUE, c('value', 'rounding'):=tstrsplit(get(var), "E\\+", fixed=TRUE)]
    dt[, value:=as.numeric(value)]
    dt[, rounding:=as.numeric(rounding)]
    if (verbose){
      print("These are the values that will be converted from scientific notation.")
      print(unique(dt[pos_sci_notation==TRUE | neg_sci_notation, .(variable=get(var), value, rounding)]))
    }
    
    #Convert values 
    dt[neg_sci_notation==TRUE, (var):=value*((.1)^rounding)]
    dt[pos_sci_notation==TRUE, (var):=value*(10)^rounding]
  } 
  
  # REMOVE ALPHANUMERIC CHARACTERS 
  dt[, (var):=gsub("[[:alpha:]]", "", get(var))] #Remove alphabetic characters. 
 
  # DIVIDE VALUES THAT HAVE A FORWARD SLASH.
  
  if (var%in%c("baseline_value")){
    if (nrow(dt[grepl("/", get(var))])>0){
      dt[grepl("/", get(var)), num:=tstrsplit(get(var), "/", keep=1)]
      dt[grepl("/", get(var)), denom:=tstrsplit(get(var), "/", keep=2)]
      dt[!is.na(num), clean_num:=gsub(",", "\\.", num)][, clean_num:=as.numeric(clean_num)] #Replace commas with periods for these variables (denominator should be 100)
      dt[!is.na(denom), clean_denom:=gsub(",", "", denom)][, clean_denom:=as.numeric(clean_denom)]
      if (verbose){
        print("These are the values that will be divided using new numerators and denominators.")
        print(unique(dt[grepl("/", get(var)), .(variable=get(var), num, clean_num, denom, clean_denom)]))
      }
      dt[grepl("/", get(var)), (var):=clean_num/clean_denom] # this piece of code seems to lead to a warning message
    } 
  }else{
  if (nrow(dt[grepl("/", get(var))])>0){
    dt[grepl("/", get(var)), num:=tstrsplit(get(var), "/", keep=1)]
    dt[grepl("/", get(var)), denom:=tstrsplit(get(var), "/", keep=2)]
    dt[!is.na(num), clean_num:=gsub(",", "\\.", num)][, clean_num:=as.numeric(clean_num)] #Replace commas with periods for these variables (denominator should be 100)
    dt[!is.na(denom), clean_denom:=gsub(",", "\\.", denom)][, clean_denom:=as.numeric(clean_denom)]
      if (verbose){
        print("These are the values that will be divided using new numerators and denominators.")
        print(unique(dt[grepl("/", get(var)), .(variable=get(var), num, clean_num, denom, clean_denom)]))
      }
    dt[grepl("/", get(var)), (var):=clean_num/clean_denom] # this piece of code seems to lead to a warning message
  } 
  }  
  #REPLACE COMMAS WITH PERIODS. 
  if (verbose & nrow(dt[grepl(",", get(var))])>0){
    print("These are the values that will have commas replaced with periods.") 
    print(dt[grepl(",", get(var)), unique(get(var))])
  }
  dt[, (var):=gsub(",", "\\.", get(var))] #replace commas with periods.
  
  #If values have multiple periods after this step, remove them.
  if (verbose & nrow(dt[str_count(get(var), "//.")>1])>0) {
    print("These are the variables that have more than one period - these periods will be removed because they were likely commas that got converted in the last step.")
    print(dt[str_count(get(var), "\\.")>1, unique(get(var))])
  }
  dt[str_count(get(var), "\\.")>1, (var):=gsub("\\.", "", get(var))] 
  
  #REMOVE PUNCTUATION # line 93 changes depending on the encoding of this file and results in not all of the punctuation getting properly cleaned.
  dt[, (var):=gsub("?|%|â|°|‰", "", get(var))]
  
  # REMOVE SPACE BETWEEN NUMBERS
  dt[, (var):=gsub("[[:space:]]", "", get(var))] 
  
  #Turn all of these variables numeric.
  dt[, (var):=as.numeric(get(var))]
  
  end_nas = nrow(dt[is.na(get(var))]) #Make sure you didn't accidentally introduce any NAs. 
  if (start_nas!=end_nas){
    print(var) 
    stop("NAs introduced by cleaning code!")
  }
  
  end_numeric = nrow(dt[grepl("[[:digit:]]", get(var))])
  if (start_numeric!=end_numeric){
    print(var)
    stop("Numeric information was lost during cleaning process!")
  }
}

#-----------------------------------------------------
# Merge/map additional values
#-----------------------------------------------------
stdnames_cb <- fread(paste0(book_dir, "indicators_codebook.csv")) # adds standardized names and SOME information on equity and RSSH and focus topics
sources_cb <- fread(paste0(book_dir, "data_source_codebook.csv"), header = TRUE) # lists data sources
reverse_cb <- fread(paste0(book_dir, "indicators_codebook_reverse.csv")) # reverse indicator variable

# clean indicators before merging
dt[, indicator:=gsub("&amp;", "&", indicator)] #Clean indicators before merging with codebook 
dt = dt[, indicator_code:=tstrsplit(indicator, ":", keep=1)] # create variable with indicator code for merging
dt[indicator_code=="GP other-2 Nombre de SVS ayant recu le kit PEP dans les 72h", 
   indicator_code:="GP other-2"]

#-------------------------------------
# add standardized name variables
dt <- merge(dt, stdnames_cb, by="indicator_code", all.x = TRUE)

#-------------------------------------
# Merge and replace the Baseline source code 
dt <- merge(dt, sources_cb, by.x="baseline_source", by.y = "source_original", all.x=TRUE)
dt <- dt[,baseline_source_code:=source_code]
dt <- dt[,c("source_code"):=NULL]

# Merge and replace the PR result source code
dt <- merge(dt, sources_cb, by.x="pr_result_source", by.y = "source_original", all.x = TRUE)
dt <- dt[,pr_result_source_code:=source_code]
dt <- dt[,source_code:=NULL]

# Merge and replace the LFA result source code
dt <- merge(dt, sources_cb, by.x="lfa_result_source", by.y = "source_original", all.x = TRUE)
dt <- dt[,lfa_result_source_code:=source_code]
dt <- dt[,source_code:=NULL]

#----------------------------------------------
# add reverse indicator variable
dt <- merge(dt, reverse_cb, by.x="indicator_code", by.y = "indicator_code", all.x = TRUE, all.y = FALSE)


#########################################################
## TYPOS
#########################################################

# verified by looking in Uganda PU/DRs--PR filled out the percetage sheet incorrectly, resulting in decimals in the wrong place
dt$target_pct[which(dt$loc_name=="uga" & dt$target_pct==1)] <- 100
dt$target_pct[which(dt$loc_name=="uga" & dt$target_pct==0.5)] <- 50
dt$target_pct[which(dt$loc_name=="uga" & dt$target_pct==0.85)] <- 85
dt$target_pct[which(dt$loc_name=="uga" & dt$baseline_year==2014 & dt$baseline_value==19 & dt$indicator_type=="Impact")] <- 6.7

#----------------------------------------------------
# Derive new variables
#----------------------------------------------------

# add variable for value type
dt[grepl('number', full_description , ignore.case = TRUE), value_type := "number"]
dt[grepl('percentage|proportion|percentage of', full_description , ignore.case = TRUE), value_type := "percentage"]
dt[grepl('rate|per 1|per 100|success rate', full_description, ignore.case = TRUE), value_type := "rate"]
dt[grepl('number and percentage', full_description , ignore.case = TRUE), value_type := "number and percentage"]
dt[grepl("Reported malaria cases", full_description , ignore.case = TRUE), value_type := "number"]

# Remove rows that are missing values for any of the result columns--there are usually blanks reported in the PUDR
# usually the values that are blank are from the disaggregated data
# these might have baseline or target values but we are not going to need these for visualizations

check_na = dt[is.na(pr_result_n) & is.na(lfa_result_n) & is.na(gf_result_n) & 
                     is.na(pr_result_pct) & is.na(lfa_result_pct) & is.na(lfa_result_pct) & 
                     is.na(pr_result_value) & is.na(lfa_result_value) & is.na(gf_result_value) &
                     is.na(lfa_result_achievement_ratio) & is.na(pr_result_achievement_ratio) & is.na(gf_result_achievement_ratio)]

# this was the original code that was adapted --leaving in in case we want to create a table that visualizes what is getting dropped
# na_budget = check_na_sum[, sum(budget, na.rm = TRUE)]
# if (na_budget!=0){
#   stop("Budgeted line items have NA for all key variables - review drop conditions before dropping NAs in module and intervention")
# }

dt = dt[!(is.na(pr_result_n) & is.na(lfa_result_n) & is.na(gf_result_n) & 
                      is.na(pr_result_pct) & is.na(lfa_result_pct) & is.na(lfa_result_pct) & 
                      is.na(pr_result_value) & is.na(lfa_result_value) & is.na(gf_result_value) &
                      is.na(lfa_result_achievement_ratio) & is.na(pr_result_achievement_ratio) & is.na(gf_result_achievement_ratio))]

# # Calculate an internal verified achievement ratio.
# dt[, any_achievement_ratio:=gf_result_achievement_ratio]
# dt[is.na(any_achievement_ratio), any_achievement_ratio:=lfa_result_achievement_ratio]
# dt[is.na(any_achievement_ratio), any_achievement_ratio:=pr_result_achievement_ratio]
# 
# # Are there any cases where your calculation of the indicator ratio would be different than the actual? 
# #yes, in that case we calculate our own
# # achievement ratio using the best numerator and denominator
# 
# # calculate '_value' variables which are either the percent reported or the numerator (if indicator is not a proportion or percent)
# # this is done for the one target value reported and the three sources of result values (PR, LFA, and GF)
# dt$target_value <- ifelse(is.na(dt$target_pct),dt$target_n, dt$target_pct)
# dt$pr_result_value <- ifelse(is.na(dt$pr_result_pct), dt$target_n, dt$pr_result_pct)
# dt$lfa_result_value <- ifelse(is.na(dt$lfa_result_pct), dt$lfa_result_n, dt$lfa_result_pct)
# dt$gf_result_value <- ifelse(is.na(dt$gf_result_pct), dt$gf_result_n, dt$gf_result_pct)
# 
# # create the any_result_value which is will gather any available result value reported by any of the three sources
# dt[, any_result_value:=gf_result_value]
# dt[is.na(any_result_value), any_result_value:=lfa_result_value]
# dt[is.na(any_result_value), any_result_value:=pr_result_value]

# # create completeness rating for target and result value
# dt$completeness_rating <- NA
# 
# dt$completeness_rating[which(   is.na(dt$target_value)  &  is.na(dt$any_result_value))] <- 1
# dt$completeness_rating[which(   is.na(dt$target_value)  & !is.na(dt$any_result_value))] <- 2
# dt$completeness_rating[which(  !is.na(dt$target_value)  &  is.na(dt$any_result_value))] <- 3
# dt$completeness_rating[which(  !is.na(dt$target_value)  & !is.na(dt$any_result_value))] <- 4
# 
# # create factor variable and assign names
# dt$completeness_rating <- factor(dt$completeness_rating)
# levels(dt$completeness_rating) <-  c("No data", "Only Result", "Only Target", "Both available")

# # calculate if the sources differ between the baseline value and the pr reported value
# dt$sources_different <- NA
# dt$sources_different[which(dt$baseline_source_code!=dt$pr_result_source_code)] <- 1
# dt$sources_different[which(dt$baseline_source_code==dt$pr_result_source_code)] <- 0

# # calculate ihme_results_achievement_ratio
# dt$ihme_result_achievement_ratio <-NA
# dt$ihme_result_achievement_ratio <- dt$any_result_value/dt$target_value
# 
# # create new variable to indicate whether target is being met
# dt$target_met <- NA
# dt$target_met[which(dt$reverse_indicator_final=="no" & dt$any_result_value >= dt$target_value)] <- "yes"
# dt$target_met[which(dt$reverse_indicator_final=="no" & dt$any_result_value < dt$target_value)] <- "no"
# dt$target_met[which(dt$reverse_indicator_final=="yes" & dt$any_result_value <= dt$target_value)] <- "yes"
# dt$target_met[which(dt$reverse_indicator_final=="yes" & dt$any_result_value > dt$target_value)] <- "no"

# calculate the result_reporter based on numerator, denominator, and percentage values reported
for(i in 1:nrow(dt)) {
  if (is.na(dt$gf_result_n[i]) && is.na(dt$gf_result_d[i]) && is.na(dt$gf_result_pct[i])){
    if (is.na(dt$lfa_result_n[i]) && is.na(dt$lfa_result_d[i]) && is.na(dt$lfa_result_pct[i])){
      if (is.na(dt$pr_result_n[i]) && is.na(dt$pr_result_d[i]) && is.na(dt$pr_result_pct[i])){
        dt[i, result_reporter:="None"]
      } else {
        dt[i, result_reporter:="PR"]
      }
    } else {
      dt[i, result_reporter:="LFA"]
    }
  } else {
    dt[i,result_reporter:="GF"]
  }
}

# use final reporter data to fill in the result columns (numerator, denominator, and percentage)
for (i in 1:nrow(dt)){
  if(dt[i, result_reporter]=="GF"){
    dt[i, result_n:=gf_result_n]
    dt[i, result_d:=gf_result_d]
    dt[i, result_pct:=gf_result_pct]
    dt[i, result_year:=as.integer(NA)]
    dt[i, result_source_code:="None reported"]
  } else if (dt[i, result_reporter]=="LFA"){
    dt[i, result_n:=lfa_result_n]
    dt[i, result_d:=lfa_result_d]
    dt[i, result_pct:=lfa_result_pct]
    dt[i, result_year:=lfa_result_year]
    dt[i, result_source_code:=lfa_result_source_code]
  } else if (dt[i, result_reporter]=="PR"){
    dt[i, result_n:=pr_result_n]
    dt[i, result_d:=pr_result_d]
    dt[i, result_pct:=pr_result_pct]
    dt[i, result_year:=pr_result_year]
    dt[i, result_source_code:=pr_result_source_code]
  } else if (dt[i, result_reporter]=="None"){
    dt[i, result_n:=as.numeric(NA)]
    dt[i, result_d:=as.numeric(NA)]
    dt[i, result_pct:=as.numeric(NA)]
  }
}

# # calculate a achievement ratio using numerator
for (i in 1:nrow(dt)){
  if (dt[i, result_reporter=="GF"]){
    dt[i, achievement_ratio_n:=as.numeric(gf_result_n/target_n)]
  } else if (dt[i, result_reporter=="LFA"]){
    dt[i, achievement_ratio_n:=as.numeric(lfa_result_n/target_n)]
  } else if (dt[i, result_reporter=="PR"]){
    dt[i, achievement_ratio_n:=as.numeric(pr_result_n/target_n)]
  }
}

# # calculate a achievement ratio using percentage values
for (i in 1:nrow(dt)){
  if (dt[i, result_reporter=="GF"]){
    dt[i, achievement_ratio_pct:=as.numeric(gf_result_pct/target_n)]
  } else if (dt[i, result_reporter=="LFA"]){
    dt[i, achievement_ratio_pct:=as.numeric(lfa_result_pct/target_n)]
  } else if (dt[i, result_reporter=="PR"]){
    dt[i, achievement_ratio_pct:=as.numeric(pr_result_pct/target_n)]
  }
}

### include the achievement ratio that is reported by the PUDR
for (i in 1:nrow(dt)){
  if (dt[i, result_reporter=="GF"]){
    dt[i, reported_achievement_ratio:=gf_result_achievement_ratio]
  } else if (dt[i, result_reporter=="LFA"]){
    dt[i, reported_achievement_ratio:=lfa_result_achievement_ratio]
  } else if (dt[i, result_reporter=="PR"]){
    dt[i, reported_achievement_ratio:=pr_result_achievement_ratio]
  }
}

#-------------------------------------------------------------
# Re-format data to make easier to visualize on tableau
#-------------------------------------------------------------
dt[pudr_sheet=="coverage_indicators_main", indicator_type:="Coverage"]
dt[pudr_sheet=="coverage_indicators_disagg", indicator_type:="Coverage_disagg"]
dt[pudr_sheet=="impact_outcome_indicators_main", indicator_type:="Impact"]

# tableau.dt <- tableau.dt[completeness_rating %in% c("Both available", "Only Result", "Only Target")]



#----------------------------------------------------------
# # Make a spreadsheet for topic area identification and one for RSSH/equity identification
# all_dt = as.data.table(read.csv(paste0(box, 'tableau_data/all_performance_indicators.csv')))
# topic_areas = unique(all_dt[, .(loc_name, indicator, module)]) # unique by loc and indicator
# rssh_equity = unique(all_dt[, .(indicator, indicator_code, module)]) # unique indicators 
#   # some of the above are different indicators only because they are in a different language...
#   # use indicator code to translate all to English (might not be possible if uga doesn't have all the same inds as drc/gtm/sen?)? 
# 
# # AB 7/2/20 quick changes for prep for Tableau
# dt = as.data.table(read.csv(paste0(box, 'UGA/prepped_data/uga_performance_indicators.csv')))

# lfa verified? not sure why this wasn't saving correctly before must be a data prep issue
master_file_list <- as.data.table(read.xlsx(paste0(box,"master_file_list.xlsx")))
merge_dt = master_file_list[, .(file_name, lfa_verified)]
setnames(merge_dt, 'lfa_verified', 'is_pudr_lfa_verified')
dt = merge(dt, merge_dt, all.x = TRUE, by = 'file_name')
dt[, lfa_verified := NULL]
setnames(dt, 'is_pudr_lfa_verified', 'lfa_verified')

# label semesters
dt[, start_date_programmatic := as.Date(start_date_programmatic)]
dt[, end_date_programmatic := as.Date(end_date_programmatic)]
sems = unique(dt[, .(start_date_programmatic, end_date_programmatic)])
setorderv(sems, 'start_date_programmatic')
sems[, semester:= .I]
sems[, semester:= paste0('Semester ', semester)]
dt = merge(dt, sems, by = c('start_date_programmatic', 'end_date_programmatic'))

# # label UGA topic areas indicators
# dt[ module == 'Prevention programs for adolescents and youth, in and out of school', unique(indicator)]
# dt[ module == 'Prevention programs for adolescents and youth, in and out of school', isTopicArea := TRUE]
# dt[ module == 'Prevention programs for adolescents and youth, in and out of school', topicAreaDesc := 'AGYW']
dt[ loc_name=="uga" & module == 'Community responses and systems', unique(indicator)]
dt[ loc_name=="uga" &  module == 'Community responses and systems', isTopicArea := TRUE]
dt[ loc_name == "uga" & module == 'Community responses and systems', topicAreaDesc := 'CSS']
dt[ loc_name == "uga" & is.na(isTopicArea), isTopicArea := FALSE]

# label equity/RSSH indicators
# rssh = read_excel(paste0('J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/2018-2020 Modular Framework.xlsx'), sheet = 'RSSH Interventions')
# rssh = as.data.table(rssh)
# rssh_modules = unique(rssh$gf_module)
# rssh_modules = rssh_modules[!rssh_modules %in% c('Program management', 'Performance Based Financing', 'Unspecified')]
# dt[, module := as.character(module)]
# dt[ module %in% rssh_modules, ]
# dt[ module == 'RSSH: Health management information systems and M&amp;E', module := 'RSSH: Health management information systems and M&E']
# dt[ module == 'RSSH: Health management information systems and M&E', isStrategicObjective := TRUE]
# dt[ module == 'RSSH: Health management information systems and M&E', strategicObjective := 'RSSH']
# 
# dt[, indicator := as.character(indicator)]
# dt[grepl('KP', indicator, ignore.case = TRUE), isStrategicObjective := TRUE]
# dt[grepl('KP', indicator, ignore.case = TRUE), strategicObjective := 'Equity']
# dt[grepl('AGYW', indicator, ignore.case = TRUE), isStrategicObjective := TRUE]
# dt[grepl('AGYW', indicator, ignore.case = TRUE), strategicObjective := 'Equity']
# dt[grepl('pregnant', indicator, ignore.case = TRUE), isStrategicObjective := TRUE]
# dt[grepl('pregnant', indicator, ignore.case = TRUE), strategicObjective := 'Equity']
# dt[grepl('antenatal', indicator, ignore.case = TRUE), isStrategicObjective := TRUE]
# dt[grepl('antenatal', indicator, ignore.case = TRUE), strategicObjective := 'Equity']

# # generate a table for a codebook: 
# # reorder column names: 
# dt = dt[, .(loc_name, grant, grant_period, grant_status, primary_recipient, 
#        file_name, lfa_verified, start_date_programmatic, end_date_programmatic, semester,
#        module, disease, isTopicArea, topicAreaDesc, isStrategicObjective, strategicObjective, 
#        indicator_code, indicator, full_description, brief_description, indicator_type, category, sub_category,
#        target_n, target_d, target_pct, target_year, 
#        result_n, result_d, result_pct, result_year, result_reporter, result_source,
#        achievement_ratio_n, achievement_ratio_pct,
#        cumulative_target, reverse_indicator, completeness_rating)]
# cols = names(dt)
# codebook = data.table(Variable=cols)
#----------------------------------------------------------

# select columns to keep
tableau.cols <- c('loc_name', 'grant', 'grant_period', 'grant_status', 'primary_recipient',
                  'file_name', 'lfa_verified', 'start_date_programmatic', 'end_date_programmatic', 'semester',
                  'module', 'disease', 'isTopicArea', 'topicAreaDesc', 'isStrategicObjective', 'strategicObjective',
                  'indicator_code', 'indicator', 'full_description', 'brief_description', 'indicator_type', 'category', 'sub-category', 
                  'target_n', 'target_d', 'target_pct', 'target_year',
                  'result_n', 'result_d', 'result_pct', 'result_year', 'result_reporter', 'result_source_code',
                  'achievement_ratio_n', 'achievement_ratio_pct', 'reported_achievement_ratio',
                  'cumulative_target', 'reverse_indicator', 'value_type')

# oc_name, grant, grant_period, grant_status, primary_recipient, 
#        file_name, lfa_verified, start_date_programmatic, end_date_programmatic, semester,
#        module, disease, isTopicArea, topicAreaDesc, isStrategicObjective, strategicObjective, 
#        indicator_code, indicator, full_description, brief_description, indicator_type, category, sub_category,
#        target_n, target_d, target_pct, target_year, 
#        result_n, result_d, result_pct, result_year, result_reporter, result_source,
#        achievement_ratio_n, achievement_ratio_pct,
#        cumulative_target, reverse_indicator, completeness_rating

tableau.dt <- dt[,..tableau.cols]

# this codebook should be updated as well as the source code should match the result reporter
setnames(tableau.dt, old=c('result_source_code'), new=c('result_source'))

# there are some duplicated values in the data
tableau.dt <- unique(tableau.dt)

# SAVE COUNTRY SPECIFIC DATA
for (c in countries){
  write.csv(tableau.dt[loc_name==c], paste0(box, c, "/prepped_data", "/", c, "_performance_indicators.csv"), row.names = FALSE)
  print(paste0(c, " country specific data saved on box folder"))
}

# write.csv(dt, paste0(box, 'UGA/prepped_data/uga_performance_indicators.csv'), row.names = FALSE)

#------------------------------------------------------
# SAVE FINAL DATA
# -----------------------------------------------------
# saveRDS(dt, paste0(prepped_dir, "cleaned_pfi.rds"))
write.csv(tableau.dt, paste0(box,"tableau_data/all_performance_indicators.csv"))
saveRDS(dt, paste0(prepped_dir, "archive/cleaned_pfi_", Sys.Date(), ".rds"))

print("Step 3: Clean and validated data completed. Validated data saved as cleaned_pfi.RDS in prepped_data folder.")

