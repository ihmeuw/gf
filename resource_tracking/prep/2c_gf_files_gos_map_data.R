# ------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Map prepped GOS and GF files to final mappings, split HIV/TB
#          combined grants, and save to final save location. 
# DATE: Last updated May 2019. 
# ------------------------------------------------------------------

#----------------------------------------------------------------------------
# Read in the version of the data you want to map (logic variables set in master file)
#----------------------------------------------------------------------------
if (prep_gos == TRUE){
  raw_data = totalGos_qtr
  raw_data[, lfa_exp_adjustment:=0] #There is no LFA expenditure adjustment in GOS data; but add it to make code run. 
} else if (prep_files == TRUE){
  raw_data = resource_database
}

#-------------------------------------------------------
# Prep mapping data for merge 
#-------------------------------------------------------
#Remove whitespaces, punctuation, and unwanted characters from module and intervention. 
raw_data = strip_chars(raw_data)

#Correct common acronyms in the resource database and the module map. 
raw_data[, module:=replace_acronyms(module)]
raw_data[, intervention:=replace_acronyms(intervention)]

module_map[, module:=replace_acronyms(module)]
module_map[, intervention:=replace_acronyms(intervention)]

#Make some raw corrections here - These weren't accurate enough to put in the map, but we still need to account for them. 
if (!'activity_description'%in%names(raw_data)){ #If this column doesn't exist, add it as 'NA' so the code below can run
  raw_data[, activity_description:=NA]
}
if (prep_files == TRUE){
  raw_data = correct_modules_interventions(raw_data)
}

#------------------------------------------------------------
# Map budgets and PUDRs to module mapping framework 
#------------------------------------------------------------

# Check for unmapped modules/interventions before mapping
gf_concat <- paste0(module_map$module, module_map$intervention)
rt_concat <- paste0(raw_data$module, raw_data$intervention)
unmapped_mods <- raw_data[!rt_concat%in%gf_concat]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention"), with= FALSE]))
  print(unique(unmapped_mods$fileName)) #For documentation in the comments above. 
  stop("You have unmapped original modules/interventions!")
}

#------------------------------------------------------------
# Remap diseases so they apply at the intervention level, 
#   not the grant-level (assigned in the file list) 
#------------------------------------------------------------

#Correct all tb/hiv to hiv/tb
raw_data[disease == 'tb/hiv', disease:='hiv/tb']

#English corrections
raw_data[module=='hivhealthsystemsstrengthening', disease:='hiv']
raw_data[module=='malhealthsystemsstrengthening', disease:='malaria']
raw_data[module=='tbhealthsystemsstrengthening', disease:='tb']

#French corrections 
raw_data[module == 'priseenchargeetpreventiondelatuberculose' & disease == 'hiv', disease:='tb']
# 
# map = copy(module_map)
# module_map=map
# #EMILY TO DELETE - DOING A TINY CHECK HERE
# raw_data = raw_data[module=="atencionyprevenciondetuberculosis" & intervention=="poblacionesclaveafectadas"]
# module_map = module_map[module=="atencionyprevenciondetuberculosis" & intervention=="poblacionesclaveafectadas"]
#----------------------------------------------------------------------------
# Merge with module map on module, intervention, and disease to pull in code
#----------------------------------------------------------------------------
if ('disbursement'%in%names(raw_data)){
  pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment', 'disbursement')]
} else {
  pre_coeff_check = raw_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment')]
  pre_coeff_check[[1]] = round(pre_coeff_check[[1]])
  pre_coeff_check[[2]] = round(pre_coeff_check[[2]])
}

mergeVars = c('disease', 'module', 'intervention')
#module_map = unique(module_map)
module_map = module_map[!is.na(code)]

mapped_data <- merge(raw_data, module_map, by=mergeVars, all.x = TRUE, allow.cartesian = TRUE)
dropped_mods <- mapped_data[is.na(mapped_data$gf_module), ]

if(nrow(dropped_mods) >0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_mods[, c("module", "intervention", "disease"), with= FALSE]))
  stop("Modules/interventions were dropped!")
}

#-------------------------------------------------------
# #Remap all RSSH codes to the RSSH disease, and make sure 
#there aren't any HSS diseases still hanging around. Remap all codes to their correct disease.  
# ------------------------------------------------------
mapped_data[substring(code, 1, 1)=='R', disease:='rssh']
mapped_data[disease == 'hss', disease:='rssh']

mapped_data[substring(code, 1, 1)=='H', disease:='hiv']
mapped_data[substring(code, 1, 1)=='T', disease:='tb']
mapped_data[substring(code, 1, 1)=='M', disease:='malaria']

#-------------------------------------------------------
# Split HIV/TB combined grants  
# ------------------------------------------------------
mapped_data = split_hiv_tb(mapped_data)

#-------------------------------------------------------
# Redistribute using mapped coefficient 
# ------------------------------------------------------
remapped_rows = nrow(mapped_data[coefficient != 1])
print(paste0("A total of ", remapped_rows, " rows will be redistributed."))
mapped_data[, budget:=budget*coefficient]
mapped_data[, expenditure:=expenditure*coefficient]
mapped_data[, lfa_exp_adjustment:=lfa_exp_adjustment*coefficient]
if ('disbursement'%in%names(mapped_data)){
  mapped_data[, disbursement:=disbursement*coefficient]
}

if ('disbursement'%in%names(mapped_data)){
  post_coeff_check = mapped_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment', 'disbursement')]
  post_coeff_check[[1]] = round(post_coeff_check[[1]])
  post_coeff_check[[2]] = round(post_coeff_check[[2]])
  post_coeff_check[[3]] = round(post_coeff_check[[3]])
  post_coeff_check[[4]] = round(post_coeff_check[[4]])
  
  stopifnot(abs(pre_coeff_check[[1]]-post_coeff_check[[1]]) < 1) #Decision by David Phillips 5/10/19 - it's okay if there is less than one cent difference between the pre- and post-redistribution. 
  stopifnot(abs(pre_coeff_check[[2]]-post_coeff_check[[2]]) < 1)
  stopifnot(abs(pre_coeff_check[[3]]-post_coeff_check[[3]]) < 1)
  stopifnot(abs(pre_coeff_check[[4]]-post_coeff_check[[4]]) < 1)
} else {
  post_coeff_check = mapped_data[, lapply(.SD, sum_na_rm), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment')]
  post_coeff_check[[1]] = round(post_coeff_check[[1]])
  post_coeff_check[[2]] = round(post_coeff_check[[2]])
  post_coeff_check[[3]] = round(post_coeff_check[[3]])
  
  stopifnot(abs(pre_coeff_check[[1]]-post_coeff_check[[1]]) < 1) #Decision by David Phillips 5/10/19 - it's okay if there is less than one cent difference between the pre- and post-redistribution. 
  stopifnot(abs(pre_coeff_check[[2]]-post_coeff_check[[2]]) < 1)
  stopifnot(abs(pre_coeff_check[[3]]-post_coeff_check[[3]]) < 1)
}

#Debug the check above, if needed. 
#What line items got changed between the two files? 
# test = unique(raw_data[, .(module, intervention, disease, budget)])
# test = test[budget!=0]
# test[, set:='raw']
# test2 = mapped_data[, .(module, intervention, disease, budget, coefficient)]
# mapped_data = mapped_data[budget!=0]
# test2[, set:='mapped']
# check = merge(test, test2, by=c('module', 'intervention', 'disease'), all=TRUE, allow.cartesian=TRUE)
# #You want to find the cases here where raw budget/coefficient doesn't equal mapped budget
# 
# check = check[is.na(set.x)|is.na(set.y)][order(module, intervention, disease)]
# check = check[budget!=0]
# View(check[1:300])
# 
# check[is.na(set.x) & module=="atencionyprevenciondetuberculosis" & intervention=="poblacionesclaveafectadas"]
# check[is.na(set.y) & module=="atencionyprevenciondetuberculosis" & intervention=="poblacionesclaveafectadas"]
#-----------------------------------------------------------
# Add in a variable for 'includes RSSH'
#-----------------------------------------------------------
#By file and grant (to catch both budgets and GOS), should be "TRUE"
#if there is at least one 'R' code. 
mapped_data[, code_start:=substring(code, 1, 1)]
codes = unique(mapped_data[, .(code_start, grant, file_name)])
codes = dcast(codes, grant+file_name~code_start, value.var='file_name')
codes[is.na(R), includes_rssh:=FALSE]
codes[!is.na(R), includes_rssh:=TRUE]
codes = codes[, .(grant, file_name, includes_rssh)]

mapped_data = merge(mapped_data, codes, all.x=T, by=c('grant', 'file_name'))

#-----------------------------------------------------------
# Add in variable for current grant, and location variable
# ----------------------------------------------------------
mapped_data$current_grant = FALSE 
for (i in 1:length(current_cod_grants)){
  mapped_data[grant==current_cod_grants[i] & grant_period==current_cod_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_gtm_grants)){
  mapped_data[grant==current_gtm_grants[i] & grant_period==current_gtm_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_uga_grants)){
  mapped_data[grant==current_uga_grants[i] & grant_period==current_uga_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_sen_grants)){
  mapped_data[grant==current_sen_grants[i] & grant_period==current_sen_grant_period[i], 
              current_grant:=TRUE]
}

if (prep_files==TRUE){
  mapped_data$loc_name = country
  for (i in 1:nrow(code_lookup_tables)){
    mapped_data[loc_name==code_lookup_tables$iso_code[i], country:=code_lookup_tables$country[i]]
  }
}

#--------------------------------------------------------------------------------
#Add in a variable for the disease of the grant #Yuck - Emily try to rewrite this code. 
#--------------------------------------------------------------------------------
mapped_data[, disease_split:=strsplit(grant, "-")]
potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')

for (i in 1:nrow(mapped_data)){
  if (mapped_data$disease_split[[i]][2]%in%potential_diseases){
    mapped_data[i, grant_disease:=sapply(disease_split, "[", 2 )]
  } else if (mapped_data$disease_split[[i]][3]%in%potential_diseases){
    mapped_data[i, grant_disease:=sapply(disease_split, "[", 3 )]
  } else if (mapped_data$disease_split[[i]][4]%in%potential_diseases){
    mapped_data[i, grant_disease:=sapply(disease_split, "[", 4 )]
  }
}

mapped_data[, disease_split:=NULL]

unique(mapped_data[!grant_disease%in%potential_diseases, .(grant, grant_disease)]) #Visual check that these all make sense. 

mapped_data[grant_disease=='C', grant_disease:='hiv/tb']
mapped_data[grant_disease=='H', grant_disease:='hiv']
mapped_data[grant_disease=='T', grant_disease:='tb']
mapped_data[grant_disease=='S' | grant_disease=='R', grant_disease:='rssh']
mapped_data[grant_disease=='M', grant_disease:='malaria']
mapped_data[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 

stopifnot(unique(mapped_data$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))

# --------------------------------------------------------
# Convert currencies to USD 
# --------------------------------------------------------
orig_rows = nrow(mapped_data)
stopifnot(mapped_data$file_currency%in%c("LOC","EUR","USD"))

needs_conversion = mapped_data[file_currency!='USD']
if (nrow(needs_conversion)!=0){
  #Do a check before and after converting to make sure you've got the same totals. 
  pre_conversion_check = mapped_data[, .(pre_budget=sum(budget, na.rm=T), pre_expenditure=sum(expenditure, na.rm=T), 
                                         pre_lfa_exp=sum(lfa_exp_adjustment, na.rm=T)), by='file_name']
  
  #Pull apart the files that are in Euros vs. USD, and convert Euros. 
  valueVars = c('budget', 'expenditure', 'disbursement', 'lfa_exp_adjustment')
  in_USD = copy(mapped_data)
  in_USD = in_USD[file_currency=='USD']
  in_USD[, budget_new:=budget]
  in_USD[, expenditure_new:=expenditure]
  in_USD[, lfa_exp_adjustment_new:=lfa_exp_adjustment]
  in_USD[, disbursement_new:=disbursement]
  
  stopifnot(needs_conversion$file_currency%in%c("LOC", "EUR")) #These are the only currencies the function supports. 
  converted_to_USD = convert_eur_usd(needs_conversion, 'year')
  mapped_data = rbind(in_USD, converted_to_USD, fill=TRUE, use.names=TRUE) #You're not losing any rows here. 
  stopifnot(nrow(mapped_data)==orig_rows)
  
  # #Post-check. 
  # mapped_data$eur_usd <- NULL
  # post_conversion_check = convert_usd_eur(mapped_data, 'year')
  # post_conversion_check = post_conversion_check[, .(post_budget=sum(budget, na.rm=T), post_expenditure=sum(expenditure, na.rm=T)), by='file_name']
  # 
  # conversion_check = merge(pre_conversion_check, post_conversion_check, by='file_name', all=T)
  # conversion_check = conversion_check[, lapply(.SD, round), .SDcols = 2:5, by='file_name'][, lapply(.SD, as.integer), .SDcols = 2:5, by='file_name']
  # if (nrow(conversion_check[pre_budget!=post_budget | pre_expenditure!= post_expenditure])==0){
  #   View(conversion_check[pre_budget!=post_budget | pre_expenditure!= post_expenditure])
  #   stop("Errors in currency conversion - review 'conversion_check'." )
  # }
  
  #If the check above works, then you're okay to rename budget and expenditure 
  mapped_data = mapped_data[, -c('budget', 'expenditure', 'disbursement', 'lfa_exp_adjustment')]
  setnames(mapped_data, c('budget_new', 'expenditure_new', 'disbursement_new', 'lfa_exp_adjustment_new'), 
           c('budget', 'expenditure', 'disbursement', 'lfa_exp_adjustment'))
}

# --------------------------------------------------------
#Validate the columns in final data and the storage types  
# --------------------------------------------------------

#Note that I'm dropping 'module' and 'intervention' - which were corrected from the original text, but are just used for mapping. EKL 1/29/19
# Only keep the variable names that are in the codebook for consistency. This should constantly be reviewed. 
dropped_vars = names(mapped_data)[!names(mapped_data)%in%codebook$Variable]
if (length(dropped_vars)!=0){
  print("Some variables are being dropped because they aren't in the codebook - Review to make sure these shouldn't be in the final data.")
  print(dropped_vars)
}
mapped_data = mapped_data[, names(mapped_data)%in%codebook$Variable, with=FALSE]

#After variables are removed, collapse dataset to simplify
byVars <- colnames(mapped_data)
if ('disbursement'%in%names(mapped_data)){
  byVars = byVars[byVars != 'budget' & byVars != 'expenditure' & byVars !='disbursement' & byVars !='lfa_exp_adjustment']
  mapped_data = mapped_data[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment', 'disbursement'), by=byVars]
} else {
  byVars = byVars[byVars != 'budget' & byVars != 'expenditure' & byVars != 'lfa_exp_adjustment']
  mapped_data = mapped_data[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment'), by=byVars]
}

#Reorder data 
if ('disbursement'%in%names(mapped_data)){
  mapped_data = mapped_data[order(grant, start_date, year, gf_module, gf_intervention, activity_description, country, loc_name, 
                                budget, expenditure, lfa_exp_adjustment, disbursement, orig_module, orig_intervention, current_grant, file_name)]
} else{
  mapped_data = mapped_data[order(grant, start_date, year, gf_module, gf_intervention, country, loc_name, 
                                  budget, expenditure, lfa_exp_adjustment, orig_module, orig_intervention, current_grant, file_name)]
}
#------------------------------------------------------------
# Remove any special characters so .csv will store correctly 
#------------------------------------------------------------
mapped_data$activity_description <- str_replace_all(mapped_data$activity_description, "[^[:alnum:]]", " ")
mapped_data$orig_module <- str_replace_all(mapped_data$orig_module, "[^[:alnum:]]", " ")
mapped_data$orig_intervention <- str_replace_all(mapped_data$orig_intervention, "[^[:alnum:]]", " ")

# ----------------------------------------------------------------------------
# Create unique datasets - final budgets, final expenditures, and absorption
# ---------------------------------------------------------------------------

# 1. Create an absorption dataset shaped wide by grant, grant period, module, and intervention, that shows budget/expenditure by semester. 
# 2. For the expenditure dataset, we should subtract the earlier quarter in a year from the later quarters, and then append all of this to create an expenditure dataset. 

if (prep_files){
  #-------------------------------------------
  #1. Budgets 
  final_budgets = mapped_data[file_iteration == "final" & data_source == "fpm"] #Only want the final versions of budgets. 
  final_budgets = final_budgets[, -c('expenditure', 'lfa_exp_adjustment', 'disbursement')]
  
  #-------------------------------------------
  #2. Expenditures - pull out expenditures file, and generate 'final expenditure' variable. 
  expenditures = mapped_data[data_source=="pudr" & file_iteration=="final"]
  expenditures[, final_expenditure:=expenditure+lfa_exp_adjustment]
  expenditures = expenditures[, -c('expenditure', 'lfa_exp_adjustment')]
  setnames(expenditures, 'final_expenditure', 'expenditure')
  
  #Add in PUDR semester variable. 
  setnames(expenditures, 'pudr_semester', 'pudr_code')
  expenditures = merge(expenditures, pudr_labels, by='pudr_code', all.x=T)
  exp_check1 = expenditures[, .(correct_exp=sum(expenditure, na.rm=T)), by=c('grant', 'grant_period', 'code', 'semester_code', 'pudr_grant_year')]
  setnames(exp_check1, 'semester_code', 'semester')
  
  #Make sure this merge worked. 
  if (nrow(expenditures[is.na(semester)])>0){
    print(unique(expenditures[is.na(semester), .(pudr_code)]))
    stop("Values of pudr_code did not merge correctly.")
  }
  
  #Generate new variables, and flag where you would have overlap in files. 
  dup_files = unique(expenditures[, .(grant, grant_period, semester_code, pudr_grant_year)][order(grant, grant_period)])
  dup_files[, dup:=seq(0, nrow(dup_files)), by=c('grant', 'grant_period')]
  dup_files = unique(dup_files[dup!=0, .(grant, grant_period)]) #But this isn't the only condition on whether you have overlap. Now, check if the same files for the same grant overlap in time. 
  
  #First, see if dup==1. This tells you that you have duplicate files you need to compare. 
  #Tag these dup files in the expenditures dataset 
  for (i in 1:nrow(dup_files)){
    expenditures[grant==dup_files$grant[i] & grant_period==dup_files$grant_period[i],
                 dup:=TRUE]
  }
  expenditures[is.na(dup), dup:=FALSE]
  
  #Second, see if you actually have overlap in the quarters, which happens when you have a year-long PUDR. 
  dup_files2 = unique(expenditures[dup==TRUE, .(grant, grant_period, pudr_grant_year, semester_code, dup)][order(grant, grant_period, pudr_grant_year, semester_code)])
  dup_files2[nchar(semester_code)==2, yearlong:=TRUE]
  dup_files2 = dup_files2[yearlong==TRUE, .(grant, grant_period, pudr_grant_year)]

  #If you find duplicate semesters, subtract them, and then reassemble the dataset. 
  if(nrow(dup_files2)>0){
    for (i in 1:nrow(dup_files2)){
      expenditures[grant==dup_files2$grant[i] & grant_period==dup_files2$grant_period[i] & pudr_grant_year==dup_files2$pudr_grant_year[i],
                   overlap:=TRUE]
    }
    expenditures[is.na(overlap), overlap:=FALSE]
    
    flagged_overlap = unique(expenditures[overlap==TRUE, .(grant, grant_period, file_name, semester_code)][order(grant, grant_period, semester_code)])
    if(nrow(flagged_overlap)!=0){
      print("The following PUDRs were flagged as overlapping, and will be subtracted for the expenditure dataset.")
      print(flagged_overlap)
    }

  #EMILY ADD A CHECK HERE THAT YOU ONLY HAVE ONE DUPLICATE THAT WILL BE SUBTRACTED. 
  #Pull out data that has overlap, and subtract earlier PUDRs from later PUDRs. 
  #Sum out the quarter-level
  
    valueVars = c('budget', 'expenditure', 'disbursement')
    exp_collapse = expenditures[overlap==TRUE]
    exp_collapse[, start_date:=min(start_date), by='file_name']
    exp_collapse = exp_collapse[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T), disbursement=sum(disbursement, na.rm=T)), 
                                by=c('grant', 'grant_period', 'code', 'semester_code', 'pudr_grant_year', 'start_date')]
    #Validate the data that you've pulled - make sure semesters will work with code below. 
    stopifnot(unique(exp_collapse$semester_code)%in%c('A', 'AB'))
    exp_collapse = dcast(exp_collapse, grant+grant_period+pudr_grant_year+code+start_date~semester_code, value.var=valueVars)
    #EMILY FLAG CASES HERE WHERE WE DON'T HAVE A MODULE/INTERVENTION IN ONE PUDR OR THE OTHER 
    
    #Subtract earlier semesters from later semesters 
    #First, replace NAs with 0's. 
    exp_collapse[is.na(budget_A), budget_A:=0] #EMILY IS THIS THE BEST WAY TO DO THIS??
    exp_collapse[is.na(budget_AB), budget_AB:=0]
    exp_collapse[is.na(expenditure_A), expenditure_A:=0]
    exp_collapse[is.na(expenditure_AB), expenditure_AB:=0]
    exp_collapse[is.na(disbursement_A), disbursement_A:=0]
    exp_collapse[is.na(disbursement_AB), disbursement_AB:=0]
    
    exp_collapse[, budget_B:=budget_AB-budget_A]
    exp_collapse[, expenditure_B:=expenditure_AB-expenditure_A]
    exp_collapse[, disbursement_B:=disbursement_AB-disbursement_A]
    
    negatives = exp_collapse[expenditure_B<0]
    if (nrow(negatives)!=0){
      print("There were negative values generated for expenditure. Review 'negative'.")
      write.csv(negatives, paste0(dir, "_gf_files_gos/", country, "/visualizations/", country, "_negative_expenditure.csv"), row.names=FALSE)
    }
    
    exp_collapse = exp_collapse[, -c('budget_AB', 'expenditure_AB', 'disbursement_AB')]
    
    #reshape this data back long, and merge back onto the rest of the expenditure dataset. 
    exp_melt = melt(exp_collapse, id.vars=c('grant', 'grant_period', 'code', 'pudr_grant_year', 'start_date'))
    exp_melt[, semester:=tstrsplit(variable, "_", keep=2)]
    stopifnot(unique(exp_melt$semester)%in%c('A', 'B'))
    exp_melt[, variable:=tstrsplit(variable, "_", keep=1)]
    stopifnot(unique(exp_melt$budget)%in%c('A', 'B'))
    
    # Cast back so budget, expenditure, and disbursement are variable names again. 
    exp_recast = dcast(exp_melt, grant+grant_period+code+pudr_grant_year+semester+start_date~variable, value.var='value')
    exp_recast[semester=="B", start_date:=start_date %m+% months(6)]
    exp_recast[, duration_quarters:=2]
    
    #Merge back onto expenditure data that DIDN'T need subtraction 
    exp_no_subtract = expenditures[overlap==FALSE, .(budget=sum(budget, na.rm=TRUE), expenditure=sum(expenditure, na.rm=TRUE), disbursement=sum(disbursement, na.rm=TRUE)), 
                                   by=c('grant', 'grant_period', 'code', 'pudr_grant_year', 'semester_code', 'start_date', 'duration_quarters')]
    setnames(exp_no_subtract, 'semester_code', 'semester')
    
    #Append datasets
    expenditures = rbind(exp_recast, exp_no_subtract) 
    
    #Create date variables 
    expenditures[, end_date:=(start_date %m+% months((duration_quarters*3)))-1]
    expenditures = expenditures[, -c('duration_quarters')]
    
    #Check duplicates
    duplicates = expenditures[duplicated(expenditures)] 
    if(nrow(duplicates)!=0){
      stop(paste0("There are ", nrow(duplicates), "in expenditures file. Review summing and appending code."))
    }
    duplicates <- NULL 
    
    # Add on additional variables 
    before_merge = nrow(expenditures)
    merge_vars = unique(mapped_data[, .(grant, grant_period, code, #These are your identifying variables in your expenditures dataset. 
                                        gf_module, gf_intervention, disease)]) #These are identified by code
    expenditures = merge(expenditures, merge_vars, by=c('grant', 'grant_period', 'code'))
    after_merge = nrow(expenditures)
    if (before_merge!=after_merge){
      stop("The number of rows before and after additional variables were added to expenditure do not match. Check merge condition.")
    }
    
    #Remove extra financial variables. 
    expenditures = expenditures[, -c('budget', 'disbursement')]
    
    #Add in grant disease variable 
    expenditures[, disease_split:=strsplit(grant, "-")]
    potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')
    
    for (i in 1:nrow(expenditures)){
      if (expenditures$disease_split[[i]][2]%in%potential_diseases){
        expenditures[i, grant_disease:=sapply(disease_split, "[", 2 )]
      } else if (expenditures$disease_split[[i]][3]%in%potential_diseases){
        expenditures[i, grant_disease:=sapply(disease_split, "[", 3 )]
      } else if (expenditures$disease_split[[i]][4]%in%potential_diseases){
        expenditures[i, grant_disease:=sapply(disease_split, "[", 4 )]
      }
    }
    
    expenditures[, disease_split:=NULL]
    
    unique(expenditures[!grant_disease%in%potential_diseases, .(grant, grant_disease)]) #Visual check that these all make sense. 
    
    expenditures[grant_disease=='C', grant_disease:='hiv/tb']
    expenditures[grant_disease=='H', grant_disease:='hiv']
    expenditures[grant_disease=='T', grant_disease:='tb']
    expenditures[grant_disease=='S' | grant_disease=='R', grant_disease:='rssh']
    expenditures[grant_disease=='M', grant_disease:='malaria']
    expenditures[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 
    
    stopifnot(unique(expenditures$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))
    
  } else { #If you don't have duplicate files, collapse your dataset to be in the same format. 
    expenditures[, start_date:=min(start_date), by='file_name']
    expenditures = expenditures[, .(expenditure=sum(expenditure, na.rm=T)),
                                by=c('grant', 'grant_period', 'code', 'year', 'pudr_grant_year', 'semester', 'start_date', 'gf_module', 'gf_intervention', 'disease')]
    #Add in PUDR label value. 
    expenditures = merge(expenditures, pudr_labels, by=c('semester', 'pudr_grant_year'), all.x=T)
    if (nrow(expenditures[is.na(semester_code)])>0){
      stop("Some values of PUDR labels did not merge correctly onto expenditure dataset.")
    }
    expenditures = expenditures[, -c('semester', 'pudr_order', 'pudr_code')]
    setnames(expenditures, 'semester_code', 'semester')
    
    #Create date variables 
    expenditures[, end_date:=(start_date %m+% months((duration_quarters*3)))-1]
    expenditures = expenditures[, -c('duration_quarters')]
    
    #Add in grant disease variable 
    expenditures[, disease_split:=strsplit(grant, "-")]
    potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')
    
    for (i in 1:nrow(expenditures)){
      if (expenditures$disease_split[[i]][2]%in%potential_diseases){
        expenditures[i, grant_disease:=sapply(disease_split, "[", 2 )]
      } else if (expenditures$disease_split[[i]][3]%in%potential_diseases){
        expenditures[i, grant_disease:=sapply(disease_split, "[", 3 )]
      } else if (expenditures$disease_split[[i]][4]%in%potential_diseases){
        expenditures[i, grant_disease:=sapply(disease_split, "[", 4 )]
      }
    }
    
    expenditures[, disease_split:=NULL]
    
    unique(expenditures[!grant_disease%in%potential_diseases, .(grant, grant_disease)]) #Visual check that these all make sense. 
    
    expenditures[grant_disease=='C', grant_disease:='hiv/tb']
    expenditures[grant_disease=='H', grant_disease:='hiv']
    expenditures[grant_disease=='T', grant_disease:='tb']
    expenditures[grant_disease=='S' | grant_disease=='R', grant_disease:='rssh']
    expenditures[grant_disease=='M', grant_disease:='malaria']
    expenditures[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 
    
    stopifnot(unique(expenditures$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))
    
  }

  #-------------------------------------------
  #3. Absorption
  absorption = mapped_data[data_source=="pudr" & file_iteration=="final", .(grant, grant_period, code, gf_module, gf_intervention, budget, expenditure, lfa_exp_adjustment, pudr_semester, start_date)]
  absorption[, expenditure:=expenditure+lfa_exp_adjustment] #Calculate final expenditure. 
  absorption = absorption[, -c('lfa_exp_adjustment')]
  setnames(absorption, 'pudr_semester', 'pudr_code')
  absorption = merge(absorption, pudr_labels, by=c('pudr_code'), all.x=T)
  if (nrow(absorption[is.na(semester)])>0){
    print(unique(absorption[is.na(semester), .(pudr_code)]))
    stop("Values of pudr_code did not merge correctly.")
  }
  
  #Make the start date the first start date of the file (i.e., collapse the quarter-level out in the next step)
  absorption[, start_date:=min(start_date), by=c('grant', 'grant_period', 'semester')]
  
  #Calculate absorption by module/intervention 
  absorption = absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                          by=c('grant', 'grant_period', 'gf_module', 'gf_intervention', 'semester', 'code', 'duration_quarters', 'start_date')]
  absorption[, absorption:=(expenditure/budget)*100]
  
  #Add additional variables 
  absorption[, end_date:=start_date %m+% months((duration_quarters*3))]
  absorption = absorption[, -c('duration_quarters')]
  absorption[, loc_name:=country]
  
  absorption[, disease:=substr(code, 1, 1)]
  absorption[disease=='H', disease:='hiv']
  absorption[disease=='T', disease:='tb']
  absorption[disease=='M', disease:='malaria']
  absorption[disease=='R', disease:='rssh']
  
  #Add in grant disease variable 
  absorption[, disease_split:=strsplit(grant, "-")]
  potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')
  
  for (i in 1:nrow(absorption)){
    if (absorption$disease_split[[i]][2]%in%potential_diseases){
      absorption[i, grant_disease:=sapply(disease_split, "[", 2 )]
    } else if (absorption$disease_split[[i]][3]%in%potential_diseases){
      absorption[i, grant_disease:=sapply(disease_split, "[", 3 )]
    } else if (absorption$disease_split[[i]][4]%in%potential_diseases){
      absorption[i, grant_disease:=sapply(disease_split, "[", 4 )]
    }
  }
  
  absorption[, disease_split:=NULL]
  
  unique(absorption[!grant_disease%in%potential_diseases, .(grant, grant_disease)]) #Visual check that these all make sense. 
  
  absorption[grant_disease=='C', grant_disease:='hiv/tb']
  absorption[grant_disease=='H', grant_disease:='hiv']
  absorption[grant_disease=='T', grant_disease:='tb']
  absorption[grant_disease=='S' | grant_disease=='R', grant_disease:='rssh']
  absorption[grant_disease=='M', grant_disease:='malaria']
  absorption[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 
  
  stopifnot(unique(absorption$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))
  
  #Make Nan, Infinity all NA 
  absorption[is.nan(absorption), absorption:=NA]
  absorption[!is.finite(absorption), absorption:=NA]
  
}

# ----------------------------------------------
# Write the prepped data as .csvs
# ---------------------------------------------

if (prep_files){
  # Save RDS file
  saveRDS(final_budgets, paste0(export_dir, "final_budgets.rds"))
  saveRDS(expenditures, paste0(export_dir, "final_expenditures.rds"))
  saveRDS(mapped_data, paste0(export_dir, "budget_pudr_iterations.rds"))
  saveRDS(absorption, paste0(export_dir, "absorption_", country, ".rds"))
  
  write.csv(final_budgets, paste0(export_dir, "final_budgets.csv"), row.names=FALSE)
  write.csv(expenditures, paste0(export_dir, "final_expenditures.csv"), row.names=FALSE)
  write.csv(mapped_data, paste0(export_dir, "budget_pudr_iterations.csv"), row.names=FALSE)
  write.csv(absorption, paste0(export_dir, "absorption_", country, ".csv"), row.names=FALSE)
}

if (prep_gos == TRUE){
  saveRDS(mapped_data, paste0(gos_prepped, "prepped_gos_data.rds"))
  write.csv(mapped_data, paste0(gos_prepped, "prepped_gos_data.csv"), row.names = FALSE)
}

print("Step C: Map data completed. Outputs saved in prepped_data folder.")