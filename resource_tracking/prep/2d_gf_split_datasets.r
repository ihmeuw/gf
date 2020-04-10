# ----------------------------------------------------------------------------
# Create datasets to release to partners
# ---------------------------------------------------------------------------

#-------------------------------------------
# 1. Final, approved budgets 
#-------------------------------------------
gep_cols = c('file_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention', 'disease', 'start_date',
             'current_grant', 'data_source', 'file_iteration','abbrev_mod', 'code',
             'grant_disease', 'loc_name', 'includes_rssh', 'kp', 'rssh', 'update_date')
cep_cols = c('file_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention', 'disease', 'start_date')

# subset to just the approved budgets 
approved_budgets = mapped_data[file_iteration == 'approved_gm' & data_source == "budget" & current_grant==TRUE] #Only want the final versions of budgets. 
approved_budgets = approved_budgets[, -c('expenditure', 'lfa_exp_adjustment', 'disbursement')]

# Subset columns to GEP and CEP variables. 
approved_budgets_gep = approved_budgets[, .(budget=sum(budget, na.rm=T)), by=gep_cols]
approved_budgets_cep = approved_budgets[, .(budget=sum(budget, na.rm=T)), by=cep_cols]
#----------------------------------------------
# 2. Most recent revision of budgets
#----------------------------------------------
gep_cols = c('file_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention', 'disease', 'start_date',
             'current_grant', 'data_source', 'file_iteration','abbrev_mod', 'code',
             'grant_disease', 'loc_name', 'includes_rssh', 'kp', 'rssh', 'update_date')
cep_cols = c('file_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention', 'disease', 'start_date')

# Order final and revised budgets by the update date of the file
revision_order = unique(mapped_data[data_source=="budget" & file_iteration%in%c('approved_gm', 'revision') & current_grant==TRUE,
                                    .(grant, grant_period, update_date, file_iteration)])[order(grant, grant_period, file_iteration, update_date)]
stopifnot(nrow(revision_order[file_iteration=="revision" & is.na(update_date)])==0) # It's okay for 'approved_gm' budgets to not have an update date, because 
                                                                                    # they always come first, but revised budgets need one so they'll be sorted correctly. 
revision_order[, budget_version:=1:.N, by=c('grant', 'grant_period')]

# For this dataset, we only want to keep the last revision, so we need to find the maximum value of 'budget_version'. 
revision_order[, last_revision:=max(budget_version), by=c('grant', 'grant_period')]
revision_order = revision_order[budget_version==last_revision]

#Now, subset the whole dataset
revision_order[, -c('budget_version', 'last_revision')]
most_recent_revisions = merge(mapped_data, revision_order, by=c('grant', 'grant_period', 'update_date', 'file_iteration'), all.y=T) # Only keep the most recent revisions you've flagged. 
most_recent_revisions = most_recent_revisions[data_source=="budget"]

# Subset columns to GEP and CEP variables. 
most_recent_revisions_gep = most_recent_revisions[, .(budget=sum(budget, na.rm=T)), by=gep_cols]
most_recent_revisions_cep = most_recent_revisions[, .(budget=sum(budget, na.rm=T)), by=cep_cols]
#------------------------------------------------------
# 3. Budget revisions
#------------------------------------------------------
gep_cols = c('file_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention', 'disease', 'start_date', 'budget_version',
             'current_grant', 'data_source', 'file_iteration','abbrev_mod', 'code',
             'grant_disease', 'loc_name', 'includes_rssh', 'kp', 'rssh', 'update_date')
cep_cols = c('file_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention', 'disease', 'start_date', 
             'file_iteration', 'budget_version')

revisions = mapped_data[current_grant==TRUE & data_source=="budget"]
if (nrow(revisions)!=0){ #You won't have budget revisions for every country. 
  #Figure out the order using the 'update_date' variable. 
  order = unique(revisions[, .(grant, grant_period, update_date, file_name, file_iteration)][order(grant_period, grant, update_date)])
  if(nrow(order[is.na(update_date)])!=0) print('Warning: There are NAs in update_date, which is being used to determine the order of revisions')
  
  order[, order:=1:.N, by=c('grant', 'grant_period', 'file_iteration')]
  
  # Add in a clean label for mapping
  order[file_iteration=="initial", budget_version:=paste0("Grantmaking iteration ", order)]
  order[file_iteration=='approved_gm', budget_version:="Approved from grantmaking"]
  order[file_iteration=="revision", budget_version:=paste0("Revision ", order)]
  order$order <- NULL 
  
  #Reshape this data wide by quarter and year. 
  revisions = merge(revisions, order, by=c('grant', 'grant_period', 'update_date', 'file_name', 'file_iteration'), all.x=T)
  
  # Subset columns to GEP and CEP variables. 
  revisions_gep = revisions[, .(budget=sum(budget, na.rm=T)), by=gep_cols]
  revisions_cep = revisions[, .(budget=sum(budget, na.rm=T)), by=cep_cols]
  
}

#-------------------------------------------------------------
# Flag most recent PUDRs - this will be used in steps 4 and 5. 
#-------------------------------------------------------------
most_recent_pudrs = unique(mapped_data[data_source=="pudr" & file_iteration=='approved_gm' & current_grant==TRUE,
                                       .(grant, grant_period, file_name, pudr_semester_financial)])
# Merge on PUDR code to get an easy numeric sorting.
setDT(pudr_labels)
setnames(most_recent_pudrs, 'pudr_semester_financial', 'pudr_code')
pudr_label_merge = unique(pudr_labels[, .(pudr_code, pudr_order)])
most_recent_pudrs = merge(most_recent_pudrs, pudr_label_merge, by=c('pudr_code'), all.x=T)
if (nrow(most_recent_pudrs[is.na(pudr_code)])>0){
  print(unique(most_recent_pudrs[is.na(pudr_code), .(pudr_code)]))
  stop("Values of pudr_code did not merge correctly.")
}

# Just keep the 'maximum' PUDR code, which represents the latest reporting for a given grant and grant period. 
most_recent_pudrs[, latest_reporting:=max(pudr_order), by=c('grant', 'grant_period')]
most_recent_pudrs = most_recent_pudrs[pudr_order==latest_reporting]

#-------------------------------------------
# 4. Absorption for most recent reporting period
#-------------------------------------------
gep_cols = c('file_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention', 'disease', 'budget', 'expenditure', 'absorption',
             'start_date', 'budget_version',
             'current_grant', 'data_source', 'file_iteration','abbrev_mod', 'code',
             'grant_disease', 'loc_name', 'includes_rssh', 'kp', 'rssh', 'update_date')
cep_cols = c('file_name', 'grant', 'grant_period', 'gf_module', 'gf_intervention', 'disease', 'budget', 'expenditure', 'absorption', 'start_date', 'budget_version')

absorption = mapped_data[file_name%in%most_recent_pudrs$file_name, .(grant, grant_period, code, gf_module, gf_intervention, 
                                                                          budget, expenditure, lfa_exp_adjustment, pudr_semester_financial, start_date)]
absorption[, expenditure:=expenditure+lfa_exp_adjustment] #Calculate final expenditure. 
absorption = absorption[, -c('lfa_exp_adjustment')]
setnames(absorption, 'pudr_semester_financial', 'pudr_code')
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
absorption[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #ONLY ONE CASE OF THIS. 

stopifnot(unique(absorption$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))

#Make Nan, Infinity all NA 
absorption[is.nan(absorption), absorption:=NA]
absorption[!is.finite(absorption), absorption:=NA]

# Subset columns to GEP and CEP variables. 
absorption_gep = copy(absorption) # Leaving this in this format for now EL 3/9/20 
absorption_cep = absorption[, .(grant, grant_period, gf_module, gf_intervention, disease, start_date, end_date, 
                                    budget, expenditure, absorption)]

#------------------------------------------------------------
# 5. Cumulative absorption, extracted from most recent PUDR
#------------------------------------------------------------
# EL 3/10/2020 - note that file name is not going to work well here, because we have some calculated cumulative absorption, 
# and this has to be summed across files. 
gep_cols = c('grant', 'grant_period', 'gf_module', 'gf_intervention', 'disease', 'cumulative_budget', 'cumulative_expenditure', 'cumulative_absorption',
             'start_date', 'end_date', 'cumul_abs_method')
cep_cols = c('grant', 'grant_period', 'gf_module', 'gf_intervention', 
             'disease', 'cumulative_budget', 'cumulative_expenditure', 'cumulative_absorption',
             'start_date', 'end_date', 'cumul_abs_method')

byVars = c('grant', 'grant_period', 'code', 'gf_module', 'gf_intervention', 
           'pudr_semester_financial', 'start_date', 'end_date', 'disease', 'cumul_abs_method')
#Flag the different ways to pull cumulative expenditure. 
# Was it entered in the PUDRs, or do we need to sum old PUDRs? 
check_exp = mapped_data[file_name%in%most_recent_pudrs$file_name, .(budget = sum(cumulative_budget, na.rm=T), 
                                    expenditure=sum(cumulative_expenditure, na.rm=T)), by='file_name']
check_exp = check_exp[expenditure==0, cumul_abs_method:='calculated']
check_exp = check_exp[expenditure!=0, cumul_abs_method:='reported_in_pudr']

check_exp = check_exp[, .(file_name, cumul_abs_method)]
all_absorption = mapped_data[file_name%in%most_recent_pudrs$file_name, .(grant, grant_period, file_name, code, gf_module, gf_intervention, disease,
                                                                         cumulative_budget, cumulative_expenditure, lfa_exp_adjustment, 
                                                                         pudr_semester_financial, cumul_exp_start_date, cumul_exp_end_date)]
all_absorption = merge(all_absorption, check_exp, by='file_name', all.x=T)
setnames(all_absorption, c('cumul_exp_start_date', 'cumul_exp_end_date'), c('start_date', 'end_date'))

# Now, run these two cumul_abs_methods separately. 
# First, reported in PUDR. 
cumulative_absorption1 = all_absorption[cumul_abs_method=="reported_in_pudr",
                                        .(cumulative_budget = sum(cumulative_budget, na.rm=T), 
                                          cumulative_expenditure=sum(cumulative_expenditure, na.rm=T)), 
                                        by=c(byVars)]
# Then, calculate using previous PUDRs. 

# In cases where cumulative expenditure isn't reported, calculate it from the previous PUDRs. 
calculate = unique(all_absorption[cumul_abs_method=="calculated", .(grant, grant_period, pudr_semester_financial, start_date)])
calculate[, concat:=paste0(grant, grant_period)]
cumulative_absorption2 = data.table()
if (nrow(calculate)!=0){
  for (c in unique(calculate$concat)){
    time_series = unique(mapped_data[data_source=="pudr" & file_iteration=='approved_gm' & paste0(grant, grant_period)==c & current_grant==TRUE, .(file_name, pudr_semester_financial, start_date)])
    time_series = time_series[order(start_date, -pudr_semester_financial)] # Want to put "AB" before "A" so year-long is prioritized. 
    
    # Flag duplicate reporting by date and file name, and keep year-long PUDRs over semester-long ones. 
    time_series[, dup_files:=1:.N, by=c('start_date')]
    time_series = time_series[!(dup_files==2 & nchar(pudr_semester_financial)==3)] # Having 3 characters is another way of saying one-semester long PUDR. 
    
    if (verbose){
      print(paste0("These are the files that are being summed together to create a full time series for ", c))
      print(time_series[, .(file_name, pudr_semester_financial, start_date)])
    }
    subset = merge(time_series, mapped_data, by=c('file_name', 'pudr_semester_financial'), all.x=T)
    setnames(subset, c('cumul_exp_start_date', 'cumul_exp_end_date'), c('start_date', 'end_date'))
    subset$cumul_abs_method <- 'calculated'
    subset = subset[, .(cumulative_budget = sum(cumulative_budget, na.rm=T), 
                               cumulative_expenditure=sum(cumulative_expenditure, na.rm=T)), by=c(byVars)]
    cumulative_absorption2 = rbind(cumulative_absorption2, subset)
  } 

  #Collapse the grant-level out after summing by semester. 
  cumulative_absorption2 = cumulative_absorption2[, .(cumulative_budget = sum(cumulative_budget, na.rm=T), 
                                                      cumulative_expenditure=sum(cumulative_expenditure, na.rm=T)), by=c(byVars)]
} 

# ***How can you verify that you've captured the whole time series? 

# Bind the data together 
if ('cumulative_absorption1'%in%ls() & 'cumulative_absorption2'%in%ls()){
  cumulative_absorption = rbind(cumulative_absorption1, cumulative_absorption2)
} else if ('cumulative_absorption1'%in%ls()){
  cumulative_absorption = cumulative_absorption1
} else {
  cumulative_absorption = cumulative_absorption2
}

# Finally, collapse out the calculation method. 
cumulative_absorption = cumulative_absorption[, .(cumulative_budget = sum(cumulative_budget, na.rm=T), 
                                                  cumulative_expenditure=sum(cumulative_expenditure, na.rm=T)), by=c(byVars)]
cumulative_absorption[, cumulative_absorption:=round((cumulative_expenditure/cumulative_budget)*100, 1)]

# Subset columns to GEP and CEP variables. 
cumulative_absorption_gep = cumulative_absorption[, gep_cols, with=FALSE]
cumulative_absorption_cep = cumulative_absorption[, cep_cols, with=FALSE]
#---------------------------------------------------------
# 6. Expenditures - pull out expenditures file, and 
#   generate 'final expenditure' variable. 
# EL 3/5/2020 - these data are not currently being released to partners, just saved internally. 
# These data are used in the health system model, but not needed now (3/20/20)
#---------------------------------------------------------
# expenditures = mapped_data[data_source=="pudr" & file_iteration=='approved_gm']
# expenditures[, final_expenditure:=expenditure+lfa_exp_adjustment]
# expenditures = expenditures[, -c('expenditure', 'lfa_exp_adjustment')]
# setnames(expenditures, c('final_expenditure', 'pudr_semester_financial'), c('expenditure', 'pudr_code'))
# 
# #Flag where you would have overlap in files. 
# dup_files = unique(expenditures[, .(file_name, start_date, grant, grant_period)]) #Everything is at the quarter-level, so do you have the same start date for two different files? 
# dup_files_wide = dcast(dup_files, grant+grant_period~start_date, value.var='file_name', fun.aggregate =length)
# 
# #Find all occurences of 2 or greater in this data table. 
# overlap = data.table() 
# for (col in names(dup_files_wide)[!names(dup_files_wide)%in%c('grant', 'grant_period')]){
#   # If there's a 2 or greater in the column, append that information to 'overlap'. 
#   overlap_subset = dup_files_wide[get(col)>=2, .(grant, grant_period, num_files=get(col))]
#   overlap_subset$start_date = as.Date(col)
#   overlap = rbind(overlap, overlap_subset, fill=T)
# }
# 
# #Flag cases of 3 or more files overlapping. 
# if (nrow(overlap[num_files>2])>0){
#   stop("More than one PUDR will be subtracted for certain grants. Review 'overlap.'")
# }
# 
# #Now that you've done the 'overlap' check, tag overlapping files by file name. 
# # You'll need to subtract at the file-level, and then re-divide to get the quarter-level. 
# overlapping_files = character()
# for (i in 1:nrow(overlap)){
#   files = expenditures[grant==overlap$grant[i] & grant_period==overlap$grant_period[i] & start_date==overlap$start_date[i], unique(file_name)]
#   overlapping_files = c(overlapping_files, files)
# }
# overlapping_files = unique(overlapping_files)
# 
# #-------------------------------------------
# # Emily Linebarger, 2/26/2020 
# # There are two really problematic overlapping PUDRs for Guatemala, for the grant GUA-311-G06-H. One has the PUDR period 3-B:4-A, and 
# # the other has 4-AB:5-A. After looking at the raw data and determining that they only overlap for one quarter, it's reasonable to assume that this is a valid 
# # overlap caused by implementation delays, and that subtracting these two files would lead to more skewed data because they don't overlap fully. 
# 
# # I'm removing these two files from the overlapping list and just summing together the total for the overlapping quarter (2016-10-01). 
# overlapping_files = overlapping_files[!overlapping_files%in%c('GUA-311-G06-H EFR_Form_2016_EFR ENGLISH VERSION.xlsx', 'GUA-311-G06-H_July_Dec17_PU 1_RevALF240518.xlsm')]
# #-------------------------------------------
# exp_overlap = expenditures[file_name%in%overlapping_files]
# exp_no_overlap = expenditures[!file_name%in%overlapping_files]
# 
# #Subtract files that overlap. 
# if (nrow(exp_overlap)>0){
#   
#   #-----------------------------------------
#   # DIRECTIONS: 
#   # Shape wide by file if there is overlap 
#   # Subtract by module and intervention. 
#   # Append onto non-overlapping files. 
#   #-----------------------------------------
#   #First, generate a PUDR order that will determine which file is subtracted from which. 
#   subtract_order = unique(exp_overlap[, .(grant, grant_period, pudr_code)])[order(grant, grant_period, pudr_code)]
#   
#   #Then, make sure you have uniqueness by grant and grant period
#   subtract_order[, pudr_start:=tstrsplit(pudr_code, "-", keep=1)]
#   subtract_order[, check_unique:=.N, by=c('pudr_start', 'grant', 'grant_period')]
#   # You have a simple reporting structure where there are 2 PUDRs reporting for every grant in every year. Remove years that don't actually overlap. 
#   subtract_order = subtract_order[check_unique>=2] 
#   
#   subtract_order = merge(subtract_order, pudr_labels, by=c('pudr_code'), all.x=T)
#   stopifnot(nrow(subtract_order[is.na(pudr_order)])==0)
#   
#   subtract_order = subtract_order[order(grant, grant_period, pudr_code)]
#   subtract_order[, seq:=1:.N, by=c('grant', 'grant_period', 'pudr_start')] #You should only have two files at this point because of the check above. 
#   subtract_order = subtract_order[, .(grant, grant_period, pudr_code, pudr_start, seq)]
#   
#   if (verbose==TRUE){
#     print("Review PUDR subtract order:")
#     print(subtract_order)
#   }
#   
#   #Make sure you only have one observation of each grant/grant_period for the cast wide below. 
#   check = subtract_order[, .(num=.N), by=c('grant', 'grant_period', 'pudr_start')]
#   stopifnot(nrow(check[num>2])==0)
#   
#   #Merge this 'seq' code to exp_overlap to shape wide. 
#   exp_overlap = merge(exp_overlap, subtract_order, by=c('grant', 'grant_period', 'pudr_code'), all=T)
#   stopifnot(nrow(exp_overlap[is.na(seq)])==0)
#   
#   #Cast wide to subtract. 
#   exp_wide = dcast(exp_overlap, grant+grant_period+pudr_start+disease+grant_status+file_iteration+orig_module+orig_intervention+gf_module+gf_intervention+code+loc_name+country+includes_rssh+current_grant~seq, 
#                    value.var=c('budget', 'expenditure', 'disbursement'), fun.aggregate=sum)
#   
#   #exp earlier semesters from later semesters 
#   #First, replace NAs with 0's. 
#   exp_wide[is.na(budget_1), budget_1:=0] #EMILY IS THIS THE BEST WAY TO DO THIS??
#   exp_wide[is.na(budget_2), budget_2:=0]
#   exp_wide[is.na(expenditure_1), expenditure_1:=0]
#   exp_wide[is.na(expenditure_2), expenditure_2:=0]
#   exp_wide[is.na(disbursement_1), disbursement_1:=0]
#   exp_wide[is.na(disbursement_2), disbursement_2:=0]
#   
#   exp_wide[, budget_2_new:=budget_2-budget_1]
#   exp_wide[, expenditure_2_new:=expenditure_2-expenditure_1]
#   exp_wide[, disbursement_2_new:=disbursement_2-disbursement_1]
#   
#   # negatives = exp_wide[expenditure_2_new<0]
#   # if (nrow(negatives)!=0){
#   #   print("There were negative values generated for expenditure. Review 'negative'.")
#   #   write.csv(negatives, paste0(dir, "visualizations/verification/", country, "/", country, "_negative_expenditure.csv"), row.names=FALSE)
#   # }
#   
#   exp_wide = exp_wide[, -c('budget_2', 'expenditure_2', 'disbursement_2')]
#   setnames(exp_wide, c('budget_2_new', 'expenditure_2_new', 'disbursement_2_new'), c('budget_2', 'expenditure_2', 'disbursement_2'))
#   
#   #Cast back long, so you can re-merge dates on. 
#   exp_recast = melt(exp_wide, id.vars=c('grant', 'grant_period', 'pudr_start', 'disease', 'grant_status', 'file_iteration', 'orig_module', 'orig_intervention', 
#                                         'gf_module', 'gf_intervention', 'code', 'loc_name', 'country', 'includes_rssh', 'current_grant'))
#   exp_recast[, seq:=tstrsplit(variable, "_", keep=2)]
#   exp_recast[, variable:=tstrsplit(variable, "_", keep=1)]
#   exp_recast = dcast(exp_recast, grant+grant_period+pudr_start+seq+disease+grant_status+file_iteration+orig_module+orig_intervention+gf_module+gf_intervention+code+loc_name+country+includes_rssh+current_grant~variable, 
#                      value.var='value', fun.aggregate=sum)
#   exp_recast[, seq:=as.integer(seq)]
#   
#   #Correct start dates, and reshape to quarter level. 
#   frame = unique(expenditures[, .(grant, grant_period, start_date, pudr_code)])
#   frame = frame[order(grant, grant_period, start_date, pudr_code)]
#   frame[, dup:=1:.N, by=c('grant', 'grant_period', 'start_date')] #Drop out the second PUDR if they are duplicated. 
#   frame = frame[dup==0]#Tricky step to make sure you don't double-count dates. 
#   
#   frame[, count:=1]
#   frame[, divisor:=sum(count), by=c('grant', 'grant_period', 'pudr_code')] #Create a variable to divide the combined PUDR budget/expenditures into quarters. 
#   
#   if (verbose==TRUE){
#     print("Review date expansion frame.")
#     print(frame)
#   }
#   
#   frame = merge(frame, subtract_order, by=c('grant', 'grant_period', 'pudr_code')) #This should give you the new start dates and denominator to divide financial data into the quarter-level. 
#   frame[, seq:=as.integer(seq)]
#   stopifnot(nrow(frame[divisor!=2])==0) #Everything should be at the semester-level at this point, so you should be dividing each semester into 2 quarters. 
#   
#   exp_recast = merge(exp_recast, frame, all=T, by=c('grant', 'grant_period', 'seq', 'pudr_start'), allow.cartesian=T)
#   
#   #Divide financial columns by 'divisor'. 
#   exp_recast[, budget:=budget/divisor]
#   exp_recast[, expenditure:=expenditure/divisor]
#   exp_recast[, disbursement:=disbursement/divisor]
#   
#   #Drop variables used for calculation, and PUDR code variable. 
#   exp_recast = exp_recast[, -c('count', 'divisor', 'pudr_code', 'seq', 'dup', 'pudr_start')]
#   
# }
# 
# #Collapse exp_no_overlap. 
# exp_no_overlap = exp_no_overlap[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T), disbursement=sum(disbursement, na.rm=T)), 
#                                 by=c('grant', 'grant_period', 'disease', 'start_date', 'grant_status', 
#                                      'file_iteration', 'orig_module', 'orig_intervention', 'gf_module', 'gf_intervention', 
#                                      'code', 'loc_name', 'country', 'includes_rssh', 'current_grant')]
# #Bind together the two types of files. 
# expenditures = rbind(exp_recast, exp_no_overlap)
# 
