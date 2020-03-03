# ----------------------------------------------------------------------------
# Create unique datasets - final budgets, final expenditures, and absorption
# ---------------------------------------------------------------------------

# 1. Create an absorption dataset shaped wide by grant, grant period, module, and intervention, that shows budget/expenditure by semester. 
# 2. For the expenditure dataset, we should subtract the earlier quarter in a year from the later quarters, and then append all of this to create an expenditure dataset. 

#-------------------------------------------
#1. Budgets 
#-------------------------------------------
final_budgets = mapped_data[file_iteration == "final" & data_source == "budget"] #Only want the final versions of budgets. 
final_budgets = final_budgets[, -c('expenditure', 'lfa_exp_adjustment', 'disbursement')]

#---------------------------------------------------------
#2. Expenditures - pull out expenditures file, and 
#   generate 'final expenditure' variable. 
#---------------------------------------------------------
expenditures = mapped_data[data_source=="pudr" & file_iteration=="final"]
expenditures[, final_expenditure:=expenditure+lfa_exp_adjustment]
expenditures = expenditures[, -c('expenditure', 'lfa_exp_adjustment')]
setnames(expenditures, c('final_expenditure', 'pudr_semester_financial'), c('expenditure', 'pudr_code'))

#Flag where you would have overlap in files. 
dup_files = unique(expenditures[, .(file_name, start_date, grant, grant_period)]) #Everything is at the quarter-level, so do you have the same start date for two different files? 
dup_files_wide = dcast(dup_files, grant+grant_period~start_date, value.var='file_name', fun.aggregate =length)

#Find all occurences of 2 or greater in this data table. 
overlap = data.table() 
for (col in names(dup_files_wide)[!names(dup_files_wide)%in%c('grant', 'grant_period')]){
  # If there's a 2 or greater in the column, append that information to 'overlap'. 
  overlap_subset = dup_files_wide[get(col)>=2, .(grant, grant_period, num_files=get(col))]
  overlap_subset$start_date = as.Date(col)
  overlap = rbind(overlap, overlap_subset, fill=T)
}

#Flag cases of 3 or more files overlapping. 
if (nrow(overlap[num_files>2])>0){
  stop("More than one PUDR will be subtracted for certain grants. Review 'overlap.'")
}

#Now that you've done the 'overlap' check, tag overlapping files by file name. 
# You'll need to subtract at the file-level, and then re-divide to get the quarter-level. 
overlapping_files = character()
for (i in 1:nrow(overlap)){
  files = expenditures[grant==overlap$grant[i] & grant_period==overlap$grant_period[i] & start_date==overlap$start_date[i], unique(file_name)]
  overlapping_files = c(overlapping_files, files)
}
overlapping_files = unique(overlapping_files)

#-------------------------------------------
# Emily Linebarger, 2/26/2020 
# There are two really problematic overlapping PUDRs for Guatemala, for the grant GUA-311-G06-H. One has the PUDR period 3-B:4-A, and 
# the other has 4-AB:5-A. After looking at the raw data and determining that they only overlap for one quarter, it's reasonable to assume that this is a valid 
# overlap caused by implementation delays, and that subtracting these two files would lead to more skewed data because they don't overlap fully. 

# I'm removing these two files from the overlapping list and just summing together the total for the overlapping quarter (2016-10-01). 
overlapping_files = overlapping_files[!overlapping_files%in%c('GUA-311-G06-H EFR_Form_2016_EFR ENGLISH VERSION.xlsx', 'GUA-311-G06-H_July_Dec17_PU 1_RevALF240518.xlsm')]
#-------------------------------------------
exp_overlap = expenditures[file_name%in%overlapping_files]
exp_no_overlap = expenditures[!file_name%in%overlapping_files]

#Subtract files that overlap. 
if (nrow(exp_overlap)>0){
  
  #-----------------------------------------
  # DIRECTIONS: 
  # Shape wide by file if there is overlap 
  # Subtract by module and intervention. 
  # Append onto non-overlapping files. 
  #-----------------------------------------
  #First, generate a PUDR order that will determine which file is subtracted from which. 
  subtract_order = unique(exp_overlap[, .(grant, grant_period, pudr_code)])[order(grant, grant_period, pudr_code)]
  
  #Then, make sure you have uniqueness by grant and grant period
  subtract_order[, pudr_start:=tstrsplit(pudr_code, "-", keep=1)]
  subtract_order[, check_unique:=.N, by=c('pudr_start', 'grant', 'grant_period')]
 # You have a simple reporting structure where there are 2 PUDRs reporting for every grant in every year. Remove years that don't actually overlap. 
  subtract_order = subtract_order[check_unique>=2] 

  subtract_order = merge(subtract_order, pudr_labels, by=c('pudr_code'), all.x=T)
  stopifnot(nrow(subtract_order[is.na(pudr_order)])==0)
  
  subtract_order = subtract_order[order(grant, grant_period, pudr_code)]
  subtract_order[, seq:=1:.N, by=c('grant', 'grant_period', 'pudr_start')] #You should only have two files at this point because of the check above. 
  subtract_order = subtract_order[, .(grant, grant_period, pudr_code, pudr_start, seq)]
  
  if (verbose==TRUE){
    print("Review PUDR subtract order:")
    print(subtract_order)
  }
  
  #Make sure you only have one observation of each grant/grant_period for the cast wide below. 
  check = subtract_order[, .(num=.N), by=c('grant', 'grant_period', 'pudr_start')]
  stopifnot(nrow(check[num>2])==0)
  
  #Merge this 'seq' code to exp_overlap to shape wide. 
  exp_overlap = merge(exp_overlap, subtract_order, by=c('grant', 'grant_period', 'pudr_code'), all=T)
  stopifnot(nrow(exp_overlap[is.na(seq)])==0)

  #Cast wide to subtract. 
  exp_wide = dcast(exp_overlap, grant+grant_period+pudr_start+disease+grant_status+file_iteration+orig_module+orig_intervention+gf_module+gf_intervention+code+loc_name+country+includes_rssh+current_grant~seq, 
                       value.var=c('budget', 'expenditure', 'disbursement'), fun.aggregate=sum)
  
  #exp earlier semesters from later semesters 
  #First, replace NAs with 0's. 
  exp_wide[is.na(budget_1), budget_1:=0] #EMILY IS THIS THE BEST WAY TO DO THIS??
  exp_wide[is.na(budget_2), budget_2:=0]
  exp_wide[is.na(expenditure_1), expenditure_1:=0]
  exp_wide[is.na(expenditure_2), expenditure_2:=0]
  exp_wide[is.na(disbursement_1), disbursement_1:=0]
  exp_wide[is.na(disbursement_2), disbursement_2:=0]
  
  exp_wide[, budget_2_new:=budget_2-budget_1]
  exp_wide[, expenditure_2_new:=expenditure_2-expenditure_1]
  exp_wide[, disbursement_2_new:=disbursement_2-disbursement_1]
  
  # negatives = exp_wide[expenditure_2_new<0]
  # if (nrow(negatives)!=0){
  #   print("There were negative values generated for expenditure. Review 'negative'.")
  #   write.csv(negatives, paste0(dir, "visualizations/verification/", country, "/", country, "_negative_expenditure.csv"), row.names=FALSE)
  # }
  
  exp_wide = exp_wide[, -c('budget_2', 'expenditure_2', 'disbursement_2')]
  setnames(exp_wide, c('budget_2_new', 'expenditure_2_new', 'disbursement_2_new'), c('budget_2', 'expenditure_2', 'disbursement_2'))
 
  #Cast back long, so you can re-merge dates on. 
  exp_recast = melt(exp_wide, id.vars=c('grant', 'grant_period', 'pudr_start', 'disease', 'grant_status', 'file_iteration', 'orig_module', 'orig_intervention', 
                                        'gf_module', 'gf_intervention', 'code', 'loc_name', 'country', 'includes_rssh', 'current_grant'))
  exp_recast[, seq:=tstrsplit(variable, "_", keep=2)]
  exp_recast[, variable:=tstrsplit(variable, "_", keep=1)]
  exp_recast = dcast(exp_recast, grant+grant_period+pudr_start+seq+disease+grant_status+file_iteration+orig_module+orig_intervention+gf_module+gf_intervention+code+loc_name+country+includes_rssh+current_grant~variable, 
                     value.var='value', fun.aggregate=sum)
  exp_recast[, seq:=as.integer(seq)]
  
  #Correct start dates, and reshape to quarter level. 
  frame = unique(expenditures[, .(grant, grant_period, start_date, pudr_code)])
  frame = frame[order(grant, grant_period, start_date, pudr_code)]
  frame[, dup:=seq(0, 1, by=1), by=c('grant', 'grant_period', 'start_date')] #Drop out the second PUDR if they are duplicated. 
  frame = frame[dup==0]#Tricky step to make sure you don't double-count dates. 
 
  frame[, count:=1]
  frame[, divisor:=sum(count), by=c('grant', 'grant_period', 'pudr_code')] #Create a variable to divide the combined PUDR budget/expenditures into quarters. 
  
  if (verbose==TRUE){
    print("Review date expansion frame.")
    print(frame)
  }
  
  frame = merge(frame, subtract_order, by=c('grant', 'grant_period', 'pudr_code')) #This should give you the new start dates and denominator to divide financial data into the quarter-level. 
  frame[, seq:=as.integer(seq)]
  stopifnot(nrow(frame[divisor!=2])==0) #Everything should be at the semester-level at this point, so you should be dividing each semester into 2 quarters. 
  
  exp_recast = merge(exp_recast, frame, all=T, by=c('grant', 'grant_period', 'seq', 'pudr_start'), allow.cartesian=T)
  
  #Divide financial columns by 'divisor'. 
  exp_recast[, budget:=budget/divisor]
  exp_recast[, expenditure:=expenditure/divisor]
  exp_recast[, disbursement:=disbursement/divisor]
 
  #Drop variables used for calculation, and PUDR code variable. 
  exp_recast = exp_recast[, -c('count', 'divisor', 'pudr_code', 'seq', 'dup', 'pudr_start')]

}

#Collapse exp_no_overlap. 
exp_no_overlap = exp_no_overlap[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T), disbursement=sum(disbursement, na.rm=T)), 
                                by=c('grant', 'grant_period', 'disease', 'start_date', 'grant_status', 
                                     'file_iteration', 'orig_module', 'orig_intervention', 'gf_module', 'gf_intervention', 
                                     'code', 'loc_name', 'country', 'includes_rssh', 'current_grant')]
#Bind together the two types of files. 
expenditures = rbind(exp_recast, exp_no_overlap)

#-------------------------------------------
#3. Absorption
#-------------------------------------------
absorption = mapped_data[data_source=="pudr" & file_iteration=="final", .(grant, grant_period, code, gf_module, gf_intervention, 
                                                                          budget, expenditure, lfa_exp_adjustment, pudr_semester_financial, start_date,
                                                                          cumulative_budget, cumulative_expenditure)]
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
absorption = absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T), 
                            cumulative_budget=sum(cumulative_budget, na.rm=T), cumulative_expenditure=sum(cumulative_expenditure, na.rm=T)), 
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

#------------------------------------------------------
# 4. Budget revisions
#------------------------------------------------------
revision_flag = unique(mapped_data[file_iteration=='revision' & data_source=="budget", .(grant, grant_period)])
revision_flag[, concat:=paste0(grant, "_", grant_period)]

revisions = mapped_data[paste0(grant, "_", grant_period)%in%revision_flag$concat & data_source=="budget"]
if (nrow(revisions)!=0){ #You won't have budget revisions for every country. 
  #Figure out the order using the 'update_date' variable. 
  order = unique(revisions[, .(grant, grant_period, update_date ,file_name)][order(grant_period, grant, update_date)])
  stopifnot(nrow(order[is.na(update_date)])==0)
  order[, order:=1:.N, by=c('grant', 'grant_period')]
  
  #Reshape this data wide by quarter and year. 
  revisions = merge(revisions, order, by=c('grant', 'grant_period', 'update_date', 'file_name'), all.x=T)
  
  revisions_collapse = revisions[, .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'order', 'year', 'quarter', 'gf_module', 'gf_intervention')]
  revisions_collapse[, quarter:=paste0('q', quarter)]
  revisions_collapse[, order:=paste0('v', order)]
  
  #Cast wide 
  revisions_collapse = dcast(revisions_collapse, grant+grant_period+gf_module+gf_intervention+year+quarter~order, value.var='budget')
  
}

