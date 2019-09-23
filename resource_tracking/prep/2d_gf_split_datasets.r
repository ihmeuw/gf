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

#If you find duplicate semesters, subtract them, and then reassemble the dataset. 
if(nrow(overlap)>0){
  for (i in 1:nrow(overlap)){
    grant_i = overlap$grant[i]
    grant_period_i = overlap$grant_period[i]
    start_date_i = overlap$start_date[i]
    expenditures[grant==grant_i & grant_period==grant_period_i, overlap:=TRUE] #Just tag on grant and grant period so that the whole file will be captured, and PUDR semesters can be relabeled correctly. 
    #The actual subtraction will happen using start_date variable. EL 9/20/2019
  }
  expenditures[is.na(overlap), overlap:=FALSE]
}

#Separate out overlapping and non-overlapping files. 
exp_overlap = expenditures[overlap==TRUE]
exp_no_overlap = expenditures[overlap==FALSE]

#If you have overlapping dates, pull them out and subtract them. 
if (nrow(exp_overlap)>0){
  #First, pull out overlapping dates from this list of overlapping grants. 
  overlap2 = unique(exp_overlap[, .(grant, grant_period, start_date, pudr_code)])
  overlap2$instance = 1
  overlap2_wide = dcast(overlap2, grant+grant_period+start_date~instance, value.var='instance', fun.aggregate=length)
  setnames(overlap2_wide, '1', 'instance')
  
  #Subset to only the cases that have 2 or more files reporting. 
  overlap2_wide = overlap2_wide[instance>=2]
  
  for (i in 1:nrow(overlap2_wide)){
    exp_overlap[grant==overlap2_wide$grant[i] & grant_period==overlap2_wide$grant_period[i] & start_date==overlap2_wide$start_date[i], 
                overlap2:=TRUE]
  }
  exp_overlap[is.na(overlap2), overlap2:=FALSE]
  
  #Pull out subtract and no subtract, and then bind them back together. 
  no_subtract = exp_overlap[overlap2==FALSE, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T), disbursement=sum(disbursement, na.rm=T)), 
                                              by=c('grant', 'grant_period', 'disease', 'start_date', 'grant_status', 'file_iteration', 'orig_module', 'orig_intervention', 'gf_module', 'gf_intervention', 
                                                   'code', 'loc_name', 'country', 'includes_rssh', 'current_grant')]
  subtract = exp_overlap[overlap2==TRUE, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T), disbursement=sum(disbursement, na.rm=T)), 
                         by=c('grant', 'grant_period', 'disease', 'start_date', 'grant_status', 'file_iteration', 'orig_module', 'orig_intervention', 'gf_module', 'gf_intervention', 
                              'code', 'loc_name', 'country', 'includes_rssh', 'current_grant', 'pudr_code')]
  setDT(pudr_labels)
  subtract = merge(subtract, pudr_labels[, .(pudr_code, semester_code, pudr_order)], by=c('pudr_code'))
  subtract = subtract[order(grant, grant_period, pudr_order)]
  
  #Make a 'labels' dataset to make sure the right semesters are getting subtracted. 
  seq_labels = unique(subtract[, .(grant, grant_period, pudr_order, pudr_code)])
  seq_labels[, pudr_seq:=seq(1, 5, by=1), by=c('grant', 'grant_period')]
  if (nrow(seq_labels[pudr_seq>2])>0) stop("More than one PUDR is being subtracted for some grants!")
  
  subtract = merge(subtract, seq_labels, by=c('grant', 'grant_period', 'pudr_order', 'pudr_code'), all.x=T)
  
  #Cast wide to subtract. 
  subtract_wide = dcast(subtract, grant+grant_period+start_date+disease+grant_status+file_iteration+orig_module+orig_intervention+gf_module+gf_intervention+code+loc_name+country+includes_rssh+current_grant~pudr_seq, 
                       value.var=c('budget', 'expenditure', 'disbursement'))
  
  #Subtract earlier semesters from later semesters 
  #First, replace NAs with 0's. 
  subtract_wide[is.na(budget_1), budget_1:=0] #EMILY IS THIS THE BEST WAY TO DO THIS??
  subtract_wide[is.na(budget_2), budget_2:=0]
  subtract_wide[is.na(expenditure_1), expenditure_1:=0]
  subtract_wide[is.na(expenditure_2), expenditure_2:=0]
  subtract_wide[is.na(disbursement_1), disbursement_1:=0]
  subtract_wide[is.na(disbursement_2), disbursement_2:=0]
  
  subtract_wide[, budget_2_new:=budget_2-budget_1]
  subtract_wide[, expenditure_2_new:=expenditure_2-expenditure_1]
  subtract_wide[, disbursement_2_new:=disbursement_2-disbursement_1]
  
  # negatives = subtract_wide[expenditure_2_new<0]
  # if (nrow(negatives)!=0){
  #   print("There were negative values generated for expenditure. Review 'negative'.")
  #   write.csv(negatives, paste0(dir, "visualizations/verification/", country, "/", country, "_negative_expenditure.csv"), row.names=FALSE)
  # }
  
  subtract_wide = subtract_wide[, -c('budget_2', 'expenditure_2', 'disbursement_2', 'budget_1', 'expenditure_1', 'disbursement_1')]
  setnames(subtract_wide, c('budget_2_new', 'expenditure_2_new', 'disbursement_2_new'), c('budget', 'expenditure', 'disbursement'))
  
  exp_overlap = rbind(subtract_wide, no_subtract)

}


#Collapse exp_no_overlap. 
exp_no_overlap = exp_no_overlap[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T), disbursement=sum(disbursement, na.rm=T)), 
                                by=c('grant', 'grant_period', 'disease', 'start_date', 'grant_status', 'file_iteration', 'orig_module', 'orig_intervention', 'gf_module', 'gf_intervention', 
                                     'code', 'loc_name', 'country', 'includes_rssh', 'current_grant')]
#Bind together the two types of files. 
expenditures = rbind(exp_overlap, exp_no_overlap)

#Add on semester variable 

#-------------------------------------------
#3. Absorption
#-------------------------------------------
absorption = mapped_data[data_source=="pudr" & file_iteration=="final", .(grant, grant_period, code, gf_module, gf_intervention, budget, expenditure, lfa_exp_adjustment, pudr_semester_financial, start_date)]
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
  order[, order:=seq(0, 10, by=1), by=c('grant', 'grant_period')]
  
  #Reshape this data wide by quarter and year. 
  revisions = merge(revisions, order, by=c('grant', 'grant_period', 'update_date', 'file_name'), all.x=T)
  
  revisions_collapse = revisions[, .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'order', 'year', 'quarter', 'gf_module', 'gf_intervention')]
  revisions_collapse[, quarter:=paste0('q', quarter)]
  revisions_collapse[, order:=paste0('v', order)]
  
  #Cast wide 
  revisions_collapse = dcast(revisions_collapse, grant+grant_period+gf_module+gf_intervention~year+quarter+order, value.var='budget')
  
}

