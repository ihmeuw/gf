# ----------------------------------------------------------------------------
# Create unique datasets - final budgets, final expenditures, and absorption
# ---------------------------------------------------------------------------

# 1. Create an absorption dataset shaped wide by grant, grant period, module, and intervention, that shows budget/expenditure by semester. 
# 2. For the expenditure dataset, we should subtract the earlier quarter in a year from the later quarters, and then append all of this to create an expenditure dataset. 


#-------------------------------------------
#1. Budgets 
#-------------------------------------------
  final_budgets = mapped_data[file_iteration == "final" & data_source == "fpm"] #Only want the final versions of budgets. 
  final_budgets = final_budgets[, -c('expenditure', 'lfa_exp_adjustment', 'disbursement')]

#---------------------------------------------------------
#2. Expenditures - pull out expenditures file, and 
#   generate 'final expenditure' variable. 
#---------------------------------------------------------
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
  dup_files[, dup:=seq(0, nrow(dup_files)), by=c('grant', 'grant_period', 'pudr_grant_year')]
  dup_files = unique(dup_files[dup!=0, .(grant, grant_period, pudr_grant_year)]) #But this isn't the only condition on whether you have overlap. Now, check if the same files for the same grant overlap in time. 
  
  #First, see if dup==1. This tells you that you have duplicate files you need to compare. 
  #Tag these dup files in the expenditures dataset 
  for (i in 1:nrow(dup_files)){
    expenditures[grant==dup_files$grant[i] & grant_period==dup_files$grant_period[i] & pudr_grant_year==dup_files$pudr_grant_year[i],
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
      write.csv(negatives, paste0(dir, "visualizations/verification/", country, "/", country, "_negative_expenditure.csv"), row.names=FALSE)
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
    
    #Reshape this data back to the quarter-level. 
    date_frame = expand.grid(pudr_grant_year = unique(exp_recast$pudr_grant_year), semester = unique(exp_recast$semester), quarter=c(1, 2))
    setDT(date_frame)
    date_frame[semester=="B", quarter:=quarter+2]
    
    #Expand the data to the quarter-level (should double the # of rows)
    nrows0 = nrow(exp_recast)
    exp_recast = merge(exp_recast, date_frame, by=c('pudr_grant_year', 'semester'), allow.cartesian=T)
    nrows1 = nrow(exp_recast)
    stopifnot(nrows0*2==nrows1)
    
    #Divide budget and expenditure variables 
    exp_recast[, budget:=budget/2]
    exp_recast[, expenditure:=expenditure/2]
    exp_recast[, disbursement:=disbursement/2]
    
    #Generate new start date variable. 
    exp_recast[quarter==1, month:="01"]
    exp_recast[quarter==2, month:="04"]
    exp_recast[quarter==3, month:="07"]
    exp_recast[quarter==4, month:="10"]
    
    exp_recast[, year:=year(start_date)]
    
    exp_recast[, start_date:=paste0(month, "-01-", year)]
    exp_recast[, start_date:=as.Date(start_date, "%m-%d-%Y")]
    exp_recast[, month:=NULL]
    exp_recast[, quarter:=NULL]
    exp_recast[, year:=NULL]
    
    #Merge back onto expenditure data that DIDN'T need subtraction 
    exp_no_subtract = expenditures[overlap==FALSE, .(budget=sum(budget, na.rm=TRUE), expenditure=sum(expenditure, na.rm=TRUE), disbursement=sum(disbursement, na.rm=TRUE)), 
                                   by=c('grant', 'grant_period', 'code', 'pudr_grant_year', 'semester_code', 'start_date')]
    setnames(exp_no_subtract, 'semester_code', 'semester')
    
    #Append datasets
    expenditures = rbind(exp_recast, exp_no_subtract) 
    
    #Create date variables 
    expenditures[, end_date:=(start_date %m+% months(3))-1]
    
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
    expenditures[, end_date:=(start_date %m+% months(3))-1]
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
#-------------------------------------------
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
  
#------------------------------------------------------
# 4. Budget revisions
#------------------------------------------------------
  revision_flag = unique(mapped_data[file_iteration=='revision' & data_source=="fpm", .(grant, grant_period)])
  revision_flag[, concat:=paste0(grant, "_", grant_period)]
  
  revisions = mapped_data[paste0(grant, "_", grant_period)%in%revision_flag$concat & data_source=="fpm"]
  if (nrow(revisions)!=0){ #You won't have budget revisions for every country. 
    #Figure out the order using the 'update_date' variable. 
    order = unique(revisions[, .(grant, grant_period, update_date ,file_name)][order(grant_period, grant, update_date)])
    stopifnot(nrow(order[is.na(update_date)])==0)
    order[, order:=seq(0, 10, by=1), by=c('grant', 'grant_period')]
    
    #Reshape this data wide by quarter and year. 
    revisions = merge(revisions, order, by=c('grant', 'grant_period', 'update_date', 'file_name'), all.x=T)
    
    revisions_collapse = revisions[, .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'order', 'year', 'quarter')]
    revisions_collapse[, quarter:=paste0('q', quarter)]
    revisions_collapse[, order:=paste0('v', order)]
    
    #Cast wide 
    revisions_collapse = dcast(revisions_collapse, grant+grant_period~year+quarter+order, value.var='budget')
    
    #Generate year totals (?) 
  }
  
