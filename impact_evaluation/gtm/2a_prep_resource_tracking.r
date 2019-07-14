# ---------------------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Prepare resource tracking data for merge 
#          with activities and outputs.  
# DATE: Last updated May 2019. 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

#------------------------------------
# TO-DO: 
#Have you pulled out all the possible codes from the FGH data? 
# Are you using the most recent file for FGH? 
#How are we going to handle missing 2018 SICOIN data?
# Is SICOIN data only GHE? 

#------------------------------------

#---------------------------------------------
#Read in datasets, and format as needed. 
# Split data into semesters! 
#--------------------------------------------- 

  
  #----------------------------------
  #GLOBAL FUND EXPENDITURES AND GOS 
  #----------------------------------
  expenditures = readRDS(expendituresFile)
  
  #Create date variable (collapsed to semester level)
  expenditures[, year:=year(start_date)]
  expenditures[, quarter:=quarter(start_date)]
  expenditures$semester = NULL
  expenditures[quarter%in%c(1, 2), semester:=0.0]
  expenditures[quarter%in%c(3, 4), semester:=0.5]
  expenditures[, date:=year+semester]
  
  #Collapse, and fix names
  expenditures = expenditures[loc_name=='gtm', .(expenditure=sum(expenditure)), by=c("date", "gf_module", "gf_intervention", "code", "disease")] #Will subset by disease in code section below. 
  setnames(expenditures, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))
  
  #----------------------------------
  # FGH DATA (OTHER_DAH)
  #----------------------------------
  fgh <- readRDS(fghFile)
  
  #Expand to semester-level, and divide all inputs by 2. 
  semesters = data.table(date=seq(1990.0, 2018.5, by=0.5))
  semesters[, year:=floor(date)]
  
  rows_before = nrow(fgh)
  sum_before = sum(fgh$disbursement, na.rm=T)
  fgh = merge(fgh, semesters, by='year', allow.cartesian=T)
  fgh[, disbursement:=disbursement/2]
  #Check that this expansion worked 
  stopifnot(nrow(fgh)==rows_before*2)
  stopifnot(sum(fgh$disbursement, na.rm=T)==sum_before)
  
  #Pull out other DAH, and collapse to the semester-level. 
  setnames(fgh, old=c('gf_module', 'gf_intervention'), new=c('module', 'intervention'))
  other_dah = fgh[(financing_source != 'The Global Fund' & financing_source != 'ghe') & loc_name=="GTM", 
                  .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(date, module, intervention, code, disease)]
  
  #---------------------------------
  #SICOIN DATA 
  #----------------------------------
  sicoin = readRDS(sicoinFile) 
  
  #Collapse into semesters - SICOIN data is monthly.  
  sicoin[, year:=year(start_date)]
  sicoin[, quarter:=quarter(start_date)]
  sicoin[quarter%in%c(1, 2), semester:=0.0]
  sicoin[quarter%in%c(3, 4), semester:=0.5]
  sicoin[, date:=year+semester]
  
  sicoin = sicoin[, .(expenditure=sum(expenditure, na.rm=T)), by=c('date', 'gf_module', 
                                                                   'gf_intervention', 'activity', 'code', 'disease')]


#-------------------------------------
# Format indicator map file 
#-------------------------------------
names(indicatorMap) = tolower(names(indicatorMap))
indicatorMap = indicatorMap[, .(indicator, type, code)]
  
#------------------------------------
# Select relevant codes. 
#------------------------------------
{
  #Subset data into each bundle of codes, and then recombine. 
  codes_needed = unique(indicatorMap$code)
  codes_needed = codes_needed[codes_needed!="H11_ALL"] #Only capture HIV/TB all under the TB code, T2_ALL. 

    #---------------------------------------------
    # GF Expenditures - can we make this a loop? 
    #---------------------------------------------
    #Make classification categories in data - first by unique codes...
    expenditures[code=="T1_1", T1_1:=TRUE]
    expenditures[code=="T1_2", T1_2:=TRUE]
    expenditures[code=="T1_5", T1_5:=TRUE]
    expenditures[code=="T1_6", T1_6:=TRUE]
    expenditures[code=="T1_7", T1_7:=TRUE]
    expenditures[code=="T2_1", T2_1:=TRUE]
    expenditures[code=="T3_1", T3_1:=TRUE]
    expenditures[code=="T3_2", T3_2:=TRUE]
    
    #And then for combination categories. 
    expenditures[substr(code, 1, 2)=='R1', R1_ALL:=TRUE]
    expenditures[substr(code, 1, 2)=='R2', R2_ALL:=TRUE]
    expenditures[substr(code, 1, 2)=='T2' | substr(code, 1, 2)=="H11", T2_ALL:=TRUE]
    expenditures[substr(code, 1, 2)=='T3', T3_ALL:=TRUE]
    expenditures[disease=='tb', TB_ALL:=TRUE]
    expenditures[disease%in%c('tb', 'hiv', 'hiv/tb'), HIV_TB_ALL:=TRUE]
    
    codes_generated = names(expenditures)[!names(expenditures)%in%c('date', 'module', 'intervention', 'code', 'disease', 'loc_name', 'expenditure')]
    if (length(setdiff(codes_needed, codes_generated))!=0) stop("Missing some codes needed for the model!") 
    
    #Review what codes each logic condition is matching to visually. 
    cols = c(4, 9:ncol(expenditures))
    unique(expenditures[, cols, with=FALSE][order(code)])
    
    #Make summary data tables for each of these codes, and then merge them together. 
    exp_wide = data.table(date = integer())
    for (col in codes_generated){
      assign(col, expenditures[get(col)==TRUE, .(exp=sum(expenditure, na.rm=T)),
                               by=c('date')])
      setnames(get(col), 'exp', paste0('exp_', col))
      exp_wide = merge(exp_wide, get(col), by=c('date'), all=T)
    }
    
    #---------------------------------------------
    # Other DAH 
    #---------------------------------------------
    #Make classification categories in data - first by unique codes...
    other_dah[code=="T1_1", T1_1:=TRUE]
    other_dah[code=="T1_2", T1_2:=TRUE]
    other_dah[code=="T1_5", T1_5:=TRUE]
    other_dah[code=="T1_6", T1_6:=TRUE]
    other_dah[code=="T1_7", T1_7:=TRUE]
    other_dah[code=="T2_1", T2_1:=TRUE]
    other_dah[code=="T3_1", T3_1:=TRUE]
    other_dah[code=="T3_2", T3_2:=TRUE]
    
    #And then for combination categories. 
    other_dah[substr(code, 1, 2)=='R1', R1_ALL:=TRUE]
    other_dah[substr(code, 1, 2)=='R2', R2_ALL:=TRUE]
    other_dah[substr(code, 1, 2)=='T2' | substr(code, 1, 2)=="H11", T2_ALL:=TRUE]
    other_dah[substr(code, 1, 2)=='T3', T3_ALL:=TRUE]
    other_dah[disease=='tb', TB_ALL:=TRUE]
    other_dah[disease%in%c('tb', 'hiv', 'hiv/tb'), HIV_TB_ALL:=TRUE]
    
    codes_generated = names(other_dah)[!names(other_dah)%in%c('date', 'module', 'intervention', 'code', 'disease', 'loc_name', 'sda_activity', 'other_dah')]
    if (length(setdiff(codes_needed, codes_generated))!=0) stop("Missing some codes needed for the model!") 
    
    #Review what codes each logic condition is matching to visually. 
    cols = c(4, 10:ncol(other_dah))
    unique(other_dah[, cols, with=FALSE][order(code)])
    
    #Make summary data tables for each of these codes, and then merge them together. 
    dah_wide = data.table(date = integer())
    for (col in codes_generated){
      assign(col, other_dah[get(col)==TRUE, .(other_dah=sum(other_dah, na.rm=T)),
                               by=c('date')])
      setnames(get(col), 'other_dah', paste0('other_dah_', col))
      dah_wide = merge(dah_wide, get(col), by=c('date'), all=T)
    }
    
    #---------------------------------------------
    # SICOIN (GHE)
    #---------------------------------------------
    #Make classification categories in data - first by unique codes...
    sicoin[code=="T1_1", T1_1:=TRUE]
    sicoin[code=="T1_2", T1_2:=TRUE]
    sicoin[code=="T1_5", T1_5:=TRUE]
    sicoin[code=="T1_6", T1_6:=TRUE]
    sicoin[code=="T1_7", T1_7:=TRUE]
    sicoin[code=="T2_1", T2_1:=TRUE]
    sicoin[code=="T3_1", T3_1:=TRUE]
    sicoin[code=="T3_2", T3_2:=TRUE]
    
    #And then for combination categories. 
    sicoin[substr(code, 1, 2)=='R1', R1_ALL:=TRUE]
    sicoin[substr(code, 1, 2)=='R2', R2_ALL:=TRUE]
    sicoin[substr(code, 1, 2)=='T2' | substr(code, 1, 2)=="H11", T2_ALL:=TRUE]
    sicoin[substr(code, 1, 2)=='T3', T3_ALL:=TRUE]
    sicoin[disease=='tb', TB_ALL:=TRUE]
    sicoin[disease%in%c('tb', 'hiv', 'hiv/tb'), HIV_TB_ALL:=TRUE]
    
    codes_generated = names(sicoin)[!names(sicoin)%in%c('date', 'gf_module', 'gf_intervention', 'activity', 'expenditure', 'code', 'disease')]
    if (length(setdiff(codes_needed, codes_generated))!=0) stop("Missing some codes needed for the model!") 
    
    #Review what codes each logic condition is matching to visually. 
    cols = c(5, 10:ncol(sicoin))
    unique(sicoin[, cols, with=FALSE][order(code)])
    
    #Make summary data tables for each of these codes, and then merge them together. 
    ghe_wide = data.table(date = integer())
    for (col in codes_generated){
      assign(col, sicoin[get(col)==TRUE, .(ghe=sum(expenditure, na.rm=T)),
                            by=c('date')])
      setnames(get(col), 'ghe', paste0('ghe_', col))
      ghe_wide = merge(ghe_wide, get(col), by=c('date'), all=T)
    }
  
}



#------------------------------------
# Shape data wide and merge.  
#------------------------------------
rt_wide <- merge(dah_wide, exp_wide, by=c('date'), all=T)
rt_wide <- merge(rt_wide, ghe_wide, by=c('date'), all=T)

#Add on GHE and OOP as control variables 
# Add on OOP. 

#---------------------------------------
# Transformations 
#---------------------------------------

#Restrict all of the financial data to the same starting year so cumulative sums will be comparable. 
# For SICOIN, the earliest is 2004. DP 7.12.19
rt_wide = rt_wide[date>=2004.0]

# # compute lags - all financial variables should be lagged 6 months. DP 7.12.19
rt_wide[, date:=date+0.5]

#Replace NAs with 0's at this point unless we hear differently from Guillermo. 
cols = names(rt_wide)[!names(rt_wide)=='date']
for (c in cols) {
  rt_wide[is.na(get(c)), (c):=0]
}

#------------------------------------
# Validate data 
#------------------------------------
# test to see if there are any zero-variance variables. 
# Just look at national-level at first, and then we might check by department later. DP 7.12.19. 
test = rt_wide[,lapply(.SD,var)]==0
test = data.table(test)
test[, var:='all']
test = melt(test, id.vars='var', value.name='bool')

if(any(test$bool)) { 
  print(test[bool==TRUE, .(variable)])
  warning("Some modules have zero variance. These will be dropped from the model.")
}

drop_cols = test[bool==TRUE, .(variable)]
keep_cols = names(rt_wide)[!names(rt_wide)%in%drop_cols$variable]
#Drop these variables from the model, and modify in model object code. 
rt_wide = rt_wide[, keep_cols, with=F]

#Save output file
saveRDS(rt_wide, outputFile2a)
print("Step 2a: Prep resource tracking completed successfully.")