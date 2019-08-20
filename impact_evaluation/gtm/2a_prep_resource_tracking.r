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
  fgh = fgh[, -c(3)] #There's an extra disease column??? 
  
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
  other_dah = fgh[(channel_agg != 'The Global Fund' & channel_agg != 'ghe') & loc_name=="GTM", 
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

  #-----------------------------------------------------------------------------
  # EL -  DECISION WAS MADE TO DRASTICALLY REDUCE NUMBER OF INPUT VARIABLES 
  # FEEDING INTO MODEL ON 8/13/19. 
  
  # ONLY WANT GF TB, GF HIV/TB, AND GF MDR-TB, AND GENERAL TB OTHER DAH AND GHE. 
  #------------------------------------------------------------------------------

  # Expenditures - code that start with T1 are general TB, T2 is HIV/TB, and T3 is MDR-TB. 
  expenditures[, short_code:=substr(code, 1, 2)]
  unique(expenditures$short_code)
  expenditures[short_code=="T1", gf_tb:=sum(expenditure, na.rm=T), by=c('short_code', 'date')]
  expenditures[short_code=="T2", gf_tbhiv:=sum(expenditure, na.rm=T), by=c('short_code', 'date')]
  expenditures[short_code=="T3", gf_mdrtb:=sum(expenditure, na.rm=T), by=c('short_code', 'date')]
  
  exp_wide = expenditures[, .(date, gf_tb, gf_tbhiv, gf_mdrtb)]
  exp_wide = expenditures[, .(gf_tb=sum(gf_tb, na.rm=T), gf_tbhiv=sum(gf_tbhiv, na.rm=T), gf_mdrtb=sum(gf_mdrtb, na.rm=T)), by='date']
  
  # GHE - just want one variable for all TB spending. 
  ghe_wide = sicoin[disease=="tb", .(ghe_tb=sum(expenditure, na.rm=T)), by=c('date')]
  
  # Other DAH - just want one variable for all TB spending. 
  odah_wide = other_dah[disease=="tb", .(odah_tb=sum(other_dah, na.rm=T)), by=c('date')]

  #------------------------------------
# Shape data wide and merge.  
#------------------------------------
rt_wide <- merge(odah_wide, exp_wide, by=c('date'), all=T)
rt_wide <- merge(rt_wide, ghe_wide, by=c('date'), all=T)

#---------------------------------------
# Transformations 
#---------------------------------------

#Restrict all of the financial data to the same starting year so cumulative sums will be comparable. 
# For SICOIN, the earliest is 2004. DP 7.12.19
#rt_wide = rt_wide[date>=2004.0]

#Restrict data to 2009 onwards - this makes the most theoretical sense given that the majority of variables have started by that time. Jen Ross, implemented by Emily Linebarger, 8.12.19 
rt_wide = rt_wide[date>=START_YEAR]

# # compute lags - all financial variables should be lagged 6 months. DP 7.12.19
rt_wide[, date:=date+0.5]

#Replace NAs with 0's at this point unless we hear differently from Guillermo. 
# cols = names(rt_wide)[!names(rt_wide)=='date']
# for (c in cols) {
#   rt_wide[is.na(get(c)), (c):=0]
# }

#------------------------------------
# Validate data 
#------------------------------------
# test to see if there are any zero-variance variables. 
# Just look at national-level at first, and then we might check by department later. DP 7.12.19. 
# test = rt_wide[,lapply(.SD,var)]==0
# test = data.table(test)
# test[, var:='all']
# test = melt(test, id.vars='var', value.name='bool')
# 
# if(any(test$bool)) {
#   print(test[bool==TRUE, .(variable)])
#   warning("Some modules have zero variance. These will be dropped from the model.")
# }
# 
# drop_cols = test[bool==TRUE, .(variable)]
# keep_cols = names(rt_wide)[!names(rt_wide)%in%drop_cols$variable]
# #Drop these variables from the model, and modify in model object code.
# rt_wide = rt_wide[, keep_cols, with=F]

#Save output file
saveRDS(rt_wide, outputFile2a)
print("Step 2a: Prep resource tracking completed successfully.")