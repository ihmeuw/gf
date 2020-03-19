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
  expenditures[, quarter:=((quarter(start_date)/4)-0.25)]
  expenditures[, date:=year+quarter]
  
  #Collapse, and fix names
  expenditures = expenditures[loc_name=='gtm', .(expenditure=sum(expenditure), na.rm=T), by=c("date", "gf_module", "gf_intervention", "code", "disease", "grant_disease")] #Will subset by disease in code section below. 
  setnames(expenditures, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))
  
  #----------------------------------
  # FGH DATA (OTHER_DAH)
  #----------------------------------
  fgh <- readRDS(fghFile)
  fgh = fgh[, -c(3)] #There's an extra disease column??? 
  
  #Expand to semester-level, and divide all inputs by 2. 
  semesters = data.table(date=seq(1990.0, 2019.0, by=0.25))
  semesters[, year:=floor(date)]
  
  rows_before = nrow(fgh)
  sum_before = sum(fgh$disbursement, na.rm=T)
  fgh = merge(fgh, semesters, by='year', allow.cartesian=T)
  fgh[, disbursement:=disbursement/4]
  #Check that this expansion worked 
  stopifnot(nrow(fgh)==rows_before*4)
  stopifnot(sum(fgh$disbursement, na.rm=T)==sum_before)
  
  #Pull out other DAH, and collapse to the semester-level. 
  setnames(fgh, old=c('gf_module', 'gf_intervention'), new=c('module', 'intervention'))
  other_dah = fgh[(channel_agg != 'The Global Fund' & channel_agg != 'ghe') & loc_name=="GTM", 
                  .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(date, module, intervention, code, disease)]
  
  #---------------------------------
  #SICOIN DATA 
  #----------------------------------
  sicoin = readRDS(sicoinFile) 
  depts = read_excel("J:/Project/Evaluation/GF/resource_tracking/_ghe/sicoin_gtm/raw_data/departments_and_municipalities_of_gtm.xlsx")
  
  #Collapse into semesters - SICOIN data is monthly.  
  sicoin[, year:=year(start_date)]
  sicoin[, quarter:=((quarter(start_date)/4)-0.25)]
  sicoin[, date:=year+quarter]
  
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
  exp_wide = expenditures[, .(gf_tb=sum(gf_tb, na.rm=T), gf_tbhiv=sum(gf_tbhiv, na.rm=T),
                              gf_mdrtb=sum(gf_mdrtb, na.rm=T)), by='date']
  
  #---------------------------------------------------------------
  #Decision 8/20/19 Jen Ross - add in RSSH spent each year as a proportion of GF spending on sub-disease (tb, tb/hiv, and mdr-tb) to each of these variables. 
  #Decision 10/2/2019 David Phillips and Jen Ross - want to code RSSH in a more sophisticated way, as an interaction term. 
  # Need to comment this section out. 
  gf_proportions = exp_wide[, .(gf_tb=gf_tb/(gf_tb+gf_tbhiv+gf_mdrtb), gf_tbhiv=gf_tbhiv/(gf_tb+gf_tbhiv+gf_mdrtb), gf_mdrtb=gf_mdrtb/(gf_tb+gf_tbhiv+gf_mdrtb)), by='date']
  gf_proportions[, total:=(gf_tb+gf_tbhiv+gf_mdrtb)]
  #For cases where total isn't one, all spending is NA.
  gf_proportions[is.na(total), c('gf_tb', 'gf_tbhiv', 'gf_mdrtb'):=1/3]

  exp_rssh_wide = expenditures[disease=='rssh' & grant_disease%in%c('hiv/tb', 'tb'), .(rssh=sum(expenditure, na.rm=T)), by='date']
  exp_rssh_wide = merge(exp_rssh_wide, gf_proportions, by='date', all=T)

  #Multiply rssh by each proportion respectively to get the total RSSH you should add to that variable.
  exp_rssh_wide[, gf_tb_rssh:=rssh*gf_tb]
  exp_rssh_wide[, gf_tbhiv_rssh:=rssh*gf_tbhiv]
  exp_rssh_wide[, gf_mdrtb_rssh:=rssh*gf_mdrtb]
  for (c in names(exp_rssh_wide)){
    exp_rssh_wide[is.na(get(c)), (c):=0]
  }

  #Merge this new rssh expenditure data back onto original GF data and add.
  exp_wide = merge(exp_wide, exp_rssh_wide[, .(date, gf_tb_rssh, gf_tbhiv_rssh, gf_mdrtb_rssh)], by='date', all=T)
  exp_wide[, gf_tb:=gf_tb+gf_tb_rssh]
  exp_wide[, gf_mdrtb:=gf_mdrtb+gf_mdrtb_rssh]
  exp_wide[, gf_tbhiv:=gf_tbhiv+gf_tbhiv_rssh]
  #--------------------------------------------------------
  exp_wide = exp_wide[, .(date, gf_tb, gf_tbhiv, gf_mdrtb)]
  
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
#EL 10/2/2019 - all RSSH variables should be lagged a total of one year, so lag this variable another 6 months. 
#EL 10/28/2019 - in model presented at September TERG, we didn't have RSSH lags. 
# rssh = rt_wide[, .(date, gf_rssh)]
# rssh[, date:=date+0.5]
# rt_wide = rt_wide[, -c('gf_rssh')] #Drop this variable, and re-merge it in after it's lagged. 

# rt_wide = merge(rt_wide, rssh, by='date', all=T)
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
saveRDS(rt_wide, paste0("J:/Project/Evaluation/GF/impact_evaluation/gtm/prepped_data/archive/", Sys.Date(), "prepped_resource_tracking.rds"))
print("Step 2a: Prep resource tracking completed successfully.")