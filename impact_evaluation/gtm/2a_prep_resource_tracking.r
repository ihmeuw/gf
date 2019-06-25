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
#--------------------------------------------- 
#Used to split other_dah and ghe into quarters. 
n_years <- (2018-1990)+1 #This is the range we have data for. 
quarters <- data.table(year=rep(1990:2018, 4))[order(year)]
  
  #----------------------------------
  #GLOBAL FUND EXPENDITURES AND GOS 
  #----------------------------------
  expenditures = readRDS(expendituresFile)
  expenditures[, year:=year(start_date)]
  expenditures = expenditures[loc_name=='gtm', .(year, gf_module, gf_intervention, code, disease, loc_name, expenditure)] #Will subset by disease in code section below. 
  setnames(expenditures, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))
  
  #----------------------------------
  # FGH DATA (OTHER_DAH)
  #----------------------------------
  fgh <- readRDS(fghFile)
  fgh = fgh[loc_name=="GTM", .(activity_description, year, loc_name, disease, code, gf_module, gf_intervention, financing_source, disbursement)]
  setnames(fgh, old=c('gf_module', 'gf_intervention'), new=c('module', 'intervention'))
  
  #Pull out other DAH. 
  other_dah = fgh[(financing_source != 'The Global Fund' & financing_source != 'ghe') , 
                  .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(year, module, intervention, code, disease, loc_name)]

  
  #----------------------------------
  # WHO EXPENDITURE DATA 
  #----------------------------------
  # who <- readRDS(whoFile) #We might not end up using this for Guatemala. 
  
  #---------------------------------
  #SICOIN DATA 
  #----------------------------------
  sicoin = readRDS(sicoinFile) #EMILY ARE YOU SURE THIS IS ALREADY SUBSET INTO GHE??? 
  
  #Collapse into years - SICOIN data is monthly. 
  sicoin[, year:=year(start_date)]
  
  sicoin = sicoin[, .(expenditure=sum(expenditure, na.rm=T)), by=c('year', 'gf_module', 
                                                                   'gf_intervention', 'activity', 'code', 'disease')]

#------------------------------------
# Validate data 
#------------------------------------
#??? Anything here? 

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
    expenditures[substr(code, 1, 2)=='T2', T2_ALL:=TRUE]
    expenditures[substr(code, 1, 2)=='T3', T3_ALL:=TRUE]
    expenditures[disease=='tb', TB_ALL:=TRUE]
    expenditures[substr(code, 1, 3)=='H11', H11_ALL:=TRUE]
    expenditures[disease%in%c('tb', 'hiv', 'hiv/tb'), HIV_TB_ALL:=TRUE]
    
    codes_generated = names(expenditures)[!names(expenditures)%in%c('year', 'module', 'intervention', 'code', 'disease', 'loc_name', 'expenditure')]
    if (length(setdiff(codes_needed, codes_generated))!=0) stop("Missing some codes needed for the model!") 
    
    #Review what codes each logic condition is matching to visually. 
    cols = c(5, 9:ncol(expenditures))
    unique(expenditures[, cols, with=FALSE][order(code)])
    
    #Make summary data tables for each of these codes, and then merge them together. 
    exp_wide = data.table(year = integer())
    for (col in codes_generated){
      assign(col, expenditures[get(col)==TRUE, .(exp=sum(expenditure, na.rm=T)),
                               by=c('year')])
      setnames(get(col), 'exp', paste0('exp_', col))
      exp_wide = merge(exp_wide, get(col), by=c('year'), all=T)
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
    other_dah[substr(code, 1, 2)=='T2', T2_ALL:=TRUE]
    other_dah[substr(code, 1, 2)=='T3', T3_ALL:=TRUE]
    other_dah[disease=='tb', TB_ALL:=TRUE]
    other_dah[substr(code, 1, 3)=='H11', H11_ALL:=TRUE]
    other_dah[disease%in%c('tb', 'hiv', 'hiv/tb'), HIV_TB_ALL:=TRUE]
    
    codes_generated = names(other_dah)[!names(other_dah)%in%c('year', 'module', 'intervention', 'code', 'disease', 'loc_name', 'sda_activity', 'other_dah')]
    if (length(setdiff(codes_needed, codes_generated))!=0) stop("Missing some codes needed for the model!") 
    
    #Review what codes each logic condition is matching to visually. 
    cols = c(5, 10:ncol(other_dah))
    unique(other_dah[, cols, with=FALSE][order(code)])
    
    #Make summary data tables for each of these codes, and then merge them together. 
    dah_wide = data.table(year = integer())
    for (col in codes_generated){
      assign(col, other_dah[get(col)==TRUE, .(other_dah=sum(other_dah, na.rm=T)),
                               by=c('year')])
      setnames(get(col), 'other_dah', paste0('other_dah_', col))
      dah_wide = merge(dah_wide, get(col), by=c('year'), all=T)
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
    sicoin[substr(code, 1, 2)=='T2', T2_ALL:=TRUE]
    sicoin[substr(code, 1, 2)=='T3', T3_ALL:=TRUE]
    sicoin[disease=='tb', TB_ALL:=TRUE]
    sicoin[substr(code, 1, 3)=='H11', H11_ALL:=TRUE]
    sicoin[disease%in%c('tb', 'hiv', 'hiv/tb'), HIV_TB_ALL:=TRUE]
    
    codes_generated = names(sicoin)[!names(sicoin)%in%c('year', 'gf_module', 'gf_intervention', 'activity', 'expenditure', 'code', 'disease')]
    if (length(setdiff(codes_needed, codes_generated))!=0) stop("Missing some codes needed for the model!") 
    
    #Review what codes each logic condition is matching to visually. 
    cols = c(6, 10:ncol(sicoin))
    unique(sicoin[, cols, with=FALSE][order(code)])
    
    #Make summary data tables for each of these codes, and then merge them together. 
    ghe_wide = data.table(year = integer())
    for (col in codes_generated){
      assign(col, sicoin[get(col)==TRUE, .(ghe=sum(expenditure, na.rm=T)),
                            by=c('year')])
      setnames(get(col), 'ghe', paste0('ghe_', col))
      ghe_wide = merge(ghe_wide, get(col), by=c('year'), all=T)
    }
  
}



#------------------------------------
# Shape data wide and merge.  
#------------------------------------
exp_wide$quarter <- NULL
dah_wide$quarter <- NULL
ghe_wide$quarter <- NULL

rt_wide <- merge(dah_wide, exp_wide, by=c('date'), all=T)
rt_wide <- merge(rt_wide, ghe_wide, by=c('date'), all=T)

#Add on GHE and OOP as control variables 
# Add on OOP. 

#Save output file
saveRDS(rt_wide, outputFile2a)
