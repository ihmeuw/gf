# ---------------------------------------------------------
# AUTHOR: Audrey Batzel and Emily Linebarger
# PURPOSE: Prepare resource tracking data for merge 
#          with activities and outputs.  
# DATE: Last updated May 2019. 
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

#------------------------------------
# TO-DO: 


#------------------------------------

#---------------------------------------------
#Read in datasets, and format as needed. 
#--------------------------------------------- 
#Used to split other_dah and ghe into quarters. 
n_years <- (2018-1990)+1 #This is the range we have data for. 
quarters <- data.table(year=rep(1990:2018, 4))[order(year)]
quarters[, quarter:=rep(1:4, n_years)]
  
  #----------------------------------
  #GLOBAL FUND EXPENDITURES AND GOS 
  #----------------------------------
  expenditures = readRDS(expendituresFile)
  expenditures = expenditures[loc_name=='gtm' & (grant_disease%in%c('tb', 'hiv', 'hiv/tb') | disease=='rssh'), #Jen - making sure we want to pull all RSSH? 
                              .(expenditure, start_date, code, loc_name, disease, gf_module, gf_intervention)]
  setnames(expenditures, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))
  
  #----------------------------------
  # FGH DATA (OTHER_DAH)
  #----------------------------------
  fgh <- readRDS(fghFile)
  fgh = fgh[loc_name=="GTM" & disease%in%c('tb', 'hss'), .(sda_activity, year, loc_name, disease, code, module_eng, intervention_eng, fin_data_type, financing_source, disbursement)]
  setnames(fgh, old=c('module_eng', 'intervention_eng'), new=c('module', 'intervention'))
  
  #Pull out other DAH. 
  other_dah = fgh[fin_data_type == 'actual' & (financing_source != 'The Global Fund' & financing_source != 'ghe') , 
                  .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(sda_activity, year, loc_name, disease, code, module, intervention)]
  
  #Split into quarters 
  other_dah = merge(quarters, other_dah, by='year', all.x = TRUE, allow.cartesian=TRUE)
  other_dah[, other_dah:=other_dah/4]
  
  #----------------------------------
  # WHO EXPENDITURE DATA 
  #----------------------------------
  who <- readRDS(whoFile) #We might not end up using this for Guatemala. 
  
  #---------------------------------
  #SICOIN DATA 
  #----------------------------------
  



#------------------------------------
# Validate data 
#------------------------------------



#------------------------------------
# Select relevant codes. 
#------------------------------------

  #-------------------------------------
  # Format indicator map file 
  #-------------------------------------
  names(indicatorMap) = tolower(names(indicatorMap))
  indicatorMap = indicatorMap[, .(indicator, type, code)]
  
  #emily start here 5/14/19
  

#Match exp and FGH data to codes to only keep relevant modules/interventions (all = TRUE)
map_codes = indicatorMap[, .(code, indicator, indicator_type)]

  
  exp_subset = merge(exp_subset, drc_mal_map_codes, by=c('code'), allow.cartesian = TRUE)
other_dah = merge(other_dah, drc_mal_map_codes, by=c('code'), allow.cartesian = TRUE)

print(paste0("Codes kept in exp data: ", unique(exp_subset[, .(code)])))
print(paste0("Codes kept in FGH data: ", unique(other_dah[, .(code)])))

#Create date variable
exp_subset[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
exp_subset[, date:=year+quarter]
other_dah[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
other_dah[, date:=year+quarter]

#Cast data wide 
exp_wide = dcast(exp_subset, date~code, value.var=c('expenditure'), fun.aggregate = sum)
frame = data.table(date=seq(1990, 2020, by=.25))
exp_wide = merge(frame, exp_wide, by='date', all.x=TRUE)
for(v in names(exp_wide)) exp_wide[is.na(get(v)), (v):=0]

other_dah_wide = dcast(other_dah, date~code, value.var=c('other_dah'), fun.aggregate = sum)
frame = data.table(date=seq(1990, 2020, by=.25))
other_dah_wide = merge(frame, other_dah_wide, by='date', all.x=TRUE)
for(v in names(other_dah_wide)) other_dah_wide[is.na(get(v)), (v):=0]

#Set column names for merge 
names = colnames(exp_wide[, 2:ncol(exp_wide)])
names <- paste("exp", names, sep = "_")
colnames(exp_wide) <- c('date', names)

names = colnames(other_dah_wide[, 2:ncol(other_dah_wide)])
names <- paste("other_dah", names, sep = "_")
colnames(other_dah_wide) <- c('date', names)

#Merge both files together 
rt_wide <- merge(other_dah_wide, exp_wide, by=c('date'))

#Add on GHE and OOP as control variables 
ghe = ghe[, .(date, ghe)]
rt_wide = merge(rt_wide, ghe, by='date', all.x = TRUE)

#Save output file
saveRDS(rt_wide, outputFile2a)
