#-----------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Validate FGH data in resource tracking database. 
#   The data sources to validate are: 
#   1. Other DAH actuals for all diseases
#   2. GHE Actuals for malaria
#   3. GHE Actuals for HIV
#   4. GHE Estimates for all diseases (?) 
#-----------------------------------------------------------


#-----------------------------------------------
# OTHER DAH ACTUALS 
#-----------------------------------------------
other_dah = readRDS(paste0(fgh_prepped, "other_dah_actuals_all.rds"))

#Check for disbected values and NAs - booleans are set up so that they will return TRUE when an error is found in the data.
any_na = (TRUE%in%is.na(other_dah))
if (any_na){
  print(which(is.na(other_dah)))
  print("There are NA values in dataset; review prep code.")
}

year_range = !(1990==min(other_dah$year) & 2018==max(other_dah$year))
if (year_range){
  print(range(other_dah$year))
  print("Years are falling outside disbected values.")
}

#Check totals by year using the raw data vs. the prepped data. 
#Pick 3 different activity descriptions from our final subset, and calculate their totals from the raw data as well by year. 
raw_dah = readRDS(paste0(fgh_raw, "FGH_EZ_2018.rds"))

#1. hiv_prev_dah_18
raw1 = raw_dah[, .(raw_disb = round(sum(hiv_prev_dah_18, na.rm=T))), by='year']
prepped1 = other_dah[activity_description=="hiv_prev_dah_18", .(prepped_disb=round(sum(disbursement, na.rm=T))), by='year']
check1 = merge(raw1, prepped1, by='year', all=T)

#2. mal_treat_dah_18
raw2 = raw_dah[, .(raw_disb = round(sum(mal_treat_dah_18, na.rm=T))), by='year']
prepped2 = other_dah[activity_description=="mal_treat_dah_18", .(prepped_disb=round(sum(disbursement, na.rm=T))), by='year']
check2 = merge(raw2, prepped2, by='year', all=T)

#3. tb_diag_dah_18 - this is one of the observations with a coefficient less than 1 in the map. 
raw3 = raw_dah[, .(raw_disb = round(sum(tb_diag_dah_18, na.rm=T))), by='year']
prepped3 = other_dah[activity_description=="tb_diag_dah_18", .(prepped_disb=round(sum(disbursement, na.rm=T))), by='year']
check3 = merge(raw3, prepped3, by='year', all=T)

if (nrow(check1[raw_disb!=prepped_disb])!=0){
  print(check1[raw_disb!=prepped_disb])
  stop("Unit test #1 failing for Other DAH data - review.")
} else if (nrow(check2[raw_disb!=prepped_disb])!=0){
  print(check2[raw_disb!=prepped_disb])
  stop("Unit test #2 failing for Other DAH data - review.")
} else if (nrow(check3[raw_disb!=prepped_disb])!=0){
  print(check3[raw_disb!=prepped_disb])
  stop("Unit test #3 failing for Other DAH data - review.")
} else {
  print("All unit tests passed for Other DAH.")
}

print("Completed step 3C: Validate FGH.")
