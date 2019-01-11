# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code from Irena Chen 
# PURPOSE: Binds together resource tracking prepped data 
#   into six key datasets: 
#   1. Final GF budgets
#   2. Final GF expenditures
#   3. GF budget iterations
#   4. Government health expenditure
#   5. Financing global health actuals
#   6. Financing global health estimates. 
#
# These data can then be used to conduct analysis. 
# DATE: Last updated December 2018. 
# ----------------------------------------------


#---------------------------------------
#To do list for this code: 
# - David wants to prioritize GOS over FPM where we have it (through 2017). 
# - Get SICOIN and FGH running 
# - Remove 'fill = TRUE' from each Rbind- data should have same column names. 


#---------------------------------------

library(data.table)
cod_prepped <- "J:/Project/Evaluation/GF/resource_tracking/cod/prepped/"
gtm_prepped <- "J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/"
uga_prepped <- "J:/Project/Evaluation/GF/resource_tracking/uga/prepped/"

final_write <- "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/"


# --------------------------------------------
# Load the prepped GOS data - to be used for both 
# final budgets and final expenditures 
#----------------------------------------------
gos_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_gos_data.csv", 
                                fileEncoding = "latin1"))

##change the dates into date format: 
gos_data$start_date <- as.Date(gos_data$start_date, "%Y-%m-%d")
gos_data$end_date <- as.Date(gos_data$end_date, "%Y-%m-%d")
gos_data[, period:=end_date - start_date]
gos_data$period <- as.integer(gos_data$period)

##since we don't have subnational data for GOS, just make it a copy of the country variable: 
gos_data$adm1 <- gos_data$loc_name
gos_data$adm2 <- gos_data$loc_name

#Make variables numeric 
gos_data$budget <- as.numeric(gos_data$budget)
gos_data$expenditure <- as.numeric(gos_data$expenditure)

#----------------------------------
# 1. FINAL GF BUDGETS 
#----------------------------------
# Merge all 3 countries 
final_budgets_cod <- readRDS(paste0(cod_prepped, "final_budgets.rds"))

final_budgets_gtm <- readRDS(paste0(gtm_prepped, "final_budgets.rds"))

final_budgets_uga = readRDS(paste0(uga_prepped, "final_budgets.rds"))

#-------------------------------------------
# Check for duplicate budget quarters - 
# this should also happen in prep process. 
# ------------------------------------------

setDT(final_budgets_cod)
check_qtr_cod <- final_budgets_cod[, .(start_date, grant_number, fileName)]
check_qtr_cod <- unique(check_qtr_cod) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_cod <- check_qtr_cod[duplicated(check_qtr_cod, by = c("start_date", "grant_number")), ]
stopifnot(nrow(check_qtr_cod)==0)

setDT(final_budgets_gtm)
check_qtr_gtm <- final_budgets_gtm[, .(start_date, grant_number, fileName)]
check_qtr_gtm <- unique(check_qtr_gtm) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_gtm <- check_qtr_gtm[duplicated(check_qtr_gtm, by = c("start_date", "grant_number")), ]
stopifnot(nrow(check_qtr_gtm)==0)

setDT(final_budgets_uga)
check_qtr_uga <- final_budgets_uga[, .(start_date, grant_number, fileName)]
check_qtr_uga <- unique(check_qtr_uga) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_uga <- check_qtr_uga[duplicated(check_qtr_uga, by = c("start_date", "grant_number")), ]
stopifnot(nrow(check_qtr_uga)==0)

#Bind budgets together
final_budgets <- rbind(final_budgets_cod, final_budgets_gtm, final_budgets_uga, fill=TRUE) 
final_budgets$start_date <- as.Date(final_budgets$start_date, "%Y-%m-%d")
final_budgets <- rbind(final_budgets, gos_data, fill = TRUE) 

#Manually edit grant numbers in GOS to match our labeling - EMILY THIS SHOULD BE DONE BACK IN THE PREP CODE. 
final_budgets[grant_number == 'GUA-M-MSPAS', grant_number:='GTM-M-MSPAS']
final_budgets[grant_number == 'GTM-T-UPCOMING', grant_number:='GTM-T-MSPAS']
final_budgets[grant_number == 'GTM-M-UPCOMING', grant_number:='GTM-M-MSPAS']
final_budgets[grant_number == 'UGD-708-G13-H', grant_number:='UGA-708-G13-H']

#Check that all grant numbers in GF budgets correlate to GOS grant numbers
fpm_grants = unique(final_budgets[data_source=='fpm', .(grant_number)])[order(grant_number)]
gos_grants = unique(final_budgets[data_source == 'gos', .(grant_number)])[order(grant_number)]
for(i in 1:nrow(fpm_grants)){
  if (!fpm_grants$grant_number[i] %in% gos_grants$grant_number){
    print("Grant number may not merge correctly: check grant labeling")
    print(paste0(i, " ", fpm_grants$grant_number[i]))
  }
}

#Where we have the same grant information in FPM and GOS, prioritize GOS for complete years. 
gos_grant_list <- unique(gos_data[, .(grant_number, year)])
for(i in 1:nrow(gos_grant_list)){ #Flag duplicate data sources for same grant number and year. 
  if (i == 1){
    gos_prioritized_budgets = final_budgets[!(data_source == 'fpm' & grant_number == as.vector(gos_grant_list$grant_number[i]) & year == as.vector(gos_grant_list$year[i]))]
  } else {
    gos_prioritized_budgets = gos_prioritized_budgets[!(data_source == 'fpm' & grant_number == as.vector(gos_grant_list$grant_number[i]) & year == as.vector(gos_grant_list$year[i]))]
  }
}


# Verify data 
na_year <- gos_prioritized_budgets[is.na(year)]
stopifnot(nrow(na_year)==0)

#Generate variables 
#final_budgets[, end_date:=start_date + period-1]

#Generate a binary variable for current grants. 
final_budgets$current_grant = FALSE 

for (i in 1:length(current_gtm_grants)){
  final_budgets[grant_number==current_gtm_grants[i] & grant_period==current_gtm_grant_period[i], current_grant:=TRUE]
}

for (i in 1:length(current_uga_grants)){
  final_budgets[grant_number==current_uga_grants[i] & grant_period==current_uga_grant_period[i], current_grant:=TRUE]
}

for (i in 1:length(current_cod_grants)){
  final_budgets[grant_number==current_cod_grants[i] & grant_period==current_cod_grant_period[i], current_grant:=TRUE]
}

all_current_grants = unique(final_budgets[current_grant==TRUE, .(grant, grant_period, fileName)])
stopifnot(nrow(all_current_grants)==14)

# Write data 
write.csv(gos_prioritized_budgets, paste0(final_write, "final_budgets.csv"), row.names = FALSE)
saveRDS(gos_prioritized_budgets, paste0(final_write, "final_budgets.rds"))


#----------------------------------
# 2. FINAL GF EXPENDITURES
#----------------------------------
# Merge all 3 countries 
final_expenditures_cod <- readRDS(paste0(cod_prepped, "final_expenditures.rds"))

final_expenditures_gtm <- readRDS(paste0(gtm_prepped, "final_expenditures.rds"))

final_expenditures_uga <- readRDS(paste0(uga_prepped, "final_expenditures.rds"))

#-------------------------------------------
# Hacky fix for duplicate budget quarters -- 
# This should be removed and handled in file prep process!! 
# ------------------------------------------

setDT(final_expenditures_cod)
check_qtr_cod <- final_expenditures_cod[, .(start_date, grant_number, fileName)]
check_qtr_cod <- unique(check_qtr_cod) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_cod <- check_qtr_cod[duplicated(check_qtr_cod, by = c("start_date", "grant_number")), ]
stopifnot(nrow(check_qtr_cod)==0)

setDT(final_expenditures_gtm)
check_qtr_gtm <- final_expenditures_gtm[, .(start_date, grant_number, fileName)]
check_qtr_gtm <- unique(check_qtr_gtm) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_gtm <- check_qtr_gtm[duplicated(check_qtr_gtm, by = c("start_date", "grant_number")), ]
stopifnot(nrow(check_qtr_gtm)==0)

setDT(final_expenditures_uga)
check_qtr_uga <- final_expenditures_uga[, .(start_date, grant_number, fileName)]
check_qtr_uga <- unique(check_qtr_uga) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_uga <- check_qtr_uga[duplicated(check_qtr_uga, by = c("start_date", "grant_number")), ]
stopifnot(nrow(check_qtr_uga)==0)

#Bind expenditures together
final_expenditures <- rbind(final_expenditures_cod, final_expenditures_gtm, final_expenditures_uga, fill = TRUE) 
final_expenditures <- rbind(final_expenditures, gos_data, fill = TRUE) 

#Correct grant labels so they merge correctly #EMILY THIS SHOULD BE DONE BACK IN THE PREP CODE 
final_expenditures[grant_number == 'GUA-M-MSPAS', grant_number:='GTM-M-MSPAS']

#Check that all grant numbers in GF expenditures correlate to GOS grant numbers
pudr_grants = unique(final_expenditures[data_source=='pudr', .(grant_number)])[order(grant_number)]
gos_grants = unique(final_expenditures[data_source == 'gos', .(grant_number)])[order(grant_number)]
for(i in 1:nrow(pudr_grants)){
  if (!pudr_grants$grant_number[i] %in% gos_grants$grant_number){
    print("Grant number may not merge correctly: check grant labeling")
    print(paste0(i, " ", pudr_grants$grant_number[i]))
  }
}

#Where we have the same grant information in FPM and GOS, prioritize GOS for complete years. 
gos_grant_list <- unique(gos_data[, .(grant_number, year)])
for(i in 1:nrow(gos_grant_list)){ #Flag duplicate data sources for same grant number and year. 
  if (i == 1){
    gos_prioritized_expenditures = final_expenditures[!(data_source == 'pudr' & grant_number == as.vector(gos_grant_list$grant_number[i]) & year == as.vector(gos_grant_list$year[i]))]
  } else {
    gos_prioritized_expenditures = gos_prioritized_expenditures[!(data_source == 'pudr' & grant_number == as.vector(gos_grant_list$grant_number[i]) & year == as.vector(gos_grant_list$year[i]))]
  }
}

# Verify data 
na_year <- final_expenditures[is.na(year)]
stopifnot(nrow(na_year)==0)

#Generate variables 
#final_expenditures[, end_date:=start_date + period-1]

# Write data 
write.csv(final_expenditures, paste0(final_write, "final_expenditures.csv"), row.names = FALSE)
saveRDS(final_expenditures, paste0(final_write, "final_expenditures.rds"))

#----------------------------------
# 3. GF FILE ITERATIONS
#----------------------------------
all_gf_cod <- readRDS("J:/Project/Evaluation/GF/resource_tracking/cod/prepped/budget_pudr_iterations.rds")
all_gf_uga <- readRDS("J:/Project/Evaluation/GF/resource_tracking/uga/prepped/budget_iterations.rds")  
all_gf_gtm <- readRDS("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/budget_iterations.rds")

all_gf_files <- rbind(all_gf_cod, all_gf_uga, all_gf_gtm, fill = TRUE)

#Write data 
write.csv(all_gf_files, paste0(final_write, "budget_pudr_iterations.csv"), row.names = FALSE)
saveRDS(all_gf_files, paste0(final_write, "budget_pudr_iterations.rds"))


#----------------------------------
# 4. GOVERNMENT HEALTH EXPENDITURE
#----------------------------------
sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                   ,fileEncoding="latin1"))

##change the start dates from factors to dates: 
sicoin_data$start_date <- as.Date(sicoin_data$start_date,"%Y-%m-%d")
totalGtm$start_date <- as.Date(totalGtm$start_date,"%Y-%m-%d")
sicoin_data$grant_period = year(sicoin_data$start_date)

#----------------------------------
# 5. FGH ACTUALS 
#----------------------------------
# fgh_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_prepped_fgh_total.csv", 
#                                 fileEncoding = "latin1"))
# 
# ##change the dates into date format: 
# fgh_data$start_date <- as.Date(fgh_data$start_date, "%Y-%m-%d")
# fgh_data$end_date <- as.Date(fgh_data$end_date, "%Y-%m-%d")
# 
# #remove "total" value
# fgh_data = fgh_data[!grepl("total", module)]
# fgh_data$grant_period = year(fgh_data$start_date)



#----------------------------------
# 6. FGH ESTIMATES
#----------------------------------

                             
# --------------------------------------------
#Check for duplicates (the GOS data has one row that is duplicated twice, but it's fine to just aggregate them)
# --------------------------------------------
# dups<-totalData[duplicated(totalData) | duplicated(totalData, fromLast=TRUE)]
# 
# ##aggregate duplicates: 
# byVars = names(totalData)[!names(totalData)%in%c('budget', 'disbursement', 'expenditure')]
# totalData= totalData[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement)),expenditure=sum(na.omit(expenditure))), by=byVars]
# 
# # --------------------------------------------
# ##export to correct folder: 
# # --------------------------------------------
# ## date variables can get messed up if we export them, so change them to 'character'
# totalData$start_date <- as.character(totalData$start_date)
# totalData$end_date <- as.character(totalData$end_date)
# 
# totalData$loc_name = ifelse(totalData$loc_name  == "Uganda", "uga", ifelse(totalData$loc_name == "Guatemala", "gtm", ifelse(totalData$loc_name == 'Congo (Democratic Republic)', "cod", tolower(totalData$loc_name))))
# 
# totalData$budget <- as.numeric(totalData$budget)
# totalData$expenditure <- as.numeric(totalData$expenditure)
# totalData$disbursement <- as.numeric(totalData$disbursement)
# 
# totalData$year <- as.Date(totalData$year, "%Y") #To check- where are we getting NA in year?? 
# 
# write.csv(totalData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv", row.names = FALSE)
# 
# # --------------------------------------------
# # This produces a dataset that prioritizes FPM data (drops GOS/PUDR is it overlap)
# # --------------------------------------------
# 
# ##pudrs overlap with the FPM budgets - drop this so we don't double count 
# fpmGtm <- totalGtm[!(data_source=="pudr")] ##all of the PUDRs we have correspond to available FPM 
# fpmUga <- totalUga[!(data_source=="pudr"&year>2015)] ##we have a lot of recent PUDRs that overlap with FPM 
# fpmCod <-  totalCod[!(data_source=="pudr")] #all of the PUDRs we have correspond to available FPM 
# 
# cleanData <- rbind(fpmGtm, fpmUga, fpmCod)
# cleanData[,end_date:=start_date+period-1]
# 
# ## some of the FPM data is missing so we'll fill it in w/ the GOS data 
# 
# gos_cod<- gos_data[country=="Congo (Democratic Republic)"]
# gos_uga <- gos_data[country=="Uganda"]
# gos_gtm <- gos_data[country=="Guatemala"]
# 
# ##as we get more FPM data, make sure to check that these years/diseases are still true: 
# ## (realistically, we're probably not going to get any historical data - pre 2015) 
# gos_cod <- gos_cod[(disease=="hss")|(disease=="hiv"&year<2012|year%in%c(2013, 2014))|(disease=="malaria"&(year<=2014))|(disease=="tb"&grant_number!="COD-T-MOH")]
# gos_uga <- gos_uga[(disease=="hiv"&(year<2011|year==2014))|(disease=="malaria"&(year%in%c(2013,2014)|year<2012))|(disease=="tb"&(year<2012|year%in%c(2013,2014)))]
# gos_gtm <- gos_gtm[(disease=="hiv"&year<2011)|(disease=="malaria"&year<2011)|(disease=="tb"&(year<2011|year==2015))]
# 
# 
# totalGos <- rbind(gos_uga, gos_cod, gos_gtm)
# 
# cleanData <- rbind(cleanData, totalGos, fill = TRUE)
# 
# ## add in a field that distinguishes between actual numbers and forecasted numbers (FGH)
# cleanData$fin_data_type <- "actual"
# 
# # --------------------------------------------
# #  Since FGH data is not at grant level (or disease level), we might as well include it in this data
# ##since there is no danger of double-counting when summing by disease/grant
# # --------------------------------------------
# cleanData <- rbind(cleanData, fgh_data, fill = TRUE)
# 
# 
# ## date variables can get messed up if we export them, so change them to 'character'
# cleanData$start_date <- as.character(cleanData$start_date)
# cleanData$end_date <- as.character(cleanData$end_date)
# 
# # --------------------------------------------
# # check for duplicates again: 
# # --------------------------------------------
# dups<-cleanData[duplicated(cleanData) | duplicated(cleanData , fromLast=TRUE)]
# 
# byVars = names(cleanData )[!names(cleanData )%in%c('budget', 'disbursement', 'expenditure')]
# cleanData = cleanData[, list(budget=sum(na.omit(budget)), disbursement=sum(na.omit(disbursement)),expenditure=sum(na.omit(expenditure))), by=byVars]
# 
# # --------------------------------------------
# ##export to correct folder: 
# # --------------------------------------------
# 
# 
# write.csv(cleanData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/cleaned_total_data.csv", row.names = FALSE)

