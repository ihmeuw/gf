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
# DATE: Last updated February 2019.  
# ----------------------------------------------


#---------------------------------------
#To do list for this code: 
# - Get SICOIN and FGH running 
# - Remove 'fill = TRUE' from each Rbind- data should have same column names. 


#---------------------------------------

cod_prepped <- paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/cod/prepped_data/")
gtm_prepped <- paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gtm/prepped_data/")
uga_prepped <- paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/uga/prepped_data/")

final_write <- paste0(j, "/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/")


# --------------------------------------------
# Load the prepped GOS data - to be used for both 
# final budgets and final expenditures 
#----------------------------------------------
gos_data <- readRDS(paste0(gos_prepped, "prepped_gos_data.rds"))
gos_data[loc_name]
            
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
check_qtr_cod <- final_budgets_cod[, .(start_date, grant, file_name)]
check_qtr_cod <- unique(check_qtr_cod) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_cod <- check_qtr_cod[duplicated(check_qtr_cod, by = c("start_date", "grant")), ]
stopifnot(nrow(check_qtr_cod)==0)

setDT(final_budgets_gtm)
check_qtr_gtm <- final_budgets_gtm[, .(start_date, grant, file_name)]
check_qtr_gtm <- unique(check_qtr_gtm) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_gtm <- check_qtr_gtm[duplicated(check_qtr_gtm, by = c("start_date", "grant")), ]
stopifnot(nrow(check_qtr_gtm)==0)

setDT(final_budgets_uga)
check_qtr_uga <- final_budgets_uga[, .(start_date, grant, file_name)]
check_qtr_uga <- unique(check_qtr_uga) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_uga <- check_qtr_uga[duplicated(check_qtr_uga, by = c("start_date", "grant")), ]
stopifnot(nrow(check_qtr_uga)==0)

#Bind budgets together
final_budgets <- rbind(final_budgets_cod, final_budgets_gtm, final_budgets_uga, fill=TRUE) 
final_budgets$start_date <- as.Date(final_budgets$start_date, "%Y-%m-%d")

#Manually edit grant numbers in GOS to match our labeling - EMILY THIS SHOULD BE DONE BACK IN THE PREP CODE. 
final_budgets[grant == 'GTM-T-UPCOMING', grant:='GTM-T-MSPAS']
final_budgets[grant == 'GTM-M-UPCOMING', grant:='GTM-M-MSPAS']
final_budgets[grant == 'UGD-708-G13-H', grant:='UGA-708-G13-H']

#Keep only GOS through 2016, and only final budgets after. (Keep all GOS for as long as we have it)
gos_data = gos_data[year <=gos_year]
final_budgets = final_budgets[year>gos_year]
final_budgets <- rbind(final_budgets, gos_data, fill = TRUE) 

# Verify data 
na_year <- gos_prioritized_budgets[is.na(year)]
stopifnot(nrow(na_year)==0)

#Check that you've got the current grants right. 
all_current_grants = unique(gos_prioritized_budgets[current_grant==TRUE, .(grant, grant_period, file_name)])
expected_current_grants <- length(current_gtm_grants) + length(current_uga_grants) + length(current_cod_grants)
stopifnot(nrow(all_current_grants)==expected_current_grants)

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
check_qtr_cod <- final_expenditures_cod[, .(start_date, grant, file_name)]
check_qtr_cod <- unique(check_qtr_cod) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_cod <- check_qtr_cod[duplicated(check_qtr_cod, by = c("start_date", "grant")), ]
stopifnot(nrow(check_qtr_cod)==0)

setDT(final_expenditures_gtm)
check_qtr_gtm <- final_expenditures_gtm[, .(start_date, grant, file_name)]
check_qtr_gtm <- unique(check_qtr_gtm) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_gtm <- check_qtr_gtm[duplicated(check_qtr_gtm, by = c("start_date", "grant")), ]
stopifnot(nrow(check_qtr_gtm)==0)

setDT(final_expenditures_uga)
check_qtr_uga <- final_expenditures_uga[, .(start_date, grant, file_name)]
check_qtr_uga <- unique(check_qtr_uga) #Remove duplicate lines in module, intervention, etc. 
#Make sure there are no duplicates in start date and grant number that are coming from different files. 
check_qtr_uga <- check_qtr_uga[duplicated(check_qtr_uga, by = c("start_date", "grant")), ]
stopifnot(nrow(check_qtr_uga)==0)

#Bind expenditures together
final_expenditures <- rbind(final_expenditures_cod, final_expenditures_gtm, final_expenditures_uga, fill = TRUE) 

#Keep only GOS through 2016, and only final expenditures after. (Keep all GOS for as long as we have it)
gos_data = gos_data[year <=2016]
final_expenditures = final_expenditures[year>2016]
final_expenditures <- rbind(final_expenditures, gos_data, fill = TRUE) 
gos_prioritized_expenditures <- rbind(final_expenditures, gos_data, fill = TRUE) 

# Verify data 
na_year <- gos_prioritized_expenditures[is.na(year)]
stopifnot(nrow(na_year)==0)

#Check that you've got the current grants right. 
all_current_grants = unique(gos_prioritized_expenditures[current_grant==TRUE, .(grant, grant_period, file_name)])
expected_current_grants <- length(current_gtm_grants) + length(current_uga_grants) + length(current_cod_grants)
stopifnot(nrow(all_current_grants)==expected_current_grants)

# Write data 
write.csv(gos_prioritized_expenditures, paste0(final_write, "final_expenditures.csv"), row.names = FALSE)
saveRDS(gos_prioritized_expenditures, paste0(final_write, "final_expenditures.rds"))

#----------------------------------
# 3. GF FILE ITERATIONS
#----------------------------------
all_gf_cod <- readRDS(paste0(j, "/Project/Evaluation/GF/resource_tracking/cod/prepped/budget_pudr_iterations.rds"))
all_gf_uga <- readRDS(paste0(j, "/Project/Evaluation/GF/resource_tracking/uga/prepped/budget_pudr_iterations.rds"))  
all_gf_gtm <- readRDS(paste0(j, "/Project/Evaluation/GF/resource_tracking/gtm/prepped/budget_pudr_iterations.rds"))

all_files = list(all_gf_cod, all_gf_gtm, all_gf_uga)
all_gf_files <- rbindlist(all_files, use.names = TRUE, fill = TRUE)

#Write data 
saveRDS(all_gf_files, paste0(final_write, "budget_pudr_iterations.rds"))
write.csv(all_gf_files, paste0(final_write, "budget_pudr_iterations.csv"), row.names = FALSE)

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
# gos_cod <- gos_cod[(disease=="hss")|(disease=="hiv"&year<2012|year%in%c(2013, 2014))|(disease=="malaria"&(year<=2014))|(disease=="tb"&grant!="COD-T-MOH")]
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

