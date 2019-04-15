#--------------------------------------------------------------
# PURPOSE: Explore GOS Data, and check for inconsistencies. 
# AUTHOR: Emily Linebarger 
# DATE: March 2019. 
# -------------------------------------------------------------
rm(list=ls())

# ----------------------------------------------
# SET UP R
# ----------------------------------------------

user = "elineb" #Change to your username 
code_dir = ifelse(Sys.info()[1]=='Windows', paste0("C:/Users/", user, "/Documents/gf/"), paste0('/homes/', user, '/gf/'))
source(paste0(code_dir, "resource_tracking/prep/_common/set_up_r.R"), encoding="UTF-8")

gos_data <- readRDS(paste0(gos_prepped, "prepped_gos_data.rds"))

raw_gos_mf  <- data.table(read.xlsx(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
                                   sheet=as.character('GOS Mod-Interv - Extract'), detectDates=TRUE))
raw_gos_sdas  <- data.table(read.xlsx(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
                                   sheet=as.character('GMS SDAs - extract'), detectDates=TRUE))
new_gos = read.xlsx(paste0(gos_raw, "By_Cost_Category_data .xlsx"), detectDates=TRUE)
modular_framework = fread(paste0(mapping_dir, "all_interventions.csv"))

#--------------------------------------------------------
# CHECK FOR MISSING DATA 
#--------------------------------------------------------
#Wherever there is a grant quarter in the final budgets that doesn't exist in GOS, take that whole grant for the grant 
# period and replace the GOS with the final budgets data. 
gos_data[, start_date:=as.Date(start_date)]
gos_data[, end_date:=as.Date(end_date)]

#Find out what quarters we have GOS data for. 
gos_timeframe = unique(gos_data[, .(grant, start_date, end_date, grant_period)])

gos_timeframe[, grant_start:=min(start_date), by='grant']
gos_timeframe[, grant_end:=max(end_date), by='grant']

#Shift all of the end-year variables up one click, and see if they correspond to another start date. 
gos_timeframe[, end_date:=end_date+days(1)]
gost_timeframe = gos_timeframe[order(grant, start_date)]
setDT(gos_timeframe)
grants=as.vector(unique(gos_timeframe[!is.na(grant), .(grant)]))

grants_with_gaps=character()
for (x in 1:nrow(grants)){
  test = gos_timeframe[grant%in%grants[x]][order(start_date)]
  if (nrow(test)!=1){ 
    for (i in 2:nrow(test)-1){
      if (test$end_date[i]!=test$start_date[i+1]){
        print(paste0("Warning: there are missing dates for ", grants[x]))
        gos_timeframe[grant==grants[x] & end_date==test$end_date[i], data_gap:=TRUE]
        grants_with_gaps = append(grants_with_gaps, as.character(grants[x]))
      }
    }
    if (test$end_date[nrow(test)]-1 != test$grant_end[nrow(test)]){
      print(paste0("Warning: there are missing dates for ", grants[x]))
      gos_timeframe[grant==grants[x] & end_date==test$end_date[i], data_gap:=TRUE]
      grants_with_gaps = append(grants_with_gaps, as.character(grants[x]))
    }
  }
}

#Need to grab the rows with data gaps, and the one immediately after them. 
gos_gaps = gos_timeframe[grant%in%grants_with_gaps, .(grant, start_date, end_date, grant_period, data_gap)]
gos_gaps[is.na(data_gap), data_gap:=FALSE]
gos_gaps1 = data.table()
for (i in 1:nrow(gos_gaps)){
  print(i)
  temp = gos_gaps[i]
  temp[data_gap==TRUE, keep:=TRUE]
}

gos_gaps = gos_gaps[order(grant, start_date)]
write.csv(gos_gaps, "J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gos/known_gos_gaps.csv", row.names=FALSE)

#---------------------------------------------------
# CHECK FOR OVERLAPPING REPORTING PERIODS
#---------------------------------------------------

#---------------------------------------------------
# REVIEW DATES 
#---------------------------------------------------

#Are the quarters standardized? Where are we missing months? 
unique(gos_data[, .(month = month(start_date))][order(month)])
unique(gos_data[, .(month = month(end_date))][order(month)])

#Where are we missing dates? 
gos_data[is.na(start_date) | is.na(end_date)]

raw_gos_sdas[is.na(`Financial Reporting Period End Date`)]
raw_gos_sdas[is.na(`Financial Reporting Period Start Date`)]

#---------------------------------------------------
# REVIEW MAPPING
#---------------------------------------------------
unique(raw_gos_mf$Module) #Prevention programs for MSM and TGs is not a standard module, and we only have 23 observations out of the total 28 possible.  
unique(modular_framework$module_eng)

unique(raw_gos_mf$Intervention) #Some of these don't fit into the modular framework either (MSM and TGs). Need to review. 
unique(modular_framework$intervention_eng)

#---------------------------------------------------
# COMPARE DIFFERENCES BETWEEN OLD GOS AND NEW FOR 2015-2017
#---------------------------------------------------
# Pull over prep code from prep_gos 
## reset column names
new_gos_15_17 = copy(new_gos)
setDT(new_gos_15_17)
old_gos_15_17 = copy(raw_gos_mf)
setDT(old_gos_15_17)

{
  #Are there differences in totals by grant, start_date, and end_date when summing the two files? 
  ## reset column names
  oldNames <- names(new_gos_15_17)
  newNames <- gsub("\\.", "_", oldNames)
  newNames = tolower(newNames)
  
  setnames(new_gos_15_17, oldNames, newNames)
  
  setnames(new_gos_15_17, old=c('calendar_year', 'component_name', 'intervention_name', 'module_name', 'expenditure_startdate', 'expenditure_enddate', 'ip_name'),
           new = c('year', 'disease', 'intervention', 'module', 'start_date', 'end_date', 'grant'))
  
  #Only keep the countries we care about 
  new_gos_15_17 = new_gos_15_17[country%in%c("Congo (Democratic Republic)", "Guatemala", "Uganda", "Senegal")]
  
  #Standardize disease column 
  new_gos_15_17[, disease:=tolower(disease)]
  new_gos_15_17[disease == "hiv/aids", disease:="hiv"]
  new_gos_15_17[disease == "tuberculosis", disease:="tb"]
  
  #Only keep expenditure aggregation category 'intervention'
  new_gos_15_17[expenditure_aggregation_type=='Intervention']
  
  #Drop columns before reshape
  new_gos_15_17 = new_gos_15_17[, -c("cost_category", "implementing_entity", "expenditure_aggregation_type")]
  
  #Standardize 'budget' and 'expenditure' columns, and melt. 
  new_gos_15_17[measure_names == "Prorated Cumulative Budget USD Equ", measure_names:='budget']
  new_gos_15_17[measure_names == "Prorated Cumulative Expenditure USD Equ", measure_names:="expenditure"]
  new_gos_15_17 = dcast(new_gos_15_17, year+country+disease+grant+start_date+end_date+module+intervention~measure_names, value.var ='measure_values', fun.aggregate = sum_na_rm)
  #Get rid of the P01, P02 etc. at the end of the string. 
  substrEnd <- function(x, n){
    substr(x, 1, nchar(x)-n+1)
  }
  new_gos_15_17[, grant:=substrEnd(grant, 4)]
  
}
{
  oldNames <- names(old_gos_15_17)
  newNames <- gsub("\\.", "_", oldNames)
  newNames = tolower(newNames)
  
  setnames(old_gos_15_17, oldNames, newNames)
  setnames(old_gos_15_17, old=c("financial_reporting_period_start_date", 'financial_reporting_period_end_date', "total_budget_amount_(in_budget_currency)", 
                                "total_expenditure_amount_(in_budget_currency)", "grant_number"),
           new=c('start_date', 'end_date', 'budget', 'expenditure', 'grant'))
}

#Merge the two files together to see what's different. 
old_date = old_gos_15_17[, .(budget_old=sum(budget, na.rm=T)), by=c('grant', 'start_date', 'end_date')]
new_date = new_gos_15_17[, .(budget_new=sum(budget, na.rm=T)), by=c('grant', 'start_date', 'end_date')]

merge1 = merge(old_date, new_date, all=T)
print(paste0(nrow(merge1), " rows in merge. Of these, ", nrow(merge1[budget_old!=budget_new]), " have budgets that don't match."))
print(merge1[budget_old!=budget_new])
merge1$diff = "MATCH"
merge1[budget_old<budget_new, diff:="OLD LESS THAN NEW"]
merge1[budget_new<budget_old, diff:="NEW LESS THAN OLD"]

#Try by year. 
old_year = old_gos_15_17[, .(budget_old=sum(budget, na.rm=T)), by=c('grant', 'year')]
new_year = new_gos_15_17[, .(budget_new=sum(budget, na.rm=T)), by=c('grant', 'year')]

merge2 = merge(old_year, new_year, all=T)
print(paste0(nrow(merge2), " rows in merge. Of these, ", nrow(merge2[budget_old!=budget_new]), " have budgets that don't match."))
print(merge2[budget_old!=budget_new])
merge2$diff = "MATCH"
merge2[budget_old<budget_new, diff:="OLD LESS THAN NEW"]
merge2[budget_new<budget_old, diff:="NEW LESS THAN OLD"]

#What are the differences in the start dates, end dates, and grants between the files? 
old_gos_15_17[, concat:=paste0(grant, "_", start_date, "_", end_date)]
new_gos_15_17[, concat:=paste0(grant, "_", start_date, "_", end_date)]

sort(unique(old_gos_15_17$concat))
sort(unique(new_gos_15_17$concat))

date_check_old = old_gos_15_17[, .(grant, start_date, end_date, concat)]
setnames(date_check_old, c("grant", "start_date", "end_date"), c("grant_old", "start_date_old", "end_date_old"))
date_check_old = unique(date_check_old)
date_check_new = new_gos_15_17[, .(grant, start_date, end_date, concat)]
setnames(date_check_new, c("grant", "start_date", "end_date"), c("grant_new", "start_date_new", "end_date_new"))
date_check_new = unique(date_check_new)

date_check = merge(date_check_old, date_check_new, by='concat', all=T)
View(date_check[grant_old%in%c('UGA-H-MoFPED', 'UGA-S-MoFPED', 'UGA-M-MoFPED') 
                | grant_new%in%c('UGA-H-MoFPED', 'UGA-S-MoFPED', 'UGA-M-MoFPED')])
