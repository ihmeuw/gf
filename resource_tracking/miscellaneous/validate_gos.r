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
for (i in 1:nrow(code_lookup_tables)){
  gos_data[country==code_lookup_tables$country[[i]], loc_name:=code_lookup_tables$iso_code[[i]]]
}

raw_gos_mf  <- data.table(read_excel(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
                                   sheet=as.character('GOS Mod-Interv - Extract')))
raw_gos_sdas  <- data.table(read_excel(paste0(gos_raw, 'Expenditures from GMS and GOS for PCE IHME countries.xlsx'),
                                   sheet=as.character('GMS SDAs - extract')))
new_gos = read.delim2(paste0(gos_raw, "Budget and exp PCE countries 2015 to 2017.txt"))
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
