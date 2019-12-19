# ------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Combine prepped absorption and performance indicator data 
# DATE: Last updated November 2019 
# ------------------------------------------------------

# Read in prepped absorption data 
absorption = readRDS(paste0(box, "tableau_data/absorption.rds"))

#Create a cumulative dataset
# Flag cases where you don't have a complete time series. A complete time series is either S1, S2, and S3 or S1-2 and S3. 
cumulative_absorption = data.table()
for (g in unique(absorption$grant)){ 
  subset = absorption[grant_period=="2018-2020" & grant==g]
  if (nrow(subset)!=0) {
    sequence = sort(unique(subset$semester))
    if ("Semester 1-2"%in%sequence & "Semester 3"%in%sequence) {
      subset[, full_time_series:=TRUE]
      subset = subset[semester%in%c('Semester 1-2', 'Semester 3'), .(loc_name, grant, grant_period, grant_disease, gf_module, gf_intervention, code, disease, full_time_series, semester, start_date,  budget, expenditure)]
      subset = melt(subset, id.vars=c('loc_name', 'grant', 'grant_period', 'grant_disease', 'gf_module', 'gf_intervention', 'code', 'disease', 'full_time_series', 'semester', 'start_date'))
      subset_wide = dcast(subset, loc_name+grant+grant_period+grant_disease+gf_module+gf_intervention+code+disease+full_time_series+start_date~semester+variable, fun.aggregate=sum)
      
      subset_wide[, cumulative_budget:=sum(`Semester 1-2_budget`, `Semester 3_budget`), by=c('loc_name', 'grant', 'grant_period', 'grant_disease', 'disease', 'full_time_series', 'gf_module', 'gf_intervention', 'start_date')]
      subset_wide[, cumulative_expenditure:=sum(`Semester 1-2_expenditure`, `Semester 3_expenditure`), by=c('loc_name', 'grant', 'grant_period', 'grant_disease', 'disease', 'full_time_series', 'gf_module', 'gf_intervention', 'start_date')]
      subset_wide = subset_wide[, .(loc_name, grant, grant_period, grant_disease, gf_module, gf_intervention, code, disease, full_time_series, start_date, cumulative_budget, cumulative_expenditure)]
      
    } else if ("Semester 1"%in%sequence & "Semester 2"%in%sequence & "Semester 3"%in%sequence) { 
      subset[, full_time_series:=TRUE]
      subset = subset[semester%in%c('Semester 1', 'Semester 2', 'Semester 3')]
      
      subset = subset[semester%in%c('Semester 1', 'Semester 2', 'Semester 3'), .(loc_name, grant, grant_period, grant_disease, gf_module, gf_intervention, code, disease, full_time_series, semester, start_date, budget, expenditure)]
      subset = melt(subset, id.vars=c('loc_name', 'grant', 'grant_period', 'grant_disease', 'gf_module', 'gf_intervention', 'code', 'disease', 'full_time_series', 'semester', 'start_date'))
      subset_wide = dcast(subset, loc_name+grant+grant_period+grant_disease+gf_module+gf_intervention+code+disease+full_time_series+start_date~semester+variable, fun.aggregate=sum)
      
      subset_wide[, cumulative_budget:=sum(`Semester 1_budget`, `Semester 2_budget`, `Semester 3_budget`), by=c('loc_name', 'grant', 'grant_period', 'grant_disease', 'disease', 'full_time_series', 'gf_module', 'gf_intervention', 'start_date')]
      subset_wide[, cumulative_expenditure:=sum(`Semester 1_expenditure`, `Semester 2_expenditure`, `Semester 3_expenditure`), by=c('loc_name', 'grant', 'grant_period', 'grant_disease', 'disease', 'full_time_series', 'gf_module', 'gf_intervention', 'start_date')]
      subset_wide = subset_wide[, .(loc_name, grant, grant_period, grant_disease, gf_module, gf_intervention, code, disease, full_time_series, start_date, cumulative_budget, cumulative_expenditure)]
      
    } else {
      subset[, full_time_series:=FALSE]
      subset = subset[, .(loc_name, grant, grant_period, grant_disease, gf_module, gf_intervention, code, disease, full_time_series, start_date, budget, expenditure)]
      subset = melt(subset, id.vars=c('loc_name', 'grant', 'grant_period', 'grant_disease', 'gf_module', 'gf_intervention', 'code', 'disease', 'full_time_series', 'start_date'))
      subset_wide = dcast(subset, loc_name+grant+grant_period+grant_disease+gf_module+gf_intervention+code+disease+full_time_series+start_date~variable, fun.aggregate=sum)
      
      subset_wide[, cumulative_budget:=sum(budget, na.rm=T), by=c('loc_name', 'grant', 'grant_period', 'grant_disease', 'disease', 'full_time_series', 'gf_module', 'gf_intervention', 'start_date')]
      subset_wide[, cumulative_expenditure:=sum(expenditure, na.rm=T), by=c('loc_name', 'grant', 'grant_period', 'grant_disease', 'disease', 'full_time_series', 'gf_module', 'gf_intervention', 'start_date')] 
      subset_wide = subset_wide[, .(loc_name, grant, grant_period, grant_disease, gf_module, gf_intervention, code, disease, full_time_series, start_date, cumulative_budget, cumulative_expenditure)]
      
    }
    cumulative_absorption = rbind(cumulative_absorption, subset_wide, use.names=TRUE)
  } 
}

# Read in prepped indicator data 
indicators = readRDS(paste0(dir, 'pudr_indicator_extraction/prepped_data/cleaned_pfi.rds'))

#Read in map 
map = data.table(read_excel(paste0(dir, "Indicators to Interventions Map.xlsx")))
map = map[weight==1] #Limit map for now

#Subset to the rows you need- just want to map code onto performance indicators data. 
map = unique(map[, .(indicator_code, code)])
#-----------------------------------------------
# Merge data together - first merge map onto indicators. 
unique(map[!indicator_code%in%indicators$indicator_code, .(indicator_code)]) #Make sure all the codes in the map exist in the data (this would be a typo otherwise) 
indicators = merge(indicators, map, by='indicator_code') #Only want instances that exist in both, so don't specify an 'all' condition.
indicators = indicators[, .(loc_name, grant, grant_period, start_date_programmatic, disease, indicator_code, indicator, baseline_value, 
                            target_value, any_result_value, ihme_result_achievement_ratio, code, reverse_indicator_final)]
setnames(indicators, 'start_date_programmatic', 'start_date')

#Then, limit absorption to only the codes that are kept after this merge, and merge on as well. 
#Want to do this merge by grant, grant period, semester, start date, module, and intervention. 
absorption = cumulative_absorption[code%in%indicators$code, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), 
                        by=c('loc_name' ,'grant', 'grant_period', 'start_date', 'disease', 'code', 'gf_module', 'gf_intervention')]
absorption[, absorption:=round((expenditure/budget)*100, 1)]
dt = merge(indicators, absorption, by=c('loc_name', 'grant', 'grant_period', 'start_date', 'disease', 'code'), all=T)

#One known issue here is that the start date/end date of the financial data don't always correspond with the start/
#end date of the programmatic data for the same PUDR. 

#Save data, and archive a version. 
saveRDS(dt, paste0(dir, "pudr_indicator_extraction/cleaned_data/absorption_indicators_combined.rds"))
saveRDS(dt, paste0(dir, "pudr_indicator_extraction/cleaned_data/archive/absorption_indicators_combined_", Sys.Date(), ".rds"))
