# ------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Combine prepped absorption and performance indicator data 
# DATE: Last updated November 2019 
# ------------------------------------------------------

# Read in prepped absorption data 
absorption = readRDS(paste0(box, "tableau_data/absorption.rds"))

# Read in prepped indicator data 
indicators = readRDS(paste0(dir, 'pudr_indicator_extraction/cleaned_data/kpi_data_for_analyses2.rds'))

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
absorption = absorption[code%in%indicators$code, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), 
                        by=c('loc_name' ,'grant', 'grant_period', 'start_date', 'disease', 'code', 'gf_module', 'gf_intervention')]
absorption[, absorption:=round((expenditure/budget)*100, 1)]
dt = merge(indicators, absorption, by=c('loc_name', 'grant', 'grant_period', 'start_date', 'disease', 'code'), all=T)

#One known issue here is that the start date/end date of the financial data don't always correspond with the start/
#end date of the programmatic data for the same PUDR. 

#Save data, and archive a version. 
saveRDS(dt, paste0(dir, "pudr_indicator_extraction/cleaned_data/absorption_indicators_combined.rds"))
saveRDS(dt, paste0(dir, "pudr_indicator_extraction/cleaned_data/archive/absorption_indicators_combined_", Sys.Date(), ".rds"))
