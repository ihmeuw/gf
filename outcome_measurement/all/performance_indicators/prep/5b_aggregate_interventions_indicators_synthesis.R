# ------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Combine prepped absorption and performance indicator data 
# DATE: Last updated November 2019 
# ------------------------------------------------------

library(data.table)

# Read in prepped absorption data 
# absorption = readRDS(paste0(box, "tableau_data/absorption.rds"))
absorption = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_modules.rds")
setnames(absorption, old=c("gf_module"), new=c('module'))

# clean absorption data

absorption$grant[which(absorption$loc_name=="Sudan" & absorption$grant=="TB")] <- "SDN-T-UNDP"
absorption$grant[which(absorption$loc_name=="Sudan" & absorption$grant=="Malaria")] <- "SDN-M-MOH"
absorption$grant[which(absorption$loc_name=="Sudan" & absorption$grant=="HIV")] <- "SUD-H-UNDP"

#absorption$grant[which(absorption$loc_name=="Mozambique" & absorption$grant=="Malaria")] <- "MOZ-M-WV" # two malaria grants
#absorption$grant[which(absorption$loc_name=="Mozambique" & absorption$grant=="HIV")] <- "MOZ-M-WV" # two hiv grants
# not sure which are the combined TB and combined HIV grants in Myanmar

# FRC: Commenting out this part because I believe synthesis data already has cumulative absorption calculated
# #Create a cumulative dataset

# Read in prepped indicator data from EHG and IHME/PATH
indicators = readRDS(paste0(dir, 'pudr_indicator_extraction/analysis/subset_data/prepped_cross_consortia_pfi_2019.RDS'))

#Read in map 
map = data.table(read_excel(paste0(dir, "Indicators to Interventions Map.xlsx")))
map = map[weight==1] #Limit map for now

#Subset to the rows you need- just want to map code onto performance indicators data. 
map = unique(map[, .(indicator_code, code, module)])

# subset to module and code to merge with absorption data
# map2 = data.table(read_excel(paste0(dir, "Indicators to Interventions Map.xlsx")))
# map2 = map2[weight==1] #Limit map for now

# Subset to the rows you need- just want to map code onto performance indicators data. 
#map2 = unique(map2[, .(module, code)])
#-----------------------------------------------
# Merge data together - first merge map onto indicators. 
unique(map[!indicator_code%in%indicators$indicator_code, .(indicator_code)]) #Make sure all the codes in the map exist in the data (this would be a typo otherwise) 
# some codes seem to not appear in the indicators data--not sure how they were dropped--possibly because
## some indicator codes are not properly cleaned in the indicator database

indicators = merge(indicators, map, by='indicator_code') #Only want instances that exist in both, so don't specify an 'all' condition.
indicators = indicators[, .(loc_name, grant, indicator_code, indicator_long, achievement_ratio, module_code, reverse_indicator_final,
                            type_desc, code, module)]

# Merge map  onto absorption data.
# unique(map2[!indicator_code%in%absorption$gf_module, .(gf_module)]) #Make sure all the codes in the map exist in the data (this would be a typo otherwise) 
# absorption = merge(absorption, map2, by.x = "gf_module", by.y = "module") #Only want instances that exist in both, so don't specify an 'all' condition.
#absorption = absorption[, .(loc_name, grant, reporting_period, indicator_code, indicator_long, achievement_ratio, gf_module, reverse_indicator_final, code,
#                            type_desc)]

# fix reporting_period
indicators$reporting_period[which(indicators$reporting_period=="s1_2019")] <- "Semester 3"
indicators$reporting_period[which(indicators$reporting_period=="s2_2018")] <- "Semester 2"

setnames(indicators, 'reporting_period', 'semester')

# clean names between the two datasets
# loc_name
indicators$loc_name[which(indicators$loc_name=="COD")] <- "DRC"
indicators$loc_name[which(indicators$loc_name=="MOZ")] <- "Mozambique"
indicators$loc_name[which(indicators$loc_name=="SDN")] <- "Sudan"
indicators$loc_name[which(indicators$loc_name=="SEN")] <- "Senegal"
indicators$loc_name[which(indicators$loc_name=="UGA")] <- "Uganda"
indicators$loc_name[which(indicators$loc_name=="MMR")] <- "Myanmar"
indicators$loc_name[which(indicators$loc_name=="KHM")] <- "Cambodia"
indicators$loc_name[which(indicators$loc_name=="GTM")] <- "Guatemala"


#Then, limit absorption to only the codes that are kept after this merge, and merge on as well. 
#Want to do this merge by grant, grant period, semester, start date, module, and intervention. 
#absorption = cumulative_absorption[code%in%indicators$code, .(budget=sum(cumulative_budget, na.rm=T), expenditure=sum(cumulative_expenditure, na.rm=T)), 
#                        by=c('loc_name' ,'grant', 'grant_period', 'start_date', 'gf_module')]
  
absorption[, absorption:=round((cumulative_expenditure/cumulative_budget)*100, 1)]

# merge on variables that are common in both
dt = merge(indicators, absorption, by=c('loc_name', 'module'), all=T) # these should be the same in both\
# the resource tracking file post_2017_map.RDS could be the solution to adding in codes that will map onto

#One known issue here is that the start date/end date of the financial data don't always correspond with the start/
#end date of the programmatic data for the same PUDR. 

#Save data, and archive a version. 
saveRDS(dt, paste0(dir, "pudr_indicator_extraction/analysis/subset_data/absorption_indicators_combined_synthesis.rds"))
saveRDS(dt, paste0(dir, "pudr_indicator_extraction/analysis/subset_data/archive/absorption_indicators_combined_", Sys.Date(), ".rds"))
