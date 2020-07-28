# ------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Combine prepped absorption and performance indicator data 
# DATE: Last updated November 2019 
# ------------------------------------------------------

library(data.table)

# Read in prepped absorption data 
absorption = readRDS("J:/Project/Evaluation/GF/resource_tracking/_other_data_sources/multi_country/2019-2020_synthesis/all_modules.rds")
setnames(absorption, old=c("gf_module", "grant_disease"), new=c('module', 'disease'))

# clean absorption data--add grant names to grant variable
absorption$grant[which(absorption$loc_name=="Sudan" & absorption$grant=="TB")] <- "SDN-T-UNDP"
absorption$grant[which(absorption$loc_name=="Sudan" & absorption$grant=="Malaria")] <- "SDN-M-MOH"
absorption$grant[which(absorption$loc_name=="Sudan" & absorption$grant=="HIV")] <- "SUD-H-UNDP"

# not sure which are the combined TB and combined HIV grants in Myanmar
# solution: will map indicators onto country regardless of grant that the funding came from
# this solution is imperfect
# instead will need to map onto grant disease




# Read in prepped indicator data from EHG and IHME/PATH
indicators = readRDS(paste0(dir, 'pudr_indicator_extraction/analysis/subset_data/prepped_cross_consortia_pfi_2019.RDS'))

# create disease variable based off grant information
indicators[substring(grant, 5, 5)=='H', disease:='hiv']
indicators[substring(grant, 5, 5)=='T', disease:='tb']
indicators[substring(grant, 5, 5)=='Z', disease:='tb']
indicators[substring(grant, 5, 5)=='M', disease:='malaria']
indicators[substring(grant, 5, 5)=='C', disease:='hiv/tb']

#Read in map 
map = data.table(read_excel(paste0(dir, "Indicators to Interventions Map.xlsx")))
map = map[weight==1] #Limit map for now

#Subset to the rows you need- just want to map code onto performance indicators data. 
map = unique(map[, .(indicator_code, code, module)])

#-----------------------------------------------
# Merge data together - first merge map onto indicators. 
unique(map[!indicator_code%in%indicators$indicator_code, .(indicator_code)]) #Make sure all the codes in the map exist in the data (this would be a typo otherwise) 
# some codes seem to not appear in the indicators data--not sure how they were dropped--possibly because
## some indicator codes are not properly cleaned in the indicator database

indicators = merge(indicators, map, by='indicator_code') #Only want instances that exist in both, so don't specify an 'all' condition.
indicators = indicators[, .(loc_name, indicator_code, indicator_long, achievement_ratio, module_code, reverse_indicator_final,
                            type_desc, code, module, disease)] # removed grant and reporting period since will no longer be used in merge


# Merge map  onto absorption data.
# unique(map2[!indicator_code%in%absorption$gf_module, .(gf_module)]) #Make sure all the codes in the map exist in the data (this would be a typo otherwise) 
# absorption = merge(absorption, map2, by.x = "gf_module", by.y = "module") #Only want instances that exist in both, so don't specify an 'all' condition.
#absorption = absorption[, .(loc_name, grant, reporting_period, indicator_code, indicator_long, achievement_ratio, gf_module, reverse_indicator_final, code,
#                            type_desc)]

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


# calculate cumulative absorption based on cumulative expenditure and cumulative budget  
absorption[, absorption:=round((cumulative_expenditure/cumulative_budget)*100, 1)]

# merge on variables that are common in both
dt = merge(indicators, absorption, by=c('loc_name', 'module', 'disease'), all=T) # these should be the same in both\
# the resource tracking file post_2017_map.RDS could be the solution to adding in codes that will map onto

#One known issue here is that the start date/end date of the financial data don't always correspond with the start/
#end date of the programmatic data for the same PUDR. 

#Save data, and archive a version. 
saveRDS(dt, paste0(dir, "pudr_indicator_extraction/analysis/subset_data/absorption_indicators_combined_synthesis.rds"))
saveRDS(dt, paste0(dir, "pudr_indicator_extraction/analysis/subset_data/archive/absorption_indicators_combined_", Sys.Date(), ".rds"))
