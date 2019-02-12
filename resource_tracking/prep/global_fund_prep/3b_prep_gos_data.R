# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code written by Irena Chen.
# PURPOSE: Master file for prepping Grant Operating System (GOS) data
#           from the Global Fund. 
# DATE: Last updated February 2019. 
# ----------------------------------------------

# TO DO
# fix time series graph so that there are gaps where appropriate (use `group` aesthetic)
# ----------------------------------------------
# Load the GOS tab from the Excel book  
# ----------------------------------------------

gos_data  <- data.table(read_excel('J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/Expenditures from GMS and GOS for PCE IHME countries.xlsx',
                                   sheet=as.character('GOS Mod-Interv - Extract')))

## reset column names
oldNames <-  c("Country","Grant Number", "Year", "Financial Reporting Period Start Date",
               "Financial Reporting Period End Date","Module", "Intervention", "Total Budget Amount (in budget currency)", 
               "Total Expenditure Amount (in Budget currency)", "Component", "Current IP  Start Date", "Current IP  End Date")
newNames <-  c("country","grant_number", "year","start_date","end_date","module","intervention", 
               "budget", "expenditure", "disease", "grant_period_start", "grant_period_end")

setnames(gos_data,oldNames, newNames)

#gos_sum <- gos_data[, sum(budget), by=.(year, country, disease)][order(country, disease, year)]

##subset the columns that we want 
gos_clean <- gos_data[, newNames, with=FALSE]

gos_clean$grant_period = paste0(year(as.Date(gos_clean$grant_period_start)), "-",year(as.Date(gos_clean$grant_period_end)))
gos_clean$grant_period_start = NULL
gos_clean$grant_period_end = NULL

# ----------------------------------------------
# Load the GMS tab from the Excel book  
# ----------------------------------------------
gms_data  <- data.table(read_excel('J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/Expenditures from GMS and GOS for PCE IHME countries.xlsx',
                                   sheet=as.character('GMS SDAs - extract')))

##repeat the subsetting that we did above (grabbing only the columns we want)
gmsOld <- c(oldNames[1:5], "Service Delivery Area", "Total Budget Amount (USD equ)", "Total Expenditure Amount (USD equ)", "Component", "Program Start Date", "Program End Date")
gmsNew <- c(newNames[1:6], newNames[8:10], "grant_period_start", "grant_period_end")
setnames(gms_data, gmsOld, gmsNew)
gms_clean <- gms_data[, gmsNew, with=FALSE]

gms_clean$grant_period = paste0(year(as.Date(gms_clean$grant_period_start)), "-",year(as.Date(gms_clean$grant_period_end)))
gms_clean$grant_period_start = NULL
gms_clean$grant_period_end = NULL

##combine both GOS and GMS datasets into one dataset
totalGos <- rbind(gms_clean, gos_clean, fill = TRUE)

# ----------------------------------------------
###### Load the GMS tab from the Excel book  ###### 
# ----------------------------------------------
map_disease <- unique(totalGos$disease)
names(map_disease) <- c("tb", "malaria", "hiv", "hss", "hiv/tb")
  
kDT = data.table(map_disease = names(map_disease), value = TRUE, disease = unname(map_disease))
totalGos[kDT, on=.(disease), disease := i.map_disease]

totalGos$data_source <- "gos"
totalGos$loc_name <- totalGos$country
totalGos$fileName = "Expenditures from GMS and GOS for PCE IHME countries.xlsx"

# ----------------------------------------------
###### Map the GOS/GMS modules to the current GF Framework ###### 
# Run the map_modules_and_interventions.R script first
# ----------------------------------------------
# totalGos <- strip_chars(totalGos, unwanted_array, remove_chars)
# 
# mapping_list <- load_mapping_list(paste0("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx")
#                                   , include_rssh_by_disease = FALSE) ##set the boolean to false for just mapping
# 
# ## before we get it ready for mapping, copy over so we have the correct punctuation for final mapping: 
# final_mapping <- copy(mapping_list)
# final_mapping$disease <- NULL ## we will be joining on code 
# setnames(final_mapping, c("module", "intervention"), c("gf_module", "gf_intervention"))
# 
# ##this loads the list of modules/interventions with their assigned codes
# gf_mapping_list <- total_mapping_list(paste0("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx"),
#                                       mapping_list, unwanted_array, remove_chars)
# 
# gos_init_mapping <- merge(totalGos, gf_mapping_list, by=c("module", "intervention", "disease"), all.x=TRUE,allow.cartesian = TRUE)
# 
# ##use this to check if any modules/interventions were dropped:
# # dropped_gf <- gos_init_mapping[is.na(gos_init_mapping$code)]
# 
# mappedGos <- merge(gos_init_mapping, final_mapping, by="code")
# 
# # ----------------------------------------------
# ###### export the mapped dataset ###### 
# # ----------------------------------------------
# 
# write.csv(mappedGos, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_gos_data.csv",
#           row.names = FALSE, fileEncoding = "latin1")



