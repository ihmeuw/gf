#--------------------------------------------------------------
# PURPOSE: Make empty dose-response grid for CIESAR to fill in. 
# Based off of the IDRC set-up code.   
# Currently setting years as 1990-2019. 
# AUTHOR: Emily Linebarger 
# DATE: March 2019. 
# -------------------------------------------------------------

rm(list=ls())
library(data.table) 
library(readxl)

save_loc <- "J:/Project/Evaluation/GF/impact_evaluation/gtm/"
#Read in the pilot data from DRC malaria for reference 
pilot_data <- readRDS("J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/pilot_data.RDS")

#Read in current version of GTM map 
gtm_map <- read_xlsx("J:/Project/Evaluation/GF/mapping/gtm/GTM Indicator Map.xlsx")
setDT(gtm_map)
gtm_map <- gtm_map[, c('Code', 'Module', 'Intervention', 'Activity Indicator', 'Output Indicator', 'Outcome Indicator', 
                       'Impact Indicator')]

colnames(gtm_map) <- c('code', 'module', 'intervention', 'activity_indicator', 'output_indicator', 
                      'outcome_indicator', 'impact_indicator')

#Only keep rows we have valid activity, output, or input data for 
activities = gtm_map[!(is.na(activity_indicator))]
activities = activities[, .(activity_indicator)] 

outputs = gtm_map[!(is.na(output_indicator))]
outputs = outputs[, .(output_indicator)] 

outcomes = gtm_map[!(is.na(outcome_indicator))]
outcomes = outcomes[, .(outcome_indicator)]

impact = gtm_map[!(is.na(impact_indicator))]
impact = impact[, .(impact_indicator)]


# What years do we need to pull exactly? Going from 1990 to 2019 right now. 
dates <- seq(from=1990, to = 2019, by=.25)

reshape_wide <- function(dt){
  names(dt) <- "indicator"
  dt <- expand.grid(dates, dt$indicator)
  names(dt) <- c('date', 'indicator')
  dt = dt[rep(row.names(dt), 2), ]
  setDT(dt)
  dt = dt[order(date, indicator)]
  dt$type <- rep(c('value', 'completeness'))
  dt$data <- NA
  dt = dcast(dt, date~indicator+type, value.var = 'data', fun.aggregate = sum)
}

activities = reshape_wide(activities)
outputs = reshape_wide(outputs)
outcomes = reshape_wide(outcomes)
impact = reshape_wide(impact)

write.csv(activities, paste0(save_loc, "blank_dr_activities.csv"), row.names = FALSE)
write.csv(outputs, paste0(save_loc, "blank_dr_outputs.csv"), row.names = FALSE)
write.csv(outcomes, paste0(save_loc, "blank_dr_outcomes.csv"), row.names = FALSE)
write.csv(impact, paste0(save_loc, "blank_dr_impact.csv"), row.names = FALSE)







