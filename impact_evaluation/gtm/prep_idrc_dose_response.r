#--------------------------------------------------------------
# PURPOSE: Make empty dose-response grid for IDRC to fill in. 
#   Currently setting years as 1990-2019. 
# AUTHOR: Emily Linebarger 
# DATE: February 11, 2019 
# -------------------------------------------------------------

rm(list=ls())
library(data.table) 
library(readxl)

save_loc <- "J:/Project/Evaluation/GF/impact_evaluation/uga/"
#Read in the pilot data from DRC malaria for reference 
pilot_data <- readRDS("J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/pilot_data.RDS")

#Read in current version of UGA map 
uga_map <- read_xlsx("J:/Project/Evaluation/GF/mapping/uga/UGA Indicator Map.xlsx")
setDT(uga_map)
uga_map <- uga_map[, c('Code', 'Module', 'Intervention', 'Activity Indicator', 'Output Indicator', 'Outcome Indicator', 
                       'Impact Indicator')]

colnames(uga_map) <- c('code', 'module', 'intervention', 'activity_indicator', 'output_indicator', 
                      'outcome_indicator', 'impact_indicator')

#Only keep rows we have valid activity, output, or input data for 
activities = uga_map[!(is.na(activity_indicator))]
activities = activities[, .(activity_indicator)] 

outputs = uga_map[!(is.na(output_indicator))]
outputs = outputs[, .(output_indicator)] 

outcomes = uga_map[!(is.na(outcome_indicator))]
outcomes = outcomes[, .(outcome_indicator)]

impact = uga_map[!(is.na(impact_indicator))]
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







