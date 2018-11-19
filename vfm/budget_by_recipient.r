#------------------------------------------------------------------------------
# AUTHOR: Emily Linebarger
# DATE: November 2018 
# PURPOSE: Calculate budget breakdown by recipient (PR vs SR)
#           in Uganda 2015-2017 grants, to be compared with GOS data. 
#           (could be modified to other grant periods/countries. )   
#
#           View target_gos_interventions to see average absorption numbers 
#           for modules/interventions that were only done by SRs in Q1 and Q2 
#           of 2015-2017 Uganda grants. This data table is calculated using 
#           GOS data from 2012 onwards. 
#-----------------------------------------------------------------------------

# ------------------
# Set up R
# ------------------
rm(list=ls())
library(data.table)
library(doBy)

# ------------------------------------------------
# Read in full prepped dataset from Uganda to 
#   isolate SR activities for target grant period 
# ------------------------------------------------
data = read.csv("J:/Project/Evaluation/GF/resource_tracking/uga/prepped/prepped_budget_data.csv")
data = as.data.table(data)

#---------------------------------------------------------
# identify SR activities, and subset to the grants we want. 
#---------------------------------------------------------

data[, SR:=TRUE]
data[grepl('MoFPED', grant_number) & recipient %in% c('MoFPED','Ministry of Finance, 
                                                      Planning and Economic Development of the Republic of Uganda'), SR:=FALSE]
data[grepl('TASO', grant_number) & recipient %in% c('TASO','The AIDS Support Organisation (Uganda) Limited'), SR:=FALSE]

#Subset to q1 and q2 of 2015-2017 grants 
data[grant_period == "2015-2017" & (start_date == "2015-01-01" | start_date == "2015-04-01")]

#--------------------------------------------------
# Split by recipient, and see if there is a difference 
#   in modules/interventions between the two. 
#-------------------------------------------------
data <- data[, c("gf_module", "gf_intervention", "budget", "SR")] 

sr <- data[SR == T]
pr <- data[SR == F]

sr = sr[, 1:3]
pr = pr[, 1:3]

sr = as.data.table(sr)
pr = as.data.table(pr)

sr = melt(sr, id = c("gf_module", "gf_intervention"))
sr = dcast(sr, gf_module+gf_intervention ~ variable, fun = sum)
pr = melt(pr, id = c("gf_module", "gf_intervention"))
pr = dcast(pr, gf_module+gf_intervention ~ variable, fun = sum)

#-------------------------------------------------
# Generate a coefficient to represent the % of overall 
# budget that's going to that module/intervention 
# ------------------------------------------------
total_budget_pr = sum(pr$budget)
total_budget_sr = sum(sr$budget)

pr$coefficient = pr$budget/total_budget_pr 
sr$coefficient = sr$budget/total_budget_sr

pr$coefficient = round(pr$coefficient, 3)
sr$coefficient = round(sr$coefficient, 3)

# Remove all rows with 0's for coefficients. 
pr = pr[coefficient!=0]
sr = sr[coefficient!=0]

#---------------------------------------------------------
#Merge back together to compare, and isolate SR activities 
#---------------------------------------------------------
combined <- merge(pr, sr, by = c("gf_module", "gf_intervention"), suffixes = c(".pr", ".sr"), all = T)
sr_activities <- combined[is.na(coefficient.pr)]
sr_activities = sr_activities[, c(-3, -4)]
rm(pr, sr, combined) #Clean up workspace 

#-----------------------------------------------------------------
# Calculate average absorption for these activities using GOS data 
#-----------------------------------------------------------------
# root directory for input/average_absorptionput
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/'

# input data
inFile = paste0(dir, 'mapping/total_resource_tracking_data.csv')

allData <- fread(inFile)
gos <- allData[data_source %in% c('gos')]

#Subset to the data we want (Uganda, only GOS data after 2012)
gos <- gos[country %in% c("Uganda")]
gos <- gos[year >= 2012]

#Merge GOS data with sr_activities found above 
target_gos_interventions <- merge(gos, sr_activities, all.y = TRUE, by = c('gf_module', 'gf_intervention'))

# compute absorption by module and intervention
byVars = c('disease', 'abbrev_module', 'abbrev_intervention')
target_gos_interventions = target_gos_interventions[, .(expenditure=sum(expenditure, na.rm=T), budget=sum(budget,na.rm=T)), by=byVars]
target_gos_interventions[, absorption:=expenditure/budget]

#Want to collapse on grant_number and module/intervention to get average over time. 
target_gos_interventions = summaryBy(absorption~disease+abbrev_module+abbrev_intervention, FUN=c(mean), data = target_gos_interventions)
target_gos_interventions$absorption.mean <- round(target_gos_interventions$absorption.mean, 2)

#Remove one row at end that has "NA" for module/intervention 
target_gos_interventions = target_gos_interventions[!is.na(abbrev_module)]

#Clean up workspace 
rm(byVars, dir, inFile, total_budget_pr, total_budget_sr)

#--------------------------------------------------
# View target_gos_interventions for final result
#--------------------------------------------------


