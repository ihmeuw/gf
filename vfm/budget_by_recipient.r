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
library(utils)

# ------------------------------------------------
# Read in full prepped dataset from Uganda to 
#   isolate SR activities for target grant period 
# ------------------------------------------------
dir = "J:/Project/Evaluation/GF/resource_tracking/uga/"
data = read.csv("J:/Project/Evaluation/GF/resource_tracking/uga/prepped/prepped_budget_data.csv")
data = as.data.table(data)

#---------------------------------------------------------
# calculate overall absorption for the malaria modules/interventions
#---------------------------------------------------------
data = data[grant_period == "2018-2020"]
data = data[start_date == "2018-01-01" | start_date == "2018-04-01"]
data = data[data_source=="pudr",]
data = data[disease=="malaria",]

malaria_summed = data[, .(tot_budget = sum(budget), tot_expenditure = sum(expenditure) ), by=c('gf_module', "gf_intervention")]
malaria_summed = malaria_summed[, absorption := tot_expenditure/tot_budget]

write.csv(malaria_summed, file=paste0(dir, "malaria_absorption_by_module_intervention.csv"))
#---------------------------------------------------------
# identify SR activities, and subset to the grants we want. 
#---------------------------------------------------------

#Subset to 2015-2017
data = data[grant_period == "2018-2020"]
data = data[data_source == "fpm"]

#summed_budgets_expenditures = data[, .(tot_budget = sum(budget), tot_expenditure = sum(expenditure) ), by=c('recipient', "grant_number", "grant_period")]

data[, SR:=TRUE]
data$recipient <- trimws(data$recipient)
data$grant_number <- trimws(data$grant_number)
data[grepl('MoFPED', grant_number) & recipient %in% c('MoFPED', 'MoPFED', 'Ministry of Finance, Planning and Economic Development of the Republic of Uganda'), SR:=FALSE]
data[grepl('TASO', grant_number) & recipient %in% c('TASO','taso', 'The AIDS Support Organisation (Uganda) Limited'), SR:=FALSE]

#--------------------------------------------------
# Split by recipient, and see if there is a difference 
#   in modules/interventions between the two. 
#-------------------------------------------------
data <- data[, c("gf_module", "gf_intervention", "budget", "SR", "start_date")] 

sr <- data[SR == T]
pr <- data[SR == F]

sr = as.data.table(sr)
pr = as.data.table(pr)

# #Subset to q1 and q2
sr <- sr[start_date == "2018-01-01" | start_date == "2018-04-01"]
pr <- pr[start_date == "2018-01-01" | start_date == "2018-04-01"]

sr <- sr[, 1:3]
pr <- pr[, 1:3]

#Melt and cast data tables to sum absorption by module/intervention 
sr = melt(sr, id = c("gf_module", "gf_intervention"))
sr = dcast(sr, gf_module+gf_intervention ~ variable, fun = sum)
pr = melt(pr, id = c("gf_module", "gf_intervention"))
pr = dcast(pr, gf_module+gf_intervention ~ variable, fun = sum)

#---------------------------------------------------------
#Merge back together to compare, and isolate SR activities 
#---------------------------------------------------------
# identify module/interventions with large proportion budgeted to SRs
combined <- merge(pr, sr, by = c("gf_module", "gf_intervention"), suffixes = c(".pr", ".sr"), all = T)
setDT(combined)
combined[is.na(combined)] <-0
combined[, sr_fraction:=budget.sr/(budget.sr+budget.pr)]
sr_activities = combined[sr_fraction>.9] 

#Clean up workspace 
rm(combined, pr, sr)

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
gos <- gos[year == 2015]
gos <- gos[start_date == '2015-01-01']

#Merge GOS data with sr_activities found above 
target_gos_interventions <- merge(gos, sr_activities, by = c('gf_module', "gf_intervention"))

# compute absorption by module and intervention
byVars = c('disease', 'gf_module', 'gf_intervention')
target_gos_interventions = target_gos_interventions[, .(expenditure=sum(expenditure, na.rm=T), budget=sum(budget,na.rm=T)), by=byVars]
target_gos_interventions[, absorption:=expenditure/budget]

#Want to collapse on grant_number and module/intervention to get average over time. 
target_gos_interventions = summaryBy(absorption~disease+gf_module+gf_intervention, FUN=c(mean), data = target_gos_interventions)
target_gos_interventions$absorption.mean <- round(target_gos_interventions$absorption.mean, 2)

avg_absorption_gos <- mean(target_gos_interventions$absorption.mean)

#Clean up workspace 
rm(byVars, dir, inFile)

#----------------------------------------------------------------
# View target_gos_interventions for final result. 
#----------------------------------------------------------------




