# ------------------------------------------------------
# Emily Linebarger, based on code by David Phillips
#
# 11/13/18
# 
# Calculates average absorption rate for 2018-2020 grants. 
# ------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(boot)
library(data.table)
library(ggplot2)
library(doBy)
# ------------------


# -----------------------------------------------------------------
# Files and directories

# root directory for input/output
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/'

# input data
inFile = paste0(dir, 'mapping/total_resource_tracking_data.csv')

# -----------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep data

# load
allData = fread(inFile)
grants <- c('UGA-C-TASO', 'UGA-H-MoFPED', 'UGA-M-MoFPED', 'UGA-M-TASO', 'UGA-T-MoFPED', 'COD-C-CORDAID', 'COD-M-SANRU', 'COD-H-MOH', 'COD-M-MOH', 
            'COD-T-MOH', 'GTM-T-MSPAS', 'GTM-H-HIVOS')
data$data_source = ifelse(data$data_source == "fpm_final", "fpm", data$data_source)
#-------------------------------------

#-----------------------------------
# PART 1: Report on absorption for 1st and 2nd quarters of 2018 cycle 
# subset to budgets
data = allData[data_source=='fpm']

# subset to pudrs
pudrs = allData[data_source=='pudr']

# keep only matching pudrs
pudrs = pudrs[grant_number %in% unique(data$grant_number)]
pudrs = pudrs[abbrev_module!='Unspecified']

# collapse to intervention-quarter level
data[, quarter:=quarter(start_date)]
pudrs[, quarter:=quarter(start_date)]
byVars = c('disease','country','grant_number','year','quarter','abbrev_module')
data=data[, .('budget'=sum(budget,na.rm=TRUE)), by=byVars]
pudrs=pudrs[, .('budget'=sum(budget,na.rm=TRUE), 
                'expenditure'=sum(expenditure,na.rm=TRUE)), by=byVars]

# use pudrs wherever we have them
data = merge(data, pudrs, by=byVars, all=TRUE, suffixes=c('_fpm','_pudr'))
data[, budget:=budget_fpm]
data[!is.na(budget_pudr), budget:=budget_pudr]
# ----------------------------------------------------------------------

#------------------------------------------
#Generate a variable for actual absorption, and average absorption from 2012-present. 

# compute actual absorption
data[, observed_absorption:=expenditure/budget]

#------------------------------------------

# -----------------------------
# Subset to only the grants from 2018 

# subset to only the 1st and 2nd quarter of the grants we want #ADD DRC PUDRS
out = data[ which(data$grant_number %in% grants), ]
out = out[which((out$quarter == 1 | out$quarter == 2) & out$year ==2018)]

#Replace NA with 0 for budget and observed absorption 
out$budget[is.na(out$budget)]<- 0 
out$observed_absorption[is.na(out$observed_absorption)]<- 0 

# subset columns
keepVars = c('disease','country','grant_number','year','quarter',
             'abbrev_module','budget','observed_absorption')
out = out[, keepVars, with=FALSE]

# reshape countries wide
out[, grant:=paste0(country, ' ', grant_number, ' (Q', quarter, ' ', year, ')')]
out = dcast.data.table(out, disease+abbrev_module~grant, fun=sum, value.var=c('budget', 'observed_absorption'))

# order columns
names = names(out)[grepl('budget',names(out))]
grants = gsub('budget_','',names)
vars = as.vector(outer(c('budget_','observed_absorption_'), grants, paste0))
out = out[, c('disease','abbrev_module',vars), with=FALSE]

#Round to 1 decimal place 
cols <- names(out)[3:length(out)]
out = out[,(cols) := round(.SD,1), .SDcols=cols]

# -----------------------------
# cod_m_sanru <-out[, grep("COD-M-SANRU", colnames(out))]
# cod_m_sanru <- append(cod_m_sanru, 1:2)
# print_data <- out[c=(cod_m_sanru)]

cod_c_cordaid <- out[, c(1, 2, 3:6)]
cod_h_moh <- out[, c(1, 2, 7:10)]
cod_m_moh <- out[, c(1, 2, 11:14)]
cod_m_sanru <- out[, c(1, 2, 15:18)]
cod_t_moh <- out[, c(1, 2, 19:22)]
uga_c_taso <- out[, c(1, 2, 23:26)]
uga_h_mofped <- out[, c(1, 2, 27:30)]
uga_m_mofped <- out[, c(1, 2, 31:34)]
uga_m_taso<- out[, c(1, 2, 35:38)]
uga_t_mofped<- out[, c(1, 2, 39:42)]



#---------------------------------------------
# PART 2: Report on historical absorption 

# subset to GOS and PUDRs
historical_exp = allData[data_source %in% c('gos')]

# aggregate the PUDRs by grant but GOS overall
historical_exp[data_source=='gos', grant_number:='all']
historical_exp[data_source=='gos', grant_period:='all']

# identify RSSH modules
historical_exp[, rssh:=grepl('R',code)]

# compute absorption by module
byVars = c('country', 'disease', 'grant_period','abbrev_module')
agg = historical_exp[, .(expenditure=sum(expenditure, na.rm=T), budget=sum(budget,na.rm=T)), by=byVars]
agg[, absorption:=expenditure/budget]

#Want to collapse on grant_number and abbrev_module to get average over time. 
agg = summaryBy(absorption~country+disease+abbrev_module, FUN=c(mean), data = agg)
agg$absorption.mean <- round(agg$absorption.mean, 2)

write.csv(agg, "J:/Project/Evaluation/GF/vfm/outputs/historical_absorption_by_country_and_disease.csv", row.names = F)
#-----------------------------------



