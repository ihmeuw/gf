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
library(googlesheets)
library(reshape2)
# ------------------


# -----------------------------------------------------------------
# Files and directories

# root directory for input/average_absorptionput
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
allData$data_source = ifelse(allData$data_source == "fpm_final", "fpm", allData$data_source)
#-------------------------------------

#---------------------------------------------
# PART 1: Report on absorption for 1st and 2nd
#         quarters of 2018 cycle 
#---------------------------------------------

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

##########################################
#Generate a variable for actual absorption
data[, observed_absorption:=expenditure/budget]


##########################################
# Subset to only the grants from 2018 

# subset to only the 1st and 2nd quarter of the grants we want #ADD DRC PUDRS
average_absorption = data[ which(data$grant_number %in% grants), ]
average_absorption = average_absorption[which((average_absorption$quarter == 1 | average_absorption$quarter == 2) & average_absorption$year ==2018)]

#Replace NA with 0 for budget and observed absorption 
average_absorption$budget[is.na(average_absorption$budget)]<- 0 
average_absorption$observed_absorption[is.na(average_absorption$observed_absorption)]<- 0 

# subset columns
keepVars = c('disease','country','grant_number','year','quarter',
             'abbrev_module','budget','observed_absorption')
average_absorption = average_absorption[, keepVars, with=FALSE]

# reshape countries wide
average_absorption[, grant:=paste0(country, ' ', grant_number, ' (Q', quarter, ' ', year, ')')]
average_absorption = dcast.data.table(average_absorption, disease+abbrev_module~grant, fun=sum, value.var=c('budget', 'observed_absorption'))

#Round to 2 decimal place 
cols <- names(average_absorption)[3:length(average_absorption)]
average_absorption = average_absorption[,(cols) := round(.SD,2), .SDcols=cols]

##########################################
#Section off by grant so it's easier to pull numbers.  

subset_columns = function(x, grant){
    columns <- grep(grant, colnames(x))
    x1 = subset(x, select = c(1, 2, columns))
    return(x1)
}

cod_c_cordaid = subset_columns(average_absorption, "COD-C-CORDAID")
cod_m_moh <- subset_columns(average_absorption, "COD-M-MOH")
cod_t_moh <- subset_columns(average_absorption, "COD-T-MOH")
cod_h_moh <- subset_columns(average_absorption, "COD-H-MOH")
cod_m_sanru <- subset_columns(average_absorption, "COD-M-SANRU")
uga_c_taso <- subset_columns(average_absorption, "UGA-C-TASO")
uga_h_mofped <- subset_columns(average_absorption, "UGA-H-MoFPED")
uga_m_mofped <- subset_columns(average_absorption, "UGA-M-MoFPED")
uga_m_taso<- subset_columns(average_absorption, "UGA-M-TASO")
uga_t_mofped<- subset_columns(average_absorption, "UGA-T-MoFPED")
gtm_h_hivos<- subset_columns(average_absorption, "GTM-H-HIVOS") #We don't have absorption numbers for this grant because we don't have a PUDR for Q1 and Q2 2018 yet. 
gtm_t_mspas <- subset_columns(average_absorption, "GTM-T-MSPAS") #We don't have absorption numbers for this grant because we don't have a PUDR for Q1 and Q2 2018 yet. 

#---------------------------------------------
# PART 2: Report on historical absorption 
#---------------------------------------------

# subset to GOS and PUDRs
historical_absorption = allData[data_source %in% c('gos')]

#Only use GOS data from 2012 on 
historical_absorption = historical_absorption[year >= 2012]

# aggregate the PUDRs by grant but GOS overall
historical_absorption[data_source=='gos', grant_number:='all']
historical_absorption[data_source=='gos', grant_period:='all']

# identify RSSH modules
historical_absorption[, rssh:=grepl('R',code)]

# compute absorption by module
byVars = c('country', 'disease', 'grant_period','abbrev_module')
historical_absorption = historical_absorption[, .(expenditure=sum(expenditure, na.rm=T), budget=sum(budget,na.rm=T)), by=byVars]
historical_absorption[, absorption:=expenditure/budget]

#Want to collapse on grant_number and abbrev_module to get average over time. 
historical_absorption = summaryBy(absorption~country+disease+abbrev_module, FUN=c(mean), data = historical_absorption)
historical_absorption$absorption.mean <- round(historical_absorption$absorption.mean, 2)

write.csv(historical_absorption, "J:/Project/Evaluation/GF/vfm/average_absorptionputs/historical_absorption_by_country_and_disease.csv", row.names = F)

#---------------------------------------------
# PART 3: Generate combined graph using 
#         consortia's numbers 
#---------------------------------------------

gs_gap() %>%
  gs_copy(to = "MyDrive")
consortia_data <- gs_title("Absorption Table")
malaria <- gs_read(consortia_data, ws = "Malaria")
hiv <- gs_read(consortia_data, ws = "HIV")
tb <- gs_read(consortia_data, ws = "TB")
hivtb <- gs_read(consortia_data, ws = "HIV-TB")

#Subset columns by grant so they can be re-combined
grant_indices <- seq(2,ncol(malaria),3)
names(malaria)[2:ncol(malaria)] <- rep(c("budget", "absorption_q1_2018", "historical_absorption"), ncol(malaria)/3)
    
prep_data = function(df, col_index) {
    df <- df[c(1,col_index:(col_index+2))]
    df$grant <- df$budget[1]
    df = df[3:nrow(df), ]
  }
    
for (index in grant_indices){
  df <- prep_data(malaria, index)
  if (index == 2) {
    malaria_prepped <- df 
  } else {
    malaria_prepped = rbind(malaria_prepped, df)  
        
  }
  print(index)
}
    
names(malaria_prepped)[1] <- "gf_module"
#Format as numeric 
  for (col in 2:4){
    malaria_prepped[, col] <- gsub("[^0-9\\.]", "", malaria_prepped[[col]])
    malaria_prepped[, col] <- as.numeric(malaria_prepped[[col]])
 }

malaria_prepped[is.na(malaria_prepped)] <- 0
malaria_prepped$country <- substring(malaria_prepped$grant, 1, 3)
malaria_melt <- melt(malaria_prepped, id = c("gf_module", "country"))
malaria_observed<-dcast(malaria_melt, gf_module ~ variable, fun = list(min, max, mean))

# Thinking about this more, my guess is that the best way to display the absorption data is 
# probably a horizontal point-range graph showing the min/mean/max for each module, faceted by disease. 
# Can you make one that’s super minimal but has kind of large text (check out base_size ggplot) 
# so we can shrink it down pretty small in the report? Doesn’t have to be today. 






