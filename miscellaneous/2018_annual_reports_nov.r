#------------------------------------------------------------------------------
# AUTHOR: Emily Linebarger
# DATE: November 2018 
# PURPOSE: Calculations for 2018 GTM Country report. 
#-----------------------------------------------------------------------------

# ------------------
# Set up R
# ------------------
rm(list=ls())
library(data.table)
library(doBy)
library(readxl)

# ------------------------------------------------
# Read in cleaned RT database 
# ------------------------------------------------
total_resource = fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv")
fgh = total_resource[data_source == "fgh"] 


#--------------------------------------------------
# Section 4.3- Cross-cutting thematic areas (p. 40, Figure 14) 
#--------------------------------------------------
#Can you do another graph as well showing what share of total program spend this represents?



#--------------------------------------------------
# Do we have the grant totals spent on HIV case detection for each of the GTM SRs in Q1 and Q2 2018 to estimate cost per case detected?
#--------------------------------------------------
target_grants = c('GTM-H-HIVOS', 'GTM-H-INCAP')
gtmData = total_resource[country == "Guatemala" & grant_number %in% target_grants & data_source == "fpm"]
stopifnot(length(unique(gtmData$fileName))==2)



#------------------------------------------------------------------------------
# AUTHOR: Emily Linebarger
# DATE: November 2018 
# PURPOSE: Calculations for 2018 UGA Country report. - RSSH Spending 
#-----------------------------------------------------------------------------

#? It might be nice to have a sentence in the Uganda report that states according to the CT, $X amount is considered RSSH-related… 
#Calculate spending on RSSH using original method (module begins with "RSSH") and using new codes sent by UGA country team. 

ugaData = cleaned_total[country == "Uganda"]
ugaData = ugaData[data_source == "fpm" & grant_period == "2018-2020"] #We don't have UGA-H-MOFPED in this grant period? 

#Function to format sda_activity strings in the same way as the RT database for merging. 
smash_strings = function(dt){
  
  remove_chars <- c("[[:punct:]]", "[^[:alnum:]]","\"", ",") 
  
  for (i in 1:ncol(dt)){
    dt[[i]] = tolower(dt[[i]])
    dt[[i]] = gsub(" ", "", dt[[i]])
    dt[[i]] = gsub(paste(remove_chars, collapse="|"), "", dt[[i]])
  }
  return(dt)
}

#Bring in each sheet of new RSSH activities from the country team, and format them. 
c_taso_codes = read_excel("C:/Users/elineb/Downloads/Uganda RSSH activities summary.xlsx", sheet = "TASO Combined")
c_taso_codes = c_taso_codes[, -1]
names(c_taso_codes) = c_taso_codes[1, ]
c_taso_codes = c_taso_codes[-1, ]
#c_taso_codes = smash_strings(c_taso_codes)

m_taso_codes = read_excel("C:/Users/elineb/Downloads/Uganda RSSH activities summary.xlsx", sheet = "TASO Malaria")
m_taso_codes = m_taso_codes[, -1]
names(m_taso_codes) = m_taso_codes[2, ]
m_taso_codes = m_taso_codes[c(-1, -2), ]
#m_taso_codes = smash_strings(m_taso_codes)

mofped_malaria_codes = read_excel("C:/Users/elineb/Downloads/Uganda RSSH activities summary.xlsx", sheet = "MoFPED Malaria")
mofped_malaria_codes = mofped_malaria_codes[, -1]
mofped_malaria_codes = mofped_malaria_codes[, -1]
names(mofped_malaria_codes) = mofped_malaria_codes[1, ]
mofped_malaria_codes = mofped_malaria_codes[-1, ]
#mofped_malaria_codes = smash_strings(mofped_malaria_codes)

mofped_hiv_codes = read_excel("C:/Users/elineb/Downloads/Uganda RSSH activities summary.xlsx", sheet = "MoFPED HIV")
mofped_hiv_codes = mofped_hiv_codes[, -1]
mofped_hiv_codes = mofped_hiv_codes[, -1]
names(mofped_hiv_codes) = mofped_hiv_codes[1, ]
mofped_hiv_codes = mofped_hiv_codes[-1, ]
#mofped_hiv_codes = smash_strings(mofped_hiv_codes)



#-----------------------------------------------------------------
#Subset each of the codes to only rssh module, so it can be merged 
# with sda_activity. 
#-----------------------------------------------------------------

#Function to rename all of the columns and rbind them so they merge correctly. 
#A more sophisticated step would be to keep these separated by disease or module, as in the original excel. 
bind_activities <- function(dt){
  names(dt) <- rep("sda_activity", ncol(dt))
  for (i in 1:ncol(dt)){
    if (i == 1){
      new_dt = dt[, i]
    } else {
      new_dt <- rbind(new_dt, dt[, i])
      
    }
  }
  new_dt = new_dt[!is.na(new_dt), ]
  new_dt = new_dt[!duplicated(new_dt), ]
  #new_dt = smash_strings(new_dt)
  return(new_dt)
  
}

c_taso_codes = bind_activities(c_taso_codes)
m_taso_codes = bind_activities(m_taso_codes)
mofped_malaria_codes = bind_activities(mofped_malaria_codes)
mofped_hiv_codes = bind_activities(mofped_hiv_codes)

#Make one large codebook so it can be applied to all grants. Could probably add in a bit more sophistication here if time. 
all_codes = rbind(c_taso_codes, m_taso_codes, id = "sda_activity")
all_codes = rbind(all_codes, mofped_malaria_codes, id = "sda_activity")
all_codes = rbind(all_codes, mofped_hiv_codes, id = "sda_activity")
all_codes = all_codes[!duplicated(all_codes), ]

pretty_activities <- copy(all_codes)
all_codes<-smash_strings(all_codes)
pretty_activities$order = seq(1, nrow(pretty_activities))
all_codes$order = seq(1, nrow(all_codes))

pretty_activities<-merge(pretty_activities, all_codes, by = "order")
pretty_activities = pretty_activities[, 2:3]
names(pretty_activities) = c("cleaned_activity", "sda_activity")

all_codes$order <- NULL

#--------------------------------------------
# Original method for calculating RSSH
#-------------------------------------------
ugaData$short_code = substring(ugaData$code, 1, 1)
orig_rssh = ugaData[short_code == "R"]

#Add in a way of calculating this when module starts with rssh? 

#--------------------------------------------------------------
# Merge on new CT codes by grant, and calculate new RSSH spending 
#--------------------------------------------------------------

#Subset to each grant from ugaData
c_taso = ugaData[grant_number == "UGA-C-TASO"]
stopifnot(length(unique(c_taso$fileName))==1)

m_taso = ugaData[grant_number == "UGA-M-TASO"]
stopifnot(length(unique(m_taso$fileName))==1)

m_mofped = ugaData[grant_number == "UGA-M-MoFPED"]
stopifnot(length(unique(m_mofped$fileName))==1)

h_mofped = ugaData[grant_number == "UGA-H-MoFPED"]
stopifnot(length(unique(h_mofped$fileName))==1)

#-----------------------------------------------------------------
#Run a quick calculation to see how many activities are merging 
c_taso_activities <- c_taso[, .(sda_activity)]
c_taso_activities <- c_taso_activities[!duplicated(c_taso_activities)]
rows_0 <- nrow(c_taso_activities) 
c_taso_activities = merge(c_taso_activities, c_taso_codes, id = "sda_activity", all.y = TRUE)
rows_1 <- nrow(c_taso_activities)

rows_1/rows_0 #40% of activities in uga-c-taso are now categorized as RSSH with these new codes. 
rm(c_taso_activities, rows_0, rows_1) 

#Run a quick calculation to see how many activities are merging 
m_taso_activities <- m_taso[, .(sda_activity)]
m_taso_activities <- m_taso_activities[!duplicated(m_taso_activities)]
rows_0 <- nrow(m_taso_activities) 
m_taso_activities = merge(m_taso_activities, m_taso_codes, id = "sda_activity", all.y = TRUE)
rows_1 <- nrow(m_taso_activities)

rows_1/rows_0 #69% of activities in uga-m-taso are now RSSH. 
rm(m_taso_activities, rows_0, rows_1) 
  
#-----------------------------------------------------------------  

c_taso = merge(c_taso, c_taso_codes, id = "sda_activity", all.y = TRUE)
m_taso = merge(m_taso, m_taso_codes, id = "sda_activity", all.y = TRUE)
m_mofped = merge(m_mofped, mofped_malaria_codes, id = "sda_activity", all.y = TRUE)
h_mofped = merge(h_mofped, mofped_hiv_codes, id = "sda_activity", all.y = TRUE)

#Remove budgets of "na" so they don't mess with calculations below 
c_taso = c_taso[!is.na(budget)]
m_taso = m_taso[!is.na(budget)]
m_mofped = m_mofped[!is.na(budget)]
h_mofped = h_mofped[!is.na(budget)]
#--------------------------------------------------------------
# Report on RSSH spending calculated in this new way 
#-------------------------------------------------------------- 

c_taso[, sum(budget)]
m_taso[, sum(budget)]
h_mofped[, sum(budget)]  
m_mofped[, sum(budget)]

nrow(c_taso[short_code == "R"])/nrow(c_taso)
nrow(m_taso[short_code == "R"])/nrow(m_taso)
nrow(m_mofped[short_code == "R"])/nrow(m_mofped)
nrow(h_mofped[short_code == "R"])/nrow(h_mofped)


#------------------------------------------------------------
#Check all data, not pre-split by grant, just in case there are differences. 
#-------------------------------------------------------------
new_rssh_not_split <- merge(ugaData, all_codes, id = "sda_activity", all.y = TRUE)
new_rssh_not_split = new_rssh_not_split[!is.na(new_rssh_not_split$budget)]

new_rssh = c_taso[, sum(budget)] + m_taso[, sum(budget)] + h_mofped[, sum(budget)] + m_mofped[, sum(budget)] #Only represents 4/5 grants in Uganda for this period. 

#------------------------------------------------------------------
#What percentage of the grant is now RSSH based on these new codes? 
#------------------------------------------------------------------

#1. When summing split by grant, need to make sure only include these grant for original RSSH. 
orig_rssh[grant_number %in% c('UGA-C-TASO', 'UGA-M-TASO', 'UGA-M-MoFPED', 'UGA-H-MoFPED'), sum(budget)]/new_rssh

#2. Include a calculation over all UGA 2018-2020 final budgets for comparison (only have detailed codes from CT for 4/5 grants.)
orig_rssh[, sum(budget)]/new_rssh_not_split[, sum(budget)]

#--------------------------------------------------------------------------
#Alternately, how many times did RSSH increase with new codes vs. with old? 
#--------------------------------------------------------------------------
#1. Broken down by grant
new_rssh/orig_rssh[grant_number %in% c('UGA-C-TASO', 'UGA-M-TASO', 'UGA-M-MoFPED', 'UGA-H-MoFPED'), sum(budget)]

#2. All data 
new_rssh_not_split[, sum(budget)]/orig_rssh[, sum(budget)]

#--------------------------------------------------------------------------
#What percentage of total UGA budget for this period is new vs. old RSSH? 
#--------------------------------------------------------------------------
paste0("New RSSH: ", round(new_rssh_not_split[, sum(budget)]/ugaData[, sum(budget)]*100, 2), "%")
paste0("Original RSSH: ", round(orig_rssh[, sum(budget)]/ugaData[, sum(budget)]*100, 2), "%")

#----------------------------------------------------------------------------------------------------

# byVars = c('grant_number', 'sda_activity', 'gf_module', 'short_code')
# new_rssh_by_grant = new_rssh_not_split[, .(budget = sum(budget)), by=byVars]
# orig_rssh_by_grant = orig_rssh[, .(orig_rssh= sum(budget)), by=byVars]
# total_by_grant = ugaData[, .(total_budget=sum(budget)), by=byVars]
# 
# total_by_grant = merge(total_by_grant, new_rssh_by_grant, id =byVars, all.x = TRUE)
# total_by_grant = merge(total_by_grant, orig_rssh_by_grant, id = byVars, all.x = TRUE)
# 
# total_by_grant[, (new_rssh/total_budget)*100, by = byVars]
# total_by_grant[, (orig_rssh/total_budget)*100, by = byVars]

# new_rssh_by_grant <- copy(new_rssh_not_split)
# new_rssh_by_grant<- merge(new_rssh_by_grant, pretty_activities, by = "sda_activity", all.X = TRUE)
# new_rssh_by_grant$rssh_type = ifelse(new_rssh_by_grant$short_code == "R", "direct RSSH", "CT RSSH")
# byVars = c('grant_number', 'cleaned_activity', 'gf_module', 'gf_intervention', 'rssh_type')
# new_rssh_by_grant = new_rssh_by_grant[, .(budget = sum(budget)), by=byVars]

new_rssh_not_split$rssh_type = ifelse(new_rssh_not_split$short_code == "R", "direct RSSH", "CT RSSH")
orig_rssh$rssh_type = ifelse(orig_rssh$short_code == "R", "direct RSSH", "CT RSSH")

new_rssh_not_split<- merge(new_rssh_not_split, pretty_activities, by = "sda_activity", all.x = TRUE)
orig_rssh<- merge(orig_rssh, pretty_activities, by = "sda_activity", all.x = TRUE)

byVars = c('grant_number', 'cleaned_activity', 'gf_module', 'gf_intervention', 'rssh_type')
orig_rssh = orig_rssh[, .(budget = sum(budget)), by=byVars]
new_rssh_not_split = new_rssh_not_split[, .(budget = sum(budget)), by=byVars]

rssh_total <- merge(new_rssh_not_split, orig_rssh, all = T)

write.csv(rssh_total, "C:/Users/elineb/Desktop/new_rssh_uga.csv", row.names = F)

#------------------------------------------------------------------------------
# AUTHOR: Emily Linebarger
# DATE: November 2018 
# PURPOSE: Calculations for 2018 UGA Country report. - RSSH Spending 
#-----------------------------------------------------------------------------
ugaData = total_resource[grant_period == "2018-2020" & data_source == "pudr"]
ugaData = ugaData[grant_number == "UGA-T-MoFPED"]
stopifnot(length(unique(ugaData$fileName))==1)
ugaData[gf_module == "Health management information system and monitoring and evaluation",  sum(budget)] 
ugaData[, sum(expenditure)/sum(budget)]


#The NTLP has made substantial progress in early 2018 towards HMIS systems integration, which is funded with an
#investment ofrepresents $2.6 million. By the end of Q2 of 2018, NTLPNTPL reported an absorption rate of 76%, with – spent on DHIS2/HMIS reporting integration.