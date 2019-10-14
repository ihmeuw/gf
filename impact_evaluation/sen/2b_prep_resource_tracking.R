# ----------------------------------------------------------
# AUTHOR: Francisco RIos-Casas
# PURPOSE: To prep resource tracking data to merge with outputs, outcomes for SENEGAL
# DATE: August 14 2019
# INSTRUCTIONS: set wd to the root of this repo
# ----------------------------------------------------------

# Set up
source('impact_evaluation/sen/set_up_r.R')

#------------------------------------
# Read in previously prepped datasets 
#------------------------------------

final_expenditures <- readRDS(expendituresFile)
who <- readRDS(whoFile)
fgh <- readRDS(fghFile)
fgh = fgh[, .(year, loc_name, disease, code, gf_module, gf_intervention, channel_agg, source, disbursement)]
setnames(fgh, old=c('gf_module', 'gf_intervention'), new=c('module', 'intervention'))

#------------------------------------
# Subset data and prep for merge
#------------------------------------

# Subset to only the columns we want from resource tracking database (TB grants in Senegal)
exp_subset <- final_expenditures[grant %in% c("SEN-Z-MOH", "SNG-T-PLAN", "SNG-T-PNT"), .(expenditure, start_date, code, disease, gf_module, gf_intervention)] # kept only Z grant & Tb-spec funding
setnames(exp_subset, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))

############################## modules ##################
# aggregate total expenditure to the modular level with proper code value and variable name
exp_mod = exp_subset[, c("code_m", "code_i") := tstrsplit(code, "_", fixed=TRUE)]
exp_mod = exp_mod[, .(expenditure=sum(expenditure, na.rm=TRUE)), by=c('start_date', 'disease', 'module', 'code_m')]

# delete rows TB care and prevention module and keep columns of interest
exp_mod = exp_mod[module!='TB care and prevention']

# rename code_m to code
setnames(exp_mod, old=c('code_m'), new = c('code'))

############################ tb care and prevention interventions #############
# subset intervention on TB care and prevention
tbcp = exp_subset[module=='TB care and prevention',.(expenditure, start_date, code, disease, intervention)]

# rename columns for merge
setnames(tbcp, old=c('intervention'), new=c('module'))

# Keep following interventions: "screening and diagnosis of disease", "treatment", "community care for TB"
tbcp <- tbcp[module %in% c('Community TB care delivery', 'Treatment', 'Case detection and diagnosis')]

# merge the two files together
exp_subset <- rbind(exp_mod, tbcp)

###### Other Sources of Health expenditure #############

# subset to only the columns we want from other develoment assistance for health
other_dah = fgh[(source != 'The Global Fund' & source != 'ghe') & loc_name=='SEN' & (disease == 'tb' | disease == 'hss' | disease == 'rssh'), 
                .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(year, loc_name, disease, code, module, intervention)]

# aggregate other_dah to the modular level with proper code value and variable name
other_dah = other_dah[, c("code_m", "code_i") := tstrsplit(code, "_", fixed=TRUE)]
other_dah = other_dah[, .(other_dah=sum(other_dah, na.rm=TRUE)), by=c('year', 'loc_name', 'disease', 'module', 'code_m')]
setnames(other_dah, old=c('code_m'), new=c('code'))

# delete rows TB care and prevention module and keep columns of interest
other_dah = other_dah[module!='TB care and prevention']

# create seperate dataset where we keep only certain tb care and prevention interventions: 
other_dah_tbcp = fgh[(source != 'The Global Fund' & source != 'ghe' & module== 'TB care and prevention') & loc_name=='SEN' & (disease == 'tb' | disease == 'hss' | disease == 'rssh'), 
                     .(other_dah = sum(disbursement, na.rm=TRUE)), by=.(year, loc_name, disease, code, module, intervention)]

# Keep following interventions: "screening and diagnosis of disease", "treatment", "community care for TB"
other_dah_tbcp <- other_dah_tbcp[intervention %in% c('Treatment', 'Case detection and diagnosis')]
other_dah_tbcp <- other_dah_tbcp[,.(year, loc_name, disease, code, intervention, other_dah)]
setnames(other_dah_tbcp, old = c('intervention'), new = c('module'))

# merge together
other_dah <- rbind(other_dah, other_dah_tbcp)

# subset domestic government health expenditure (fgh) on TB in senegal
ghe = who[loc_name == 'sen' & indicator=='domestic_ghe_tb', .(ghe = sum(expenditure, na.rm = TRUE)), 
          by = .(year)]

# add in information on domestic spending on TB from the financing global health databases at IHME
# this data comes from
domesticdata <- data.table(year=c(2005, 2006, 2008, 2009),
                           ghe=c(171438,336223,499203,462615))
# merge domestic spending with other TB spending
ghe <- rbind(ghe, domesticdata, fill=TRUE)

# Split data into quarters
n_years <- (2018-1990)+1 #This is the range we have data for. 
quarters <- data.table(year=rep(1990:2018, 4))[order(year)]
quarters[, quarter:=rep(1:4, n_years)]

#Split each data set out by quarter
other_dah = merge(quarters, other_dah, by='year', all.x = TRUE, allow.cartesian=TRUE)
other_dah[, other_dah:=other_dah/4]

ghe = merge(quarters, ghe, by='year', all.x = TRUE, allow.cartesian = TRUE)
ghe[, ghe:=ghe/4]
ghe[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
ghe[, date:=year+quarter]

exp_subset[, quarter:=quarter(start_date)]
exp_subset[, year:=year(start_date)]
exp_subset[, start_date:=NULL]

#Create date variable
exp_subset[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
exp_subset[, date:=year+quarter]
other_dah[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
other_dah[, date:=year+quarter]

#Cast data wide 
exp_wide = dcast(exp_subset, date~code, value.var=c('expenditure'), fun.aggregate = sum)
frame = data.table(date=seq(1990, 2020, by=.25))
exp_wide = merge(frame, exp_wide, by='date', all.x=TRUE)
for(v in names(exp_wide)) exp_wide[is.na(get(v)), (v):=0]

other_dah_wide = dcast(other_dah, date~code, value.var=c('other_dah'), fun.aggregate = sum)
frame = data.table(date=seq(1990, 2020, by=.25))
other_dah_wide = merge(frame, other_dah_wide, by='date', all.x=TRUE)
for(v in names(other_dah_wide)) other_dah_wide[is.na(get(v)), (v):=0]

#Set column names for merge 
names = colnames(exp_wide[, 2:ncol(exp_wide)])
names <- paste("exp", names, sep = "_")
colnames(exp_wide) <- c('date', names)

names = colnames(other_dah_wide[, 2:ncol(other_dah_wide)])
names <- paste("other_dah", names, sep = "_")
colnames(other_dah_wide) <- c('date', names)

# Merge both files together 
rt_wide <- merge(other_dah_wide, exp_wide, by=c('date'))

###############################
# Limitation: not sure what these datasets refer to
#############################

#Add on GHE and OOP as control variables 
ghe = ghe[, .(date, ghe)]
rt_wide = merge(rt_wide, ghe, by='date', all.x = TRUE)
# oop = oop[, .(date, oop)]
# rt_wide = merge(rt_wide, oop, by='date', all.x = TRUE)

#------------------------------
# Save output file
#------------------------------

# remove columns we don't want (R1 Procurement and supply chain management systems, R7 community responses and systems, R99 unspecified RSSH, T4 Program Management, T99 Unspecified TB Spending)
rt_wide = rt_wide[,c("exp_R1", "exp_R7", "other_dah_R99", "exp_T4", "other_dah_T99", "exp_R99"):=NULL]

# save
saveRDS(rt_wide, outputFile2b)
archive(outputFile2b)
