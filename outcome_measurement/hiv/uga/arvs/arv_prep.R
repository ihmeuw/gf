# ARV stockouts by facility - data prep
# Preps ARV Stockout data from CDC Option B+ Dashboard: 
# http://dashboard.mets.or.ug/jasperserver/slevel/KPIs/107_BPlus_Data_Per_Facility
#
# Caitlin O'Brien-Carelli
# 10/1/2019

# Data are updated through September 1, 2019
# ----------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)

# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/arv_data/')

# working directory to aggregate
setwd(dir)

# output directory for the prepped data 
OutDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# ----------------------
# read in the files 

i = 1
files = list.files('./', recursive=TRUE)

for (f in files) {
  
  # import the csv and convert to a data table
  arv_data = fread(paste0(dir, f), skip=1)
  
  # ----------------------
  # check if the anticipated variables are included
  names = names(arv_data)
  ant_names = c("Health Facility", "ART Accredited",                                            
  "V3", "a. Total ANC1\nvisits",                                     
  "b. Total No of mothers tested at 1st ANC visit\n","c. Total No HIV Tested Positive at 1st ANC visit",          
  "d. Total no with known HIV+ by the 1st ANC visit", "e. Total initiating Option B+ at 1st ANC visit",            
  "f. Total no of mothers on ART treatment by 1st ANC visit", "g. Total missed appointments",                              
  "h. Has Test Kits stockout", "i. Has ARVs stockout",                                      
  "1. Proportion of ANC1 with unknwon status tested", "2. Proportion of HIV+ women not yet on ART initiated",      
  "3. Proportion of women tested positive out of those tested")
  
  # check if any variable names are not included in the data 
  if (length(names[!(ant_names %in% names)])!=0) print(paste0("Crap! File ", f, " has some unanticipated variables!")) 
  
  # ----------------------
  # create useful variable names 
  setnames(arv_data, ant_names,
           c("facility", "art", "district", "anc_visits", 
             "mothers_tested_anc1", "hiv_pos_anc1", "known_hiv_anc1", "option_b_plus_anc1",
             "moms_on_art_by_anc1", "missed_appts", "tests", "arv", 
             "prop_anc1_tested", "prop_hiv_pos_not_yet_on_art", "prop_hiv_pos_of_tested"))
  
  # ----------------------
  # a number of rows are duplicated identically in 2018/19
  # 2013 - 2017 data do not contain this error
  # this appears to be a system error - these entries are IDENTICAL
  # drop identical entries before dropping entries that are identical except for the partner
  arv_data = arv_data[!(duplicated(arv_data))]
  
  #----------------------------------
  # remove 'district' from district names
  arv_data[ , district:=unlist(lapply(strsplit(arv_data$district, '\\s' ), '[', 1))]
  
  # change the names of districts to match the shape file 
  arv_data[district=="Bunyangabu", district:="Kabarole"]
  arv_data[district=="Kagadi", district:="Kibaale"]
  arv_data[district=="Kakumiro", district:="Kibaale"]
  arv_data[district=="Namisindwa", district:="Manafwa" ]
  arv_data[district=="Omoro", district:="Gulu"]
  arv_data[district=="Pakwach", district:="Nebbi"]
  arv_data[district=="Sembabule", district:="Ssembabule"]
  arv_data[district=="Kikuube", district:="Hoima"]
  arv_data[district=="Kwania", district:="Apac"]
  
  # ----------------------
  
  # ----------------------
  # prep facilities
  
  # ----------------------
  # fix facilities in which the names are poorly formatted
  
  # there is one facility that has the same name in two districts
  # change the name so they will not be aggregated (they are distinct)
  arv_data[facility=='Bugaya HC III ( Buvuma )', facility:='Bugaya Buvuma HC III']
  
  # a few facilities contain details in parentheses - eliminate
  arv_data[facility=='Ococia (Orungo) St. Clare', facility:='Ococia Orungo St. Clare']
  arv_data[facility=='Kanyaryeru (Lake Mburo)', facility:='Kanyaryeru Lake Mburo']
  arv_data[facility=='Mutolere (St. Francis)', facility:='Mutolere St. Francis']
  
  # ----------------------
  # create a variable for implementing partner
  arv_data[ , impl_partner:=unlist(lapply(strsplit(arv_data$facility, '\\(' ), '[', 2))]
  arv_data[ , impl_partner:=trimws(gsub(")", "", impl_partner))]
  
  # a few facilities do not include the name Reach Out in parentheses
  arv_data[grepl("Reach Out", facility) & is.na(impl_partner), impl_partner:='Reach Out']
  arv_data[ , facility:=trimws(gsub("Reach Out - ", "", facility))]
  
  # some facilities have the district in parentheses
  # drop these - they are not implementing partners
  # there are no repeat facilities distinguished by district
  arv_data[impl_partner==district, impl_partner:=NA]
  
  # remove the implementing partner from the facility name
  # some facilities are distinguished by a type in parentheses
  # for example, there is a "pabbo (govt)" HC III and a "pabbo (NGO)" HC III
  # these should be left with names intact to distinguish distinct values
  arv_data[ , facility_new:=unlist(lapply(strsplit(arv_data$facility, '\\(' ), '[', 1))]
  arv_data[ , facility_new:=trimws(facility_new, which='right')]
  
  # replace facility names to names without implementing partner in parentheses
  arv_data[!grepl("II", arv_data$impl_partner) & !grepl("HC", arv_data$impl_partner), facility:=facility_new]
  arv_data[ ,facility_new:=NULL]
  
  #-----------------------------------
  # create a variable that lists all implementing partners
  # allows you to drop individual rows for each
  
  # for the facilities without an implementing partner, replace with missing
  arv_data[grepl("II", arv_data$impl_partner) | grepl("HC", arv_data$impl_partner),
           impl_partner:=NA]
  
  # now create a list of implementing partners within a single field
  arv_data[!is.na(impl_partner), partners:= paste0(impl_partner, collapse=", "), by="facility"]
  arv_data[ ,impl_partner:=NULL]
  setnames(arv_data, 'partners', 'impl_partners')
  
  #------------------------------------
  # differences in implementing partners make it challenging to eliminate duplicate entries
  # all facilities of the same name should have the same list of partners
  
  # create a list of unique implementing partners by facility for later use
  partners = arv_data[!is.na(impl_partners), .(impl_partners2=unique(impl_partners)), by=facility]
  arv_data = merge(arv_data, partners, by='facility', all.x=TRUE)
  arv_data[is.na(impl_partners) & !is.na(impl_partners2), impl_partners:=impl_partners2]
  arv_data[ , impl_partners2:=NULL]
  
  #-----------------------------------
  # drop duplicates
  # duplicates are only because of implementing partners 

  # drop duplicate entries a second time (now that implementing partners are removed)
  arv_data = arv_data[!(duplicated(arv_data))]
  
  # test code to look for facilities with duplicate entries
  # some facilities have two entries per facility, but one with all missing values
  # remove duplicate facility names with all missing values
  facilities =  arv_data[duplicated(facility), unique(facility)]
  arv_data = arv_data[!(facility %in% facilities & is.na(anc_visits))] 
  
  # there should now not be any duplicate facilities
  if (arv_data[duplicated(facility), length(unique(facility))]!=0) print(paste("Duplicate facilities in file:", f))
  
  #-----------------------------------
  # format the names of facilities to be less silly
  
  arv_data[ , facility:=gsub("REGIONAL REF", "Regional Referral Hospital", facility)]
  arv_data[ , facility:=gsub("HOSPITAL", "Hospital", facility)]
  arv_data[ , facility:=gsub("CLINIC", "Clinic", facility)]   
  arv_data[ , facility:=gsub("GOVT", "", facility)]    

  #----------------------------------
  
  # ----------------------
  # add facility level
  arv_data[ ,facility1:=tolower(facility)]

  # add all major facility levels
  arv_data[(grepl(pattern="ii", facility1)) & !(grepl(pattern="\\siii", facility1)), level:='HC II'] 
  arv_data[(grepl(pattern="iii", facility1)), level:='HC III'] 
  arv_data[grepl(pattern="\\siv",facility1), level:='HC IV']
  arv_data[grepl(pattern="hospital", facility1) | grepl(pattern="regional ref", facility1) , level:='Hospital']
  arv_data[grep("national", facility1), level:='Hospital']
  
  # add taso clinincs
  arv_data[grepl(pattern="taso", facility1), level:='TASO']

  # set all other facilities to other
  arv_data[is.na(level), level:='Other']
  arv_data[ ,facility1:=NULL]
  
  # ----------------------  
  # fix TASO clinic names - some include -NR at the end
  arv_data[level=='TASO', facility:=gsub('-NR', "", facility)]
  arv_data[level=='TASO', facility:=gsub('- NR', "", facility)]
  
  # check white space is trimmed from all names
  arv_data[ , facility:=trimws(facility, which='both')]
  
  # ----------------------
  # convert Y/Ns to logicals
  # do not do as a ifelse because of missing data and variable types
  arv_data[art=='Y', art_site:=TRUE]
  arv_data[art=='N', art_site:=FALSE]
  
  arv_data[tests=='Y', test_kits:=TRUE]
  arv_data[tests=='N', test_kits:=FALSE]
  
  arv_data[arv=='Y', arvs:=TRUE]
  arv_data[arv=='N', arvs:=FALSE]
  
  # ----------------------

  # ----------------------
  # create a week variable
  week = strsplit(f, '\\s')[[1]][2]
  if (substr(week, 3, 3)!=')') week = substr(week, 2, 3)
  if (substr(week, 3, 3)==')') week = substr(week, 2, 2)
  arv_data[ , week:=as.numeric(week)]
  
  # add a year variable
  arv_data[ , year:=as.numeric(strsplit(f, '/')[[1]][1])]

  # generate a date for the start date of the week
  arv_data[ , day:=(week*7)-6]
  arv_data[ , date:=strptime(paste(year, day), format="%Y %j")] # warnings are ok
  arv_data[ , date:=as.Date(date)]
  
  # generate a monthly date
  arv_data[ , month:=strsplit(as.character(date), '-')[[1]][2]]
  arv_data[ , month:=as.Date(paste0(year, '-', month, '-01'), format='%Y-%m-%d')]
  
  # drop week and day, as well as previously used string variables
  arv_data[ ,c('day', 'tests', 'arv', 'art', 'week'):=NULL]
  
  #-----------------------
  # final check on duplicate entries and facility names 
  if (nrow(arv_data[duplicated(arv_data)])!=0) print(paste("Duplicate rows in file:", f))
  if (nrow(arv_data[duplicated(arv_data$facility)])!=0) print(paste("Duplicate facilities in file:", f))
  
  #-----------------------
  # bind each week of data to the full data to create a complete data set
  if(i==1) full_data = arv_data
  if(i>1) full_data = rbind(full_data, arv_data)
  i = i+1
  
}

#---------------------------
# drop out the 37 facilities that never reported 

missing = full_data[ , .(check=all(is.na(arvs)), check_t=all(is.na(test_kits))), by=facility]
missing = missing[check==TRUE & check_t==TRUE]
full_data = full_data[!(facility %in% missing$facility)]

#------------------------------------
# merge in the regions
regions = fread(paste0(j, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
regions = regions[ ,.(map_region = region10_name, district = dist112_name)]
regions = regions[!duplicated(district)]
full_data = merge(full_data, regions, by='district', all.x=T)

# check that every district has a region associated with it
full_data[is.na(map_region)]

#-------------------------------------
# format regions for tables 
full_data[ ,region:=map_region]
full_data[ ,region:=gsub("_", " ", region)]

#-------------------------------------
# final fixes to implemnting partners

# Walter Reed is sometimes cut off
full_data[impl_partners=='Walter', impl_partners:='Walter Reed']

# test variable to seach for incomplete entries
full_data[ , test:=paste0(impl_partners, " ")]

# differ only by the -
full_data[test=='RHITES- ', impl_partners:='RHITES']
full_data[test=='URC- ', impl_partners:='URC']
full_data[test=='IDI- ', impl_partners:='IDI']
full_data[grepl("RHITES- ", test), impl_partners:=gsub("RHITES-", "RHITES", impl_partners)]
full_data[grepl("RHITES-,", test), impl_partners:=gsub("RHITES-,", "RHITES,", impl_partners)]

#-------------------------------------
# drop the weeks with no reporting

# some of these are not present on the site
# just downloaded the template
missing_weeks = full_data[ ,.(test= sum(anc_visits, na.rm=T)), by=date]
missing_weeks = missing_weeks[test==0]$date

# drop out these empty weeks
full_data = full_data[!(date %in% missing_weeks)]

#-------------------------------------
# save the output

# get the minimum and maximum year and add to the file name for export
min_year = full_data[ , min(year(date))]
max_year = full_data[ , max(year(date))]

# save the full data as a data table
saveRDS(full_data, paste0(OutDir, 'prepped_data/arv_stockouts_full_', min_year, '_', max_year, '.rds'))

#-------------------------------------
# subset to the relevant variables

# subset the data to the variables for regressions and descriptives
sub_data = full_data[ ,.(facility, level, art_site,  district, region, map_region,
                         date, month, year, anc_visits, test_kits, arvs,
                         impl_partner=impl_partners)]

# save a subset of the data just for stockout analysis 
saveRDS(sub_data, paste0(OutDir, 'prepped_data/arv_stockouts_', min_year, '_', max_year, '.rds'))

#----------------------------


