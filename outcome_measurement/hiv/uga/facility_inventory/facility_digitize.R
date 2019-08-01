# Digitize the Uganda Health Facility Inventory
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/22/2019
# ----------------------------------------------

# ------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(openxlsx)
library(zoo)
library(tools)
# ------------------------------
# load the data 

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/facility_masterlist/')

dt = data.table(read.xlsx(paste0(dir, 'National Health Facility Master List 2018.xlsx')))

# ------------------------------
# subset to the start of the facilities

# drop the first 929 rows
dt = tail(dt, -929)
dt = head(dt, n=7922) # drop out the last six rows that are just test

# rename columns to be easier to manipulate
setnames(dt, c('OYAM', 'NAPAK'), c('X1', 'X2'))

# drop out the rows that just have the document title
dt = dt[!(is.na(X1) & is.na(X2) & grepl("HEALTH", X5))]

# drop useless rows with facility totals
dt = dt[!(grepl("EALTH ", X1) | grepl("EALTH ", X2) | grepl("EALTH ", X3) | grepl("EALTH ", X4))]

# drop columns with only missing values
dt[ , c('X11', 'X12', 'X13', 'X14'):=NULL]

#-------------------------------
# facility names

levels = c('Medical', 'HC II', 'HC III', 'HC IV', 'Hospital', 'Clinic', 'SC', 'NRH', 'RRH')

# X1 and X2 do not contain facility names
for (l in levels) {
   more_than_l = nchar(l)
   dt[grepl(l, X3) & more_than_l < nchar(X3), facility_name1:=X3] 
   dt[facility_name1=='HC III', facility_name1:=NA]}

for (l in levels) {
  more_than_l = nchar(l)
  dt[grepl(l, X4) & more_than_l < nchar(X4), facility_name2:=X4]
  dt[facility_name2=='HC III', facility_name2:=NA]}

dt[ , facility:=facility_name1]
dt[is.na(facility), facility:=facility_name2]
dt[ ,c('facility_name1', 'facility_name2'):=NULL]

#-------------------------------
# add districts

# grab the districts
dt[grepl("DISTRICT", X1), district:=tolower(X1)]
dt[grepl("DISTRICT", X2), district:=tolower(X2)]
dt[ , district:=na.locf(district)]
dt[ , district:=toTitleCase(district)]

# strange set of rows
dt = dt[is.na(X9) & is.na(X10)]
dt[ ,c('X9', 'X10'):=NULL]

#--------------------------------------
# add the subregion

# search for the 11 regions in Uganda
regions = c('CENTRAL 1', 'CENTRAL 2', 'KAMPALA', 'EAST CENTRAL', 'MID EASTERN',
            'NORTH EASTERN', 'KARAMOJA', 'MID NORTHERN', 'WEST NILE',
            'MID WESTERN', 'SOUTH WESTERN')
dt[X1 %in% regions | X2 %in% regions, sub_region:=tolower(X1)]
dt[(X1 %in% regions | X2 %in% regions) & is.na(sub_region), sub_region:=tolower(X2)]

# fill in all the regions
dt[ , sub_region:=na.locf(sub_region)]
dt[!is.na(sub_region), sub_region:=toTitleCase(sub_region)]

# check that no districts were assigned to more than one sub region 
more_than_one = dt[ ,.(sub_region=unique(sub_region)), by=district]
more_than_one[ , regions:=.N, by=district]
more_than_one[2 <= regions]

# these are errant values - drop these rows
dt = dt[!(sub_region=='Mid Western' & district=='Zombo District')]
dt = dt[!(sub_region=='West Nile' & district=='Soroti District')]

dt[district=='Buikwe District', sub_region:='Central 1']
dt[district=='Soroti District', sub_region:='North Eastern']
dt[district=='Zombo District', sub_region:='West Nile']

#--------------------------------------
# check that the districts are in the correct regions

checks = dt[ ,.(sub_region=unique(sub_region)), by=district]

# ------------------------------
# add the region

dt[sub_region=='Central 1' | sub_region=='Central 2' | sub_region=='Kampala', region:='Central']
dt[sub_region=='East Central' | sub_region=='Mid Eastern' | sub_region=='North Eastern', region:='Eastern']
dt[sub_region=='Karamoja' | sub_region=='Mid Northern' | sub_region=='West Nile', region:='Northern']
dt[sub_region=='Mid Western' | sub_region=='South Western', region:='Western']

#--------------------------------------
# county

# grab the counties
dt[grepl("\\sHSD", X1) & X1!='HSD', hsd:=tolower(X1)]
dt[grepl("HSD", X2) & is.na(hsd) & X2!='HSD', hsd:=tolower(X2)]

# these rows are purely labels - drop out at the lowest level of geographic split
dt[  , check1:=paste0(X1, X2, X3, X4, X5)]
dt[ ,  check2:=toupper(check1)]
dt[ , label:=(check1==check2)]
dt = dt[label!=TRUE]
dt[ ,c('check1', 'check2', 'label'):=NULL]

# format the hsd names
dt[!is.na(hsd), hsd:=toTitleCase(hsd)]
dt[!is.na(hsd), hsd:=gsub("Hsd", "HSD", hsd)]

# fix a few small errors
dt[is.na(hsd) & grepl('HSSD', X2), hsd:=X2]
dt[is.na(hsd) & grepl(')HSD', X1), hsd:=X1]
dt[!is.na(hsd), hsd:=gsub("HSSD", "HSD", hsd)]
dt[!is.na(hsd), hsd:=gsub("HSD", " HSD", hsd)]

#--------------------------------------
# fix a few facilities

dt[(X4=='HC II' | X4=='Clinic') & is.na(facility), facility:=X3]

# ------------------------------
# authority and ownership

# ------------------------------
# generate authority 

# create a vector of potential authorities
auths = c('AIC', 'CAFU', 'CBO','MOES','MOH','NGO','PRIVATE', 
'SDA','SOS','TASO', 'UCBHCA', 'UCMB','UMMB','UNHCR', 
'UOMB','UPDF','UPF','UPMB', 'UPS')

# eliminate obvious errant values
dt = dt[X5!="Regional Referral Hospital" | X5!='Special Clinic']

# create the authority variable from main variable and shifted over
dt[toupper(X5) %in% auths, authority:=X5]
dt[toupper(X6) %in% auths, authority:=X6]

# drop rows without authority - these are not facilities
dt = dt[!is.na(authority)]
#--------------------------------------
# generate ownership

own = c('GOVT', 'PFP', 'PNFP')
dt[toupper(X6) %in% own, ownership:=X6]
dt[toupper(X7) %in% own, ownership:=X7]
dt[toupper(X8) %in% own, ownership:=X8]

# every value in these columns is captured in authority and ownership
dt[ ,c('X6','X7', 'X8'):=NULL]

#--------------------------------------
# facility levels 

levels = c('Medical', 'HC II', 'HC III', 'HC IV', 'Hospital', 'Clinic', 'SC', 'NRH', 'RRH', 'RH')

dt[X4 %in% levels, level:=X4]
dt[X5 %in% levels, level:=X5]
dt[is.na(level) & (grepl('clinic', tolower(X4))), level:='Clinic']
dt[is.na(level) & (grepl('clinic', tolower(X5))), level:='Clinic']

#--------------------------------------
# fix the few remaining hsds and facilities

# input the remaining facility names
dt[!is.na(X1) & !is.na(X2) & is.na(facility), facility:=X3]
dt = dt[!(is.na(facility) & X4 %in% levels)] # these are geographic units
dt[is.na(facility), facility:=X4]
dt = dt[!is.na(facility)]

# input the remaining hsds
dt[is.na(hsd) & X1=='*' & X3=='Kirinya HC II', hsd:='Bukooli Central HSD']
dt[is.na(hsd) & X1=='*', hsd:='Bukooli North HSD']

# final missing hsds
dt[X2=='', hsd:='']
dt[X2=='', hsd:='']
dt[X2=='', hsd:='']
dt[X2=='', hsd:='']
dt[X2=='', hsd:='']
dt[X2=='', hsd:='']
dt[X2=='', hsd:='']
dt[X2=='', hsd:='']
dt[X2=='', hsd:='']
dt[X2=='', hsd:='']


#--------------------------------------
# check that

# check that no hsds were assigned to more than one district
more_than_one = dt[ ,.(hsd=unique(hsd)), by=district]
more_than_one[ , hsds:=.N, by=hsd]
View(more_than_one[!is.na(hsd) & 2 <= hsds]) # 22 hsds occur in more than one district




dt[ ,index:=seq(1:nrow(dt))]












# ------------------------------
# county and municipality

# grab the counties
dt[grepl("COUNTY", X1), county:=tolower(X1)]
dt[grepl("COUNTY", X2) & is.na(county), county:=tolower(X2)]
dt[ , county:=na.locf(county)]
dt[ , county:=toTitleCase(county)]

# drop out the rows with just district names
dt = dt[!((grepl("COUNTY", X1) | grepl("COUNTY", X2)) & is.na(X3)==T & is.na(X4)==T & is.na(X5)==T & is.na(X6)==T & is.na(X7)==T)]

# remove municipality
dt = dt[!(grepl("MUNICIPALITY", X1) | grepl("MUNICIPALITY", X2))]


#--------------------------------------
# get facility level

levels = c('HC II', 'HC III', 'HC IV', 'Hospital', 'Clinic', 'SC', 'NRH', 'RRH', 'RH')

dt[X4 %in% levels, level:=X4]
dt[(X5 %in% levels) & is.na(level), level:=X5]

# drop errant values with no level 
dt[  , test_upper:=toupper(X1)]
dt = dt[X1!=test_upper]
dt[ ,test_upper:=NULL]

# check X2
dt[is.na(level) & grepl('HC II', X2), level:='HC II']
dt[is.na(level) & grepl('HC III', X2), level:='HC III']
dt[is.na(level) & grepl('HC IV', X2), level:='HC IV']
dt[is.na(level) & grepl('Hospital', X2), level:='Hospital']
dt[is.na(level) & grepl('Clinic', X2), level:='Clinic']

# some clinics are spelled wrong
dt[ , lower_test:=tolower(X4)]
lower_levels = tolower(levels)
dt[(lower_test %in% lower_levels) & is.na(level), level:='Clinic'] # all are clinics
dt[ , lower_test:=NULL]
dt = dt[!is.na(level)] # all of these values are errors

#--------------------------------------
dt[ ,unique(facility)]





#--------------------------------------


dt[grepl("\\d", dt$X2)]




dt[is.na(X2)==T & is.na(X3)==T & is.na(X4)==T & is.na(X5)==T & is.na(X6)==T & is.na(X7)==T & is.na(X8)==T & is.na(X9)==T & is.na(X10)==T & is.na(X11)==T, label:=T]

# add the district name

dt[grep('District', HF), district_test:=trimws(HF)]
dt[ , check:=sapply(strsplit(district_test, " "), length)]
dt[check > 3, district_test:=NA]
dt[ , fix:=(check > 3)]

# add the ones that accidentally concatenated
dt[fix==T, district_replace:=trimws(unlist(lapply(str_split(HF, "District"), "[", 1)))]
dt[fix==T & sapply(strsplit(district_replace, " "), length)==1, district_test:=paste0(district_replace, " District")]
dt[ ,district_test:=(gsub("\\s\\s", "\\s", district_test))]
dt[ ,district_test:=(gsub("\\s\\s\\s", "\\s", district_test))]

# fix spellings
dt[grep("Luwero", district_test), district_test:='Luweero District']

# ------------------------------
# subset the data data table and incorporate the district names
dt = dt[-1]

# fix the insane variable name and delete
setnames(dt, "HEALTH.FA.CILIT.Y.INVENT.OR.Y.-.UGAND.A.|.87", 'Xother')
dt[ , Xother:=NULL]

# drop out formatting
dt = dt[HF!="#"]

# drop out values that are numbering by excel
numbers= c(1:1000)
numbers = as.character(numbers)
dt[HF %in% numbers, HF:=NA]
dt[ ,X2:=NULL] # always empty


#------------------------------------
# determine which districts are missing

# set working directory
setwd(paste0(j, '/Project/Evaluation/GF/mapping/uga/'))

# uganda shapefile
DistMap = shapefile('uga_dist112_map.shp')

# create a list of districts
districts = paste(DistMap@data$dist112_na, "District")
dt_districts = dt[ ,unique(district_test)]

districts[!districts %in% dt_districts]

dt[!(district_test %in% districts)]



dt[test > 3, new_dist:=trimws(unlist(lapply(strsplit(district_test, "District"), "[", 1)))]
dt[test > 3, new_dist2:=word(new_dist, start=1L, end=1L)]



dt[ , test:=word(district_test, start=2L, end=2L)]
dt[!is.na(test) & test!='District']




dt[ ,.(HF, )]


dt[ , district_test:=NULL]

# create subdistricts
setnames(dt, 'X3', 'sub_district')
# ------------------------------
# eliminate the code columns that were shifted

# drop the variables at the end that were simply shifted over
dt[!is.na(X60), X59:=X60]
dt[!is.na(X61), X59:=X61]
dt[!is.na(X62), X59:=X62]
dt[!is.na(X63), X59:=X63]
dt[!is.na(X64), X59:=X64]

# all of these rows are solely labels
dt = dt[X59!='HSDT Code']

# replace the NHPI codes that were shifted over
dt[is.na(X48) & !is.na(X49), X48:=X49]
dt[is.na(X48) & !is.na(X50), X48:=X50]
dt[is.na(X48) & !is.na(X51), X48:=X51]
dt[is.na(X48) & !is.na(X52), X48:=X52]
dt[is.na(X48) & !is.na(X53), X48:=X53]
dt[is.na(X48) & !is.na(X54), X48:=X54]
dt[is.na(X48) & !is.na(X55), X48:=X55]
dt[is.na(X48) & !is.na(X56), X48:=X56]
dt[is.na(X48) & !is.na(X57), X48:=X57]
dt[is.na(X48) & !is.na(X58), X48:=X58]

# rename the accurate columns
setnames(dt, c('X48', 'X59'), c('nhpi_code', 'hsdt_code'))

# X45 is always missing
dt[ ,c('X45', 'X23', 'X24'):=NULL]
# ------------------------------
# fix facility level

setnames(dt, 'X22', 'level')

# two straigghtforward variables containing only level
dt[is.na(level) &!is.na(X25), level:=X25]
dt[is.na(level) &!is.na(X26), level:=X26]

# create a variable for the rows that contain level
dt[grepl('HC', dt$X27) | grepl('Hosp', X27) | grepl('Cli', X27) | grepl('I', X27), level_mark:=TRUE]
dt[grepl('HC', dt$X28) | grepl('Hosp', X28) | grepl('Cli', X28) | grepl('I', X28), level_mark:=TRUE]
dt[grepl('HC', dt$X29) | grepl('Hosp', X29) | grepl('Cli', X29) | grepl('I', X29), level_mark:=TRUE]
dt[grepl('HC', dt$X30) | grepl('Hosp', X30) | grepl('Cli', X30) | grepl('I', X30), level_mark:=TRUE]
dt[grepl('HC', dt$X31) | grepl('Hosp', X31) | grepl('Cli', X31) | grepl('I', X31), level_mark:=TRUE] #X32 does not contain levels
dt[grepl('HC', dt$X33) | grepl('Hosp', X33) | grepl('Cli', X33) | grepl('I', X33), level_mark:=TRUE]
dt[grepl('HC', dt$X34) | grepl('Hosp', X34) | grepl('Cli', X34) | grepl('I', X34), level_mark:=TRUE]
dt[grepl('HC', dt$X35) | grepl('Hosp', X35) | grepl('Cli', X35) | grepl('I', X35), level_mark:=TRUE]
dt[grepl('HC', dt$X36) | grepl('Hosp', X36) | grepl('Cli', X36) | grepl('I', X36), level_mark:=TRUE]
dt[grepl('HC', dt$X37) | grepl('Hosp', X37) | grepl('Cli', X37) | grepl('I', X37), level_mark:=TRUE]
dt[grepl('HC', dt$X38) | grepl('Hosp', X38) | grepl('Cli', X38) | grepl('I', X38), level_mark:=TRUE]

dt[is.na(level_mark), level_mark:=FALSE]

# replace with accurate levels from other variables
dt[is.na(level) & level_mark==T & !is.na(X27), level:=X27]
dt[is.na(level) & level_mark==T & !is.na(X28), level:=X28]
dt[is.na(level) & level_mark==T & !is.na(X29), level:=X29]
dt[is.na(level) & level_mark==T & !is.na(X30), level:=X30]
dt[is.na(level) & level_mark==T & !is.na(X31), level:=X31]
dt[is.na(level) & level_mark==T & !is.na(X33), level:=X33] #X32 does not contain levels
dt[is.na(level) & level_mark==T & !is.na(X34), level:=X34]
dt[is.na(level) & level_mark==T & !is.na(X35), level:=X35]
dt[is.na(level) & level_mark==T & !is.na(X36), level:=X36]
dt[is.na(level) & level_mark==T & !is.na(X37), level:=X37]
dt[is.na(level) & level_mark==T & !is.na(X38), level:=X38]
dt[ ,level_mark:=NULL]

# fix some of the worst mistake
dt[level=='Hospi', level:='Hospital']
dt[grepl("II", level) & !grepl("III", level),level:='HC II']
dt[grepl("Cl", level) & !grepl("I", level),level:='Clinic']
dt[level=='e III' | level=="III", level:="HC III"]
dt[grepl("MOH", level) , level:=trimws(unlist(lapply(str_split(level, "MOH"), "[", 1)))]
dt[!is.na(level) & (nchar(level)==1 | nchar(level) > 10), level:=NA]
dt[level=='SC', level:=NA]

# add strange levels from concatenated variables
dt[is.na(level) & grepl('Hospital', HF), level:='Hospital']
dt[is.na(level) & grepl('HC II', HF), level:='HC II']
dt[is.na(level) & grepl('HC III', HF), level:='HC III']
dt[is.na(level) & grepl('HC IV', HF), level:='HC IV']

# less reliable but final variables
dt[is.na(level) & grepl('Hospital', X8), level:='Hospital']
dt[is.na(level) & grepl('HC II', X8), level:='HC II']
dt[is.na(level) & grepl('HC III', X8), level:='HC III']
dt[is.na(level) & grepl('HC IV', X8), level:='HC IV']

dt[is.na(level) & grepl('Hospital', X10), level:='Hospital']
dt[is.na(level) & grepl('HC II', X10), level:='HC II']
dt[is.na(level) & grepl('HC III', X10), level:='HC III']
dt[is.na(level) & grepl('HC IV', X10), level:='HC IV']

# ------------------------------
# get the facility names from a variety of columns

setnames(dt, 'X8', 'facility')
dt[is.na(facility) & !is.na(X9), facility:=X9]
dt[!is.na(facility) & !is.na(X10) & nchar(facility)==1 , facility:=paste0(facility, X10)]
dt[is.na(facility) & !is.na(X6), facility:=X6]
dt[is.na(facility) & !is.na(X7), facility:=X7]
dt[is.na(facility) &!is.na(X10), facility:=X10]
dt[is.na(facility) &!is.na(X11), facility:=X11]
dt[is.na(facility) &!is.na(X12), facility:=X12]
dt[is.na(facility) &!is.na(X13), facility:=X13]
dt[is.na(facility) &!is.na(X14), facility:=X14]
dt[is.na(facility) &!is.na(X15), facility:=X15]
dt[is.na(facility) &!is.na(X16), facility:=X16]
dt[is.na(facility) &!is.na(X17), facility:=X17]
dt[is.na(facility) &!is.na(X18), facility:=X18]
dt[is.na(facility) &!is.na(X19), facility:=X19]
dt[is.na(facility) &!is.na(X20), facility:=X20]

# facility formatting is weird
dt[ ,facility:=unlist(trimws(facility))]

# fill in the final facility names
dt[is.na(facility) & !is.na(HF), test:=trimws(unlist(lapply(str_split(HF, 'HSD'), "[", 2)))]
dt[grep("\\s\\s\\s\\s", test), test:=trimws(unlist(lapply(str_split(test, '\\s\\s\\s'), "[", 1)))]
dt[is.na(facility), facility:=test]
dt[ ,test:=NULL]
dt[is.na(facility), facility:=trimws(unlist(lapply(str_split(X2, 'HSD'), "[", 2)))]

# ------------------------------










# grab the remaining 266 names
dt[!is.na(HF), hsd:=trimws(unlist(lapply(str_split(HF, 'HSD'), "[", 1)))]

# drop the entries that are only numbers
numbers2 = as.character(c(1:5000))
dt[hsd %in% numbers2, hsd:=NA]

# format the sub districts
dt[!is.na(hsd) , hsd:=paste0(hsd, ' HSD')]

dt[!is.na(hsd), hsd_new:=unlist(lapply(str_split(hsd, '\\s\\s\\s'), "[", 2))]

dt[grep('HSD')]

# ------------------------------



for (v in vars) {
  
  dt[grepl('HC', dt$v) | grepl('Hosp', v) | grepl('Cli', v) | grepl('I', v), level_mark:=TRUE]
  
}




dt[ ,last:=word(X2, -1)]
dt[is.na(sub_district) & last=='HSD', sub_district:=X2]

# add the health sub-district name
dt[grep('HSD', HF), sub_district:=trimws(as.character(HF))]

# ------------------------------
# fix ownership

# initial fix on ownership
dt[is.na(X41) & !is.na(X40), X41:=X40]
dt[is.na(X41) & !is.na(X42), X41:=X42]
dt[is.na(X41) & !is.na(X43), X41:=X43]
dt[is.na(X41) & !is.na(X44), X41:=X44]
dt[is.na(X41) & !is.na(X46), X41:=X46]
dt[is.na(X41) & !is.na(X47), X41:=X47]
dt[ , c('X40','X42', 'X43', 'X44', 'X45', 'X46', 'X47'):=NULL] #X45 is always missing




# ------------------------------
# drop rows that are only district or subcounty titles
dt = dt[!grep('District', HF)]
dt[grep('HSD', HF), X3:=HF]


names_vec = as.character(unlist(dt[1]))
names_vec = names_vec[!is.na(names_vec)]
replace_names = names(dt)[!is.na(dt[1])]
setnames(dt, replace_names, names_vec)
setnames(dt, 'Abim District', 'district')
dt = dt[-1]

# ------------------------------




new = NULL

dt_new = dt[-1]

for (v in names(dt)) {
  var_name = as.character(v)
 if(all(is.na(dt$var_name))) {
   print(var_name)
 } else { print(past10)} 






names_vec = as.character(unlist(dt[1]))
names_vc = names_vc[!is.na(names_vc)]
if (is.na(dt[1])==FALSE) 
# ------------------------------




for(v in names(dt)) if(all(is.na(dt[[v]]))) dt[[v]] = NULL




# reset the names to the first row
names_vector = as.character(dt[1])



setnames(dt, names_vector)


# ------------------------------







dt[all(is.na(1:5))]

check_all = all(is.na())

dt[ ,lapply(.SD, all(is.na()))]