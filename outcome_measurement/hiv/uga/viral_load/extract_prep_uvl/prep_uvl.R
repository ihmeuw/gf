# prep function - source from merge_uvl.R
# Caitlin O'Brien-Carelli
# 3/31/2019
#-----------------------

# to run the function without the merge:
# dt = readRDS(paste0(out_dir, 'merged_vl_', min_date, '_', max_date, '.rds'))

prep_uvl = function(x) {

#-----------------------
# set working directory
prep_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')
setwd(prep_dir)

# ----------------------------------------------
# drop out patients in 'facility left blank' facilities

print(paste("There are", dt[is.na(facility), sum(patients_received)], 
            "patients where the facility is missing."))

dt = dt[!is.na(facility)]
#-----------------------------------------
# change district names from new 2016/17 districts to match the shape file

merge_new_dists = function(x) {
  x[district=="Bunyangabu", district:="Kabarole"]
  x[district=="Butebo", district:="Pallisa"]
  x[district=="Kagadi", district:="Kibaale"]
  x[district=="Kakumiro", district:="Kibaale"]
  x[district=="Kyotera", district:="Rakai"]
  x[district=="Namisindwa", district:="Manafwa" ]
  x[district=="Omoro", district:="Gulu"]
  x[district=="Pakwach", district:="Nebbi"]
  x[district=="Rubanda", district:= "Kabale"]
  x[district=="Rukiga", district:="Kabale"]
  x[district=="Luwero", district:="Luweero"]
  x[district=="Sembabule", district:="Ssembabule"]
  
}

# run the function on your data set
# there should be 113 districts - 112 plus one missing
merge_new_dists(dt)
length(unique(dt$district))

# ------------------------
# full data table of all duplicate entries as single entries
# drop out hub
byvars = c("facility_id", "facility", "dhis2_name", "hub",  "district", "date",
           "age", "sex")

dt = dt[ ,lapply(.SD, sum), by=byvars, .SDcols=4:10]

# -------------------------------------------
# determine facility levels from facility name

# separate out the number from facility name to get facility level
dt[, facility_1:=paste(facility, '_')] # creates a space after the name
dt[, facility_1:=tolower(facility_1)]

# set the levels
dt[grepl(pattern="\\sii", facility_1), level:='HC II'] 
dt[grepl(pattern="iii", facility_1), level:='HC III']
dt[grepl(pattern="\\siv",facility_1), level:='HC IV']
dt[grepl(pattern="hospital", facility_1) & !grepl(pattern='military hospital', facility_1), level:='Hospital']
dt[grepl(pattern="military hospital", facility_1), level:='Military Hospital']

# TASO health centers do not have associated levels
dt[grepl(pattern="taso", facility_1), level:='TASO']

#-----------------------------------------------
# use dhis2name to get facility levels not contained in the udt dashboard name
dt[!is.na(dhis2_name), dhis2_name1:=tolower(paste(dhis2_name, '_'))] # creates a space after the name

# set the levels where level is in dhis2 name but not in the vl dashboard name
dt[grepl(pattern="\\sii\\s", dhis2_name1), level2:='HC II'] 
dt[grepl(pattern="iii", dhis2_name1), level2:='HC III']
dt[grepl(pattern="\\siv",dhis2_name1), level2:='HC IV']
dt[grepl(pattern="hospital", dhis2_name1) & !grepl(pattern='military hospital', dhis2_name1), level2:='Hospital']
dt[grepl(pattern="military hospital", dhis2_name1), level2:='Military Hospital']

# replace missing levels from facility names with levels from dhis2names
dt[is.na(level), level:=level2]

# number of facilities without level
dt[is.na(level), length(unique(facility))]

#--------------------------------
# create a variable to identify prisons
dt[grep(pattern="prison", facility_1), prison:=T]
dt[grep(pattern="prison", dhis2_name1), prison:=T] # using dhis2name captures a remand center
dt[is.na(prison), prison:=F]

# delete excess variables
dt[ , c("facility_1", "dhis2_name1", 'level2'):=NULL]

#--------------------------------

#-----------------------------------------------
# assign unknown sex using mean imputation

# create a data set to calculate the sex ratio by district
# the first data table represents the total number of each sex for each variable
vars = c('district', 'sex')
ratio = dt[!is.na(sex), lapply(.SD, sum), by=vars, .SDcols=9:15]

# create a data table of total for all variables, regardless of sex
total = ratio[ ,lapply(.SD, sum), .SDcols=3:9, by=district]
total[ , sex:='Total']

# bind them together and drop the males
total = rbind(ratio, total)

# there should be 112 each
total[ ,length(unique(district)), by=sex]

# drop out the males to create a female ratio
total = total[sex!='Male']
total = total[order(district, sex)]

#-------------------------------
# replace the female values with the ratio for every variable

#  write as a function that tells you the percentage of the group that is female
create_ratio = function(x) { x = (x/shift(x, type='lead'))}

# create a data set with the female ratio
females = total[ ,lapply(.SD, create_ratio), .SDcols=3:9, by='district']
females = females[!is.na(samples_received)]
females[ ,sex:='Female']
#------------------------------
# replace the total values with the male ratio

males = females[ ,lapply(.SD, function(x) {x = 1 - x}), by='district', .SDcols=2:8]
males[ ,sex:='Male']

#-------------------------------
# rbind them together and apply

# create a single ratio data set
ratio = rbind(females, males)

# reset the names for the merge
setnames(ratio, c("patients_received", "samples_received",  "rejected_samples",  "dbs_samples",   
                  "total_results", "suppressed", "valid_results"),
                  c("patients_received1", "samples_received1",  "rejected_samples1",  "dbs_samples1",   
                   "total_results1", "suppressed1", "valid_results1"))

# replace the missing values with the patients received ratio
replace_vars = c("patients_received1", "samples_received1",  "rejected_samples1",  "dbs_samples1",   
"total_results1", "suppressed1", "valid_results1")
for (r in replace_vars) { ratio[is.na(get(r)), (r):=patients_received1]}

#-------------------------------
# create a data set of unknowns - double in size to apply ratios

fems = dt[is.na(sex)]
fems[ , sex:='Female']
men = dt[is.na(sex)]
men[ , sex:='Male']
unknown_sex = rbind(fems, men) 

unknowns = merge(unknown_sex, ratio, by=c('district', 'sex'), all.x=T)
 
#-------------------------------
# merge in the ratios

# calculate the new values
unknowns[ , patients_received:=(patients_received*patients_received1)]
unknowns[ , samples_received:=(samples_received*samples_received1)]
unknowns[ , rejected_samples:=(rejected_samples*rejected_samples1)]
unknowns[ , dbs_samples:=(dbs_samples*dbs_samples1)]
unknowns[ , total_results:=(total_results*total_results1)]
unknowns[ , suppressed:=(suppressed*suppressed1)]
unknowns[ , valid_results:=(valid_results*valid_results1)]

# drop unnecessary variables
unknowns[ , c("patients_received1", "samples_received1",  "rejected_samples1",  "dbs_samples1",   
             "total_results1", "suppressed1", "valid_results1"):=NULL]

# round to single digit
vars = c( 'facility_id', 'facility', 'dhis2_name', 'hub',  
          'district', 'sex', 'age', 'date', 'level', 'prison')

unknowns = unknowns[ ,lapply(.SD, round, 1), .SDcols=9:15, by=vars]

#-------------------------------
# merge in new values
dt = dt[!is.na(sex)]
dt = rbind(dt, unknowns)

# sum over values to ensure new females and males are incorporated
dt = dt[ ,lapply(.SD, sum), .SDcols=9:15, by=vars]

#-------------------------------
# final quality checks 

#--------------------------------------------
# check equality constraints
# if the variables violate equality constraints, alter them

# samples received can be greater than patients received
dt[samples_received < rejected_samples, rejected_samples:=samples_received]
dt[samples_received < dbs_samples, dbs_samples:=samples_received]
dt[samples_received < total_results, total_results:=samples_received]
dt[samples_received < valid_results, valid_results:=samples_received]
dt[total_results < valid_results, valid_results:=total_results]
dt[valid_results < suppressed, suppressed:=valid_results]

# -------------------------------------------
# calculate plasma samples
dt[ , plasma_samples:=(samples_received - dbs_samples)]

#--------------------------------------------
# final equality constraints check
if (nrow(dt[samples_received < rejected_samples]) +
nrow(dt[samples_received < dbs_samples]) +
nrow(dt[samples_received < plasma_samples]) +
nrow(dt[samples_received < total_results]) +
nrow(dt[samples_received < valid_results]) +
nrow(dt[total_results < valid_results]) +
nrow(dt[valid_results < suppressed]) ==0) { print("Equality constraints met!")
  } else {print("Gahh! Look at your data.")}

#-------------------------------
# add a year variable
dt[ ,year:=year(date)]

#-------------------------------
return(dt)

}



