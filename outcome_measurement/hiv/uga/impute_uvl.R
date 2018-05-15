# ----------------------------------------------
# Caitlin O'Brien-Carelli

# 5/8/2018
# Multiple imputation for the Uganda Viral Load Dashboard
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(Amelia)
library(MASS)
library(gtools)

# set input/output directory
# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uvl1 <- readRDS(paste0(dir, "/sex_data.rds"))
uganda_vl <- uvl1

# start from scale up and continue through april 2018
uganda_vl <- uganda_vl[!(month==3 & year==2018)] # fix me
uganda_vl <- uganda_vl[year==2016 | year==2017 | year==2018]
uganda_vl <- uganda_vl[sex!='Unknown'] # there should not be unknowns in the data set; confirm

# check for duplicate entries 
uganda_vl[sex=='Female', sex1:=1]
uganda_vl[sex=='Male', sex1:=2]
uganda_vl[ ,combine:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl[order(combine)]
uganda_vl[duplicated(combine)]
uganda_vl[,combine:=NULL]
uganda_vl[,sex1:=NULL]

# ----------------------------------------------
# prep the data for imputation 

# --------------
# add an expanded data set that has missing rows for missing values

# store list of unique faciltiies, sexes and dates
f_ids <- unique(uganda_vl$facility_id)
length(f_ids)
sexes <- unique(uganda_vl$sex)
dates <- seq(from=min(uganda_vl$date), to=max(uganda_vl$date), by='month')

# make a "fully rectangularized" dataset with all months for each facility-sex
expanded_data <- data.table(expand.grid(f_ids, dates, sexes))
setnames(expanded_data, c('facility_id', 'date', 'sex'))

# --------------------

# merge in the blank rows for facility_id (by date, sex)
uganda_vl <- merge(uganda_vl, expanded_data, by=c('facility_id', 'date', 'sex'), all=TRUE)

#--------------------
# check for duplicats
uganda_vl[sex=='Female', sex1:=1]
uganda_vl[sex=='Male', sex1:=2]
uganda_vl[ ,combine:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl[duplicated(combine)]

#-------------------
# collapse the duplicate entries to only single values
# this only matters if there are duplicate entries - should be none (perform as a check)
sumVars <- c("patients_received", "samples_received", "rejected_samples", "plasma_samples",
             "dbs_samples", "samples_tested", "valid_results", "suppressed")

uganda_vl <- uganda_vl[, lapply(.SD, sum, na.rm=FALSE), by=c('facility_id', 'facility_name', 'dhis2name', 
                                                    'sex', 'date', 'district_id', 'dist_name', 'level'),
                                                    .SDcols=sumVars]
#-------------------

#-------------------
# run a for loop that fills in missing identifier values

ids <- uganda_vl[!is.na(facility_name), .(facility_id=unique(facility_id)),  
                 by=.(facility_name, level, dhis2name, district_id, dist_name)]

for (f in ids$facility_id) {
  uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, level:=ids[facility_id==f]$level]  
  uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, dhis2name:=ids[facility_id==f]$dhis2name]   
  uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, district_id:=ids[facility_id==f]$district_id]   
  uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, dist_name:=ids[facility_id==f]$dist_name] 
  uganda_vl <- uganda_vl[is.na(facility_name) & facility_id==f, facility_name:=ids[facility_id==f]$facility_name]
  
  print(f)
}

#-------------------
# add zeroes to the data where one sex is present but the other is missing in a specific month
# for example, in january 2016 in facility 2 if males are present but females are NA

#-------------------
# male data but missing females
uganda_vl[ ,combine:= paste0(facility_id, '_', date)]

# male data but missing females
missing_females <- uganda_vl[sex=='Female' & is.na(patients_received), .(combine=combine)]

males <- uganda_vl[sex=='Male']
males <- males[!is.na(patients_received), .(combine)]
males <- merge(males, missing_females, by='combine')

males[ , missing_females:=1]
uganda_vl <- merge(uganda_vl, males, by='combine', all.x=TRUE)
uganda_vl[is.na(missing_females), missing_females:=0]

#add zeroes for all female rows that in dates and facilities that have male data but no female data is reported
uganda_vl[missing_females==1 & sex=='Female', patients_received:=0]
uganda_vl[missing_females==1 & sex=='Female', samples_received:=0]
uganda_vl[missing_females==1 & sex=='Female', rejected_samples:=0]
uganda_vl[missing_females==1 & sex=='Female', plasma_samples:=0]
uganda_vl[missing_females==1 & sex=='Female', dbs_samples:=0]
uganda_vl[missing_females==1 & sex=='Female', samples_tested:=0]
uganda_vl[missing_females==1 & sex=='Female', suppressed:=0]
uganda_vl[missing_females==1 & sex=='Female', valid_results:=0]

#-------------------
# female data but missing males
missing_males <- uganda_vl[sex=='Male' & is.na(patients_received), .(combine=combine)]

females <- uganda_vl[sex=='Female']
females <- females[!is.na(patients_received), .(combine)]
females <- merge(females, missing_males, by='combine')

females[ , missing_males:=1]
uganda_vl <- merge(uganda_vl, females, by='combine', all.x=TRUE)
uganda_vl[is.na(missing_males), missing_males:=0]

# add zeroes for all the missing female entries where these is male data
uganda_vl[missing_males==1 & sex=='Male', patients_received:=0] 
uganda_vl[missing_males==1 & sex=='Male', samples_received:=0] 
uganda_vl[missing_males==1 & sex=='Male', rejected_samples:=0] 
uganda_vl[missing_males==1 & sex=='Male', plasma_samples:=0] 
uganda_vl[missing_males==1 & sex=='Male', dbs_samples:=0] 
uganda_vl[missing_males==1 & sex=='Male', samples_tested:=0] 
uganda_vl[missing_males==1 & sex=='Male', suppressed:=0] 
uganda_vl[missing_males==1 & sex=='Male', valid_results:=0] 

#-----------------------

#---------------------
# check for duplicates
# create a unique identifier (char) of facilityid_date_sex
uganda_vl[sex=="Female", sex1:=1 ]
uganda_vl[sex=="Male", sex1:=2]

uganda_vl[ ,combine1:= paste0(facility_id, '_', date, '_', sex1)]
uganda_vl[,length(unique(combine1))] 

uganda_vl[duplicated(combine1)] # no duplicates
uganda_vl[,combine1:=NULL] # then delete the identifier
uganda_vl[,combine:=NULL]
uganda_vl[,sex1:=NULL]

#---------------------
# descriptives for missing males and females
# the number of males in facilities and months where there is no female data
uganda_vl[missing_females==1 & sex=='Male', .(patients_received, sum(patients_received)), by=.(sex, facility_id, date)]


# ----------------------------------------------
# prep the data for the imputation

#------------------------
# change integers to numerics to accomodate decimal values
# drop out month, year and leave date 

Vars <- c("patients_received", "samples_received", "rejected_samples","dbs_samples", "plasma_samples",
          "samples_tested", "suppressed", "valid_results")

uganda_vl <- uganda_vl[,lapply(.SD, as.numeric), by=c('facility_id', 'facility_name', 'dhis2name', 
                                               'district_id', 'dist_name', 'sex', 'date', 'missing_males', 'missing_females', 'level'), 
                                              .SDcols=Vars]

#---------------------------
# create a variable marking which rows to impute
uganda_vl[is.na(patients_received), imputed:=1]
uganda_vl[!is.na(patients_received), imputed:=0]

#---------------------------

#---------------
# drop out plasma samples
uganda_vl[ , plasma_samples:=NULL]

# mark the 0s
uganda_vl[patients_received==0 & imputed==0, patients_received0:=T] # this will only be true when one sex is present and not the other
uganda_vl[samples_received==0 & imputed==0, samples_received0:=T] # this will only be true when one sex is present and not the other
uganda_vl[rejected_samples==0 & imputed==0, rejected_samples0:=T]
uganda_vl[dbs_samples==0 & imputed==0, dbs_samples0:=T]
uganda_vl[samples_tested==0 & imputed==0, samples_tested0:=T]
uganda_vl[valid_results==0 & imputed==0, valid_results0:=T]
uganda_vl[suppressed==0 & imputed==0, suppressed0:=T]

uganda_vl[patients_received!=0 & imputed==0, patients_received0:=F] 
uganda_vl[samples_received!=0 & imputed==0, samples_received0:=F] 
uganda_vl[rejected_samples!=0 & imputed==0, rejected_samples0:=F]
uganda_vl[dbs_samples!=0 & imputed==0, dbs_samples0:=F]
uganda_vl[samples_tested!=0 & imputed==0, samples_tested0:=F]
uganda_vl[valid_results!=0 & imputed==0, valid_results0:=F]
uganda_vl[suppressed!=0 & imputed==0, suppressed0:=F]

#---------------------------------













#---------------
# add 1 to every value in the data set that is not being impited

uganda_vl[!is.na(patients_received), patients_received:=(patients_received+1)] 
uganda_vl[!is.na(samples_received), samples_received:=(samples_received+1)] 
uganda_vl[!is.na(rejected_samples), rejected_samples:=(rejected_samples+1)]
uganda_vl[!is.na(dbs_samples), dbs_samples:=(dbs_samples+1)] 
uganda_vl[!is.na(samples_tested), samples_tested:=(samples_tested+1)]
uganda_vl[!is.na(valid_results), valid_results:=(valid_results+1)]
uganda_vl[!is.na(suppressed), suppressed:=(suppressed+1)]

#---------------------------

#------------------------
# calculate the suppression ratio
uganda_vl[suppressed > valid_results] # check equality constraints
uganda_vl[, ratio:=(suppressed/valid_results)]


#------------------------

# if valid results is 0, alter the ratio to be the mean district suppression ratio
#compute district average ratio among nonzeroes
uganda_vl[valid_results0==FALSE, district_ratio:=mean(ratio, na.rm=TRUE), by='district_id']
uganda_vl[, district_ratio:=mean(district_ratio, na.rm=TRUE), by='district_id']

# replace ratio to district average when the number of samples was zero
uganda_vl[valid_results0==TRUE, ratio:=district_ratio]
uganda_vl[ , district_ratio:=NULL]
 
#------------------------
# transform the suppression ratio
hist(uganda_vl$ratio)

# keep track of 1s - the suppression ratio can never be 0 bc the numerator is never 0
uganda_vl[ratio==1, ratio1:=T] 
uganda_vl[ratio<1, ratio1:=F] 

#---------------
# transform the 1s

# replace suppression ratio = 1 with the maximum possible value < 1
uganda_vl[, max_ratio:= uganda_vl[ratio<1, max(ratio, na.rm=T)]]
uganda_vl[ratio1==T, ratio:=max_ratio]

# check all 0s and 1s have been removed
uganda_vl[ ,.(max(ratio, na.rm=T))]
uganda_vl[ ,.(min(ratio, na.rm=T))]

#---------------
# logit transform the ratio
uganda_vl[ , ratio:=logit(ratio)]
hist(uganda_vl$ratio)

#---------------------------
# log the counts

# remove the numerator from the data set
uganda_vl[ , suppressed:=NULL]
uganda_vl[ , max_ratio:=NULL]
uganda_vl[ , district_ratio:=NULL]




#------------------------------------
# log all the variables to impute

uganda_vl[ , patients_received:=log(patients_received)]
uganda_vl[ , samples_received:=log(samples_received)]
uganda_vl[ , rejected_samples:=log(rejected_samples)]
uganda_vl[ , dbs_samples:=log(dbs_samples)]
uganda_vl[ , samples_tested:=log(samples_tested)]
uganda_vl[ , valid_results:=log(valid_results)] 

# remove plasma samples to calculate later on
uganda_vl[ , plasma_samples:=NULL]

#------------------------------------

# ---------------
# drop out demarcation variables (use a merge after imputation to identify 0s and 1s)

uvl <- uganda_vl[ , .(facility_id, facility_name, dhis2name, district_id, dist_name, sex, date,
                             patients_received, samples_received, rejected_samples, dbs_samples, samples_tested, valid_results, 
                              ratio=sup_ratio, level, 
                              imputed, missing_males, missing_females, ratio0, ratio1,
                               patients_received0, samples_received0, rejected_samples0, dbs_samples0, samples_tested0, valid_results0)]


# data prep for imputation complete!!! :)
#-----------------------------------


#------------------------------------
# run imputation using amelia

#---------------
# make cs variable
uvl[, cs_variable:=paste0(facility_id, sex)]

# set idvars
idVars <- c("facility_id", "sex", "facility_name", "dhis2name", "dist_name", "imputed", 
            'missing_males', 'missing_females', 'ratio0', 'ratio1',
            'patients_received0', 'samples_received0', 'rejected_samples0', 'dbs_samples0', 'samples_tested0', 'valid_results0')

# list the variables to lag
lagVars <- c( "patients_received", "samples_received", "rejected_samples", "dbs_samples", "samples_tested", "valid_results", "ratio")

# run imputation
imputed_data <- amelia(uvl, m=50, cs='cs_variable', ts='date', lags=lagVars, idvars=idVars, noms='district_id', ords='level', parallel='snow')


#------------------
# polytime versions 
imputed_time2 <- amelia(uvl, m=2, cs='cs_variable', ts='date', lags=lagVars, idvars=idVars, noms='district_id', ords='level', polytime==2, parallel='snow')
imputed_time3 <- amelia(uvl, m=2, cs='cs_variable', ts='date', lags=lagVars, idvars=idVars, noms='district_id', ords='level', polytime==3, parallel='snow')
#------------------

# graph one test case to see how it looks
cstmp = sample(unique(uvl$cs_variable),1)
merged = merge(uvl, imputed_data[[1]]$imp1, by=c('cs_variable','date'), all=T)
ggplot(merged[cs_variable==cstmp], aes(y=ratio.y, x=date)) + 
	geom_point(color='red') + 
	geom_point(aes(y=ratio.x), color='black') + 
	labs(title=paste('Facility-sex:', cstmp))

#--------------------------------------------------------------
# bind the imputations together to create a single data set called amelia_data

for( i in 1:50 ) {
  imputed_data$imputations[[i]]$imputation_number <- i
  if (i==1)  amelia_data <- data.table(imputed_data$imputations[[i]])
  if (i>1) amelia_data <- rbind(amelia_data, imputed_data$imputations[[i]])
}

# save the imputed data as an RDS
saveRDS(amelia_data, file= paste0(dir, "/imputed_full.rds"))  



#---------------------------------

avl <- readRDS(paste0(dir,"/imputed_full.rds"))

#--------------------------------------------------------------

avl[ , .(mean(patients_received, na.rm=TRUE))]
avl[ , .(mean(ratio, na.rm=TRUE))]

avl[is.na(patients_received)]
avl[is.na(imputed)]

#-------------------------------------------------------------
# prepare the imputed data sets for analysis

# COUNTS
# exponentiate the counts
expVars <- c("patients_received", "samples_received", "rejected_samples", 
           "dbs_samples", "samples_tested", "valid_results")

avl <- avl[, lapply(.SD, exp), by=c('facility_id', 'facility_name', 'dhis2name', 'sex', 'date', 'district_id', 'dist_name', 'level', 'imputed', 'ratio',
              'missing_males', 'missing_females', 'ratio0', 'ratio1', 'patients_received0', 'samples_received0', 'rejected_samples0', 
              'dbs_samples0', 'samples_tested0', 'valid_results0'), .SDcols=expVars]

# transform the indentified 0s back to 0; all values should be 1 before transformation
avl[patients_received0==T, unique(patients_received)] 
avl[patients_received0==T, unique(samples_received)] 

avl[patients_received0==T, patients_received:=0] 
avl[samples_received0==T, samples_received:=0] 
avl[rejected_samples0==T, rejected_samples:=0]
avl[dbs_samples0==T, dbs_samples:=0]
avl[samples_tested0==T, samples_tested:=0]
avl[valid_results0==T, valid_results:=0]

# drop the identifiers
avl[ ,patients_received0:=NULL] 
avl[ ,samples_received0:=NULL] 
avl[ ,rejected_samples0:=NULL]
avl[ ,dbs_samples0:=NULL]
avl[ ,samples_tested0:=NULL]
avl[ ,valid_results0:=NULL]

#-------------------------------------
# back transform the viral suppression ratio 

# inverse logit the viral suppression ratio
avl[ ,ratio:=inv.logit(ratio)]

# if a ratio was originally 1, change it back to 1
avl[ratio1==1, unique(ratio)]
avl[ratio1==1, length(unique(ratio))]
avl[ratio1==1, .(mean(ratio, na.rm=T))]
avl[ratio1==1, ratio:=1]

# change the 0s back to 0
avl[ratio0==1, unique(ratio)]
avl[ratio0==1, length(unique(ratio))]
avl[ratio0==1, .(mean(ratio, na.rm=T))]
avl[ratio0==1, ratio:=0]

avl[ratio==0.8333333]
avl[ratio==0.9991618]
avl[ ,summary(ratio)]

# district ratio




# remove the ratio identifiers
avl[,ratio0:=NULL]
avl[,ratio1:=NULL]

#-------------------------------------

# confirm these are rational values
table_ratio <- avl[ ,.(ratio=mean(ratio, na.rm=T)), by=.(date, imputed)]
ggplot(table_ratio, aes(x=date, y=ratio, color=factor(imputed))) + geom_point() + geom_line()

table_ratio_sex <- avl[ ,.(ratio=mean(ratio, na.rm=T)), by=.(date, sex)]
ggplot(table_ratio_sex, aes(x=date, y=ratio, color=factor(sex))) + geom_point() + geom_line()

table_1 <- avl[ ,.(pts=mean(patients_received, na.rm=T)), by=.(date, imputed)]
ggplot(table_1, aes(x=date, y=pts, color=factor(imputed))) + geom_point() + geom_line()

#-------------------------------------
# calculate the number suppressed using the ratio and the number of valid results

avl[ , suppressed:=(valid_results*ratio)]

#--------------------------------

















# Valid results and viral suppression ratio by sex, facility
# Compare imputed and original data by month, facility id, sex using valid results and the viral suppression ratio

# store identifiers
idVars <- c("facility_id", "facility_name", "sex", "date")

# create a long data set with totals by district
ratio_loop <- avl[sex!='Unknown',.(valid_results=mean(valid_results),
                        ratio=mean(ratio)),
                        by=idVars]

# reshape indicators long for district data
ratio_loop <- melt(ratio_loop, id.vars=idVars)
View(ratio_loop)

# label the variables for graph titles and put the graphs in an intuitive order
ratio_loop$variable <- factor(ratio_loop$variable, 
                           levels=c("valid_results", "ratio"), 
                           labels=c("Number of valid test results", "Percent virally suppressed"))

# single test graph
f=27
name2 <- unique(ratio_loop[facility_id==f]$facility_name)

ggplot(ratio_loop[facility_id==f], aes(y=value, x=date, color=sex, group=sex)) + 
  geom_point() +
  geom_line(alpha=0.5) + 
  facet_wrap(~variable, scales='free_y') +
  labs(title=name2, x="Date", color="Sex") +
  theme_bw()


# ---------------------------------
# loop over all facilities printing valid test results and the suppression ratio

list_of_plots = NULL
i=1

for(f in unique(ratio_loop$facility_id)) {
  
  # set the title to the facility name
  name2 <- unique(ratio_loop[facility_id==f]$facility_name)
  
  # create a graph of the monthly data stratified by sex
  list_of_plots[[i]] <- ggplot(ratio_loop[facility_id==f], aes(y=value, x=date, color=sex, group=sex)) + 
    geom_point() +
    geom_line(alpha=0.5) + 
    facet_wrap(~variable, scales='free_y') +
    labs(title=name2, x="Date", color="Sex") +
    theme_bw()
  
  i=i+1
  
}

pdf(paste0(dir,'/webscrape_agg/outputs/imputed/results_ratio_facility.pdf'), height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()




