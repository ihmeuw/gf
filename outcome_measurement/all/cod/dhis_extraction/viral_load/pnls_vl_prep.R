# Prep & remove outliers from the COD DHIS2 PNLS Viral Load data 

# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 9/19/2018
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr) 
# --------------------

#------------------------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#-----------------------------------
# install quant reg and load

install.packages("quantreg", lib=paste0(dir, 'quantreg_5.36'))
library(SparseM, lib.loc=paste0(dir, '/quantreg_5.36/'))
library(quantreg, lib.loc=paste0(dir, '/quantreg_5.36/'))
?rq

#-------------------------------------
# read in the subset of PNLS data specific to viral load 

vl <- readRDS(paste0(dir, 'prepped/viral_load_pnls.rds'))

#------------------------
# demarcate 'support' entries compared to regular entries

vl[ ,element_eng:=tolower(element_eng)]
vl[grep(element_eng, pattern='support'), support:='Yes']
vl[is.na(support), support:='No']

#----------------------------------
# merge in new elements

# create stratifications by population
vl[grep(element_eng, pattern='lactating'), group:='Lactating women']
vl[grep(element_eng, pattern='pregnant'), group:='Pregnant women']
vl[grep(element_eng, pattern='fe'), group:='Pregnant women']
vl[grep(element_eng, pattern='initial'), group:='Initial test']
vl[grep(element_eng, pattern='initiation'), group:='Initial test']
vl[grep(element_eng, pattern='6'), group:='After 6 months']
vl[grep(element_eng, pattern='other'), group:='Other']
vl[grep(element_eng, pattern='male'), group:='MSM']

#-----------------
# change category to sex and case status
vl[ ,category:=tolower(category)]
vl[grep(category, pattern='nc'), case:='New' ]
vl[grep(category, pattern='ac'), case:='Old' ]

vl[grep(category, pattern='féminin'), sex:='Female' ]
vl[grep(category, pattern='masculin'), sex:='Male']
vl[grep(group, pattern='women'), sex:='Female' ]
vl[grep(group, pattern='MSM'), sex:='Male']

vl[ ,category:=NULL]

#-----------------------
# restructure the data to have single data points with groupifications

vl[grep(element_eng, pattern='received'), variable:='test']
vl[grep(element_eng, pattern='undetectable'), variable:='und']

# label the variable
vl$variable <- factor(vl$variable, c('test', 'und'),
                      c('PLHIV who received a VL test', 'PLHIV with undetectable VL'))

#------------------------
# test graphs

test <- vl[ ,.(value=sum(value)), by=.(variable, group, date, support)]

ggplot(test[support=='Yes'], aes(x=date, y=value, color=group, group=group )) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable)

ggplot(test[support=='No'], aes(x=date, y=value, color=group, group=group )) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable)

# see if the totals are the same for support/not
sup <- vl[ ,.(value=sum(value)), by=.(date, support, variable)]

ggplot(sup, aes(x=date, y=value, color=support)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable)

#-----------------------
# restrcture the data to have two variables and associated risk groups

vl <- vl[support=='Yes',.(value=sum(value, na.rm=T)), 
    by=.(variable, date, org_unit_id, org_unit,level, health_zone, dps, mtk,
         group, case, sex)]
         
#------------------------
# quantile regression to remove outliers

#--------------------------
# loop over groups to identify outliers, subsetting by variable 

# create a set of only vl tests
test <- vl[variable=='PLHIV who received a VL test', .(value=sum(value)), 
           by=.(org_unit, date, group, case, sex)]

for (g in unique(test$group)) {
  
  quantFit <- rq(value~date+factor(org_unit), data=test[group==g], tau=0.5)
  r <- resid(quantFit) 
  und[group==g, resid:=r] 
  print(paste("Completed group:", g))
  
}

hist(test$resid)
test[resid > (median(r)+(3*sd(r)))]
test[ resid > 200]

#------------------
# create a set of only undetectable vl
und <- vl[variable=="PLHIV with undetectable VL", .(value=sum(value)), 
          by=.(org_unit, date, group, case, sex)]

for (g in unique(und$group)) {
  
  quantFit <- rq(value~date+factor(org_unit), data=und[group==g], tau=0.5)
  r <- resid(quantFit) 
  und[group==g, resid:=r] 
  print(paste("Completed group:", g))
  
}

hist(und$resid)
und[resid > (median(r)+(3*sd(r)))]
und[ resid > 200]

#------------------------
# eliminate the outliers





#---------------------------
# print pdfs of the outliers and eliminate them






#-------------------------
# save the new data set with outliers removed

saveRDS(paste0(vl, paste0(dir, 'prepped/viral_load_pnls_qr.rds')))

#--------------------------

