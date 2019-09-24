# Visualize the PNLS HIV testing data 
# Final prep file for usable data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
# 8/24/2019
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(RColorBrewer)
library(gridExtra)
library(grid)
# --------------------

# --------------------
# run these data for global fund provinces only?

gf_only = TRUE

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the shared directory
# dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# read in the data from j
# dt = readRDS(paste0(dir, 'prepped/pnls_final/pnls_vct_final.rds'))

# set the local directory
dir = "C:/Users/ccarelli/Documents/pnls_data/"

# read in the data from a local directory
dt = readRDS(paste0(dir, 'pnls_vct_final_labels.rds'))

# ----------------------------------
# set output directories 

# export locally as a pdf
if (gf_only==FALSE) outDir = paste0(dir, 'outputs/pnls_graphs_both_funders.pdf')
if (gf_only==TRUE) outDir = paste0(dir, 'outputs/pnls_graphs_global_fund_only.pdf') 

#--------------------------------------
# determine if the data will be both funders or gf only

# create a copy of the data for comparison graphs regardless of subset
compare = copy(dt)

# subset to only data from global fund provinces
if (gf_only==TRUE) dt = dt[funder=='The Global Fund']

# create a key population binary
dt[subpop!='Clients' & subpop!='Couples' & subpop!='Patients', key_pop:='Key population']
dt[subpop=='Clients' | subpop=='Patients', key_pop:='General population']
dt[subpop=='Couples', key_pop:='Couples']
#-------------------------------------------------
# MAIN DESCRIPTIVE TABLE
# compared tests, cases, test positivity, and tests per case by population 

# generate the table
tab = dt[variable=='Tested and received the results' | variable=='HIV+',.(value=sum(value)), 
         by=.(year=year(date), subpop, variable)][rev(order(variable, value))]

# calculate tests performed
tests = tab[variable=='Tested and received the results' & year==2017]
setnames(tests, 'value', '2017')
tests[ ,year:=NULL]
tests2 = tab[variable=='Tested and received the results' & year==2018]
setnames(tests2, 'value', '2018')
tests2[ ,year:=NULL]
tests = merge(tests, tests2, by=c('subpop', 'variable'), all=T)

# calculate hiv+
hiv = tab[variable=='HIV+' & year==2017]
setnames(hiv, 'value', '2017')
hiv[ ,year:=NULL]
hiv2 = tab[variable=='HIV+' & year==2018]
setnames(hiv2, 'value', '2018')
hiv2[ , year:=NULL]
hiv = merge(hiv, hiv2, by=c('subpop', 'variable'), all=T)

# test positivity rate
setnames(hiv, c('2017', '2018'), c('hiv2017', 'hiv2018'))
setnames(tests, c('2017', '2018'), c('tests2017', 'tests2018'))
hiv[ ,variable:=NULL]
tests[,variable:=NULL]
rate = merge(tests, hiv, by='subpop')
rate[ , ratio2017:=100*hiv2017/tests2017]
rate[ , ratio2018:=100*hiv2018/tests2018]
rate[ , ratio2017:=round(ratio2017, 1)]
rate[ , ratio2018:=round(ratio2018, 1)]

# ratio of positive tests to all tests
# how many tests were performed for one positive test?
rate[ , one_pos2017:=round(tests2017/hiv2017, 1)]
rate[ , one_pos2018:=round(tests2018/hiv2018, 1)]

# round the values - some decimals from imputation 
rate[ , tests2017:=round(tests2017)]
rate[ , tests2018:=round(tests2018)]
rate[ , hiv2017:=round(hiv2017)]
rate[ , hiv2018:=round(hiv2018)]

#----------------------------------------------------
# export the table 

write.csv(rate, paste0(dir, 'pnls_hiv_testing_table_gf_only.csv'))

#----------------------------------------------------

#-----------------------------------
# PEFAR versus GF table

# tests performed
keyt = compare[variable=='Tested and received the results' & subpop!='Clients' & year(date)==2018, .(value=sum(value)), by=.(funder, variable)]
cltt = compare[variable=='Tested and received the results' & subpop=='Clients' & year(date)==2018, .(value=sum(value)), by=.(funder, variable)]
totalt = compare[variable=='Tested and received the results' & year(date)==2018, .(value=sum(value)), by=.(funder, variable)]
keyt[ ,subpop:='Key populations']
cltt[ ,subpop:='Clients']
totalt[ ,subpop:='Total']
tested = rbind(keyt, cltt, totalt)

# hiv+
keyh = compare[variable=='HIV+' & subpop!='Clients' & year(date)==2018, .(value=sum(value)), by=.(funder, variable)]
clth = compare[variable=='HIV+' & subpop=='Clients' & year(date)==2018, .(value=sum(value)), by=.(funder, variable)]
totalh = compare[variable=='HIV+' & year(date)==2018, .(value=sum(value)), by=.(funder, variable)]
keyh[ ,subpop:='Key populations']
clth[ ,subpop:='Clients']
totalh[ ,subpop:='Total']
hiv = rbind(keyh, clth, totalh)

table = rbind(tested, hiv)
table = dcast(table, subpop+funder~variable)

#-----------------------------------

#----------------------------------------------------
# export the table 

write.csv(table, paste0(dir, 'compare_pepfar_gf_table_2018.csv'))

#----------------------------------------------------
# use to compare pepfar and the gf in kinshasa only, tests and positivity 

compare[variable=='Tested and received the results' & subpop!='Clients' & year(date)==2018 & dps=='Kinshasa', .(value=sum(value)), by=.(funder, variable)]
compare[variable=='Tested and received the results' & year(date)==2018 & dps=='Kinshasa', .(value=sum(value)), by=.(funder, variable)]
compare[variable=='HIV+' & year(date)==2018 & dps=='Kinshasa', .(value=sum(value)), by=.(funder, variable)]


#----------------------------------------------------
# Descriptive statistics 

# testing among the general versus key populations 
dt[subpop!='Clients' & subpop!='Patients' & subpop!='Couples', key_pop:='Key population']
dt[subpop=='Clients' & subpop=='Patients', key_pop:='General population']
dt[subpop=='Couples', key_pop:='Couples']

# total tests 2018
dt[variable=='Tested and received the results' & year(date)==2018, .(value=sum(value))]
dt[variable=='Tested and received the results' & year(date)==2018 & key_pop=='Key population', 
   .(value=sum(value))]

x = dt[variable=='Tested and received the results' & year(date)==2018, .(value=sum(value))]
y = dt[variable=='Tested and received the results' & year(date)==2018 & key_pop=='Key population', 
   .(value=sum(value))]
100*y/x


z = dt[variable=='Tested and received the results' & year(date)==2018 & dps!='Kinshasa', .(value=sum(value))]
r = dt[variable=='Tested and received the results' & year(date)==2018 & key_pop=='Key population' & dps!='Kinshasa', 
       .(value=sum(value))]
100*r/z

#----------------------------------------------------

# testing among the general versus key populations by dps
# total tests 2018
gen = dt[variable=='Tested and received the results'  , .(gen_value=sum(value)), by=dps]
keys = dt[variable=='Tested and received the results' & year(date)==2018 & key_pop=='Key population', 
   .(key_value=sum(value)), by=dps]

gen_keys = merge(gen, keys, by='dps')
gen_keys = gen_keys[order(dps)]
gen_keys[ ,total:=gen_value+key_value]
gen_keys[ , percent_on_keys:=round(100*(key_value/total), 1)]
gen_keys[order(percent_on_keys)]

#----------------------------------------------------
# export the table 

write.csv(gen_keys, paste0(dir, 'tests_on_key_pops_dps.csv'))

#----------------------------------------------------

#----------------------------------------------------
# overview descriptives

dt[year(date)==2018 & variable=='Tested and received the results', .(value=sum(value))]
dt[year(date)==2018 & variable=='Tested and received the results' & subpop!='Clients',
   .(value=sum(value))]



x = dt[year(date)==2018 & variable=='Tested and received the results', .(value=sum(value))]
y = dt[year(date)==2018 & variable=='Tested and received the results' & subpop!='Clients',
   .(value=sum(value))]
100*y/x


# mean monthly test positivity
mon = dt[(variable=='Tested and received the results' | variable=='HIV+') & subpop=='Clients' & funder=='The Global Fund']
mon = mon[ ,.(value=sum(value)), by=.(date, variable)]
mon = dcast(mon, date~variable)
setnames(mon, c('date', 'hiv', 'tests'))
mon[ ,ratio:=100*(hiv/tests)]
mon[ ,mean(ratio)]

# mean monthly test positivity
mon = dt[(variable=='Tested and received the results' | variable=='HIV+') & subpop!='Clients' & funder=='The Global Fund']
mon = mon[ ,.(value=sum(value)), by=.(date, variable)]
mon = dcast(mon, date~variable)
setnames(mon, c('date', 'hiv', 'tests'))
mon[ ,ratio:=100*(hiv/tests)]
mon[ ,mean(ratio)]












