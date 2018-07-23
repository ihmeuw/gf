# Prep the COD DHIS2 PNLS data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/12/2018
#
# Upload the RDS data from DHIS2 and merge with the meta data 
# Prep the data sets for analysis and the Tableau Dashboard

# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(xlsx)
library(stringr) # to extract meta data from file names
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#------------------------------
# viral load analysis
# export a data set for viral load data; on the cluster
# 
# # import the pnls data and subset it to the elements that concern viral load
# pnls <- readRDS(paste0(dir, 'prepped_data/pnls.rds'))
# pnls <- data.table(pnls)
# 
# viral_load_elements <- c( 'cNCibxShDa6', 'QKTxjkpD2My', 'W90ci1ldX1C', 'hNWooK76isO',
#   'iHUxYVgu1qj', 'iPgvI70DJSZ', 'doq0Fivo5ew', 'Kutdws0o2vL', 'oC2u60ANRUL','tHZ6KxIksXA',  'uKEhVPh720x', 'jowAqQ7YpEC', 'd2RyaUn9ZHm', 'yjZFUr1GlQM', 'Puph0kCuE1g', 'Mg2cOozNDHa',
#  'zJBuEb9hpNq', 'gNNyKuf2joZ', 'BvZVoaCgTQD', 'tYuKqogS7vD', 'JKWTF9Bgsm4', 'B5tuUwTHAlj')
# 
#  # create a viral load data set
# vl <- pnls[element_id %in% viral_load_elements]
# vl <- data.table(vl)
# 
# # save the viral load data set to prepped data
#  saveRDS(vl, paste0(dir, 'prepped_data/viral_load_pnls.rds'))

#------------------------------


#----------------------------------
# import the viral load data set and 

# import the data and convert it to a data table
vl <- readRDS(paste0(dir, 'prepped_data/viral_load_pnls.rds'))
vl <- data.table(vl)

#------------------------
# subset the data to only the elements that don't include 'support'
sub <- c('cNCibxShDa6', 'QKTxjkpD2My', 'W90ci1ldX1C', 'hNWooK76isO',
'iHUxYVgu1qj', 'uKEhVPh720x', 'jowAqQ7YpEC', 'd2RyaUn9ZHm',
'yjZFUr1GlQM', 'Puph0kCuE1g', 'Mg2cOozNDHa')

# subset the viral load data to elements that do not include 'support'
vl <- vl[element_id %in% sub]

# view the unique elements in the subset of vl data 
vl[ ,.(unique(element_eng), unique(element_id))]

# rearrange the variables in an intuitive over
vl <- vl[ ,.(element=element_eng, org_unit, date, value, category, 
       level, dps, mtk, element_fr=element, element_id, org_unit_id)]

#------------------------
# create a variable for people who 'benefitted' from viral load testing

# viral load testing
test <- c('cNCibxShDa6', 'QKTxjkpD2My', 'W90ci1ldX1C', 'hNWooK76isO', 'iHUxYVgu1qj')

# create a variable called 'tested' that identifies viral load tests performed
vl[element_id %in% test, tested:='Yes']
vl[!element_id %in% test, tested:='No']

# create a categorical variable for the sub-populations 
vl[element_id=='Mg2cOozNDHa' , strat:='Male partners']
vl[element_id=='d2RyaUn9ZHm' | element_id=='W90ci1ldX1C' , strat:='Lactating women']
vl[element_id=='hNWooK76isO' | element_id=='yjZFUr1GlQM' , strat:='Pregnant women']
vl[element_id=='iHUxYVgu1qj' | element_id=='Puph0kCuE1g' , strat:='Initial']
vl[element_id=='cNCibxShDa6' | element_id=='uKEhVPh720x' , strat:='After six months']
vl[element_id=='jowAqQ7YpEC' | element_id=='QKTxjkpD2My' , strat:='Other']


vl[ ,unique(category)]
vl[category=='Féminin, NC', category:='Féminin, Nouveau Cas']
vl[category=='Féminin, AC', category:='Féminin, Ancien Cas']
vl[category=='Masculin, NC', category:='Masculin, Nouveau Cas']
vl[category=='Masculin, AC', category:='Masculin, Ancien Cas']

#------------------------

#------------------------
# eliminate values where detected is greater than tested

# dvl <- vl[ ,.(value=sum(value)), by=.(element, element_id, date, category, org_unit, dps, mtk, tested)]
# errors[tested=='Yes', test:=value]
# errors[tested=='No', det:=value]
# 
# errors <- errors[ ,.(test=sum(test, na.rm=T), det=sum(det, na.rm=T)), by=.(date, org_unit)]
# 
# errors[det > test, unique(org_unit)]
# 
# errors
#-----------------------
# viral load testing after six months on ARVs

# only include people on ARVs for 6 mos.
six <- vl[strat=='After six months']
six[ ,.(unique(element), unique(element_id))]

# Fix the English
six[element_id=='cNCibxShDa6', element:='PLHIV on ARVs who received a VL test after 6 mos.']
six[element_id=='uKEhVPh720x', element:='PLHIV on ARVs with undetectable VL after 6 mos.']

#------------------------
# prep the data for visualization 

six <- six[ ,.(value=sum(value)), by=.(element, element_id, date, category, org_unit, dps)]

# create a ratio:
six[element_id=='cNCibxShDa6', test:=value]
six[element_id=='uKEhVPh720x', det:=value]

six_drop <- six[ ,.(test=sum(test, na.rm=T), det=sum(det, na.rm=T)), by=.(date, category, org_unit)]
six <- six_drop[det <= test] # drop out rows where undetectable is greater than tested

# percent suppressed
six_ratio <- six[ ,.(test=sum(test, na.rm=T), det=sum(det, na.rm=T)), by=.(date, category)]



six_ratio[ , ratio:=100*(det/test)]

six_ratio2 <- six_graph[ ,.(test=sum(test, na.rm=T), det=sum(det, na.rm=T)), by=.(date)]
six_ratio2[ , ratio:=100*(det/test)]

# viral supression ratio and tests performed, shaped long
test1 <- six_ratio2[ ,.(date, value=test)]
test1[ , facet:='test']
ratio2 <- six_ratio2[ ,.(date, value=ratio)]
ratio2[ , facet:='ratio']
six_graph3 <- merge(test1, ratio2, by=c('date', 'value', 'facet'), all=T)

six_graph3$facet <- factor(six_graph3$facet, levels=c('ratio', 'test'), 
                         labels=c('Percent virally suppressed', 'Number of samples submitted for VL testing'))
# by DPS
six_pr <- six[ ,.(value=sum(value)), by=.(element, element_id, date, dps)]
six_pr_cat <- six[ ,.(value=sum(value)), by=.(element, element_id, date, category, dps)]

#------------------------------

ratio_loop <- vl[element_id=='cNCibxShDa6' | element_id=='uKEhVPh720x']

tests <- ratio_loop[element_id=='cNCibxShDa6', .(test=sum(value, na.rm=T)), by=.(date, dps)]
und <- ratio_loop[element_id=='uKEhVPh720x', .(detect=sum(value, na.rm=T)), by=.(date, dps)]

ratio1 <- merge(tests, und, by=c('date', 'dps'), all=T)
ratio1[ ,ratio:=100*(detect/test)]

idVars <- c('date', 'dps')
ratio_long <- melt(ratio1, id.vars = idVars)

ratio_long1 <- ratio_long[variable!='detect']

ratio_long$variable <- factor(ratio_long$variable, levels=c('test', 'detect', 'ratio'),
                              labels=c('Viral load tests performed', 'Virally suppressed persons',
                                       'Percent virally suppressed (%)'))

ratio_long1$variable <- factor(ratio_long1$variable, levels=c('test', 'ratio'),
                               labels=c('Viral load tests performed', 
                                        'Percent virally suppressed (%)'))
#------------------------------

list_of_plots = NULL
i=1
for(f in unique(ratio_long1$dps)) {
  
  list_of_plots[[i]] <- ggplot(ratio_long1[dps==f], aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    facet_wrap(~variable, scales='free_y') +
    labs(x='Date', y=' ', title=f) +
    theme_bw() +
    theme(legend.position="none")
  
  i=i+1
  
}

#------------------------

pdf(paste0(dir, 'cod_viral_load.pdf'), height=6, width=9)


# viral suppression ratio
ggplot(six_ratio2, aes(x=date, y=ratio, color='red')) +
  geom_point() +
  geom_line() +
  labs(title='Percent virally suppressed', y="% with undetectable viral load of tests peformed", x='Date') +
  theme_bw() +
  theme(legend.position="none")

# counts of tests and undetectable
ggplot(six_graph, aes(x=date, y=value, color=category, group=category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element) +
  labs(title='PLHIV receiving VL testing and PLHIV with undetectable VL', y="Count", x='Date') +
  theme_bw()

# viral supression ratio and tests performed
ggplot(six_graph3, aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~facet, scales='free_y') +
  labs(x='Date', y=" ") +
  theme_bw()

# viral suppression ratio by sex category
ggplot(six_ratio, aes(x=date, y=ratio, color=category, group=category)) +
  geom_point() +
  geom_line() +
  labs(title='Percent virally suppressed, by sex', y="% with undetectable viral load of tests peformed", x='Date') +
  theme_bw()

#------------------------
# by DPS

ggplot(six_pr, aes(x=date, y=value, color=dps, group=dps)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element) +
  labs(title='PLHIV receiving VL testing and PLHIV with undetectable VL', 
       y="Count", x='Date', color='DPS') +
  theme_bw()

ggplot(six_pr_cat[element_id=='cNCibxShDa6'], aes(x=date, y=value, color=dps, group=dps)) +
  geom_point() +
  geom_line() +
  facet_wrap(~category) +
  labs(title=six_pr_cat$element, 
       y="Count", x='Date', color='DPS') +
  theme_bw()

ggplot(six_pr_cat[element_id=='uKEhVPh720x'], aes(x=date, y=value, color=dps, group=dps)) +
  geom_point() +
  geom_line() +
  facet_wrap(~category) +
  labs(title='PLHIV on ARVs with undetectable viral load after 6 mos.', 
       y="Count", x='Date', color='DPS') +
  theme_bw()



for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
}


dev.off()

#-----------------------------------------

# eliminate the categories
det <- six[ ,.(value=sum(value)), by=.(element, date, org_unit, level, dps)]

tested <- det[element=='PLHIV on ARVs who received a VL test after 6 mos.']
detect <- det[element=='PLHIV on ARVs with undetectable VL after 6 mos.']

# 20 org_units report tests performed but not any undetectable results
tested[!org_unit %in% detect$org_unit, unique(org_unit)]


det[indicator=='tested', test:=value]
det[indicator=='suppressed', sup:=value]

# sum across elements
det <- det[ ,.(test=sum(test, na.rm=T), sup=sum(sup, na.rm=T)), by=.(date, org_unit, level, dps)]

# there should now be no missing values
det[is.na(test)]
det[is.na(sup)]

idVars <- c('date', 'org_unit', )

det <- melt(det, id.vars=c(d))

f <- "kr Tshikaji Hôpital Général de Référence"


ggplot(det[org_unit==f], aes(x=date, y=value, color=variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  labs(x='Date', y=' ', title=f) +
  theme_bw() +
  theme(legend.position="none")





