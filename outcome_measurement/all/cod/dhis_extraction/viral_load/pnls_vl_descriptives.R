# Prep & analyze the COD DHIS2 PNLS Viral Load data 
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

vl[grep(element_eng, pattern='support'), support:='Yes']
vl[grep(element_eng, pattern='Support'), support:='Yes']
vl[is.na(support), support:='No']

#----------------------------------
# merge in new elements

vl[ ,element_eng:=tolower(element_eng)]

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
vl[grep(category, pattern='NC'), case:='New' ]
vl[grep(category, pattern='AC'), case:='Old' ]

vl[grep(category, pattern='féminin'), sex:='Female' ]
vl[grep(category, pattern='masculin'), sex:='Male']
vl[grep(group, pattern='women'), sex:='Female' ]
vl[grep(group, pattern='MSM'), sex:='Male']

vl[ ,category:=NULL]

#-----------------------
# restructure the data to have single data points with groupifications

vl[grep(element_eng, pattern='received'), variable:='PLHIV who received a VL test']
vl[grep(element_eng, pattern='undetectable'), variable:='PLHIV with undetectable VL']

#------------------------
# test graph

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
# drop out stratified elements and create two variables with associated risk groups

vl <- vl[support=='Yes',.(value=sum(value, na.rm=T)), 
    by=.(variable, date, org_unit_id, org_unit,level, health_zone, dps, mtk,
         group, case, sex)]
         






#------------------------
# run a quantile regression

quantFit <- rq(value~date+factor(org_unit), data=vl, tau=0.5)
r <- resid(quantFit)

hist(r)
vl[ ,resid:=r]

pmtct2[resid >200]

pmtct2[r>(median(r)+(3*sd(r)))]



quantFit2 <- rq(value~date+factor(org_unit)+factor(element)+date*factor(strat), data=test, tau=0.5)


#-------------------------------------------
# run on a sample pmtct data set
pw <- vl[element_id=='hNWooK76isO',.(value=sum(value)), by=.(date, org_unit, element, level, dps)]

quantFit2 <- rq(value~date+factor(org_unit), data=pw2, tau=0.5)

r2 <- resid(quantFit)
hist(r)

pw[ ,resid:=r2]
pw[resid >200]

pw[r2>(median(r)+(3*sd(r)))]

# graph two examples of the residuals 
ggplot(pw[org_unit=="kn Roi Baundouin 1Er Centre Hospitalier"], 
       aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)


ggplot(pw2[org_unit=="hk Kyubo Centre de Santé de Référence"], 
       aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

#---------------

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





