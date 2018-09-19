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
# restrcture the data to have two variables and associated risk groups

vl <- vl[support=='Yes',.(value=sum(value, na.rm=T)), 
    by=.(variable, date, org_unit_id, org_unit,level, health_zone, dps, mtk,
         group, case, sex)]
         
#------------------------
# run a quantile regression

for (v in unique(vl$variable)) {

quantFit <- rq(value~date+factor(org_unit), data=vl[variable==v], tau=0.5)
r <- resid(quantFit) 
vl[variable==v, resid:=r] 
  
}

test <- vl[variable=="PLHIV who received a VL test",.(org_unit, date, group, case, sex, value)]
  
quantFit <- rq(value~date+factor(org_unit), data=test, tau=0.5)
r <- resid(quantFit) 
hist(r)
test[ , resid:=r]
test[resid > (median(r)+(3*sd(r)))]
test[ resid > 200]


und <- vl[variable=="PLHIV with undetectable VL",.(org_unit, date, group, case, sex, value)]

for (g in unique(und$group)) {
  
  quantFit <- rq(value~date+factor(org_unit), data=und[group==g], tau=0.5)
  r <- resid(quantFit) 
  und[group==g, resid:=r] 
  
}



#-------------------------------------------
# visuals


# facilities reporting
facilities <- vl[ ,.(facilities=length(unique(org_unit))), by=date]
facilities[ , fac:='Facilities reporting (total)']
facilities2 <- vl[value>0, .(facilities=length(unique(org_unit))), by=date]
facilities2[ , fac:='Facilities reporting at least one VL test']
facilities <- rbind(facilities, facilities2)

ggplot(facilities, aes(x=date, y=facilities, color=fac, group=fac)) +
  geom_point() +
  geom_line() +
  labs(title='Facilities reporting, viral load testing', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Facilities', x='Date', color='Facilities') +
  theme_bw() 

# facilities reporting by level
fac1 <- vl[ ,.(facilities=length(unique(org_unit))), by=.(date, level)]
fac1[ , fac:='Facilities reporting (total)']
fac2 <- vl[value>0, .(facilities=length(unique(org_unit))), by=.(date, level)]
fac2[ , fac:='Facilities reporting at least one VL test']
fac3 <- rbind(fac1, fac2)

ggplot(fac3, aes(x=date, y=facilities, color=level, group=level)) +
  geom_point() +
  geom_line() +
  facet_wrap(~fac) +
  labs(title='Facilities reporting by facility level, viral load testing', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Facilities', x='Date', color='Health facility level') +
  theme_bw() 


all <- vl[ ,.(value=sum(value)), by=.(date, variable)]

ggplot(all, aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  labs(title='Viral load tests performed & results, DRC', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 


all_group <- vl[ ,.(value=sum(value)), by=.(date, variable, group)]

ggplot(all_group, aes(x=date, y=value, color=variable, group=variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~group, scales='free_y') +
  labs(title='VL tests performed and VL test results, DRC', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 


# proportions
prop <- all
prop[variable=='PLHIV who received a VL test', variable:='test']
prop[variable=='PLHIV with undetectable VL', variable:='und']
prop <- data.table(dcast(prop, date ~ variable))
prop[ , ratio:=(100*(und/test))]
prop[ , und:=NULL]
prop <- melt(prop, id.vars='date')

ggplot(prop, aes(x=date, y=value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  labs(title='Viral load tests performed & percent virally suppressed, DRC', subtitle='Jan. 2017 - June 2018',
       caption='Source: PNLS Canevas Unique FOSA', y='Count', x='Date', color='Variable') +
  theme_bw() 












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





