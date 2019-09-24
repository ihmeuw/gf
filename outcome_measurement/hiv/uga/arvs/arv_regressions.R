# ARV stockouts by facility - regressions

# Caitlin O'Brien-Carelli
# 9/16/2019

# ----------------------
# Set up R
rm(list=ls())
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(plyr)
library(data.table)
library(dummies)
library(stargazer)
# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')
setwd(paste0(dir, "outputs/regression_output/"))

# working directory to aggregate
dt = readRDS(paste0(dir, 'arv_stockouts_2013_2018.rds'))

# subset dates to before November 2018
dt = dt[year!=2013] 

# ----------------------
# convert variable types for regressions

# factor region, with Kampala as the reference
# order is determined by the HDI
dt$region = factor(dt$region, c("Kampala", "Central_1", 
  "Central_2", "East_Central", "Eastern",
  "Southwest", "Western", "Karamoja", "North", "West_Nile"), 
  c("Kampala", "Central 1", 
    "Central 2", "East Central", "Eastern",
    "Southwest", "Western", "Karamoja", "North", "West Nile"))

# factor level 
dt[ , level:=factor(level)]

# ----------------------
# set up the arv data for linear regression 

arv = dt[art_site==T, .(value=sum(arvs, na.rm=T), 
          anc_visits=sum(anc_visits, na.rm=T)),
          by=.(facility, level, year, region)]

# set up the test kit data for linear regression 

test = dt[ , .(test_value=sum(test_kits, na.rm=T), 
               anc_visits=sum(anc_visits, na.rm=T)),
           by=.(facility, level, year, region)]

# add a binary for is an art site as of january 2018
facilities = dt[art_site==T & month=='2018-01-01',.(facility = unique(facility))]
facilities[ ,art_site:=TRUE]
test = merge(test, facilities, by='facility', all=T)
test[is.na(art_site), art_site:=FALSE]

# categorical variable for anc visits
facs = test[ ,length(unique(facility))]

# ----------------------
# check the cataegorical variables 

# categorical variable for anc visits, art sites 
ggplot(arv, aes(x=level, y=anc_visits)) +
  geom_jitter() +
  theme_bw()+
  labs(x='Health facility level (platform)', 
       y='Number of ANC visits', 
       title='Number of first ANC visits at ART sites')+
  theme(text=element_text(size=18))

# stock outs by facility level - arvs 
ggplot(arv, aes(x=level, y=value)) +
  geom_jitter() +
  theme_bw()+
  labs(x='Health facility level (platform)', 
       y='Number of weeks out of stock of ARVs',
       title = "Total weeks stocked out of ARVs by level, ART accredited sites")+
  theme(text=element_text(size=18))

# stock outs by region - arvs
ggplot(arv, aes(x=region, y=value)) +
  geom_jitter() +
  theme_bw()+
  labs(x='Region', 
       y='Number of weeks out of stock of ARVs',
       title='Total weeks stocked out of ARVs by region, ART accredited sights')+
  theme(text=element_text(size=18))

# anc visits by level - test kits
ggplot(test, aes(x=level, y=anc_visits)) +
  geom_jitter() +
  theme_bw()+
  labs(x='Health facility level (platform)', 
       y='Number of ANC visits',
       title = "Number of ANC visits by facilitity level, all facilities", 
       subtitle = paste0("n=", facs)) +
  theme(text=element_text(size=18))

# stock outs by facility level - tests
ggplot(test, aes(x=level, y=test_value)) +
  geom_jitter() +
  theme_bw()+
  labs(x='Health facility level (platform)', 
       y='Number of weeks out of stock of HIV test kits', 
       title = "Total weeks stocked out of tests by health facility level")+
  theme(text=element_text(size=18))

# stock outs by region - tests
ggplot(test, aes(x=region, y=test_value)) +
  geom_jitter() +
  theme_bw()+
  labs(x='Region', 
       y='Number of weeks out of stock of HIV test kits',
       title = 'Total weeks stocked out of tests by region')+
  theme(text=element_text(size=18))

# ----------------------
# stock out models - linear regression models 

# arv stock outs preferred model
arv_results = lm(value~level+anc_visits+region+factor(year), data=arv)
summary(arv_results)

arv_results_yr = lm(value~level+anc_visits+region+year, data=arv)
summary(arv_results_yr)

# test kits preferred model
test_results = lm(test_value~level+art_site+anc_visits+region+factor(year), data=test)
summary(test_results)

test_results_yr = lm(test_value~level+art_site+anc_visits+region+year, data=test)
summary(test_results)

stargazer(arv_results,  arv_results_yr, test_results, test_results_yr,
          type='text', align=TRUE,
          dep.var.labels=c("Weeks out of ARVs", "Weeks out of HIV tests"),
          covariate.labels = c("Health Center III", "Health Center IV",
                               "Hospital", "Other facilities", "TASO centers", "ART site",
                               "First ANC visits", "Region: Central 1", "Region: Central 2",
                               "Region: East Central", "Region: Eastern", "Region: Southwest",
                               "Region: Western", "Region: Karamoja", "Region: North", "Region: West Nile",
                               "2015", "2016", "2017", "2018", "Year"), out = "linear_arv_tests_all_vars_factor_year.txt", 
                                no.space=TRUE)

# --------------------------------------------
# alternative regressions - predictions for region 

# model with region effect
lmFit = lm(value~region, data=arv)

# predict for each category
predictionFrame = data.table(region=unique(arv$region))
predictionFrame[, estimate:=round(predict(lmFit, newdata=predictionFrame), 1)]

ggplot(predictionFrame, aes(x=reorder(region, -estimate), y=estimate, label=estimate)) +
  geom_bar(position="dodge", stat="identity", aes(fill='#4575b4')) +
  geom_text(aes(label=estimate), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values=c('#4575b4')) +
  theme(legend.position = "none", text=element_text(size=18)) +
  labs(x="Region", y="Estimate", 
       title='Predicted mean weeks stocked out per facility, ARVs') 

# model with region effect, test kits
lmFit2 = lm(test_value~region, data=test)

# predict for each category
predictionFrame2 = data.table(region=unique(test$region))
predictionFrame2[, estimate:=round(predict(lmFit2, newdata=predictionFrame2), 1)]

ggplot(predictionFrame2, aes(x=reorder(region, -estimate), y=estimate, label=estimate)) +
  geom_bar(position="dodge", stat="identity", aes(fill='#4575b4')) +
  geom_text(aes(label=estimate), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values=c('#d73027')) +
  theme(legend.position = "none", text=element_text(size=18)) +
  labs(x="Region", y="Estimate", 
       title='Predicted mean weeks stocked out per facility, Test kits') 

# --------------------------------------------
# alternative regressions - predictions for level

# model with level effect
lmFit3 = lm(value~level, data=arv)

# predict for each category
predictionFrame3 = data.table(level=unique(arv$level))
predictionFrame3[, estimate:=round(predict(lmFit3, newdata=predictionFrame3), 1)]

ggplot(predictionFrame3, aes(x=reorder(level, -estimate), y=estimate, label=estimate)) +
  geom_bar(position="dodge", stat="identity", aes(fill='#4575b4')) +
  geom_text(aes(label=estimate), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values=c('#4575b4')) +
  theme(legend.position = "none", text=element_text(size=18)) +
  labs(x="Health facility level", y="Estimate", 
       title='Predicted mean weeks stocked out per facility, ARVs') 

# model with region effect, test kits
lmFit4 = lm(test_value~level, data=test)

# predict for each category
predictionFrame4 = data.table(level=unique(test$level))
predictionFrame4[, estimate:=round(predict(lmFit4, newdata=predictionFrame4), 1)]

ggplot(predictionFrame4, aes(x=reorder(level, -estimate), y=estimate, label=estimate)) +
  geom_bar(position="dodge", stat="identity", aes(fill='#4575b4')) +
  geom_text(aes(label=estimate), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values=c('#d73027')) +
  theme(legend.position = "none", text=element_text(size=18)) +
  labs(x="Health facility level", y="Estimate", 
       title='Predicted mean weeks stocked out per facility, Test kits') 

means = arv[ , .(mean = round(mean(value), 1)), by=region]
predictionFrame = merge(predictionFrame, means, by='region', all=T)
predictionFrame = melt(predictionFrame, id.vars='region')

ggplot(predictionFrame, aes(x=reorder(region, -value), y=value, label=value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  theme(legend.position = "none", text=element_text(size=18)) +
  labs(x="Region", y="Estimate", 
       title='Predicted mean weeks stocked out per facility, ARVs') 

#-----------------------------------------------------------
# descriptive frequency and duration - arvs 

# duration of stock outs 
arv_freq = dt[art_site==T,.(facility, date, year, arvs)]
cols = c('facility', 'date')
setorderv(arv_freq, cols)

# calculate the duration of each stock out within each facility
for (f in unique(arv_freq$facility)) {
  arv_freq[facility==f, count:=seq_len(.N), by=rleid(arvs)]
  arv_freq[facility==f, group:=rleid(arvs)]
  arv_freq[facility==f, duration:=max(count), by=group]
  
  arv_freq[arvs!=TRUE | is.na(arvs), group:=NA] 
  arv_freq[arvs!=TRUE | is.na(arvs), duration:=NA]   
  arv_freq[ , count:=NULL]
  arv_freq[facility==f & arvs==T, group:=rleid(group)]
}

# longest stock out
max = arv_freq[arvs==T,.(max_dur=max(duration)), by=.(facility, year)]
arv = merge(arv, max, by=c('facility', 'year'), all.x=T) #check it includes only art sites
arv[is.na(max_dur), max_dur:=0]

# mean annual stock out
# take the unique duration for each stock out 
calc = arv_freq[!is.na(group)]
calc = calc[ ,.(duration = unique(duration)), by=.(facility, group, year)] # some stock outs span years... 
calc = calc[ ,.(mean_dur=round(mean(duration), 1)), by=.(facility, year)]
arv = merge(arv, calc, by=c('facility', 'year'), all=T)

#------------------------------------------
# visualize the overlap 

ggplot(arv, aes(x=max_dur, y=value, color=level)) +
  geom_jitter(alpha=0.6) +
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(x="Maximum stock out duration", y='Total days stocked out',
       color='Health facility level') 

ggplot(arv, aes(x=max_dur, y=value, color=level)) +
  geom_jitter(alpha=0.6) +
  facet_wrap(~year)+
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(x="Maximum stock out duration", y='Total days stocked out',
       color='Health facility level') 

ggplot(arv, aes(x=mean_dur, y=value, color=level)) +
  geom_jitter(alpha=0.6) +
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(x="Mean stock out duration", y='Total days stocked out',
       color='Health facility level') 

# total count of stock outs


  
#------------------------------------------



