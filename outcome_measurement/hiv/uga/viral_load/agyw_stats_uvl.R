# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 4/18/2019
# Rbind the UVL data sets together
# Run dist_facilities_uvl.R to download facility and district names
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(stringr) 
library(plyr)
library(RColorBrewer)
library(gplots)
library(corrplot)
library(boot)
library(betareg)
# --------------------

# -----------------------------------------------
# detect if operating on windows or on the cluster 

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
# ------------------------------
# set directories and upload data 

inDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/prepped/')
outDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/age_analyses/')

# upload the data 
dt = readRDS(paste0(inDir, 'uvl_prepped_2014_2018_.rds'))

#--------------------------------------
# merge in regions

# import the regions 
regions = fread(paste0(j, '/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv'))
regions = regions[ ,.(dist112_name, region10_alt)]
setnames(regions, c('district', 'region'))
regions = regions[!duplicated(regions)]

# merge the regions into the data 
dt = merge(dt, regions, by='district', all.x=T)

#--------------------------------------
# factor categorical variables

# factor age category
dt$age = factor(dt$age, levels = c("0 - 4", "5 - 9", "10 - 14", "15 - 19",
                                   "20 - 24", "25 - 29", "30 - 34", "35 - 39", 
                                   "40 - 44", "45 - 49"))
# add an associated integer
dt[ ,age_cont:=(5*as.numeric(age))]

# sex, district, facility level
dt[ ,sex:=factor(sex)]
dt[ ,district:=factor(district)]
dt[ ,level:=factor(level)]
dt[ ,region:=factor(region)]

# print levels to check
dt[ ,lapply(.SD, function(x) print(levels(x))), .SDcols=c(5:7, 9)]

#--------------------------
# function to lemon squeeze the data 

smithsonTransform = function(x) { 
  N=length( x[!is.na(x)] )
  prop_lsqueeze = (((x*(N-1))+0.5)/N)
}

#---------------------------

#-------------------------------------------------------------------------
# facility level regression 

# sum to the facility level
vl = dt[  ,.(suppressed=sum(suppressed), valid_results=sum(valid_results)), 
          by=.(facility, level, sex, age, age_cont, district, region)]
vl[ , ratio:=(suppressed/valid_results)]

# visualize the distribution of the ratios
ggplot(vl, aes(x=ratio)) +
  geom_histogram(color='black', fill='white') +
  theme_bw() +
  theme(text=element_text(size=24)) +
  labs(title = "Viral suppression ratio (facility level)", y='Count of ratios', x='Ratio')

# lemon squeeze the ratio
vl[ ,lemon:=smithsonTransform(ratio)]

# graph the two distributions
vl_new = melt(vl, id.vars=c('facility', 'level', 'district', 'region', 'sex', 'age', 'age_cont'))
vl_new = vl_new[variable=='ratio' | variable=='lemon']
vl_new[variable=='ratio', variable:='Viral suppression ratio']
vl_new[variable=='lemon', variable:='Lemon-squeezed viral suppression ratio']

# visualize the distribution of ratios
ggplot(vl_new, aes(x=value)) +
  geom_histogram(color='black', fill='white') +
  theme_bw() +
  facet_wrap(~variable, scales='free') +
  theme(text=element_text(size=18)) +
  labs(title = "Viral suppression ratio (facility level)", y='Count of ratios', x='Ratio')

# check for heteroscedastisticity in the age distribution
ggplot(vl, aes(x=age, y=ratio)) +
  geom_jitter() +
  theme_bw() +
  theme(text=element_text(size=22)) +
  labs(title = "Viral suppression ratio within a health facility", y='Age category', x='Ratio')

# facet by sex
ggplot(vl, aes(x=age, y=ratio, color=sex)) +
  geom_jitter() +
  theme_bw() +
  facet_wrap(~sex) +
  theme(text=element_text(size=22), axis.text.x=element_text(size=12, angle=90)) +
  labs(title = "Viral suppression ratio within a health facility", y='Age category', x='Ratio') 


#------------------------
# test regressuib for interpretation 

beta_test2 = betareg(lemon~age_cont+sex+region, vl)
summary(beta_test2)

agyw = betareg(lemon~region*factor(age)*sex, vl)

predict(beta_test)

#-------------------------




# run the nice model
betaFac = betareg(lemon~sex+age_cont+level+district, vl)

# summarize the model 
summary(betaFac)

# run with region instead of district
betaFac_reg = betareg(lemon~sex+age_cont+level+region, vl)

# summarize the region model 
summary(betaFac_reg)
predict(betaFac_reg)
#-------------------------------------------------------------------------

#-----------------------------------------------
# district level regression 
# use facility level to examine at the lowest level

# sum to the district level
dist = dt[  ,.(suppressed=sum(suppressed), valid_results=sum(valid_results))  , by=.(district, region, sex, age)]
dist[ , ratio:=(suppressed/valid_results)]

# visualize the distribution of the ratios
ggplot(dist, aes(x=ratio)) +
  geom_histogram(color='black', fill='white') +
  theme_bw() +
  theme(text=element_text(size=24)) +
  labs(title = "Viral suppression ratio (district level)", y='Count of ratios', x='Ratio')

# number of observationa
dist[ ,lemon:=smithsonTransform(ratio)]

# graph the two distributions
new = melt(dist, id.vars=c('district', 'sex', 'age'))
new = new[variable=='ratio' | variable=='lemon']
new[variable=='ratio', variable:='Viral suppression ratio']
new[variable=='lemon', variable:='Lemon-squeezed viral suppression ratio']

# visualize the distribution of ratios
ggplot(new, aes(x=value)) +
  geom_histogram(color='black', fill='white') +
  theme_bw() +
  facet_wrap(~variable, scales='free')+
  theme(text=element_text(size=18)) +
  labs(title = "Viral suppression ratio (district level)", y='Count of ratios', x='Ratio',
       subtitle="Lemon squeeze formula: (ratio(N-1)+0.5)/N")

# models
betaDist = betareg(lemon~sex+age+district, dist)

# print the output
summary(betaDist)

#--------------------------------------------------
# original smithson transformation function 

smithsonTransform_og = function(x) { 
  N=length( x[!is.na(x)] )
  prop_lsqueeze = logit(((x*(N-1))+0.5)/N)
}

smithsonTransformNoLogit = function(x) { 
  N=length( x[!is.na(x)] )
  prop_lsqueeze = (((x*(N-1))+0.5)/N)
}

x = runif(100, 0, 1)
x = c(x,c(0,0,1,1))

hist(x)
hist(logit(x))
hist(smithsonTransform(x))
hist(smithsonTransformNoLogit(x))

#-------------------------------------------------------------------------






