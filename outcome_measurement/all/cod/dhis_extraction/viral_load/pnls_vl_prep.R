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
library(quantreg)
# --------------------
# shell script to 
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1327 -s 20 -P snis_prep

#------------------------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#-----------------------------------
# install quant reg and load

# install.packages("quantreg", lib=paste0(dir, 'quantreg_5.36'))
# library(SparseM, lib.loc=paste0(dir, '/quantreg_5.36/'))
# library(quantreg, lib.loc=paste0(dir, '/quantreg_5.36/'))
# ?rq

#-------------------------------------
# read in the subset of PNLS data specific to viral load 

#vl <- readRDS(paste0(dir, 'prepped/viral_load_pnls.rds'))


# interim data set
vl <- readRDS(paste0(dir, 'prepped/quantreg_results.rds'))

#------------------------
# demarcate 'support' entries compared to regular entries and remove support

vl[ ,element_eng:=tolower(element_eng)]
vl[grep(element_eng, pattern='support'), support:='Yes']
vl[is.na(support), support:='No']
vl <- vl[support=='No']
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

vl[grep(element_eng, pattern='received'), variable:='PLHIV who received a VL test']
vl[grep(element_eng, pattern='undetectable'), variable:='PLHIV with undetectable VL']

#-----------------------
# restrcture the data to have two variables and associated risk groups

vl <- vl[  ,.(value=sum(value)), by=.(variable, date, org_unit_id, org_unit,level, 
                                               health_zone, dps, mtk, group, case, sex)]

#-----------------------
# drop any values that violate equality constraints
und <- vl[variable=='PLHIV with undetectable VL']
tests <- vl[variable=='PLHIV who received a VL test']

setnames(und, 'value', 'und')
und[ ,variable:=NULL]
setnames(tests, 'value', 'test')
tests[ ,variable:=NULL]

# drop values in which one variable is present but not the other
rat <- merge(und, tests)

# maintain equality constraints
rat <- rat[test >= und]

# percentage of entries that include both tests performed and results
new = merge(und, tests, all=T)
nrow(rat)/(nrow(new))

# reshape long

idVars = c("date", "org_unit_id", "org_unit", "level", "health_zone", "dps", "mtk", "group", "case", "sex")
rat = melt(rat, id.vars=idVars)

rat[variable=='und', variable:='PLHIV with undetectable VL']
rat[variable=='test', variable:='PLHIV who received a VL test']

# reassign the new data set to vl
vl = rat


#------------------------
# save the interim output to code off of until outlier removal

#saveRDS(vl, paste0(dir, 'prepped/viral_load_pnls_interim.rds'))

#----------------------------------------------------------
# OUTLIER REMOVAL
# quantile regression to remove outliers

# -------------------
# loop over groups to identify outliers, subsetting by variable 

und <- vl[variable=="PLHIV with undetectable VL", .(value=sum(value)),
            by=.(org_unit, date, group, case, sex)]

test <- vl[variable=="PLHIV who received a VL test", .(value=sum(value)),
           by=.(org_unit, date, group, case, sex)]


# run the quantile regression on vl tests performed
for (g in unique(test$group)) {

  quantFit <- rq(value~date+factor(org_unit), data=test[group==g], tau=0.5)
  r <- resid(quantFit)
  test[group==g, resid:=r]
  print(paste('Completed:', g))
  
}

saveRDS(test, paste0(dir, 'viral_load/tests.rds'))

for (g in unique(und$group)) {
  
  quantFit <- rq(value~date+factor(org_unit), data=und[group==g], tau=0.5)
  r <- resid(quantFit)
  und[group==g, resid:=r]
  print(paste('Completed:', g))
  
}

saveRDS(und, paste0(dir, 'viral_load/undetectable.rds'))

        
#---------------------------------------
# print pdfs of the outliers and eliminate them

# undetectable vl
hist(und$resid)
und[ resid > 200]
und[resid > (median(resid)+(3*sd(resid)))]

#------------------
# time series for only what it identified

# read in the data set you want to analyze
dt <- readRDS(paste0(dir, 'viral_load/tests.rds'))

# calculate the median and MAD for the residuals
dt[ , m:=(median(resid))]
dt[ , s:=(sd(resid))]

# subset to only the org units with outliers (more than 3 SDs)
out_orgs <- dt[resid > (median(resid)+(10*sd(resid))), unique(org_unit)]
out <- dt[org_unit %in% out_orgs]

# uniquely identify the outliers
out[resid >= (median(resid)+(4*sd(resid))), outlier:='4 SDs']
out[resid >= (median(resid)+(5*sd(resid))), outlier:='5 SDs']
out[resid >= (median(resid)+(6*sd(resid))), outlier:='6 SDs']
out[resid >= (median(resid)+(10*sd(resid))), outlier:='10 SDs']
out[ resid < (median(resid)+(4*sd(resid))), outlier:='Nope']

# subset to only the groups with an outlier
out[ , combine:=paste0(org_unit, group)]
combos <- out[outlier!='Nope', combine]
out <- out[combine %in% combos]

# sex is concurrent with group
out[ ,combine:=NULL]

# create plots of the outliers
list_of_plots = NULL
i=1


for(o in unique(out$org_unit)) {
  for (f in unique(out$sex)) {
  # facility name
  name <- unique(out[org_unit==o]$org_unit)
  sex1 <- unique(out[sex==f]$sex)
  
  # list of plots
  list_of_plots[[i]] <-  ggplot(out[org_unit==o & sex==f], aes(x=date, y=value, color=case, group=case)) + 
    geom_line(alpha=0.5) + 
    geom_point(aes(color=outlier)) + 
    facet_wrap(~group, scales='free_y') +
    theme_bw() + labs(title=name, x='Date', y='Count', color='Outlier', subtitle=sex1) +
    scale_color_manual(values=c('Old' = '#9ecae1', 'New'='#3182bd', 'Nope'='#9ecae1', '4 SDs'='#d73027',
                                '5 SDs'='#fc8d59' ,'6 SDs'='#fee08b', '10 SDs'='#1a9850'))
  
  i=i+1
  
}}

pdf(paste0(dir, 'viral_load/outliers_group_undetectable_vl_10SDs.pdf'), height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()


#-------------------------
# save the new data set with outliers removed

saveRDS(paste0(vl, paste0(dir, 'prepped/viral_load_pnls_qr.rds')))

#--------------------------
# create a set of only undetectable vl - sum over case

und <- vl[variable=="PLHIV with undetectable VL", .(value=sum(value)),
          by=.(org_unit, date, group, sex)]

list_of_candidate_plots = list()
p <- 1

  for (g in unique(und$group)) {

  # subset to current data
  currentData = und[group==g]

  # fit regression
  quantFit <- rq(value~date+factor(org_unit), data=currentData, tau=0.5)
  
  # store model estiamtes
  currentData[ , f := predict(quantFit)]
  currentData[ , r := resid(quantFit)]
  currentData[, s := mad(r)]
  
  if (is.na(currentData$s)) currentData[, s := 0]
  currentData[, f3u := f + (3*s)]
  currentData[, f3l := f - (3*s)]
  currentData[, candidates:=(value > f3u | value < f3l)]

  if (any(currentData$candidates)) 
    currentData[, f1u := f + s]
    currentData[, f2u := f + (2*s)]
    currentData[, f4u := f + (4*s)]
    currentData[, f1u := f - s]
    currentData[, f2u := f - (2*s)]
    currentData[, f4u := f - (4*s)]

    for (o in unique(currentData$org_unit)) { 
      currentPlot = ggplot(currentData[org_unit==o], aes(y=value, x=date)) +
        geom_ribbon(aes(ymin=f3u, ymax=f3u)) +
        geom_ribbon(aes(ymin=f2u, ymax=f2u)) +
        geom_ribbon(aes(ymin=f1u, ymax=f1u)) +
        geom_ribbon(aes(ymin=f4u, ymax=f4u), alpha=.5) +
        geom_line(aes(y=f)) 
        geom_point() 
        
        +
        scale_color_manual(values=c('TRUE'='red', 'FALSE'='black'))

      list_of_candidate_plots[[p]] <- currentPlot
      p <- p+1
      
      color=as.character(candidates)

    }

  und[group==g, resid:=r]
  print(paste("Completed group:", g))
}

hist(und$resid)
und[resid > (median(r)+(4*sd(r)))]
und[ resid > 200]


# 
# 
# list_of_plots = NULL
# i=1
# 
# for(o in unique(out$org_unit)) {
#   
#   # facility name
#   name <- unique(out[org_unit==o]$org_unit)
#   
#   cols <- c("Yes" ='red', "No" = "blue")
#   
#   # list of plots
#   list_of_plots[[i]] <-  ggplot(out[org_unit==o], aes(x=date, y=value, color=case, group=case)) + 
#     geom_line(alpha=0.5) + 
#     geom_point(aes(color=outlier)) + 
#     facet_wrap(~group, scales='free_y') +
#     theme_bw() + labs(title=name, x='Date', y='Count', color='Case')
#   
#   
#   
#   i=i+1
#   
# # 
# # #------------------------
# # check equality constraints
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
