# ----------------------------------------------
# Audrey Batzel
#
# 8/23/18
# 
# Compare provinces involved in PBF to those not doing PBF; make other graphs for TERG slides
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
## Set up R / install packages
# --------------------
rm(list=ls())

library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(stats)
library(Rcpp)
library(readxl)
library(cem)
library(boot)
library(tidyr)
library(broom)
# --------------------  


# ----------------------------------------------
## Overview - Files and Directories
# ----------------------------------------------
# data directory
# when run on Unix, data directory needs to be set to /home/j (to run on the cluster), so set this here:
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir_data = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/')
output_dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/visualizations/')
dir_worldpop = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/worldpop_data/')

# input files
input_dt <- "dt_for_matching_analysis_of_pbf.csv"
hz_pops <- "hz_pops.xlsx"

# output files
variable_hists = "cem_vars_hist_comparison.pdf"
# ----------------------------------------------


# ----------------------------------------------
# read in data
dt <- read.csv(file=paste0(dir_data, input_dt))
dt <- as.data.table(dt)

# pop <- read_excel(paste0(dir_worldpop, hz_pops))
# ----------------------------------------------


# ----------------------------------------------
# cem match: automatic bin choice
# data(LL)
# mat <- cem(treatment="treated", data=LL, drop="re78")
# mat
# lFit = lm(re78~treated, data=LL, weights=mat$w)


# identify which HZ's had PBF in any year
dt[, ever_pbf := pbf]

# make pbf yes/no correspond to the year when pbf started.
dt[, pbf:= "no"]
dt[year>=year_start_pbf, pbf := "yes"]
# subset to JUST 2013, before any pbf started
dt_2013 <- dt[year == 2013, ]
outcome_2013 = dt_2013$cases_treated
# ----------------------------------------------


# ----------------------------------------------
# Difference in differences analysis

# match the "ever PBF" hzs with never PBF hzs in 2013 only
dt_2013 = dt_2013[,names(dt_2013)[!names(dt_2013)%in%c("X", "year_start_pbf", "cases_treated", "pbf", "year")],with=F]
dt_2013[, ever_pbf:=ifelse(ever_pbf=='yes',1,0)]
dt_2013 <- dt_2013[,-c("health_area", "medical_surgical_center")]

vars_cutpoints <- colnames(dt_2013)[!colnames(dt_2013) %in% c('dps', 'health_zone', 'ever_pbf', 'funder_2017')]
nCuts = 4
mycp <- setNames( as.list( c(4, rep(nCuts, (length(vars_cutpoints)-1) ))), vars_cutpoints  )

cemData = copy(dt_2013)
matches <- cem(treatment='ever_pbf', data=as.data.frame(cemData), drop=c('dps','health_zone'), cutpoints = mycp)

cemData[, match_group:=matches$mstrata]

# make a new data table that discards all non-matches (but keeps every year for the ones that HAD a match in 2013)
# non-matches health zones:
non_matches <- cemData[is.na(match_group),]

dt2 <- dt[!health_zone %in% non_matches$health_zone]
dt2 <- dt2[,-c("health_area", "medical_surgical_center")]
dt2 = dt2[,names(dt2)[!names(dt2)%in%c("X", "year_start_pbf", "pbf")],with=F]

# make a binary variable that is 1 in 2014-2017 and 0 otherwise for all HZ's
dt2[ , post_pbf := ifelse(year>=2014,1,0)]
dt2[ , ever_pbf := ifelse(ever_pbf=='yes',1,0)]

# run the regression:
# calculate coverage
dt2[, coverage:=cases_treated/cases]
# drop rows where coverage > 1
dt2 <- dt2[coverage<1, ]
# create a small placeholder value for where coverage = 0, so that we can logit transform it
q = quantile(dt2$coverage, .01)
dt2[coverage==0, coverage:=q]

lFit = lm(logit(coverage)~post_pbf*ever_pbf, data=dt2)
summary(lFit)
# ----------------------------------------------


# ----------------------------------------------

#dt <- dt[year>=2014,]

outcome = dt$cases_treated
dt = dt[,names(dt)[!names(dt)%in%c("X", "year_start_pbf", "cases_treated")],with=F]
dt[, pbf:=ifelse(pbf=='yes',1,0)]

# COME BACK TO THIS AND GET IT RIGHT
# dt[, fac1:=medical_surgical_center+general_reference_hospital+hospital+secondary_hospital+hospital_center]
# dt[, fac2:=reference_health_center+health_center+medical_center]
# dt[, fac3:=health_post+polyclinic+dispensary+health_area]
# dt = dt[,c("dps", "health_zone", "year", "pbf", "cases", 'funder_2017', 'fac1','fac2','fac3'),with=FALSE]

# exclude health areas and medical surgical centers due to no variance (???)
dt <- dt[,-c("health_area", "medical_surgical_center")]

# define cutpoints - cutpoints is a named list and the elements are either vector of cutpoints, a number of cutpoints, or method for
# automatic bin construction
vars_cutpoints <- colnames(dt)[!colnames(dt) %in% c('dps', 'health_zone', 'pbf', 'funder_2017')]

nCuts = 4

# VECTOR OF CUTPOINTS:
# mycp <- list(cases=quantile(dt$cases, seq(0,1,length.out=nCuts)), 
#              fac1=quantile(dt$fac1, seq(0,1,length.out=nCuts)),
#              fac2=quantile(dt$fac2, seq(0,1,length.out=nCuts)),
#              fac3=quantile(dt$fac3, seq(0,1,length.out=nCuts)),
#              year=2009:2018)

# subtract 1/add 1 to ends of cutpoints list elements

# NUMBER OF CUTPOINTS:
mycp <- setNames( as.list( c(4, rep(nCuts, (length(vars_cutpoints)-1) ))), vars_cutpoints  )

# we might want to exclude health areas from the matching variables
# same with medical surgical centers

# ok so here's my recommendation:
# clinics: 1 break at 0.5
# dispensaries: 2 breaks at 0.5 and 2.5
# general reference hospitals: 2 breaks at 0.5 and 1.5
# hospitals: breaks at 0.5 and 1.5, maybe 2.5 (try with and without)
# hospital centers: breaks at 0.5, 1.5, 2.5 and maybe 3.5 (try with and without)
# polyclinics: breaks at 0.5 and 1.5
# reference health centers: breaks at 0.5, 2.5 and maybe 5.5
# secondary hospitals: break at 0.5 

# medical centers are hard to see, can you remake the histogram and exclude above 15?

# for everything else, (cases, health centers, health posts), do 6 quantiles

# then for the sensitivity analyses, we can basically just do variants of the above that have more breaks for everything, fewer breaks for everything, more for one at a time holding the others constant, fewer for one at a time holding the others constant

# can you include population as another matching variable too? put it in the "6-quantile" group
# ----------------------------------------------


# ----------------------------------------------
cemData = copy(dt)
matches <- cem(treatment='pbf', data=as.data.frame(cemData), drop=c('dps','health_zone'), cutpoints = mycp)
matches

cemData[, match_group:=matches$mstrata]

# how to edit this?
# cemData[, .(cases=mean(cases),fac1=mean(fac1), fac2=mean(fac2), fac3=mean(fac3)), by=c('pbf','match_group')]
# cemData[match_group %in% unique(matches$mstrata)[1:10], .(cases=mean(cases),fac1=mean(fac1), fac2=mean(fac2), fac3=mean(fac3)), by=c('pbf','match_group')][order(match_group, pbf)]
# ----------------------------------------------


# ----------------------------------------------
# run analysis
cemData[, weight:=matches$w]
analysisData = copy(cemData)
analysisData[, cases_treated:=outcome]
analysisData[, coverage:=cases_treated/cases]
# drop rows where coverage > 1
analysisData = analysisData[coverage<1]
# create a small placeholder value for where coverage = 0, so that we can logit transform it
q = quantile(analysisData$coverage, .01)
analysisData[coverage==0, coverage:=q]

lFit = lm(logit(coverage)~pbf, data=analysisData, weights=weight)
summary(lFit)

dt_lfit <- tidy(lFit)
ci_95 <- confint(lFit)
ci_95 <- tidy(ci_95)

lFit = lm(logit(coverage)~pbf, data=analysisData)
summary(lFit)

lFit = lm(logit(coverage)~(pbf*factor(year)), data=analysisData)
summary(lFit)

analysisData2 <- analysisData[, -c("dps", "health_zone", "cases", "cases_treated", "weight", "match_group")]
lFit = lm(logit(coverage)~., data=analysisData2)
summary(lFit)
# ----------------------------------------------


# ----------------------------------------------
vars_to_graph <- colnames(dt)[5: (length(colnames(dt))-3)]


pdf(paste0(output_dir, variable_hists), height=9, width=11)

for (v in vars_to_graph){
  g<- ggplot(dt, aes(x=get(v), group=as.character(pbf), fill=as.character(pbf))) + 
      geom_histogram(alpha = .2) + 
      facet_wrap( ~ year ) +
      theme_bw() +
      labs( x = v )
  print(g)
}

dev.off()











