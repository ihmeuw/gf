# pull DHS/MICS numbers of treatment-seeking for LRI

# set up R
rm(list=ls())
library(data.table)
library(stringr)
library(ggplot2)

# data directory
dir = 'J:/Project/LRI/GBD/Treatment Covariates/01_UbCov_Extracted'

# files
inFiles = list.files(path=dir, pattern='UGA*', full.names=TRUE)

# load data
data = lapply(inFiles, fread)
data = rbindlist(data, fill=TRUE)

# collapse
agg = data[, .(pct_tx=mean(lri_tx, na.rm=T)), by=c('survey_name','year_start','year_end')]

# label surveys better
agg[year_start==year_end, survey:=paste0(str_sub(survey_name,-3,99), ' ', year_start)]
agg[year_start!=year_end, survey:=paste0(str_sub(survey_name,-3,99), ' ', year_start, '/', year_end)]

# pull in prevalence survey estimate
agg = rbind(agg, data.table(survey_name='TB Prevalence Survey', year_end=2015, year_start=2014, survey='TB Prevalence Survey 2014/2015', pct_tx=0.61), fill=T)

# graph
ggplot(agg[survey_name!='TB Prevalence Survey'], aes(y=pct_tx, x=year_start+((year_end-year_start)/2))) + 
	geom_point(aes(color='DHS/MIS'), size=4) + 
	geom_point(data=agg[survey_name=='TB Prevalence Survey'], aes(color='TB Prevalence Survey'), size=4) + 
	geom_text(data=agg, aes(label=survey), hjust=1, vjust=1, size=2.5) + 
	expand_limits(x = 1986, y = c(0,1)) + 
	labs(title='Treatment-Seeking Behavior from Household Surveys', 
		y='Proportion with Symptoms who Sought Care', x='Survey Year', 
		color='Survey Type', caption='Note: Definitions are different between DHS/MIS and TB Prevalence Survey.\n (DHS/MIS asks about any cough symptoms among children. \nTB Prevalence Survey asks about chronic cough for all ages)') + 
	theme_bw()
	