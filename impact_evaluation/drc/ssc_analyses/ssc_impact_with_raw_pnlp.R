# ------------------------------------------------------------------------------
# Audrey Batzel
# code from David Phillips
# 2/26/20
# Remake DiD figure showing under-5 malaria mortality in health zones with ssc vs those wihtout using raw PNLP data
# ------------------------------------------------------------------------------

# -------------------
# Set up R
rm(list=ls())
library(data.table)
# -------------------

# ---------------------------------------------------------------------------------------
# Files, directories and settings

# whether or not to run analysis among UNICEF health zones only
fullpackageOnly = TRUE

# root directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')

# input file
#inFile = paste0(dir, '/impact_evaluation/cod/prepped_data/second_half_data_pre_model.rdata')
inFile = paste0(dir, 'impact_evaluation/cod/prepped_data/base_pnlp_sigl_combined_data_hz_level.rds')
inFileUnadjusted = paste0(dir, 'impact_evaluation/cod/prepped_data/outcomes_impact_corrected.RDS')

# file listing health zones
hzFile = paste0(dir, '/mapping/cod/ssc_lists/prepped_hz_list.csv')

# file identifying dps's
dpsFile = './core/hz_renaming_file.csv'

# output files
outFile = paste0(dir, 'impact_evaluation/cod/prepped_data/ssc_analyses/DiD_input_data.rdata')
outFile2 = paste0(dir, 'impact_evaluation/cod/visualizations/ssc_analyses/figure_under5_malariaMortality_iCCM_rawPNLP.pdf')
# modify output file names if we're running analysis among UNICEF health zones only
if(fullpackageOnly) outFile = gsub('.rdata', '_full_package_HZs_only.rdata', outFile)
# ---------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load/prep data

# load input data
data = readRDS(inFileUnadjusted)

# load list of health zones with full package
hzList = fread(hzFile)

# add extra health zones to full-package list
# (list emailed to us by Eugene Nsambu July 29, 2019)
# (these were manually updated in the spreadsheet as of 2/25/2020)
extraHZs = c('basoko','isangi','yabahondo','yaleko','yakusu','aketi','buta')
hzList[, full_package:=unicef_supported]
hzList[health_zone %in% extraHZs, full_package:=1]

# subset to only health zones with the full package if specified
if(fullpackageOnly) hzList = hzList[full_package==1]

# subset columns
hzList = hzList[,c('health_zone','full_package')]
data = data[, c('health_zone','date','malariaDeaths_under5','malariaDeaths_under5_rate')]

# drop rows post-2017 because DHIS doesn't have age-specific mortality
data = data[date<2018]

# check for health zones in the list that aren't in the data 
# kikwit-nord is ok because it's combined with kikwit-sud in the data
# there are 10 others that aren't explained
hzList$health_zone[!hzList$health_zone %in% data$health_zone]

# identify 'intervention' health zones in the data
hzList[, intervention:=1]
data = merge(data, hzList, by='health_zone', all.x=TRUE)
data[is.na(intervention), intervention:=0]
data[, intervention_label:=ifelse(intervention==1, '2. Health Zones with SSCs (intervention)', '1. Health Zones without SSCs (control)')]
if(fullpackageOnly)  data[, intervention_label:=ifelse(intervention==1, '2. Health Zones with iCCM (intervention)', '1. Health Zones without iCCM (control)')]

# identify before/after
data[, period:=ifelse(date<2017, 0, 1)]
data[, period_label:=ifelse(period==1, '2. After 2017', '1. Before 2017')]

# subset to only GF DPSs
dpsList = unique(fread(dpsFile)[dps!='0',c('dps','health_zone'),with=FALSE])
gfDPS = c('bas-uele', 'equateur', 'haut-uele', 'ituri', 'kinshasa', 'kongo-central', 
          'kwango', 'kwilu', 'mai-ndombe', 'maniema', 'mongala', 'nord-kivu', 
          'nord-ubangi', 'sud-ubangi', 'tshopo', 'tshuapa')
dpsList[dps=='kasai-central' & health_zone=='lubunga', health_zone:='lubunga2'] # this is about to get dropped because PMI
dpsList[dps=='nord-ubangi' & health_zone=='bili', health_zone:='bili2'] # both "bili" health zones are in the intervention
data = merge(data, dpsList, by='health_zone', all.x=TRUE)
data = data[dps %in% gfDPS]

# drop health zone without denominators
data = data[health_zone!='nzanza']

data[malariaDeaths_under5_rate>2000, malariaDeaths_under5_rate:=NA]
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Aggregate data

# take averages by intervention/period to have a data frame to predict amongst
means = data[, .(
  malariaDeaths_under5_rate=mean(malariaDeaths_under5_rate), 
  lower_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.2), 
  upper_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.8)), 
  by=c('period_label','intervention_label', 'period', 'intervention')]

# take averages by intervention/date for time series graph of data
means_ts = data[, .(
  malariaDeaths_under5_rate=median(malariaDeaths_under5_rate), 
  lower_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.2), 
  upper_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.8)), 
  by=c('intervention_label', 'date', 'intervention')]
# ------------------------------------------------------------------------------


# switch title if specified
if (fullpackageOnly==FALSE) intlab = 'SSCs'
if (fullpackageOnly==TRUE) intlab = 'iCCM'

pdf(outFile2, height = 9, width = 11)
# time series graph of the malaria data
ggplot(means_ts, aes(y=malariaDeaths_under5_rate, ymin=lower_pctle_malaria, 
                          ymax=upper_pctle_malaria, x=date, color=intervention_label, fill=intervention_label)) + 
  geom_ribbon(alpha=.5) + 
  geom_line(size=1.25) + 
  scale_fill_manual(values=c('#33a02c', '#1f78b4')) + 
  scale_color_manual(values=c('#33a02c', '#1f78b4')) + 
  labs(title='Malaria Mortality Under 5', 
       subtitle=paste('Comparing Health Zones with and without', intlab), 
       y='Under-5 Malaria Mortality Rate (per 10,000 population)', x='Period', 
       color='', fill='', 
       caption='Lines and intervals show median, 20th and 80th percentiles of health zones') + 
  theme_bw() + theme(text = element_text(size = 18), legend.position = 'bottom')
dev.off()
