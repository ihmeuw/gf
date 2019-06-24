# Visualize the PNLS HIV testing data 
# Final prep file for usable data 
#
# ----------------------------------------------
# Caitlin O'Brien-Carelli
# 6/17/19
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(openxlsx)
library(RColorBrewer)
# --------------------

# shell script for working on the cluster
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 10 -P snis_download

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

# read in the data 
dt = readRDS(paste0(dir, 'prepped/pnls_final/pnls_vct_final.rds'))
setnames(dt, 'element_eng', 'variable')

# ----------------------------------------------------------
# create a funder binary
dt[dps=='Haut Katanga' | dps=='Lualaba', funder:='PEPFAR']
dt[!(dps=='Haut Katanga' | dps=='Lualaba'), funder:='The Global Fund']

#------------------------------------
# minor outlier screen

dt[ , max(value), by=variable]
dt = dt[value!=769776 & value!=29841 & value!=10000 & value!=6985 & !(variable=='HIV+' & value==510)]

#------------------------------------
# subset to only 2017 and 2018

dt = dt[date < '2019-01-01']
#------------------------------------
# factor sub populations for graphs 

dt$subpop = factor(dt$subpop, 
                   c("prisoner", "trans", "idu", "trucker", "msm", "uniform", "csw_customer", 
                     "fisher", "miner", "other_groups", "csw", "customer"),    
                   c("Prisoners", "Trans people", "IDUs", "Truckers", "MSM", 
                     "Military personnel", "CSW Clients", "Fisher people", "Miners", 
                     "Other groups", "CSWs", "Patients")) 

#------------------------------------
# create smaller health facility groupings for graphs 

dt[grep('hospital',facility_level), next_level:='Hospitals']
dt[facility_level=='reference_health_center', next_level:='Reference health centers']
dt[facility_level=='health_center' | facility_level=='health_post' | facility_level=='dispensary', next_level:='Health centers, posts, and dispensaries']
dt[is.na(next_level), next_level:='Other types of facilities']

# factor facility level for graphs
dt$facility_level = factor(dt$facility_level, 
                  rev(c("health_center", "reference_health_center", "health_post", "hospital", 
                    "general_reference_hospital", "hospital_center", "medical_center",
                    "clinic", "secondary_hospital",  "dispensary","polyclinic", "medical_surgical_center")),
                  rev(c("Health Center", "Reference Health Center", "Health Post", "Hospital", 
                    "General Reference Hospital", "Hospital Center", "Medical Center",
                    "Clinic", "Secondary Hospital",  "Dispensary","Polyclinic", "Medical surgical center")))

#------------------------------------------------------------------
# HIV Testing Visualizations

# EXPORT AS A PDF
# pdf(paste0(dir, 'outputs/pnls_hiv_testing/pnls_vct_graphs.pdf'), width=12, height=9)

#----------------------
# COLOR SCHEMES

quad_colors = c('#542788','#66bd63', '#b2182b', '#4575b4')
sex_colors = c('#b2182b', '#4575b4')
tri_colors = c('#a50026', '#fdae61', '#abd9e9')
test_colors = c('#a50026', '#fdae61',  '#4575b4')
colors12 = c(brewer.pal(11, 'Spectral'), '#a6d96a')
bi= c("#fdae61", "#8073ac")
op = c('#f1a340', '#998ec3')
#----------------------

#---------------------------------------------------------------------
# REPORTING COMPLETENESS


#-----------------------------------------------------------
# Reporting completeness

# facilities reporting and patients who received a consultation by sex
fac = dt[ ,.(facilities_reporting=length(unique(org_unit))), by=date]
pts = dt[variable=="New patients who received a treatment consultation"
         ,.(patients=sum(value)), by=.(sex, date)]
fac = merge(fac, pts, by='date')

# add in total patients, not by sex
tot = dt[variable=="New patients who received a treatment consultation", .(total=sum(value)), by=date]
total_pts = sum(tot$total)
fac = merge(fac, tot, by='date')

# shape it long
fac = melt(fac, id.vars=c('date', 'sex'))
fac = fac[!(sex=='Male' & variable=='facilities_reporting')]
fac = fac[!(sex=='Male' & variable=='total')]

# reset the sex names
fac[variable=='facilities_reporting',sex:='Health facility']
fac[variable=='total', sex:='Total']
fac[variable=='total', variable:='patients']

# label the variables
fac$variable = factor(fac$variable, c('facilities_reporting', 'patients'),
                      c("Health facilities reporting", "Patients"))
fac$sex = factor(fac$sex, c('Health facility', 'Total', 'Female', 'Male'),
                 c('Health facility', 'Total patients',
                   'Female', 'Male'))
lines = c('solid', 'dashed', 'solid', 'solid', 'solid')

# plot of facilities reporting and patients who received a consultation
ggplot(fac, aes(x=date, y=value, color=sex, group=sex)) +
  geom_point(alpha=0.4) +
  geom_line(aes(linetype=sex)) +
  scale_linetype_manual(values=lines, guide=F) +
  facet_wrap(~variable, scales='free_y') +
  scale_color_manual(values=quad_colors) +
  theme_bw() + labs(x='Date', y='Count', color='',
                    title="Reporting completeness: facilities reporting and patients who received a consultation*",
                    caption='*Indicator: new patients who received a treatment consultation') +
  theme(text=element_text(size=18), axis.title=element_text(size=18), axis.text=(element_text(size=16)),
        plot.title=element_text(size=20), legend.text =element_text(size=18),
        plot.subtitle=element_text(size=16))

#-----------------
# reporting over time by health facility level

fac2 = dt[ ,.(facilities_reporting=length(unique(org_unit))), by=.(date, facility_level)]
fac2[facility_level=='Health Center', hc:='Health centers']
fac2[facility_level!='Health Center' | is.na(facility_level), hc:='Other types of health facilities']

ggplot(fac2, aes(x=date, y=facilities_reporting, color=facility_level)) +
  geom_point(alpha=0.4) +
  geom_line() +
  facet_wrap(~hc, scales='free_y') +
  theme_bw() + labs(x='Date', y='Facilities reporting', color='Health facility level',
                    title="Reporting completeness by health facility level",
                    subtitle=("Health centers reported separately in order to highlight the scale of reporting"),
                    caption="Source: SNIS-PNLS")

#-----------------
# bar graph of reporting composition over time by health facility level

fac_bar = dt[ ,.(facilities = length(unique(org_unit_id))), by=.(date, facility_level)]

ggplot(fac_bar[!is.na(facility_level)], aes(x=date, y = facilities, fill = facility_level)) + 
  geom_bar(position = "fill",stat = "identity") +
  labs(x='Date', y='Percentage of all health facilities reporting',
       title='Composition of facilities reporting', fill="Facility level") +
  scale_fill_manual(values = colors12) +
  theme_bw()

#-----------------
# reporting by smaller groupings of health facilities

sample = dt[ ,length(unique(org_unit_id))]
final_report = dt[ ,.(facilities=length(unique(org_unit_id))), by=.(next_level, date)]

ggplot(final_report, aes(x=date, y=facilities, color=next_level)) +
  geom_point(alpha=0.4) +
  geom_line() +
  theme_bw() + labs(x='Date', y='Count', color='Health facility level',
                    title="Total health facilities reporting by level",
                    subtitle=(paste0(sample, " facilities reported to PNLS on HIV testing")),
                    caption="Source: SNIS")

#-----------------
# bar graph of reporting composition over time by aggregate categories of facility level

ggplot(final_report[!is.na(next_level)], aes(x=date, y = facilities, label=facilities, fill = next_level)) + 
  geom_bar(position = "fill",stat = "identity") +
  labs(x='Date', y='Percentage of all health facilities reporting', 
       fill='Facility level', title='Composition of health facilities by level, categorized') +
  scale_fill_manual(values = colors12) +
  theme_bw()

#-----------------
# facilities reported out of ever reported (percentage)

ever_reported = dt[ ,.(total_report=length(unique(org_unit))), by=facility_level]
totals = merge(fac2, ever_reported, by='facility_level', all.x=T)
totals[ ,reporting_ratio:=round(100*(facilities_reporting/total_report))]
totals[ ,label:=paste0(facility_level, ' (n=', total_report, ')')]

ggplot(totals[!is.na(facility_level)], aes(x=date, y=reporting_ratio, color=label)) +
  geom_point(alpha=0.4) +
  geom_line() +
  theme_bw() + labs(x='Date', y='Percent of facilities reporting', color='Health facility level (n = number of facilities)',
                    title="Percentage of facilities reporting that have ever reported",
                    subtitle=("Denominator: all the health facilities that have ever reported to the data set"),
                    caption="Source: SNIS")


#-----------------
# demonstrate trends in reporting and testing

trends = dt[variable=='Tested and received the results' ,.(Tested=sum(value), Facilities_Reporting=length(unique(org_unit_id))), by=date]
trends = melt(trends, id.vars='date')
trends[variable=='Facilities_Reporting', variable:='Facilities Reporting']

ggplot(trends, aes(x=date, y=value, color=variable)) +
 geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  theme(text = element_text(size=18)) +
  scale_color_manual(values=c('#7a0177', '#fa9fb5')) +
  theme_bw() +
  labs(color="", x='Date', y='Count', title="Note: trends in counts reflect trends in reporting") +
  scale_y_continuous(labels = scales::comma)

#----------------------
# every variable on one graph

all = dt[ ,.(value=sum(value)), by=.(date , variable)]

ggplot(all, aes(x=date, y=value, color=variable)) +
  geom_point() +
  geom_line() +
  labs(x='Date', y='Count', 
       title='Counts of variables in PNLS-VCT', color="Variables in the VCT data set") +
  theme(text = element_text(size=18)) +
  theme_bw()

#-----------------------------------------------------------------

#-----------------------------------------------------------------
# TESTS PERFORMED

tests = dt[variable=='Tested' | variable=='Counseled' | variable=='Counseled and tested' | variable=='Tested and received the results']

tests$variable = factor(tests$variable,
                        c("Counseled", "Tested and received the results", "Tested",
                          "Counseled and tested"),                        
                          c("Counseled", "Tested and received the results", "Tested",
                          "Counseled and tested"))

#----------------------
# all tests by variable 
t1 = tests[ ,.(value=sum(value)), by=.(variable, date, subpop)]

ggplot(t1, aes(x=date, y=value, color=variable))+
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  facet_wrap(~subpop, scales='free_y') +
  scale_color_manual(values=c('#ef8a62', '#67a9cf', '#91cf60', '#998ec3')) +
  labs(color="", x='Date', y='Count', 
       title='HIV testing variables') +
  theme(text = element_text(size=18)) +
  scale_y_continuous(labels = scales::comma)

#----------------------
# all tests by variable - bar

t2 = tests[ ,.(value=sum(value)), by=.(variable, year=year(date))]
t2[ ,year:=as.factor(year)]

ggplot(t2, aes(x=variable, y=value, fill=year)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values=rev(brewer.pal(4, 'Blues'))) +
  labs(title = 'HIV testing variables', x="",
       subtitle = 'Counseled and Tested only available for all patients; Counseled and tested only available for key populations*',
       caption = '*Only Tested and received the results is comparable for all groups',
       y='Count', fill="Year") +
  theme(text = element_text(size=16))

#------------------------------
# subset to the key testing variables

tests_alt = dt[variable=='Counseled' | variable=='Tested and received the results']
t3 = tests_alt[ ,.(value=sum(value)), by=.(variable, year=year(date))]
t3[ ,year:=as.factor(year)]

ggplot(t3[variable=='Tested and received the results'], aes(x=variable, y=value, fill=year)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values=op) +
  labs(title = 'Tested for HIV and received the results', 
       y='Count', fill="Year", x="") +
  theme(text = element_text(size=18))

#------------------------------
# time trend of key testing variable

t4 = tests_alt[variable=='Tested and received the results', .(value=sum(value), facilities=length(unique(org_unit_id))), by=date]
t4[ , mean_tests:=round(value/facilities, 1)]
t4[ ,facilities:=NULL]
t4 = melt(t4, id.vars='date')
t4$variable = factor(t4$variable, c('value', 'mean_tests'),
                     c('Tested and received the results', 
                     'Number tested and received the results per facility'))

ggplot(t4, aes(x=date, y=value, color=variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  scale_color_manual(values=c('#0868ac', '#fec44f')) +
  labs(color="", x='Date', y='Count', 
       title='Tested for HIV and received the results') +
  theme(text = element_text(size=18)) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma)

#------------------------------
# hiv testing and counseling by sex
t5 = tests_alt[ ,.(value=sum(value)), by=.(variable, date, sex)]

ggplot(t5[!is.na(sex)], aes(x=date, y=value, color=sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable) +
  scale_color_manual(values=sex_colors) +
  labs(color="", x='Date', y='Count', 
       title='HIV testing and counseling',
       caption = "Note: counseled may not include key populations; checking") +
  theme(text = element_text(size=18)) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma)


#-----------------------------------
# # mean patients counseled and tested by facility and by sex
mean_tests = tests_alt[ , .(value=sum(value), facilities=length(unique(org_unit_id))), by=.(date, variable)]
mean_tests[ ,mean_tests:=value/facilities]

ggplot(mean_tests, aes(x=date, y=mean_tests, color=variable)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=op) +
  labs(color="", x='Date', y='Count',
       title='Mean HIV patients counseled and tested per facility per month') +
  theme(text = element_text(size=18))

#------------------------------
# tested and received the results by sex, subpop

t6 = tests_alt[variable=='Tested and received the results',.(value=sum(value)), by=.(date, sex, subpop)]

ggplot(t6, aes(x=date, y=value, color=sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~subpop, scales='free_y') +
  scale_color_manual(values=sex_colors) +
  labs(color="", x='Date', y='Count',
       caption = 'Note: gender of transgender people unlikely to be recorded consistently',
       title='HIV testing and counseling') +
  theme(text = element_text(size=18)) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma)

#-------------------------------------------------------------
# NORMALIZED TESTING VARIABLES

#-----------------------------------
# mean tests per facility by level
mean_tests2 = tests_alt[ , .(value=sum(value), facilities=length(unique(org_unit_id))), by=.(date, facility_level, next_level)]
mean_tests2[ , mean_tests:=value/facilities]

ggplot(mean_tests2[!is.na(facility_level)], aes(x=date, y=mean_tests, color=facility_level)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~next_level) +
  labs(color="", x='Date', y='Count', 
       title='Mean HIV tests per facility per month by level') +
  theme(text = element_text(size=14)) +
  scale_y_continuous(labels = scales::comma)

#-----------------------------------
# mean tests per facility per month by level 

sun = c(brewer.pal(9, 'YlOrRd'), brewer.pal(5, 'Reds'))

mean_tests_cat = tests_alt[ , .(value=sum(value), facilities=length(unique(org_unit_id))), by=.(date, facility_level)]
mean_tests_cat[ , mean_tests:=value/facilities]
mean_tests_cat[ , mean_tests_mo:=mean(mean_tests), by=facility_level]
mean_tests_cat = mean_tests_cat[ ,.(mean_tests=unique(mean_tests_mo)), by=facility_level][rev(order(mean_tests))]
mean_tests_cat[ ,mean_tests:=round(mean_tests)]

ggplot(mean_tests_cat[!is.na(facility_level)], aes(x=reorder(facility_level, -mean_tests), y = mean_tests, 
                          label=mean_tests, fill=facility_level)) +
  geom_bar(stat='identity') +
  geom_text(position=position_stack(vjust = 0.5), color='white') +
  scale_fill_manual(values=sun) +
  theme_bw() +
  labs(title='Mean tests performed at each facility per month by level',
       subtitle = 'Indicator: for each level and month, mean of total tests/total facilities',
       x='Facility level', y='Mean tests per facility per month', fill="Facility level") +
  theme(text=element_text(size=16), axis.text.x = (element_text(angle=90)))

#-----------------------------------
# mean tests per facility per month by level by year

mean_tests_cat = tests_alt[ , .(value=sum(value), facilities=length(unique(org_unit_id))), by=.(date, facility_level)]
mean_tests_cat[ , mean_tests:=value/facilities]
mean_tests_cat[ , year:=year(date)]
mean_tests_cat[ , mean_tests_mo:=round(mean(mean_tests)), by=.(facility_level, year)]
mean_tests_cat = mean_tests_cat[ ,.(mean_tests=unique(mean_tests_mo)), by=.(facility_level, year)]

ggplot(mean_tests_cat[!is.na(facility_level)], aes(x=reorder(facility_level, -mean_tests), y = mean_tests, 
                                                   label=mean_tests, fill=facility_level)) +
  geom_bar(stat='identity') +
  geom_text(position=position_stack(vjust = 0.5), color='white') +
  facet_wrap(~year) +
   scale_fill_manual(values=sun) +
  theme_bw() +
  labs(title='Mean tests performed at each facility per month by level, year',
       subtitle = 'Indicator: for each level, total tests/total facilities',
       x='Facility level', y='Mean tests per facility per month', fill="Facility level") +
  theme(text=element_text(size=16), axis.text.x = (element_text(angle=90)))

#-----------------------------------

#----------------------------------------------------------
# testing among key populations

#-------------------------------
# testing among key populations over time 
t5 = tests_alt[variable=='Tested and received the results',.(value=sum(value)), by=.(subpop, date)]
t5[subpop=='Patients', key_pop:='All patients']
t5[subpop!='Patients', key_pop:='Key population']

ggplot(t5, aes(x=date, y=value, color=subpop)) +
  geom_point() +
  geom_line() +
  facet_wrap(~key_pop, scales='free_y') +
  labs(color="", x='Date', y='Count', 
       title='Patients who were tested for HIV and received the results') +
  theme(text = element_text(size=18)) + 
  theme_bw() 

#-------------------------------
# counseling testing among key populations over time 
pop = t5[key_pop=='Key population']
pop[ ,key_pop:=NULL]
couns = dt[variable=='Counseled and tested',.(value=sum(value)), by=.(subpop, date)]
pop[, set:='Tested and received the results']
couns[ ,set:='Counseled and tested']
check = rbind(pop, couns)

ggplot(check, aes(x=date, y=value, color=set)) +
  geom_point() +
  geom_line() +
  facet_wrap(~subpop) +
  labs(color="", x='Date', y='Count', 
       title='Counseled and tested = patients tested for key populations',
       subtitle = "Diagnostic graph showing a repeat indicator") +
  theme(text = element_text(size=18)) + 
  theme_bw() 

#-------------------------------
# tests performed by key population by year
t5_bar = t5[ ,.(value=sum(value)), by=.(subpop, key_pop, year=year(date))]

ggplot(t5_bar, aes(x=subpop, y=value, label=value, fill=factor(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  facet_wrap(~key_pop, scales="free") +
  scale_fill_manual(values = c('#d73027', '#fdae61')) +
  labs(title = 'Tested for HIV and received the results by year',
       y='Tested for HIV', x="Key population", fill="Year") +
  theme(text = element_text(size=14), axis.text.x=element_text(size=12, angle=90))

#-------------------------------

key = t5[ ,.(value=sum(value)), by=.(date, key_pop)]
key = dcast(key, date~key_pop)
setnames(key, c("Date", "pts", "key_pop"))
key[ ,ratio:=round(100*(key_pop/pts), 1)]
key = melt(key, id.vars='Date')


ggplot(key, aes(x=Date, y=value, color=variable)) +
 geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(title = 'Tested for HIV and received the results by year',
       y='Tested for HIV', x="Key population", fill="Year") +
  theme(text = element_text(size=14))


#-------------------------------
# tests performed by key population and by funder by year
tf_bar = dt[ ,.(value=sum(value)), by=.(subpop, year=year(date), funder)]

ggplot(tf_bar[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=factor(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw() +
  facet_wrap(~funder) +
  scale_fill_manual(values = c('#9ebcda', '#0570b0')) +
  labs(title = 'Tested for HIV and received the results by funder, year',
       y='Tested for HIV', x="Key population", fill="Year") +
  theme(text = element_text(size=16), axis.text.x=element_text(size=12, angle=90)) +
  scale_y_continuous(labels = scales::comma)

#-------------------------------
# mean tests performed per facility for each sub population by year

t6 = t5[ ,.(value=sum(value)), by=.(subpop, key_pop, year=year(date))]
fac6 = dt[ ,.(facilities=length(unique(org_unit_id))), by=.(year=year(date))]
t6 = merge(t6, fac6, by='year')
t6[ ,mean_tests:=round(value/facilities, 1), by=.(year, subpop)]

ggplot(t6[subpop!='Patients'], aes(x=subpop, y=mean_tests, label=mean_tests, fill=factor(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=mean_tests), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values = c('#66bd63', '#fee08b')) +
  labs(title = 'Mean patients per health facility who were tested and received their results',
       y='Tested for HIV', x="Key population", fill="Year") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

#-------------------------------
# mean tests performed per facility for each sub population by funder, year

tf = dt[ ,.(value=sum(value)), by=.(subpop, funder, year=year(date))]
facf = dt[ ,.(facilities=length(unique(org_unit_id))), by=.(year=year(date), funder)]
tf = merge(tf, facf, by=c('year', 'funder'))
tf[ , mean_tests:=round(value/facilities), by=.(year, subpop, funder)]

ggplot(tf[subpop!='Patients'], aes(x=subpop, y=mean_tests, label=mean_tests, fill=factor(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=mean_tests), vjust=-1, position=position_dodge(0.9)) +
  facet_wrap(~funder) +
  theme_bw() +
  scale_fill_manual(values = c('#33a02c', '#b2df8a')) +
  labs(title = 'Mean patients per health facility who were tested and received their results',
       y='Tested for HIV', x="Key population", fill="Year") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

#-----------------------------------------------------------------
#-----------------------------------------------------------------
# HIV CASE  IDENTIFICATION 

#---------------------------------
cases = dt[variable=='HIV+' | variable=='HIV+ and informed of their results']

# check the cases variable
 c1 = cases[ ,.(value=sum(value)), by=.(sex, date, variable)] 
 
 ggplot(c1, aes(x=date, y=value, color=variable)) +
          geom_point() +
          geom_line() +
          facet_wrap(~sex) +
          scale_color_manual(values=bi) +
          theme_bw() +
   labs(title = 'Patients who tested HIV+',
        y='HIV+', x="Date", color='Sex') +
   theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))
 
# percentage informed of their results
 # this ratio should never be over 100
 
 
 
 
 c2 = dcast(c1, sex+date~variable)
 setnames(c2, c('HIV+', 'HIV+ and informed of their results'),
          c('hiv_pos', 'informed'))
c2[ , ratio:=100*round(informed/hiv_pos, 1)]
c2 = melt(c2, by=c('sex', 'date'))

ggplot(c2, aes(x=date, y=ratio, color=sex)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~sex) +
  labs(title = 'Percentage of patients who tested HIV+ who were informed of their results', 
       subtitle = 'This ratio should never be over 100: data quality question',
       y='Percent (%)', x="Date", color='Sex') +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

#---------------------------------------
# cases by key population, year, funder

hiv = dt[variable=='HIV+']

#------------------------
# hiv+ key populations case identified

pos_bar = dt[variable=='HIV+', .(value=sum(value)), by=subpop][rev(order(value))]
n = pos_bar[subpop!='Patients', sum(value)]

ggplot(pos_bar[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill='#fc9272')) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(position=position_stack(vjust = 0.5)) +
  theme_bw() +
  scale_fill_manual(values='#c51b7d') +
  labs(title = 'HIV cases identified by key population',
       subtitle=paste0('2017 - 2018 (n = ', n, ')'), x='Key Population',
       y='Number who tested HIV+') +
  guides(fill=FALSE) +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

#------------------------
# key populations by year

h1 = hiv[ , .(value=sum(value)), by=.(subpop, year=year(date))]
h1[ , year:=factor(year)]
n_1 = h1[subpop!='Patients' , sum(value)]

ggplot(h1[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=factor(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values=c('#92c5de', '#f4a582')) +
  labs(title = 'Key populations: tested positive for HIV',
       subtitle = paste0("n = ", n_1),
       y='HIV+', x="Key population", fill="Year") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

#------------------------
# tested HIV+ by funder

h2 = hiv[ , .(value=sum(value)), by=.(subpop, funder, year=year(date))]
n_2 = h2[ , sum(value)]

# key populations tested hiv+ by funder
ggplot(h2[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=funder)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  facet_wrap(~year) +
  theme_bw() +
  scale_fill_manual(values = c('#fdae61', '#313695')) +
  labs(title = 'Key populations: tested positive for HIV by funder',
       subtitle = (paste0("Total HIV cases identified: ", n_2)),
       y='HIV+', x="Key population", fill="Funder") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

# hiv+ cases identified by funder - stacked
pos_bar_funder = dt[variable=='HIV+', .(value=sum(value)), by=.(subpop, funder)][rev(order(value))]

ggplot(pos_bar_funder[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=funder)) +
  geom_bar(stat="identity") +
  geom_text(position=position_stack(vjust = 0.5)) +
  theme_bw() +
  scale_fill_manual(values = c('#fdae61', '#c2a5cf')) +
  labs(title = 'HIV cases identified by funder and key population',
       subtitle=paste0('2017 - 2018 (n = ', n, ')'), x='Key Population',
       y='Number tested HIV+', fill='Sex') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16))

# all patients tested hiv+ by funder
ggplot(h2[subpop=='Patients'], aes(x=subpop, y=value, label=value, fill=funder)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  facet_wrap(~year) +
  theme_bw() +
  scale_fill_manual(values = c('#5aae61', '#f1b6da')) +
  labs(title = 'Total patients tested positive for HIV by funder',
       subtitle= (paste0("Total HIV cases identified: ", n)),
       y='HIV+', x="Key population", fill="Funder") +
  theme(text = element_text(size=18))

#----------------------------------
# spaghetti plot of cases identified by risk group
pos = dt[variable=='HIV+', .(value=sum(value)), by=.(date, subpop)]

ggplot(pos[subpop!='csw_client'], aes(x=date, y=value, color=subpop)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(text=element_text(size=18))

# same graph by men, women
pos_bar_sex = dt[variable=='HIV+', .(value=sum(value)), by=.(subpop, sex)]

ggplot(pos_bar_sex[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=sex)) +
  geom_bar(stat="identity") +
  geom_text(position=position_stack(vjust = 0.5)) +
  theme_bw() +
  scale_fill_manual(values=c('#92c5de', '#f4a582')) +
  labs(title = 'HIV cases identified by sex and key population',
       subtitle=paste0('2017 - 2018 (n = ', n, ')'), x='Key Population',
       y='Number tested HIV+', fill='Sex*',
       caption='*Transgender people are likely misclassified') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16))

# same graph by men, women
pos_bar_sex = dt[variable=='HIV+', .(value=sum(value)), by=.(subpop, sex, year=year(date))]

ggplot(pos_bar_sex[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=sex)) +
  geom_bar(stat="identity") +
  geom_text(position=position_stack(vjust = 0.5)) +
  facet_wrap(~year) +
  theme_bw() +
  scale_fill_manual(values=c('#bf812d', '#80cdc1')) +
  labs(title = 'HIV cases identified by key population',
       subtitle=paste0('2017 - 2018 (n = ', n, ')'), x='Key Population',
       y='Number tested HIV+', fill='Sex*',
       caption='*Transgender people are likely misclassified') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16))

# all tests
# same graph by men, women
pos_bar_sex2 = dt[variable=='HIV+', .(value=sum(value)), by=.(sex, date, funder)]
  
ggplot(pos_bar_sex2, aes(x=date, y=value, label=value, fill=sex)) +
  geom_bar(stat="identity") +
  theme_bw() +
  facet_wrap(~funder) +
  scale_fill_manual(values=c('#bf812d', '#80cdc1')) +
  labs(title = 'HIV cases identified by key population',
       y='Number tested HIV+', fill='Sex*',
       caption='*Transgender people are likely misclassified') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16))

#------------------------

#-----------------------------------------------------------------
# Cascades

#-------------------------
# overall patients

#-------------------------
# tested and patients

cas = dt[variable=='Tested and received the results' | variable=='HIV+']

# tests performed and positive tests by population
cad1 = cas[ ,.(value=sum(value)), by=.(date, variable, subpop)]

ggplot(cad1, aes(x=date, y=value, color=variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~subpop, scales='free_y') +
  theme_bw() +
  labs(title = "HIV tests and positive HIV tests by sub-population",
       x='Date', y='Count', color="") +
  theme(text = element_text(size=18))

#-----------------------------------
# test positivity ratio over time

# tests performed and positive tests by population
cad2 = dcast(cad1, date+subpop~variable)
setnames(cad2, c("HIV+", "Tested and received the results"),
         c('hiv', 'tests'))

cad2[ , ratio:=round(100*(hiv/tests), 1)]
cad2[hiv > tests, ratio:=NA]

ggplot(cad2, aes(x=date, y=ratio, color=subpop)) +
  geom_point() +
  geom_line() +
  facet_wrap(~subpop, scales='free_y') +
  theme_bw() +
  labs(title = "Test positivity rate by sub-population",
       x='Date', y='Percent (%)', color="") +
  theme(text = element_text(size=18))

# same graph - test positivity by key pop
ggplot(cad2, aes(x=date, y=ratio, color=subpop)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title = "Test positivity rate by sub-population",
       x='Date', y='Percent (%)', color="") +
  theme(text = element_text(size=18))

#-----------------------------------
# tests and hiv+ tests by population bar

cad3 = cad2[ ,.(hiv=sum(hiv), tests=sum(tests)), by=.(subpop, year=year(date))]
cad3 = melt(cad3, id.vars=c('subpop', 'year'))
n_tests = cad3[subpop!='Patients' ,sum(value)]
cad3$variable = factor(cad3$variable, c('hiv', 'tests'), c('HIV+', 'HIV tests performed'))
test_cols = c('#f1a340', '#998ec3')

ggplot(cad3[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=variable)) +
  geom_bar(stat="identity") +
  facet_wrap(~year) +
  theme_bw() +
  scale_fill_manual(values=test_cols) +
  labs(title = 'HIV cases identified out of patients tested',
       subtitle=paste0('2017 - 2018 (n = ', n_tests, ')'), x='Key Population',
       y='Count', fill='Sex') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16))


#----------------------------------------------------

#----------------------------------------------------

list_of_plots = NULL
i=1

for(v in unique(dt$variable)) {
  
  name = as.character(v)
   
  new = dt[variable==v, .(value=sum(value)), by=date]
  
  list_of_plots[[i]] = ggplot(new, aes(x=date, y=value)) + 
    geom_point() + 
    geom_line(alpha=0.5) + 
    theme_bw() + 
    labs(title=name, x='Date', y='Count')
  i=i+1
}

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

# #----------------------------------------------------

dev.off() 


