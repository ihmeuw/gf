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

dt$subpop = factor(dt$subpop, c("miner", "couple", "csw_customer", "other_groups", "csw", "idu", "customer",
                                "trucker", "fisher", "serodisc", "msm", "prisoner", "uniform", "trans"),    
                   c('Miners', 'Couples', 'CSW Clients', 'Other Groups', 'CSWs', 'IDUs', 'Patients', 'Truckers', 'Fisherpeople',
                     'Serodiscordant couples', 'MSM', 'Prisoners', 'Military personnel', 'Trans people'))

# create smaller health facility groupings for graphs 
dt[grep('hospital',facility_level), next_level:='Hospitals']
dt[facility_level=='reference_health_center', next_level:='Reference health centers']
dt[facility_level=='health_center' | facility_level=='health_post' , next_level:='Health centers and health posts']
dt[is.na(next_level), next_level:='Other types of facilities']

#------------------------------------------------------------------
# HIV Testing Visualizations

# pdf(paste0(dir, 'outputs/pnls/pnls_vct_graphs.pdf'), width=12, height=7)

#----------------------
# COLOR SCHEMES

quad_colors = c('#542788','#66bd63', '#b2182b', '#4575b4')
sex_colors = c('#b2182b', '#4575b4')
tri_colors = c('#a50026', '#fdae61', '#abd9e9')
test_colors = c('#a50026', '#fdae61',  '#4575b4')

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
                    title="Reporting completeness: facilities reporting and patients receiving a consultation",
                    caption="Source: SNIS") +
  theme(text=element_text(size=18), axis.title=element_text(size=18), axis.text=(element_text(size=16)),
        plot.title=element_text(size=20), legend.text =element_text(size=18),
        plot.subtitle=element_text(size=16))

#-----------------
# reporting over time by health facility level

fac2 = dt[ ,.(facilities_reporting=length(unique(org_unit))), by=.(date, facility_level)]
fac2[facility_level=='health_center', hc:='Health centers']
fac2[facility_level!='health_center' | is.na(facility_level), hc:='Other types of health facilities']

ggplot(fac2, aes(x=date, y=facilities_reporting, color=facility_level)) +
  geom_point(alpha=0.4) +
  geom_line() +
  facet_wrap(~hc, scales='free_y') +
  theme_bw() + labs(x='Date', y='Facilities reporting', color='Health facility level',
                    title="Reporting completeness by health facility level",
                    subtitle=("Health centers reported separately in order to highlight scale of reporting"),
                    caption="Source: SNIS-PNLS")

#-----------------
# reporting by smaller groupings of health facilities

sample = dt[ ,length(unique(org_unit_id))]

final_report = dt[ ,.(facilities=length(unique(org_unit_id))), by=.(next_level, date)]
ggplot(final_report, aes(x=date, y=facilities, color=next_level)) +
  geom_point(alpha=0.4) +
  geom_line() +
  theme_bw() + labs(x='Date', y='Count', color='Health facility level',
                    title="Total health facilities reporting by level",
                    subtitle=(paste0(sample, "facilities have ever reported")),
                    caption="Source: SNIS")

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

ggplot(trends, aes(x=date, y=value, color=variable)) +
 geom_point() +
  geom_line() +
  facet_wrap(~variable, scales='free_y') +
  theme(text = element_text(size=18)) +
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
       title='Counts of variables', color="Variables in the VCT data set") +
  theme(text = element_text(size=18)) +
  theme_bw()

# every variable on one graph - bar
all2 = dt[ ,.(value=sum(value)), by=variable][order(value)]

ggplot(all2, aes(x=variable, y=value, fill='#998ec3')) +
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16)) +
  theme_bw() +
  scale_fill_manual(values='#998ec3') +
  guides(fill=F) +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90)) +
  scale_y_continuous(labels = scales::comma)

#-----------------------------------------------------------------
# tests performed 

tests = dt[variable=='Tested' | variable=='Counseled' | variable=='Tested and received the results' | variable=='Tested and informed of their results']

tests$variable = factor(tests$variable, c("Counseled", "Tested and received the results", "Tested",
                          "Counseled and tested", "Tested and informed of their results"),                        
        c("Counseled", "Tested and received the results", "Tested",
                          "Counseled and tested", "Tested and informed of the results"))

#----------------------
# all tests by variable 
t1 = tests[ ,.(value=sum(value)), by=.(variable, date)]

ggplot(t1, aes(x=date, y=value, color=variable))+
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=brewer.pal(5, 'RdYlBu')) +
  labs(color="", x='Date', y='Count', 
       title='HIv testing variables') +
  theme(text = element_text(size=26)) +
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
       y='Count', fill="Year") +
  theme(text = element_text(size=22), axis.text.x=element_text(size=12, angle=90))

#------------------------------
# subset to the key testing variables

tests_alt = dt[variable=='Counseled' | variable=='Tested and received the results']
t3 = tests_alt[ ,.(value=sum(value)), by=.(variable, year=year(date))]
t3[ ,year:=as.factor(year)]

ggplot(t3, aes(x=variable, y=value, fill=year)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values=sex_colors) +
  labs(title = 'HIV testing and counseling',
       y='Count', fill="Year") +
  theme(text = element_text(size=22), axis.text.x=element_text(size=12, angle=90))

#------------------------------
# time trend of key testing variable

t4 = tests_alt[ ,.(value=sum(value)), by=.(variable, date)]

ggplot(t4, aes(x=date, y=value, color=variable)) +
  geom_point() +
  geom_line() +
  labs(color="", x='Date', y='Count', 
       title='HIv testing and counseling') +
  theme(text = element_text(size=20)) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma)

#------------------------------
# mean patients counseled and tested by facility and by sex

# mean tests per facility
mean_tests = tests_alt[ , .(value=sum(value), facilities=length(unique(org_unit_id))), by=.(date, variable)]
mean_tests[ ,mean_tests:=value/facilities]

ggplot(mean_tests, aes(x=date, y=mean_tests, color=variable)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(color="", x='Date', y='Count', 
       title='Mean HIV patients counseled and tested per facility per month') +
  theme(text = element_text(size=20))

# mean tests per facility by level
mean_tests2 = tests_alt[ , .(value=sum(value), facilities=length(unique(org_unit_id))), by=.(date, facility_level, next_level)]
mean_tests2[ , mean_tests:=value/facilities]

ggplot(mean_tests2, aes(x=date, y=mean_tests, color=facility_level)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~next_level) +
  labs(color="", x='Date', y='Count', 
       title='Mean HIV tests per facility per month by level') +
  theme(text = element_text(size=20)) +
  scale_y_continuous(labels = scales::comma)

#------------------------------

#----------------------------------------------------------
# testing among key populations

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
       title='Counseled and tested = patients tested for key populations') +
  theme(text = element_text(size=18)) + 
  theme_bw() 


# tests performed by key population by year
t5_bar = t5[ ,.(value=sum(value)), by=.(subpop, key_pop, year=year(date))]

ggplot(t5_bar[key_pop=='Key population'], aes(x=subpop, y=value, label=value, fill=factor(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  labs(title = 'Tested for HIV and received the results by year',
       y='Tested for HIV', x="Key population", fill="Year") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))


# mean tests performed for each sub population by facility by year
t6 = t5[ ,.(value=sum(value)), by=.(subpop, key_pop, year=year(date))]
fac6 = dt[ ,.(facilities=length(unique(org_unit_id))), by=.(year=year(date))]
t6 = merge(t6, fac6, by='year')
t6[ ,mean_tests:=round(value/facilities, 1), by=.(year, subpop)]

ggplot(t6[subpop!='Patients'], aes(x=subpop, y=mean_tests, label=mean_tests, fill=factor(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=mean_tests), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  labs(title = 'Mean patients per health facility who were testged and received their results',
       y='Tested for HIV', x="Key population", fill="Year") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))


#-----------------------------------------------------------------
# case identification

cases = dt[variable=='HIV+' | variable=='HIV+ and informed of their results']







#-----------------------------------------------------------------
# Cascades

dt[variable=='New patients who received a treatment consultation', variable:='New patients']

# 
tested = dt[variable=="New patients" | variable=='Tested' | variable=='Counseled' ,
            .(value=sum(value)), by=.(date, variable)]

ggplot(tested, aes(x=date, y=value, color=variable)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=test_colors) +
  labs(color="", x='Date', y='Count', 
       title='New patients who were counseled and/or received a HIV test') +
  theme(text = element_text(size=26))











ggplot(pos_bar[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill='#fc9272')) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(position=position_stack(vjust = 0.5)) +
  theme_bw() +
  labs(title = 'HIV cases identified by key population',
       subtitle=paste0('2017 - 2018 (n = ', n, ')'), x='Key Population',
       y='Number who tested HIV+') +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))







# bar graph key populations 


dt[ ,subpop:=as.character(subpop)]

# spaghetti plot of cases identufied by risk group
pos = dt[variable=='HIV+', .(value=sum(value)), by=.(date, subpop)]

ggplot(pos[subpop!='csw_client'], aes(x=date, y=value, color=subpop)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(text=element_text(size=18))

pos_bar = dt[variable=='HIV+', .(value=sum(value)), by=subpop][rev(order(value))]
n = pos_bar[subpop!='Patients',sum(value)]

ggplot(pos_bar[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill='#fc9272')) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(position=position_stack(vjust = 0.5)) +
  theme_bw() +
  labs(title = 'HIV cases identified by key population',
       subtitle=paste0('2017 - 2018 (n = ', n, ')'), x='Key Population',
       y='Number who tested HIV+') +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

# same graph by men, women

pos_bar_sex = dt[variable=='HIV+', .(value=sum(value)), by=.(subpop, sex)][rev(order(value))]


ggplot(pos_bar_sex[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=sex)) +
  geom_bar(stat="identity") +
  geom_text(position=position_stack(vjust = 0.5)) +
  theme_bw() +
  labs(title = 'HIV cases identified by key population',
       subtitle=paste0('2017 - 2018 (n = ', n, ')'), x='Key Population',
       y='Number tested HIV+', fill='Sex*',
       caption='*Transgender people are likely misclassified') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16))


pos_bar_funder = dt[variable=='HIV+', .(value=sum(value)), by=.(subpop, funder)][rev(order(value))]


ggplot(pos_bar_funder[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=funder)) +
  geom_bar(stat="identity") +
  geom_text(position=position_stack(vjust = 0.5)) +
  theme_bw() +
  labs(title = 'HIV cases identified by funder and key population',
       subtitle=paste0('2017 - 2018 (n = ', n, ')'), x='Key Population',
       y='Number tested HIV+', fill='Sex*',
       caption='*Transgender people are likely misclassified') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16))






ggplot(rep_level, aes(x=level, y=sites, fill='GeneXpert Sites', label=sites)) +
  geom_bar(stat="identity") +
  geom_bar(aes(y=rep_level$reported, fill='Reported'), stat="identity") + 
  geom_text(size = 4, position=position_stack(vjust = 0.5)) +
  scale_fill_manual(name='', values=alt_bar_colors) + theme_minimal() +
  labs(x='Region', y='Total GeneXpert Sites', subtitle='Value = number of total sites*', 
       title="Number of GeneXpert sites reporting by health facility level",
       caption=paste0("*", no_reports, ' total facilities did not report.' )) +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12))



#----------------------------------------------------

# test positivity ratio over time








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


