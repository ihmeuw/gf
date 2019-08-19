
# EXPORT AS A PDF
pdf(paste0(dir, 'outputs/pnls_hiv_testing/pnls_vct_subset.pdf'), width=12, height=9)


#-------------------------------------------------------
# Loop of positive tests by key populations

pos = dt[variable=='HIV+' | variable=='Tested and received the results', .(value=sum(value)), by=.(date, subpop, variable)]
pos = dcast(pos, date+subpop~variable)
setnames(pos, c('date', 'subpop', 'hiv', 'tests'))
pos[ ,ratio:=100*(hiv/tests)]
pos[ , ratio:=round(ratio, 1)]
pos = melt(pos, id.vars=c('date', 'subpop'))
pos$variable = factor(pos$variable, c('tests', 'hiv', 'ratio'),
                      c('Tested and received the results', 'HIV+', "Percent positive (%)"))

totpts = dt[variable=='Tested and received the results' & subpop=='Patients', sum(value)]
tothiv = dt[variable=='HIV+' & subpop=='Patients', sum(value)]

for (s in unique(pos$subpop)) {
  
  pop_name = as.character(s)
  if (s == 'Couples') next
  
  p1 = ggplot(pos[subpop==s & variable!="Percent positive (%)"], aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_color_manual(values=c('#feb24c', '#fb6a4a')) +
    labs(title=paste0('Tests and HIV+ tests: ', pop_name), color="", x='Date', y="Count")
  
  p2 = ggplot(pos[subpop==s & variable!="Percent positive (%)"], aes(x=date, y=value, fill=variable)) +
    geom_bar(position = "fill",stat = "identity") +
    labs(title=paste0('Percentage of HIV tests that were positive: ', pop_name), x='Date', y = 'Ratio', fill="") +
    scale_fill_manual(values = c('#feb24c', '#fb6a4a')) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90))
  
  
  p3 = ggplot(pos[subpop==s & variable=="Percent positive (%)"], aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_color_manual(values='#8c96c6') +
    guides(color=FALSE) +
    labs(title=paste0('Percentage of HIV tests that were positive: ', pop_name), color="", x='Date', y="Percent (%)") + guides(fill=FALSE)
  
  p4 = ggplot(pos[subpop=='Patients' & variable=="Percent positive (%)"], aes(x=date, y=value, color=variable)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      scale_color_manual(values='#d73027') +
      guides(color=FALSE) +
      labs(title=paste0('Percentage of HIV tests that were positive: All patients'), color="", x='Date', y=" ",
         subtitle=paste0('Tested for HIV = ', totpts, "; tested HIV+ = ", tothiv)) 
  
  grid.arrange(p1, p2, p3, p4, nrow=2) }

#-------------------------------------------------------

#-----------------------------------
# tests and hiv+ tests by population as a percentage bar

ggplot(cad3, aes(x=subpop, y=value, label=value, fill=variable)) +
  geom_bar(position = "fill",stat = "identity") +
  facet_wrap(~year) +
  labs(title='Percentage of HIV tests that were positive',
       x='', y = 'Percent', fill="") +
  scale_fill_manual(values = c("#de2d26", "#fcbba1")) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90))

#-----------------------------------

#-----------------------------------
# tests and hiv+ tests by population bar

cad3 = cad2[ ,.(hiv=sum(hiv), tests=sum(tests)), by=.(subpop, year=year(date))]
cad3 = melt(cad3, id.vars=c('subpop', 'year'))
n_tests = cad3[subpop!='Patients' & variable=='tests', sum(value)]
cad3$variable = factor(cad3$variable, c('hiv', 'tests'), c('HIV+', 'HIV tests performed'))
test_cols = c('#f1a340', '#998ec3')

ggplot(cad3[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=variable)) +
  geom_bar(stat="identity") +
  facet_wrap(~year) +
  theme_bw() +
  scale_fill_manual(values=test_cols) +
  labs(title = 'HIV cases identified out of patients tested for key populations',
       subtitle=paste0('2017 - 2018 (n = ', n_tests, ' patients tested)'), x='Key Population',
       y='Count', fill='') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16))



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
# reporting by smaller groupings of health facilities

sample = dt[ ,length(unique(org_unit_id))]
final_report = dt[ ,.(facilities=length(unique(org_unit_id))), by=.(next_level, date)]
levels = dt[ ,.(total=length(unique(org_unit_id))), by=next_level]
final_report = merge(final_report, levels, by='next_level', all=T)
final_report[ ,label:=paste0(next_level, " (n=", total, ")")]

ggplot(final_report, aes(x=date, y=facilities, color=label)) +
  geom_point(alpha=0.4) +
  geom_line() +
  theme_bw() + labs(x='Date', y='Count', color='Health facility level',
                    title="Total health facilities reporting by level",
                    subtitle=(paste0(sample, " facilities reported to PNLS on HIV testing")),
                    caption="Source: SNIS")

#-----------------
# bar graph of reporting composition over time by health facility level

fac_bar = dt[ ,.(facilities = length(unique(org_unit_id))), by=.(date, facility_level)]

ggplot(fac_bar[!is.na(facility_level)], aes(x=date, y = facilities, fill = facility_level)) + 
  geom_bar(position = "fill",stat = "identity") +
  labs(x='Date', y='Percentage of all health facilities reporting',
       title='Composition of facilities reporting', fill="Facility level") +
  scale_fill_manual(values = colors12) +
  theme_bw()


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

ggplot(t1[!is.na(subpop)], aes(x=date, y=value, color=variable))+
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  facet_wrap(~subpop, scales='free_y') +
  scale_color_manual(values=c('#ef8a62', '#67a9cf', '#91cf60', '#998ec3')) +
  labs(color="", x='Date', y='Count', 
       title='HIV testing variables') +
  theme(text = element_text(size=18)) +
  scale_y_continuous(labels = scales::comma)

#------------------------------
# subset to the key testing variables

tests_alt = dt[variable=='Counseled' | variable=='Tested and received the results']
t3 = tests_alt[ ,.(value=sum(value)), by=.(variable, year=year(date), sex)]
t3[ ,year:=as.factor(year)]

ggplot(t3[variable=='Tested and received the results'], aes(x=variable, y=value, fill=year)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  facet_wrap(~sex) +
  theme_bw() +
  scale_fill_manual(values=op) +
  labs(title = 'Tested for HIV and received the results', 
       y='Count', fill="Year", x="") +
  theme(text = element_text(size=18)) + 
  scale_y_continuous(labels = scales::comma)

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
  guides(color=FALSE) +
  scale_y_continuous(labels = scales::comma)

#-----------------------------------
# mean patients counseled and tested by facility and by sex
mean_tests = tests_alt[ , .(value=sum(value), facilities=length(unique(org_unit_id))), by=.(date, variable)]
mean_tests[ ,mean_tests:=value/facilities]

ggplot(mean_tests, aes(x=date, y=mean_tests, color=variable)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_color_manual(values=op) +
  labs(color="", x='Date', y='Count',
       title='Mean HIV patients counseled and tested per facility per month', 
       subtitle = "Mean of total patients counseled or tested per month/total facilities reporting per month") +
  guides(color=FALSE) +
  theme(text = element_text(size=18))

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
  guides(fill=FALSE) +
  labs(title='Mean tests performed at each facility per month by level, year',
       subtitle = 'Indicator: for each level, total tests/total facilities',
       x='Facility level', y='Mean tests per facility per month', fill="Facility level") +
  theme(text=element_text(size=16), axis.text.x = (element_text(angle=90)))


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
       y='Tested for HIV', x="", fill="Year") +
  theme(text = element_text(size=14), axis.text.x=element_text(size=12, angle=90))

#-------------------------------
# tests on key populations out of all tests

key = t5[ ,.(value=sum(value)), by=.(date, key_pop)]
key = dcast(key, date~key_pop)
setnames(key, c("Date", "pts", "key_pop"))
key[ ,ratio:=round(100*(key_pop/pts), 1)]
key = melt(key, id.vars='Date')
key$variable = factor(key$variable, c('pts', 'key_pop', 'ratio'),
                      c('All patients', 'Key population', 'Percent of tests on key pops. (%)'))

ggplot(key, aes(x=Date, y=value, color=variable)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(title = 'Tested for HIV and received the results',
       x="Date", y="", color="") +
  guides(color=FALSE) +
  theme(text = element_text(size=14))

#-------------------------------
# counts of tests and tests among key populations 

p1 = ggplot(key[variable!="Percent of tests on key pops. (%)"], aes(x = Date, y = value, label=value, fill=variable)) +
  geom_bar(stat ="identity", position="stack") +
  labs(title="Tested for HIV and received the results", x="Date",
       y="Count", fill="") +
  theme_bw()

p2 = ggplot() + 
  geom_point(data = filter(key, variable %in% "Percent of tests on key pops. (%)"), aes(x = Date, y = value)) + 
  geom_line(data =  filter(key, variable %in% "Percent of tests on key pops. (%)"), aes(x = Date, y = value)) +
  labs(title = "Percent of tests among key populations", x="Date", y="Percent (%)") +
  theme_bw()

grid.arrange(p1, p2, nrow=1)

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

#------------------------
# key populations by year

h1 = hiv[ , .(value=sum(value)), by=.(subpop, year=year(date))]
h1[ , year:=factor(year)]
n_1 = h1[subpop!='Patients' , sum(value)]
n_2 = h1[subpop=='Patients' , sum(value)]

ggplot(h1[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=factor(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values=c('#92c5de', '#f4a582')) +
  labs(title = 'Key populations: tested positive for HIV',
       subtitle = paste0("n = ", n_1, "; total HIV+ patients = ", n_2),
       y='HIV+', x="Key population", fill="Year") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

#------------------------
# key populations by year compared to all patients 

h1 = hiv[ , .(value=sum(value)), by=.(subpop, year=year(date))]
h1[ , year:=factor(year)]
n_1 = h1[subpop!='Patients' , sum(value)]
h1[subpop!='Patients', pt:='Key Populations']
h1[subpop=='Patients', pt:='Patients']
pts_1 = h1[subpop=='Patients' ,sum(value)]

ggplot(h1, aes(x=subpop, y=value, label=value, fill=factor(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=value), vjust=-1, position=position_dodge(0.9)) +
  theme_bw() +
  facet_wrap(~pt, scales='free') +
  scale_fill_manual(values=c('#92c5de', '#f4a582')) +
  labs(title = 'Key populations: tested positive for HIV',
       subtitle = paste0("Key populations = ", n_1, "; total HIV+ patients = ", pts_1),
       y='HIV+', x=" ", fill="Year") +
  theme(text = element_text(size=18), axis.text.x=element_text(size=12, angle=90))

#------------------------ 
# hiv+ key pops and all patients over time
htime = dt[variable=='HIV+', .(value=sum(value)), by=.(date, subpop)]
htime[ , key:=(subpop=='Patients')]
htime = htime[ ,.(value=sum(value)), by=.(date, key)]
htime[key==FALSE ,key2:='All patients']
htime[key==TRUE ,key2:='Key populations']

ggplot(htime, aes(x=date, y=value, color=key2)) +
  geom_point() +
  geom_line() +
  labs(title='HIV+', x='Date', y='HIV+', color="",
       subtitle=paste0("Key populations: ", n_1, "; All patients: ", n_2)) +
  theme_bw() +
  theme(text = element_text(size=18))

#------------------------
# hiv+ cases identified by funder - stacked
pos_bar_funder = dt[variable=='HIV+', .(value=sum(value)), by=.(subpop, funder)]

ggplot(pos_bar_funder[subpop!='Patients'], aes(x=subpop, y=value, label=value, fill=funder)) +
  geom_bar(stat="identity") +
  geom_text(position=position_stack(vjust = 0.5)) +
  theme_bw() +
  scale_fill_manual(values = c('#fdae61', '#c2a5cf')) +
  labs(title = 'HIV cases identified by funder and key population',
       subtitle=paste0('2017 - 2018 (n = ', n, ')'), x='Key Population',
       y='Number tested HIV+', fill='Funder') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16))


#------------------------
# hiv+ cases identified by sex, funder 
pos_bar_sex2 = dt[variable=='HIV+', .(value=sum(value)), by=.(sex, date, funder)]

ggplot(pos_bar_sex2, aes(x=date, y=value, label=value, fill=funder)) +
  geom_bar(stat="identity") +
  theme_bw() +
  facet_wrap(~sex) +
  scale_fill_manual(values=c('#bf812d', '#80cdc1')) +
  labs(title = 'HIV cases identified by sex, funder',
       y='Number tested HIV+', fill='Funder', x ='Date') +
  theme(axis.text.x=element_text(size=16, angle=90), axis.text.y=element_text(size=16))


#-------------------------
# tested and patients

cas = dt[variable=='Tested and received the results' | variable=='HIV+']

#-------------------------
# tests performed and positive tests by subpopulation
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
labels = cad1[ ,.(value=sum(value)), by=subpop]
labels = labels[ ,label:=paste0(subpop, " (n=", value, ")")]
cad2 = dcast(cad1, date+subpop~variable)
setnames(cad2, c("HIV+", "Tested and received the results"),
         c('hiv', 'tests'))
cad2 = merge(cad2,labels, by="subpop", all=TRUE)

cad2[ , ratio:=round(100*(hiv/tests), 1)]
cad2[hiv > tests, ratio:=NA]

ggplot(cad2, aes(x=date, y=ratio, color=subpop)) +
  geom_point() +
  geom_line() +
  facet_wrap(~label, scales='free_y') +
  theme_bw() +
  labs(title = "Test positivity rate by sub-population",
       x='Date', y='Percent (%)', color="") +
  theme(text = element_text(size=18), legend.position="none")


dev.off()


