# Prep the ARV data set in PNLS
# Final prep file for usable data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/30/19
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
setwd("C:/Users/ccarelli/local/gf/")

# read in the data 
dt = readRDS(paste0(dir, 'prepped/pnls_arv.rds'))

# subset to before August of 2018
dt = dt[date < '2018-09-01']

#--------------------------
# sum over case as it is not useful
vars = c("org_unit_id",   "org_unit", "date", "element",
         "subpop", "sex", "age", "org_unit_type", "level", 
         "dps", "health_zone", "mtk", "maternity", "tb")

dt = dt[ ,.(value=sum(value)), by=vars]

# read in the outliers and remove them
out = readRDS(paste0(dir, 'pnls_outliers/list_of_arv_outliers.rds'))

# create a unique identifier to remove
out[ ,combine:=paste0(org_unit_id, element, age, sex, subpop, as.character(date))]
dt[ ,combine:=paste0(org_unit_id, element, age, sex, subpop, as.character(date))]

# drop the outliers 
dt = dt[!combine %in% out$combine]
dt[ ,combine:=NULL]

# remove one outlier by hand in which the qr did not work
dt = dt[!(value==651 & element=="Malnourished PLHIV who received supplemental nutrition")]

# --------------------
# source the standardization functions for the geographic units and apply
source("./core/standardizeHZNames.R")
source("./core/standardizeDPSNames.R")
dt$health_zone = standardizeHZNames(dt$health_zone)
dt$health_zone = standardizeDPSNames(dt$dps)

# -------------------------------------------------
# ARV Visuals 

pdf(paste0(dir, 'outputs/pnls/arv_initial_graphs_new.pdf'), width=12, height=7)
#----------------------
# COLOR SCHEMES

quad_colors = c('#542788','#66bd63', '#b2182b', '#4575b4')
sex_colors = c('#b2182b', '#4575b4')
tri_colors = c('#a50026', '#fdae61', '#abd9e9')

#----------------------
# Reporting completeness

# facilities reporting and patients on art by sex
fac = dt[ ,.(facilities_reporting=length(unique(org_unit))), by=date]
pts = dt[element=="Patients still on ART in the structure" | element=="Patients on ART in the Podi" 
         ,.(patients=sum(value)), by=.(sex, date)]
fac = merge(fac, pts, by='date')

# add in total patients 
tot = dt[element=="Patients still on ART in the structure" | element=="Patients on ART in the Podi" 
               ,.(total=sum(value)), by=date]
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
                      c("Health facilities reporting", "Patients on ART"))
fac$sex = factor(fac$sex, c('Health facility', 'Total', 'Female', 'Male'),
                 c('Health facility', 'Total patients enrolled', 
                   'Female', 'Male'))
lines = c('solid', 'dashed', 'solid', 'solid', 'solid')

# plot of facilities reporting and art enrollment
ggplot(fac, aes(x=date, y=value, color=sex, group=sex)) +
  geom_point(alpha=0.4) +
  geom_line(aes(linetype=sex)) +
  scale_linetype_manual(values=lines, guide=F) +
  facet_wrap(~variable, scales='free_y') +
  scale_color_manual(values=quad_colors) +
  theme_bw() + labs(x='Date', y='Count', color='',
                    title="Reporting completeness and scale up: PNLS Canevas Unique FOSA",
                    caption="Source: SNIS") +
  theme(axis.title=element_text(size=18), axis.text=(element_text(size=16)), 
        plot.title=element_text(size=20), legend.text =element_text(size=18), 
        plot.subtitle=element_text(size=16))

#-----------------
# reporting over time by health facility level
fac2 = dt[ ,.(facilities_reporting=length(unique(org_unit))), by=.(date, level)]
fac2[level=='health_center', hc:='Health centers']
fac2[level!='health_center' | is.na(level), hc:='Other types of health facilities']

ggplot(fac2, aes(x=date, y=facilities_reporting, color=level)) +
  geom_point(alpha=0.4) +
  geom_line() +
  facet_wrap(~hc, scales='free_y') +
  theme_bw() + labs(x='Date', y='Facilities reporting', color='Health facility level',
                    title="Reporting completeness by health facility level",
                    subtitle=("Health centers reported separately to highlight scale of reporting"),
                    caption="Source: SNIS")

#----------------------------------------------------
# HIV-RELATED SERVICES

serv_vars = c("PLHIV screened for TB in the month", "PLHIV on Cotrimoxazole prophylaxis",
  "PLHIV who received a CD4 test", "PLHIV enrolled in HIV-related services",
  "PLHIV on IPT", "Malnourished PLHIV who received supplemental nutrition",
  "PLHIV who received a nutritional assessment and counseling", "Patients still on ART in the structure",
  "Patients on ART in the Podi")
serv = dt[element %in% serv_vars]
serv = serv[ ,.(value=sum(value)), by=.(sex, date, element)]

# rename the variables and fix the order
serv$element= factor(serv$element, c("Patients still on ART in the structure", "Patients on ART in the Podi",
   "PLHIV on Cotrimoxazole prophylaxis", "PLHIV screened for TB in the month",  "PLHIV on IPT", 
       "PLHIV who received a CD4 test", "PLHIV enrolled in HIV-related services",
        "PLHIV who received a nutritional assessment and counseling",
           "Malnourished PLHIV who received supplemental nutrition"),
                     c("Patients on ART in health facilities", "Patients on ART in Podi",
                                     "PLHIV on Cotrimoxazole prophylaxis", "PLHIV screened for TB ",  "PLHIV on IPT", 
                                      "PLHIV who received a CD4 test", "PLHIV enrolled in HIV-related services",
                                       "PLHIV who received a nutritional assessment",
                                       "Malnourished PLHIV who received nutrition"))

# summary plot of hiv-related service variables 
ggplot(serv, aes(x=date, y=value, color=sex)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~element, scales='free_y') +
  scale_color_manual(values=sex_colors) +
  theme_bw() +
  labs(color='Sex', x='Date', y='Count',
       title="Summary of HIV-related services") + 
  scale_y_continuous(labels = scales::comma)

#----------------------------------------------------
# TB-related

tb = dt[tb==T | element=='Patients still on ART in the structure' | element=="Patients on ART in the Podi"]
tb[element=='Patients still on ART in the structure' | element=="Patients on ART in the Podi", element:="Patients on ART"]

#---------------------------------------
# summary of TB-related variables
tb_sex = tb[ ,.(value=sum(value)), by=.(element, date, sex)]
tb_sex[element=="PLHIV diagnosed with TB who started anti-TB treatment during the month",
         element:="PLHIV diagnosed with TB who started TB treatment"]

tb_sex$element = factor(tb_sex$element, c("Patients on ART", "PLHIV screened for TB in the month", "PLHIV on IPT",
                "TB/HIV co-infected patients on ART", "TB cases detected among PLHIV in the month",
                "PLHIV diagnosed with TB who started TB treatment"),
                c("Patients on ART", "PLHIV screened for TB", "PLHIV on IPT",
                "TB/HIV co-infected patients on ART", "TB cases detected among PLHIV",
                 "PLHIV diagnosed with TB who started TB treatment"))

# summary of all tb/hiv variables
ggplot(tb_sex, aes(x=date, y=value, color=sex)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~element, scales='free_y') +
  scale_color_manual(values=sex_colors) +
  theme_bw() +
  labs(color='Sex', x='Date', y='Count',
       title="Summary of TB/HIV variables")

#---------------------------------------
# IPT summary

ipt = tb[element=="Patients on ART" | element=="PLHIV on IPT",
              .(value=sum(value)), by=.(date, sex, element)]

# Patients on IPT
ggplot(ipt, aes(x=date, y=value, color=element)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~sex, scales='free_y') +
  scale_color_manual(values=tri_colors) +
  theme_bw() +
  labs(color='', x='Date', y='Count',
       title="HIV+ patients on Isoniazid Preventive Therapy")

#---------------------------------------
# TB screening 

# tb screening summary  
tb_screen = tb[element=="Patients on ART" | element=="PLHIV screened for TB in the month" | element=="TB cases detected among PLHIV in the month"]
tb_screen = tb_screen[ ,.(value=sum(value)), by=.(date, element, sex)]
tb_screen = dcast(tb_screen, sex+date~element)
setnames(tb_screen, c('Sex', 'Date','screened', 'on_art', 'detected'))
tb_screen[ , screen_ratio:=round(100*(screened/on_art), 1)]
tb_screen[ , pos_ratio:=round(100*(detected/screened), 1)]

tb_ratio = tb_screen[ ,.(screen_ratio, pos_ratio, Date, Sex)]
tb_ratio = melt(tb_ratio, id.vars=c('Date', 'Sex'))

tb_ratio$variable = factor(tb_ratio$variable, c('screen_ratio', 
          'pos_ratio'), c("Percent of PLHIV screened for TB", 
                          "Percent of PLHIV diagnosed with TB"))

# percent screened and diagnosed with tb
ggplot(tb_ratio, aes(x=Date, y=value, color=Sex)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  scale_color_manual(values=sex_colors) +
  labs(color='Sex', x='Date', y='Percent (%)',
       title='Percentage of PLHIV on ART screened for TB and diagnosed with TB',
       caption='Source: SNIS PNLS') +   
  theme(axis.title=element_text(size=18), axis.text=(element_text(size=16)), 
                                              plot.title=element_text(size=20), legend.title=element_text(size=16),
                                              legend.text =element_text(size=18), 
                                              plot.subtitle=element_text(size=16), strip.text=element_text(size=16))

#---------------------------------------
# tb screening summary  by age and sex
tb_age = tb[element=="Patients on ART" | element=="PLHIV screened for TB in the month" | element=="TB cases detected among PLHIV in the month"]
tb_age = tb_age[ ,.(value=sum(value)), by=.(date, element, sex, age)]
tb_age = dcast(tb_age, sex+age+date~element)
setnames(tb_age, c('Sex', 'Age', 'Date','screened', 'on_art', 'detected'))
tb_age[ , screen_ratio:=round(100*(screened/on_art), 1)]
tb_age[ , pos_ratio:=round(100*(detected/screened), 1)]

tb_ratio1 = tb_age[ ,.(screen_ratio, pos_ratio, Date, Sex, Age)]
tb_ratio1 = melt(tb_ratio1, id.vars=c('Date', 'Sex', 'Age'))

tb_ratio1$variable = factor(tb_ratio1$variable, c('screen_ratio', 
                                                'pos_ratio'), c("Percent of PLHIV screened for TB", 
                                                                "Percent of PLHIV diagnosed with TB"))

# percent screened and diagnosed with tb - males
ggplot(tb_ratio1[Sex=='Male' & !is.na(Age)], aes(x=Date, y=value, color=Age)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  scale_color_manual(values=brewer.pal(9, 'RdYlBu')) +
  labs(color='Age', x='Date', y='Percent (%)',
       title='Percentage of PLHIV on ART screened for and diagnosed with TB: Males',
       caption='Source: SNIS PNLS')

# percent screened and diagnosed with tb - females
ggplot(tb_ratio1[Sex=='Female' & !is.na(Age)], aes(x=Date, y=value, color=Age)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  scale_color_manual(values=brewer.pal(9, 'RdYlBu')) +
  labs(color='Sex', x='Date', y='Percent (%)',
       title='Percentage of PLHIV on ART screened for and diagnosed with TB: Females',
       caption='Source: SNIS PNLS')

#------------------
# associated counts of art enrolled, screened, TB+
tb_screen_sub = tb_screen[ ,.(Sex, Date, screened, on_art, detected)]
tb_screen_sub = melt(tb_screen_sub, id.vars=c('Sex', 'Date'))

# label the variables
tb_screen_sub$variable = factor(tb_screen_sub$variable, c('on_art',
              'screened', 'detected'), c('Enrolled on ART',
              'Screened for TB', 'TB cases detected') )

# art enrolled, screened for tb, tb cases detected
ggplot(tb_screen_sub, aes(x=Date, y=value, color=Sex)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  scale_color_manual(values=sex_colors) +
  labs(color='Sex', x='Date', y='Count',
       title='PLHIV enrolled on ART, screened for TB, and diagnosed with TB')

#------------------
# comparative counts - enrolled on ART and screened by sex

tb_hiv = tb[element=="PLHIV screened for TB in the month" | element=="Patients on ART"]
tb_hiv = tb_hiv[ ,.(value=sum(value)), by=.(date, element, sex)]
tb_hiv_wide = dcast(tb_hiv, sex+date~element)
setnames(tb_hiv_wide, c('Sex', 'Date', 'screened', 'on_art'))
tb_hiv_wide[ ,ratio:=round(100*(screened/on_art), 1)]

# count of patients screened for tb out of those on ART
ggplot(tb_hiv, aes(x=date, y=value, color=element)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~sex) +
  theme_bw() +
  scale_color_manual(values=sex_colors) +
  labs(color=' ', x='Date', y='Count', 
       title="Patients on ART screened for TB by sex")

#------------------
# comparative counts - detected and started treatment

tb_tx = tb[element=="TB cases detected among PLHIV in the month" | element=="PLHIV diagnosed with TB who started anti-TB treatment during the month"]
tb_tx = tb_tx[ ,.(value=sum(value)), by=.(date, element, sex)]
tb_tx = dcast(tb_tx, sex+date~element)

# add a ratio
setnames(tb_tx, c('Sex', 'Date', 'txed', 'cases'))
tb_tx[ ,ratio:=round(100*(txed/cases), 1)]
tb_tx = melt(tb_tx, id.vars=c('Sex', 'Date'))


tb_tx$variable = factor(tb_tx$variable, c('cases', 'txed', 'ratio'),
                        c('TB cases detected among PLHIV', 
                          'PLHIV diagnosed with TB who started anti-TB treatment',
                          'Percentage of cases treated'))

# count of patients screened for tb out of those on ART
ggplot(tb_tx[variable!='Percentage of cases treated'], aes(x=Date, y=value, color=variable)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~Sex, scales='free_y') +
  theme_bw() +
  scale_color_manual(values=sex_colors) +
  labs(color='Sex', x='Date', y='Count', 
       title="TB cases detected who were treated (equality issues)")

#--------------------
# pregnant and lactating women 

# tb screening among plw
tb_plw = tb[element=="Patients on ART" | element=="PLHIV screened for TB in the month" | element=="TB cases detected among PLHIV in the month"]
tb_plw = tb_plw[subpop=='plw']
tb_plw = tb_plw[ ,.(value=sum(value)), by=.(date, element)]
tb_plw = dcast(tb_plw, date~element)

# create a ratio data set
setnames(tb_plw, c('Date','screened', 'on_art', 'detected'))
tb_plw[ , screen_ratio:=round(100*(screened/on_art), 1)]
tb_plw[ , pos_ratio:=round(100*(detected/screened), 1)]
tb_plw_ratio = tb_plw[ ,.(screen_ratio, pos_ratio, Date)]
tb_plw[ ,c('screen_ratio', 'pos_ratio'):=NULL]

tb_plw = melt(tb_plw, id.vars='Date')

# label the variables
tb_plw$variable = factor(tb_plw$variable, c('on_art',
                'screened', 'detected'), c('Enrolled on ART',
               'Screened for TB', 'TB cases detected'))

# counts of plw screened for and diagnosed with tb
ggplot(tb_plw, aes(x=Date, y=value)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(x='Date', y='Count',
       title='HIV+ pregnant and lactating women screened for and diagnosed with TB',
       caption='Source: SNIS PNLS')

tb_plw_ratio = melt(tb_plw_ratio, id.vars='Date')
tb_plw_ratio$variable = factor(tb_plw_ratio$variable, c('screen_ratio', 'pos_ratio'), 
                    c('Percent screened for TB', 'TB cases detected'))

# percent screened and diagnosed with tb
ggplot(tb_plw_ratio, aes(x=Date, y=value)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  facet_wrap(~variable, scales='free_y') +
  labs(x='Date', y='Percent(%)',
       title='Percentage of HIV+ Pregnant and lactating women screened for and diagnosed with TB',
       caption='Source: SNIS PNLS')

#----------------------------------------------------
# post-exposure prophylaxis

pep = dt[element=="HIV-exposed Persons who received a PEP Kit" | element=="HIV-exposed Persons who received a PEP Kit within 72 hours"]
pep = pep[ ,.(value=sum(value)), by=.(date, sex, element)]

# post-exposure prophylaxis
ggplot(pep, aes(x=date, y=value, color=sex)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  facet_wrap(~element, scales='free_y') +
  scale_color_manual(values=sex_colors) +
  labs(x='Date', y='Count',
       title='HIV-exposed persons who received post-exposure prophylaxis',
       caption='Source: SNIS PNLS')

#----------------------------------------------------
# Sruvivors of sexual violence

# SUBSET TO SVS
svs = dt[subpop=='svs']

# calculate a ratio for the label
sex_ratio = svs[ ,.(value=sum(value)), by=.(date, sex)]
sex_ratio[ ,.(value=mean(value)), by=sex]
sex_ratio = dcast(sex_ratio, date~sex)
sex_ratio[ , percent_f:=100*(Female/(Female + Male))]
mean = sex_ratio[ , .(mean=round(mean(percent_f), 1))]

svs = svs[ ,.(value=sum(value)), by=.(element, date, age)]

# summary of survivors of sexual violence
ggplot(svs, aes(x=date, y=value, color=age)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  facet_wrap(~element, scales='free_y') +
  scale_color_manual(values=rev(brewer.pal(9, 'BrBG'))) +
  labs(x='Date', y='Count',
       title='Survivors of sexual violence', color="Age",
       subtitle = paste0('Mean monthly percent female: ', mean, '%; age data are complete for SVS'),
       caption='Source: SNIS PNLS')

svs$element = factor(svs$element, c("HIV-exposed Persons who received a PEP Kit",
            "HIV-exposed Persons who received a PEP Kit within 72 hours", 
                "Received medical care within 72 hours"), c("HIV-exposed Persons who received a PEP Kit",
                  "SVS received a PEP kit within 72 hours",
                  "SVS received medical care within 72 hours"))

# summary of survivors of sexual violence
ggplot(svs[element!="HIV-exposed Persons who received a PEP Kit"], aes(x=date, y=value, color=age)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  facet_wrap(~element, scales='free_y') +
  scale_color_manual(values=rev(brewer.pal(9, 'BrBG'))) +
  labs(x='Date', y='Count',
       title='Survivors of sexual violence', color="Age",
       subtitle = paste0('Mean monthly percent female: ', mean, '%; age data are complete for SVS'),
       caption='Source: SNIS PNLS') +
  theme(axis.title=element_text(size=18), axis.text=(element_text(size=16)), 
        plot.title=element_text(size=20), legend.title=element_text(size=16),
        legend.text =element_text(size=18), 
        plot.subtitle=element_text(size=16), strip.text=element_text(size=16))




#----------------------------------------------------
# viral load testing



#----------------------------------------------------
# visualize all of the variables
# organize the order

element_vector = c("Patients still on ART in the structure",  "Patients on ART in the Podi",       
                   "PLHIV awaiting care", "PLHIV registered as LTFU in the course of a month",
                   "PLHIV who developed side effects", "PLHIV who received a CD4 test", 
                   "PLHIV enrolled in HIV-related services",  "PLHIV on Cotrimoxazole prophylaxis", 
                   "Victims of accidental exposure","Received medical care within 72 hours",
                   "HIV-exposed Persons who received a PEP Kit", "HIV-exposed Persons who received a PEP Kit within 72 hours", 
                   "PLHIV screened for TB in the month","PLHIV on IPT", "TB/HIV co-infected patients on ART", 
                   "TB cases detected among PLHIV in the month", 
                   "PLHIV diagnosed with TB who started anti-TB treatment during the month", 
                   "PLHIV who received a nutritional assessment and counseling", 
                   "Malnourished PLHIV", "Malnourished PLHIV who are not responding to treatment", 
                   "Malnourished PLHIV LTFU", "Malnourished PLHIV who received supplemental nutrition", "Malnourished PLHIV who were rehabilitated",                               
                   "Malnourished PLHIV who died", "PLHIV who received an initial viral load test",
                   "PLHIV who received a viral load test", "PLHIV with an undetectable viral load after six months", 
                   "PLHIV on ARVs who received a viral load test after six months", "PLHIV with an undetectable viral load", 
                  "PLHIV on ART with an undetectable viral load after the initial test", "Registered deaths of PLHIV in the course of a month")

loop = dt[ ,.(value=sum(value)), by=.(element, date, sex)]
list_of_plots = NULL
i=1

for(e in element_vector) {
  # look up district name
   
  name = e
  list_of_plots[[i]] = ggplot(loop[element==e], aes(x=date, y=value, color=sex)) + 
    geom_point() + 
    geom_line(alpha=0.5) + 
    scale_color_manual(values=sex_colors) +
    theme_bw() + 
    labs(title=name, x='Date', y='Count', color='Sex')
    i=i+1
}

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

# #----------------------------------------------------
# 
# # deaths 
# # pull in gbd estimates 
# deaths = dt[element=="Registered deaths of PLHIV in the course of a month" ]

# deaths = dt[element=="Registered deaths of PLHIV in the course of a month"]

dev.off() 


