
#Prep the COD DHIS2 data 
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/18/2018
#
# Upload the RDS data from DHIS2 and merge with the meta data 
# prep the data sets for analysis and the Tableau Dashboard

# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(stringr) # to extract meta data from file names
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#--------------------
# Initial cleaning after download
# Import base services data set and convert to a data table

base <- readRDS(paste0(dir, 'prepped_data/base.rds'))
base <- data.table(base)

#--------------------
# label "category" for the graphs
base$category <- factor(base$category, levels=c(">5 ans", "<5 ans", "default", "Féminin, 5 ans et plus",   "Masculin, Moins de 5 ans",
                                               "Féminin, Moins de 5 ans", "Masculin, 5 ans et plus" ),
                                 labels=c("5 and over", "Under 5", "default", "Female, 5 and over", "Male, under 5",
                                          "Female, under 5", "Male, 5 and over"))
#--------------------
# create diseases or type-specific data sets

# create a malaria only data set
mal <- base[type=='malaria']
mal <- mal[year=='2017' | year=='2018']
mal[ ,.(unique(element), unique(element_id))]
mal[ ,.(unique(element), unique(element_fr))]
# create an HIV only data set

# Create a SVS subset

#---------------------------
# fix the english translations for the malaria indicators

# RDTs
mal[element_id=='CIzQAR8IWH1', element:='A 1.4 RDT - performed']
mal[element_id=='SpmQSLRPMl4', element:='A 1.4 RDT - positive']

# Cases
mal[element_id=='aK0QXqm8Zxn', element:='A 1.4 Presumed malaria']
mal[element_id=='JXm8J8GRJxI', element:='A 1.4 Presumed malaria, treated']
mal[element_id=='rfeqp2kdOGi', element:='A 1.4 Confirmed case of simple malaria']
mal[element_id=='nRm30I4w9En', element:='A 1.4 Confirmed case of simple malaria, treated']
mal[element_id=='AxJhIi7tUam', element:='A 1.4 Severe malaria']
mal[element_id=='CGZbvJchfjk', element:='A 1.4 Severe malaria treated']

# alternate 1.5 indicators - pregnant women
mal[element_id=='jocZb4TE1U2', element:='A 1.5 Confirmed case of simple malaria - pregnant woman']
mal[element_id=='wleambjupW9', element:='A 1.5 Confirmed simple malaria treated according to PN - pregnant woman']

mal[element_id=='scjrnpQxZ85', element:='A 1.5 Severe malaria - pregnant woman']
mal[element_id=='ukhWLb7dPET', element:='A 1.5 Severe malaria treated - pregnant woman']

# SP indicators phrased differently (translation OK)
mal[element_id=='okUsVMBrhZC', element:='A 2.1 Sulfadox. + Pyrimét 1st dose']   
mal[element_id=='IhUBOQkWKoo', element:='A 2.1 Sulfadox. + Pyrimét 4th dose']   

# LLIN indicators phrased differently
mal[element_id=='jrtkdRjvNKv', element:='A 2.1 LLINs distributed - ANC 2nd visit+']   
mal[element_id=='uV53nh3MrYl', element:='A 2.1 LLINs distributed - 1st ANC visit']  

#----------------------------------------

mal[ ,.(unique(element), unique(element_fr))]
mal[ ,.(unique(element), unique(element_id))]

#----------------------------------------------
# Malaria indicators - create unique data sets to graph
# Cases, RDTs, and LLINs


#----------------------------------------
# Malaria cases 
cases <- c('AxJhIi7tUam', 'CGZbvJchfjk', 'ELYUZa9bViU', 'JXm8J8GRJxI','Y7cmEYFWu5y',
           'aK0QXqm8Zxn', 'aZwnLALknnj','jocZb4TE1U2', 'rfeqp2kdOGi', 'scjrnpQxZ85',
           'ukhWLb7dPET', 'wleambjupW9', 'nRm30I4w9En')
mal_cases <- mal[element_id %in% cases ]

#-------------------
# tables for graphs

# all cases - presumed, suspected, treated
mal_cases_total <- mal_cases[ ,.(value=sum(value)), by=.(date, element, element_id, category)]
mal_cases_pr <- mal_cases[ ,.(value=sum(value)), by=.(date, element, element_id, province, mtk)] 

# compare cases to cases treated
mal_compare <- mal_cases[ ,.(value=sum(value)), by=.(date, element, element_id)]
mal_compare <- mal_compare[element_id=='aK0QXqm8Zxn' | element_id=='JXm8J8GRJxI' | element_id=='rfeqp2kdOGi' | element_id=='nRm30I4w9En']
mal_compare[element_id=='aK0QXqm8Zxn', presume:=1]
mal_compare[element_id=='JXm8J8GRJxI', presume:=1]
mal_compare[element_id=='nRm30I4w9En', presume:=0]
mal_compare[element_id=='rfeqp2kdOGi', presume:=0]
mal_compare$presume <- factor(mal_compare$presume, level=c(0, 1), labels=c('Confirmed malaria', 'Presumed malaria'))

mal_compare[element_id=='aK0QXqm8Zxn', treat:=0]
mal_compare[element_id=='JXm8J8GRJxI', treat:=1]
mal_compare[element_id=='nRm30I4w9En', treat:=1]
mal_compare[element_id=='rfeqp2kdOGi', treat:=0]


# presumed malaria/confirmed malaria
mal_pc <- mal_cases_total[element_id=='aK0QXqm8Zxn' | element_id=='JXm8J8GRJxI' | element_id=='rfeqp2kdOGi' | element_id=='nRm30I4w9En']
mal_presumed_pr <- mal_cases_pr[element_id=='aK0QXqm8Zxn' | element_id=='JXm8J8GRJxI']
mal_confirmed_pr <- mal_cases_pr[element_id=='rfeqp2kdOGi' | element_id=='nRm30I4w9En']

# severe cases 
mal_sev_total <- mal_cases_total[element_id=='AxJhIi7tUam' | element_id=='CGZbvJchfjk' | element_id=='scjrnpQxZ85' | element_id=='ukhWLb7dPET' ]
mal_sev_pr <- mal_cases_pr[element_id=='AxJhIi7tUam' | element_id=='CGZbvJchfjk' | element_id=='scjrnpQxZ85' | element_id=='ukhWLb7dPET' ]

#---------------------
# malaria cases graphs 

# malaria cases - presumed/confirmed, all DPS
ggplot(mal_pc, aes(x=date, y=value, color=category, group=category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element) +
  theme_bw() +
  labs(title='Presumed and confirmed malaria cases by age, all DPS') +
  scale_y_continuous(labels = scales::comma)



# severe malaria graphs
ggplot(mal_sev_total, aes(x=date, y=value, color=category, group=category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~element) +
  theme_bw() +
  labs(title='Cases of severe malaria, all DPS')


ggplot(mal_compare, aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  facet_wrap(~presume) +
  theme_bw() +
  labs(title='Cases diagnosed compared to cases treated, all DPS') +
  scale_y_continuous(labels = scales::comma)



# comparison between simple and confirmed, cases and cases treated 
ggplot(mal_pc, aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  facet_wrap(~category) +
  theme_bw() +
  labs(title='Malaria cases, all DPS') +
  scale_y_continuous(labels = scales::comma)

# presumed malaria cases, MTK
ggplot(mal_presumed_pr[mtk==1], aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  facet_wrap(~province) +
  theme_bw() +
  labs(title='Presumed malaria cases, Maniema, Kinshasa, and Tshopo', y='Count') +
  scale_y_continuous(labels = scales::comma)

# confirmed malaria cases, MTK
ggplot(mal_confirmed_pr[mtk==1], aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  facet_wrap(~province) +
  theme_bw() +
  labs(title='Confirmed malaria cases, Maniema, Kinshasa, and Tshopo', y='Count') +
  scale_y_continuous(labels = scales::comma)

# severe malaria, MTK
ggplot(mal_sev_pr[mtk==1], aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  facet_wrap(~province) +
  theme_bw() +
  labs(title='Severe malaria cases, Maniema, Kinshasa, and Tshopo', y='Count') +
  scale_y_continuous(labels = scales::comma)

# pregnant women


#----------------------------------------
#RDTs - completed and positive

# create data tables of rapid diagnostic test indicators
rdt_ids <- c('CIzQAR8IWH1', 'SpmQSLRPMl4')
rdt <- mal[element_id %in% rdt_ids]

rdt1 <- rdt[ ,.(value=sum(value)), by=.(date, element)]
rdt_pr <- rdt[ ,.(value=sum(value)), by=.(date, element, mtk, province)]

# ratio of RDTs positive to complete - total
rdt1[element=='A 1.4 RDT - completed', complete:=value]
rdt1[element=='A 1.4 RDT - positive', positive:=value]
rdt_ratio <- rdt1[ ,.(complete=sum(complete, na.rm=T), positive=sum(positive, na.rm=T)), by=date]
rdt_ratio[ , ratio:=100*(positive/complete)]

# ratio of RDTs positive to complete - dps
rdt_pr[element=='A 1.4 RDT - completed', complete:=value]
rdt_pr[element=='A 1.4 RDT - positive', positive:=value]
rdt_ratio_pr <- rdt_pr[ ,.(complete=sum(complete, na.rm=T), positive=sum(positive, na.rm=T)), by=.(date, province, mtk)]
rdt_ratio_pr[ , ratio:=100*(positive/complete)]


#------------------------------------------
# rdt graphs
ggplot(rdt1, aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x='Date', y='Number of tests', title='Rapid diagnostic tests (RDTs) for malaria - completed/positive, all DPS', 
       caption="Source: DHIS2", color='RDT') +
  scale_y_continuous(labels = scales::comma)

# percentage of RDTs completed that were positive
ggplot(rdt_ratio, aes(x=date, y=ratio)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x='Date', y='Number of tests', title='Percentage of RDTs completed that were positive', 
       caption="Source: DHIS2", color='RDT') +
  scale_y_continuous(labels = scales::comma)



ggplot(rdt_pr[mtk==1], aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~province) +
  labs(x='Date', y='Number of tests', title='Rapid diagnostic tests (RDTs) for malaria, provincial approach provinces', 
       caption="Source: DHIS2", color='Province') +
  scale_y_continuous(labels = scales::comma)

ggplot(rdt_ratio_pr[mtk==1], aes(x=date, y=ratio)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~province) +
  labs(x='Date', y='Percent', title='Ratio of positive RDTs to RDTs completed, provincial approach DPS', 
       caption="Source: DHIS2") +
  scale_y_continuous(labels = scales::comma)

ggplot(rdt_pr, aes(x=date, y=value, color=province, group=province)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~element) +
  labs(x='Date', y='Number of tests', title='Rapid diagnostic tests (RDTs) for malaria, all DPS', 
       caption="Source: DHIS2", color='Province') +
  scale_y_continuous(labels = scales::comma)

ggplot(rdt_ratio_pr, aes(x=date, y=ratio, color=province, group=province)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x='Date', y='Percent', title='Ratio of positive RDTs to RDTs completed, all DPS', 
       caption="Source: DHIS2") +
  scale_y_continuous(labels = scales::comma)


#-----------------------------------------
# confirmed cases to RDTs 





#----------------------------------------
# Sulfadox. + Pyrimét 

# Creat data tables of SP indicators
sp_ids <-  c('IhUBOQkWKoo', 'fsBcCCKHGUx', 'okUsVMBrhZC', 'pOZcn5GNnRD')  
sp <- mal[element_id %in% sp_ids]

# sp tables for graphs
sp1 <- sp[ ,.(value=sum(value)), by=.(date, element)]
sp_pr <- sp[ ,.(value=sum(value)), by=.(date, element, mtk, province)]

sp2 <- sp1[ ,.(value=sum(value)), by=element]
sp2[ ,element:=factor(element)]

doses <- c("1st dose", "2nd dose", "3rd dose", "4th dose") 

#---------------------------
# sp graphs
ggplot(sp1, aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x='Date', y='Count (doses distributed)', title='Sulfadoxine/pyrimethamine by dose', 
       caption="Source: DHIS2", color='Dose')

ggplot(sp_pr[mtk==1], aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~province) +
  labs(x='Date',  y='Count (distributed)', title='Sulfadoxine/pyrimethamine by dose, Maniema, Kinshasa, Tshopo', 
       caption="Source: DHIS2", color='Dose')

ggplot(sp_pr, aes(x=date, y=value, color=province, group=province)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~element) +
  labs(x='Date',  y='Count (doses distributed)', title='Sulfadoxine/pyrimethamine by dose, all DPS', 
       caption="Source: DHIS2", color='DPS') +
  scale_y_continuous(labels = scales::comma)

ggplot(sp2) +
  geom_bar(aes(x=as.integer(element) + 0.2, y= (value - 1000000) ), stat='identity', alpha=0.2, width=0.6) +
  geom_bar(aes(x=as.integer(element), y=value, fill=factor(element)), stat='identity', width=0.6) +
  theme_bw()  +
  labs(title='Sulfadoxine/pyrimethamine by dose, all DPS', subtitle='January 2017 - May 2018', fill='Dose',
       y='Count of doses distributed') +
  scale_x_continuous(breaks = 1:4,labels = doses) +
  scale_y_continuous(labels = scales::comma)



#----------------------------------------
# LLIns 

# Create LLIN data tables
llin_ids <- c('jrtkdRjvNKv', 'uV53nh3MrYl')
llin <- mal[element_id %in% llin_ids]

llin1 <- llin[ ,.(value=sum(value)), by=.(date, element)]
llin_pr <- llin[ ,.(value=sum(value)), by=.(date, element, mtk, province)]


# LLIN descriptive graphs
ggplot(llin1, aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x='Date', y='Number of tests', title='Long-lasting insecticide treated nets (LLINs) distributed', 
       caption="Source: DHIS2", color='LLINs') +
  scale_y_continuous(labels = scales::comma)

ggplot(llin_pr[mtk==1], aes(x=date, y=value, color=province, group=province)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~element) +
  labs(x='Date', y='Number of tests', title='Long-lasting insecticide treated nets (LLINs) distributed', 
       caption="Source: DHIS2", color='Province') +
  scale_y_continuous(labels = scales::comma)

ggplot(llin_pr, aes(x=date, y=value, color=province, group=province)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~element) +
  labs(x='Date', y='Number of tests', title='Long-lasting insecticide treated nets (LLINs) distributed', 
       caption="Source: DHIS2", color='Province') +
  scale_y_continuous(labels = scales::comma)

dev.off()
#----------------------------------------------





# hiv


#--------------------
# Survivors of sexual violence
svs <- base[svs==1]

# there are no stratifications for SVS
svs[ , category:=NULL]
svs[ , age:=NULL]
svs[ , sex:=NULL]

# print a list of data elements
svs[ ,.(unique(element), unique(element_id))]


svs_main <- c('AzbPgAky5SS', 'BzwsgyBcWgU', 'DecbSLAlRXR', 'xqXMQlrbZj3', 'MH8JTwkch8B', 'WP3amzUfh41', 'EDLJ2mtHBUx')
svs3 <- svs[svs_main]

# time series map of SVS 
svs_cases <- svs[element_id=='AzbPgAky5SS' | element_id=='BzwsgyBcWgU']
svs_cases1 <- svs_cases[ ,.(value=sum(value)), by=.(element, date, month, year)]
svs_cases_pr <- svs_cases[year=='2017' | year=='2018' ,.(value=sum(value)), by=.(element, date, province)]
svs_cases_mtk <- svs_cases[year=='2017' | year=='2018' ,.(value=sum(value)), by=.(element, date, year, mtk)]

svs3 <- svs3[year==2017 | year==2018]
svs_3_tot <- svs3[ ,.(value=sum(value)), by=.(date, element)]









pdf(paste0(dir, 'dhis_descriptives.pdf'), height=6, width=9)

# new cases SVS and female svs
ggplot(svs_3_tot, aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x='Date', y='Count', title='Survivors of sexual violence', caption='Source:DHIS2', subtitle='January 2017 - May 2018') + 
  scale_y_continuous(labels = scales::comma)


# new cases SVS and female svs
ggplot(svs_cases1, aes(x=date, y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x='Date', y='New cases', title='Survivors of sexual violence', caption='Source:DHIS2') + 
  scale_y_continuous(labels = scales::comma)

# reporting
ggplot(svs_cases1, aes(x=factor(month), y=value, color=element, group=element)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~year, scale='free_y') +
  labs(x='Date', y='Month', title='Survivors of sexual violence - reporting completeness by year', caption='Source:DHIS2') + 
  scale_y_continuous(labels = scales::comma)

# new cases SVS and female svs
ggplot(svs_cases_pr, aes(x=date, y=value, color=province, group=province)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~element, scales='free_y') +
  labs(x='Date', y='New cases', title='Survivors of sexual violence', caption='Source:DHIS2') + 
  scale_y_continuous(labels = scales::comma)

# new cases SVS and female svs MTK compared to highest province
ggplot(svs_cases_pr[province=='Maniema' | province=='Tshopo' | province=='Kinshasa' | province=='Nord-Kivu' ],
      aes(x=date, y=value, color=province, group=province)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~element, scales='free_y') +
  labs(x='Date', y='New cases', title='Survivors of sexual violence, highest province vs. MTK', caption='Source:DHIS2') + 
  scale_y_continuous(labels = scales::comma)

# new cases SVS and female svs MTK
ggplot(svs_cases_pr[province=='Maniema' | province=='Tshopo' | province=='Kinshasa'],
       aes(x=date, y=value, color=province, group=province)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~element, scales='free_y') +
  labs(x='Date', y='New cases', title='Survivors of sexual violence', caption='Source:DHIS2', color='DPS') + 
  scale_y_continuous(labels = scales::comma)




pdf(paste0(dir, 'outputs.pdf'), height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
}

dev.off()

list_of_plots = NULL
i=1
for(f in unique(test$element_id)) {
  # look up district name
  name <-  unique(test[element_id==f]$element)
  
  # make your graph
  
  list_of_plots[[i]] <- ggplot(test[element_id==f], aes(x=date, y=value, color=category, group=category)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(x='Date', y='Count', color='Category', title=name)
  
  
  
  i=i+1
  
}











