# PNLP and SNIS Comparison
# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 9/13/2018

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
# --------------------

# --------------------
# set working directories 

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

pnlpFile <- paste0(dir, '../National_Malaria_Program/pre_imputation_PNLP_2017.csv')

snisFile <- paste0(dir, '/prepped/tabl/tabl_base_no_outliers.rds')

#------------------------
# import pnlp data and subset to malaria case data

pnlp = fread(pnlpFile)

# ----------------------
# export a list of the number of facilities
facilities_pnlp <- pnlp[ ,.(value=healthFacilities_total), by=.(health_zone, date)]
facilities_pnlp[ ,date:=as.Date(date)]


#-----------------------------------------------------
# select only the necessary variables

idVars = c('health_zone', 'dps', 'date')
keepVars = c(idVars, 
				'RDT_completed', 'RDT_positive', 'ITN_distAtANC', 
				'mildMalariaTreated_5andOlder', 'mildMalariaTreated_pregnantWomen',
                'mildMalariaTreated_under5', 'newCasesMalariaMild_5andOlder',
                'newCasesMalariaMild_pregnantWomen', 'newCasesMalariaMild_under5',
                'newCasesMalariaSevere_5andOlder', 'newCasesMalariaSevere_pregnantWomen',
                'newCasesMalariaSevere_under5', 'presumedMalaria_5andOlder',
                'presumedMalaria_pregnantWomen', 'presumedMalaria_under5',
                'severeMalariaTreated_5andOlder', 'severeMalariaTreated_pregnantWomen',
                'severeMalariaTreated_under5', 'SSCRDT_completedUnder5', 'SSCRDT_positiveUnder5', 
				'SSCRDT_completed5andOlder', 'SSCRDT_positive5andOlder', 
				'SSCACT_under5', 'SSCACT_5andOlder')
pnlp = pnlp[ , ..keepVars]

# convert date to a date variable
pnlp[ ,date:=as.Date(date)]

# reshape pnlp long
pnlp = melt(pnlp, id.vars = idVars)

#-----------------------------------------
# create types so you can visualize pairs of variables together 

pnlp[variable=='RDT_completed' | variable=='RDT_positive', type:='rdt']
pnlp[grepl('SSCRDT', variable) , type:='rdt_chw']
pnlp[grepl('mild', tolower(variable)), type:='simple']
pnlp[grepl('severe', tolower(variable)), type:='severe']
pnlp[grepl(variable, pattern='presumed'), type:='presumed']
pnlp[grepl(variable, pattern='ITN'), type:='llin']

#----------------------------
# set the categories
pnlp[grep(variable, pattern='5andOlder'), category:='5+']
pnlp[grep(variable, pattern='under5'), category:='<5']
pnlp[grep(variable, pattern='pregnantWomen'), category:='pregnant woman']
        
pnlp[is.na(category), category:='default']

#-------------------         
# reset variable names to be the same in both data sets 

pnlp[grep(variable, pattern='newCasesMalariaMild'), variable:='Case of uncomplicated malaria']
pnlp[grep(variable, pattern='mildMalariaTreated'), variable:='Case of uncomplicated malaria treated']
pnlp[grep(variable, pattern='newCasesMalariaSevere'), variable:='Case of severe malaria']
pnlp[grep(variable, pattern='severeMalariaTreated'), variable:='Case of severe malaria treated']
pnlp[grep(variable, pattern='presumedMalaria'), variable:='Case of presumed malaria']

#--------------------------------------
# identify the data set for the merge

pnlp[ , set:='pnlp']
#--------------------
# replace health zone and dps with lower case/no spaces

pnlp[ ,health_zone:=tolower(health_zone)]
pnlp$health_zone <- gsub( '\\s', '', pnlp$health_zone)
pnlp[ , dps:=tolower(dps)]
pnlp$dps <- gsub( '\\s', '', pnlp$dps)

#------------------------
# import SNIS data 

# import the snis base services data 
base <- readRDS(snisFile)

# eliminate unnecessary variables and subset to 2017
# FIGURE OUT WHY THIS DROPS SOME ROWS OUT
base <- base[ date < '2018-01-01',.(value=sum(value)), by=.(org_unit, health_zone, dps, date, category, variable=element_eng )]

base <- base[variable!='A 1.4 Presumed malaria treated']
base <- base[variable!='A 1.4 Suspected malaria case' ]

#----------------------
# set the categories
base[grep(category, pattern='>5 ans'), category:='5+']
base[grep(category, pattern='<5 ans'), category:='<5']
base[grep(variable, pattern='pregnant woman'), category:='pregnant woman']

base[ ,category:=as.character(category)]

#----------------------------------------
# change the variable names to match pnlp

base[variable=='A 1.4 RDT positive', variable:='RDT_positive']
base[variable=='A 1.4 RDT performed', variable:='RDT_completed']

base[grep(variable, pattern='LLIN'), variable:='ITN_distAtANC']

base[variable=='A 1.4 Confirmed simple malaria' | variable=='A 1.5 Confirmed simple malaria - pregnant woman', variable:='Case of uncomplicated malaria']
base[variable=='A 1.4 Confirmed simple malaria treated' | variable=='A 1.5 Confirmed simple malaria treated - pregnant woman', variable:='Case of uncomplicated malaria treated']

base[variable=='A 1.4 Presumed malaria', variable:='Case of presumed malaria']

base[variable=='A 1.4 Severe malaria' | variable=='A 1.5 Severe malaria - pregnant woman', variable:='Case of severe malaria']
base[variable=='A 1.4 Severe malaria treated' | variable=='A 1.5 Severe malaria treated - pregnant woman', variable:='Case of severe malaria treated']

#-----------------------------------------
# create a variable type

base[grep(variable, pattern='uncomplicated'), type:='simple']
base[grep(variable, pattern='presumed'), type:='presumed']
base[grep(variable, pattern='severe'), type:='severe']
base[grep(variable, pattern='RDT'), type:='rdt']
base[grep(variable, pattern='ITN'), type:='llin']
#---------------------------------
# demarcate the data set the data came from
base[ ,set:='base']

#--------------------------------------------------
# subset the health zones to only the overlap 

# replace health zone and dps with lower case/no spaces
base[, health_zone:=tolower(health_zone)]
base[, health_zone:=gsub( '\\s', '', health_zone)]
base[, dps:=tolower(dps)]
base[, dps:=gsub( '\\s', '', dps)]

#-------------------------
# number of facilities merge in later 
facilities_base <- base[ ,.(facilities_reporting=length(unique(org_unit))), 
                            by=.(date, variable, health_zone)]

#------------------------
#sum over the variables to create single variables with categories
base <- base[ ,.(value=sum(value, na.rm=T)), by=.(health_zone, dps, date, variable, category, type, set)]

#--------------------------------
# subset to only the health zones in both data sets

bhz <- unique(base$health_zone)
phz <- unique(pnlp$health_zone)

bhz[bhz %in% phz]
bmiss = bhz[!(bhz %in% phz)]
pmiss = phz[!(phz %in% bhz)]

# # function that converts health zones
# convertt_hzs <- function(x) {
#   
#   x[health_zone== ,health_zone:=]
#   x[health_zone== ,health_zone:=]
#   x[health_zone== ,health_zone:=]
#   x[health_zone== ,health_zone:=]
#   x[health_zone== ,health_zone:=]
#   x[health_zone== ,health_zone:=]
#   x[health_zone== ,health_zone:=]
#   x[health_zone== ,health_zone:=]
#   x[health_zone== ,health_zone:=] 
#   x[health_zone== ,health_zone:=]
#   x[health_zone== ,health_zone:=]
#     
# }

# 'bogosenubea'      'massa'            'montngafula'      'mampoko'          'binzameteo'       'kitangwa'         'kimbilulenge'     'lilangabobangi'  
# 'lualaba'          'mweneditu'        'makisokisangani'  'gety'             'ruashi'           'kaniama'          'kalemie'          'kasavubu'        
# 'sakania'          'muetshi'          'manguredjipa'     'citenge'          'kamiji'           'kuimba'           'tchomia'          'mitimurhesa'     
#  'wambalwadi'       'benatshadi'       'banzowmoke'       'kabondodianda'    'ngandajika'       'bandalungwa'      'ntandembelo'      'tshishimbi'      
# 'yasabonga'        'bipemba'          'bongandanga'      'kisanji'          'kalambayikabanga' 'katoyi'           'binzaozone'       'omendjadi'       
#  'kiambi'           'busanga'          'saramabila'       'nyankunde'        'hautplateau'     

# 
# convert_hzs(base)


base <- base[health_zone %in% phz]
pnlp <- pnlp[health_zone %in% bhz]

#-----------------------------------
# bind the two data sets together 

dt <- rbind(base, pnlp)

#------------------------------------------
# fix dps to be the same

dt$dps <- gsub( '-', '', dt$dps)
dt[dps=='bascongo', dps:='kongocentral']

#------------------------------------
# label the data sets for the graphs 
dt$set <- factor(dt$set, 
                 levels=c('base', 'pnlp'), 
                 labels=c('SNIS: Services de Base', 'PNLP: program Data'))

#---------------------------------------
# GRAPHS 
# national trend graphs - sum over category (same graph)

trend2 <- dt[ ,.(value=sum(value, na.rm=T)), by=.(date, variable, set)]

list_of_plots = NULL
i=1

for(f in unique(trend2$variable)) {
  
  # set the title to the facility name
  name <- unique(trend2[variable==f]$variable)
  
  # create a graph of the monthly data stratified by sex
  list_of_plots[[i]] <- ggplot(trend2[variable==f], aes(y=value, x=date, color=set, group=set)) + 
    geom_point() +
    geom_line(alpha=0.5) + 
    labs(title=name, x='Date', y='Count') +
    theme_bw() +
    scale_y_continuous(labels = scales::comma) +
    theme_bw()
  
  i=i+1
  
}

pdf(paste0(dir,'compare_pnlp/pnlp_compare_natl.pdf'), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

#------------------------------
# national trend graphs - facet by data set and include category

trend <- dt[ ,.(value=sum(value, na.rm=T)), by=.(date, category, variable, set)]

list_of_plots = NULL
i=1

for(f in unique(trend$variable)) {
  
  # set the title to the facility name
  name <- unique(trend[variable==f]$variable)
  
  # create a graph of the monthly data stratified by sex
  list_of_plots[[i]] <- ggplot(trend[variable==f], aes(y=value, x=date, color=category, group=category)) + 
    geom_point() +
    geom_line(alpha=0.5) + 
    labs(title=name, x='Date', y='Count') +
    facet_wrap(~set) +
    theme_bw() +
    scale_y_continuous(labels = scales::comma)
  
  i=i+1
  
}

pdf(paste0(dir,'compare_pnlp/pnlp_compare_natl_facet.pdf'), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

#-----------------------------
# dps level - no category, same graph

prov1 <- dt[ ,.(value=sum(value, na.rm=T)), by=.(date, variable, dps, set)]

list_of_plots = NULL
i=1

for(f in unique(prov1$variable)) {
  for (p in unique(prov1$dps)) {
    
    # set the title to the facility name
    name <- unique(prov1[variable==f]$variable)
    dps_name <- unique(prov1[dps==p]$dps)
    
    # create a graph of the monthly data stratified by sex
    list_of_plots[[i]] <- ggplot(prov1[variable==f & dps==p], aes(y=value, x=date, color=set, group=set)) + 
      geom_point() +
      geom_line(alpha=0.5) + 
      labs(title=name, subtitle=dps_name, x='Date', y='Count') +
      theme_bw() +
      scale_y_continuous(labels = scales::comma)
    
    i=i+1
    
  }}

pdf(paste0(dir,'compare_pnlp/pnlp_compare_dps_no_category.pdf'), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

#---------------------------------
# dps level - facet by data set and include category

prov <- dt[ ,.(value=sum(value, na.rm=T)), by=.(date, category, variable, dps, set)]

list_of_plots = NULL
i=1

for (p in unique(prov$dps)) {
for(f in unique(prov$variable)) {
  
  # set the title to the facility name
  name <- unique(prov[variable==f]$variable)
  dps_name <- unique(prov[dps==p]$dps)
  
  # create a graph of the monthly data stratified by sex
  list_of_plots[[i]] <- ggplot(prov[variable==f & dps==p], aes(y=value, x=date, color=category, group=category)) + 
    geom_point() +
    geom_line(alpha=0.5) + 
    labs(title=name, subtitle=dps_name, x='Date', y='Count') +
    facet_wrap(~set) +
    theme_bw() +
    scale_y_continuous(labels = scales::comma)
  
  i=i+1
  
}}

pdf(paste0(dir,'compare_pnlp/pnlp_compare_dps_facet.pdf'), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

#---------------------------------------
# health zone level, facet wrapped by data set and include category

hz <- dt[ ,.(value=sum(value, na.rm=T)), by=.(date, category, variable, health_zone, set)]

list_of_plots = NULL
i=1

for(f in unique(hz$variable)) {
for (z in unique(hz$health_zone)) {
    
    # set the title to the facility name
    name <- unique(hz[variable==f]$variable)
    hz_name <- unique(hz[health_zone==z]$health_zone)
    
    # create a graph of the monthly data stratified by sex
    list_of_plots[[i]] <- ggplot(hz[variable==f & health_zone==z], aes(y=value, x=date, color=category, group=category)) + 
      geom_point() +
      geom_line(alpha=0.5) + 
      labs(title=hz_name, subtitle=name, x='Date', y='Count') +
      facet_wrap(~set) +
      theme_bw() +
      scale_y_continuous(labels = scales::comma)
    
    i=i+1
    
  }}

pdf(paste0(dir,'compare_pnlp/pnlp_compare_hz_facet.pdf'), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

#------------------------------------
# multiple pdfs by variable; each pdf includes all health zones 
hz1 <- dt[ ,.(value=sum(value, na.rm=T)), by=.(date, variable, health_zone, dps, set)]

list_of_plots = NULL
i=1

for(f in unique(hz1$variable)) {
  for (p in unique(hz1$health_zone)) {
    
    # set the title to the facility name
    name <- unique(hz1[variable==f]$variable)
    hzname <- unique(hz1[health_zone==p]$health_zone)
    
    # create a graph of the monthly data stratified by sex
    list_of_plots[[i]] <- ggplot(hz1[variable==f & health_zone==p], aes(y=value, x=date, color=set, group=set)) + 
      geom_point() +
      geom_line(alpha=0.5) + 
      labs(title=name, subtitle=hzname, x='Date', y='Count') +
      theme_bw() +
      scale_y_continuous(labels = scales::comma)
    
    i=i+1
    
  }}


outFiles = paste0(dir,'compare_pnlp/all_vars_hz/pnlp_compare_dps_variable', unique(prov1$variable), '.pdf')
j=1 # start a counter for 'chunks' of graphs
n = length(unique(hz1$health_zone)) # count the length of a chunk
pdf(outFiles[1]) # open the first pdf
for(i in seq(length(list_of_plots))) {
  if ((i%%(n+1))==0) { # if we're at the first graph of the next chunk...
    dev.off() # close the previous pdf ...
    j=j+1 # add to the 'chunk' counter ...
    pdf(outFiles[j]) # and open the next pdf
  }
  print(list_of_plots[[i]]) # print the plot to the current pdf
}
dev.off() # close the last pdf

#---------------------------------------------------------
# create a national base data set
# facet wrapped by facility counts
natl_base <- dt[set=='SNIS: Services de Base' ,.(value=sum(value, na.rm=T)), by=.(date, variable, set) ]
natl_pnlp <- dt[set=='PNLP: program Data',.(value=sum(value, na.rm=T)), by=.(date, variable, set) ]
natl <- rbind(natl_base, natl_pnlp)
natl[ ,ind:='Variable']


fac_b <- facilities_base[ ,.(value=sum(facilities_reporting)),  by=.(date, variable)]
fac_p <- facilities_pnlp[ ,.(facilities=sum(value)), by=date]
fac <- merge(fac_b, fac_p, by='date')

fac1 <- fac[ ,.(date, variable, value)]
fac2 <- fac[ ,.(date, variable, value=facilities)]
fac1[ ,set:='SNIS: Services de Base']
fac2[ ,set:='PNLP: program Data']

fac = rbind(fac1, fac2)
fac[ ,ind:='Facilities Reporting']

# rbind the data to the facilities reporting
natl = natl[ ,.(date, variable, set, ind, value)]
fac = fac[,.(date, variable, set, ind, value)]

natl = rbind(natl, fac)


natl$ind <- factor(natl$ind, 
                 levels=c('Variable', 'Facilities Reporting'), 
                 labels=c('Count of Variable', 'Number of Facilities Reporting'))


# print the graphs!

list_of_plots = NULL
i=1

for(f in unique(natl$variable)) {
  
  # set the title to the facility name
  name <- unique(natl[variable==f]$variable)
  
  # create a graph of the monthly data stratified by sex
  list_of_plots[[i]] <- ggplot(natl[variable==f], aes(y=value, x=date, color=set, group=set)) + 
    geom_point() +
    geom_line(alpha=0.5) + 
    labs(title=name, x='Date', y='Count', color='Date Set' ) +
    facet_wrap(~ind, scales='free_y') +
    theme_bw() +
    scale_y_continuous(labels = scales::comma)
  
  i=i+1
  
}

pdf(paste0(dir,'compare_pnlp/pnlp_compare_natl_facilities.pdf'), height=6, width=10)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

#---------------------------------------
# descriptive stats 
test = dt[ , .(value=sum(value, na.rm=T)), by=.(variable, set)]
test = test[  , .(mean=(value/12)), by=.(set, variable)]

fwrite(test, paste0(dir, 'means_real.csv' ))

var <- fac_b[ ,mean(value), by=variable]
fwrite(var, paste0(dir, 'means_var.csv' ))



