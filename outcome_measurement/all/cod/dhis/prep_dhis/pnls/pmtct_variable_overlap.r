#-----------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Compare overlap in PNLS prevention of mother-to-child
# transmission dataset
# DATE: June 2019
#----------------------------------------------------------

#Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(openxlsx)
# --------------------

# shell script for working on the cluster
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 2 

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
setwd(dir)

dt = readRDS(paste0(dir, 'prepped/pnls_final/pnls_pmtct.rds'))
dt[, year:=year(date)]

#----------------------------------------------------------
# Define some useful functions for checks below 
#----------------------------------------------------------
#Given 2 element IDs, does the first one always have less than the second for date and org unit ID? 
subset_check = function(element1, element2){
  subset1 = dt[element_id==element1, .(value1=sum(value, na.rm=T)), by=c('org_unit_id', 'date')]
  subset2 = dt[element_id==element2, .(value2=sum(value, na.rm=T)), by=c('org_unit_id', 'date')]
  subset = merge(subset1, subset2, by=c('org_unit_id', 'date'), all=T)
  subset[, diff:=value2-value1]
  
  if (nrow(subset[!is.na(diff) & diff<0])){
    return(FALSE)
  }
  return(TRUE)
} 

#--------------------------------------------------------
# Variables of interest: 
# 1. Counseled, tested, and HIV+ for pregnant women
# 2. Tested and HIV+ for infants
#--------------------------------------------------------
#What subpopulations are already isolated (should be all of them, or review prep code if not)
unique(dt$subpop)


#---------------------------------------------------------------------
# Pregnant and lactating women variable overlap analysis
#---------------------------------------------------------------------
#What are the unique element IDs for pregnant and lactating women? 
plw = unique(dt[subpop=="plw", .(element_eng, element_id)])


#Generate a variable that represents the total number of PLW counseled. 
{
  counseled_vars = c()
}

#Generate a variable that represents the total number of PLW tested. 
{
  tested_vars = plw[grep("tested", tolower(element_eng)), unique(element_id)]
  plw[element_id%in%tested_vars, unique(element_eng)]
  
  #Do a visual check to make sure you haven't missed any 
  plw[!element_id%in%tested_vars, .(element_eng)] #All are included EKL 6/21
  
  #Compare the two variables that are included. 
  plw_values = dt[element_id%in%tested_vars & subpop=="plw", .(value=sum(value, na.rm=T)), by=c('date', 'element_eng')]
  ggplot(plw_values, aes(x=date, y=value, color=element_eng))+
    geom_line() + geom_point() + 
    theme_bw() + 
    labs(title="Comparing overlap in PLW testing variables", x="Date", y="Value, summed by month")
  
  #It looks like "Tested HIV+ in the delivery room and were put on ART after delivery" is very low; what are the unique values?
  unique(dt[element_id=="zH4glMMjhcX", .(value)]) #The highest number is 21.
  
  #DECISION: Should we just drop these? There are so few. 
}

#Generate a variable that represents the total number of PLW that are HIV+. 
{
  positive_vars = plw[grep("HIV\\+", element_eng), unique(element_id)]
  unique(plw[element_id%in%positive_vars, .(element_eng, element_id)])
  final_positive_vars = copy(positive_vars)
  
  #Do a visual check to make sure you haven't missed any 
  plw[!element_id%in%positive_vars, .(element_eng)] #All are included EKL 6/21
  
  
  x = dt[element_id %in% positive_vars]
  x = x[ ,.(value=sum(value)), by=.(date, element_eng)]
  ggplot(x, aes(x=date, y=value, color=element_eng)) +
    geom_point() +
    geom_line()
  
  #-------------------------------------------
  #Do a reporting completeness graph 
  #-------------------------------------------
  FacsReportingByDate = dt[, .(facilities=length(unique(org_unit_id))), by=date]
  FacsReportingDrugByDate = dt[subpop=="plw" & element_id%in%positive_vars, .(facs_reporting_on_drug=as.numeric(length(unique(org_unit_id)))), by=date]
  
  #Did they report for the specific drug you're targeting?
  report = merge(FacsReportingByDate, FacsReportingDrugByDate, by='date', all.x=TRUE)
  
  report[is.na(facs_reporting_on_drug), facs_reporting_on_drug:=0]
  report[ , drug_reporting_ratio:=100*(facs_reporting_on_drug/facilities)]
  
  # convert to numerics and round
  report[, drug_reporting_ratio:=round(drug_reporting_ratio, 1)]
  
  ggplot(report, aes(x=date, y=drug_reporting_ratio)) +
    geom_point(size=0.8) +
    geom_line() +
    geom_line() +
    theme_bw() +
    # facet_wrap(~indicator) +
    scale_color_manual(values = brewer.pal(4, 'RdYlBu')) +
    labs(x='Date', y='Number of health facilities', title='Number of facilities reporting on HIV+ indicators')
  
  #Make a series of graphs to explore the overlap between these variables. 
  #----------------------------------------------------------------------------
  # Graph all HIV+ variables for the month with the highest reporting. 
  #----------------------------------------------------------------------------
  positive_values = dt[element_id%in%positive_vars & subpop=="plw", .(value=sum(value, na.rm=T)), by=c('date', 'element_eng')]
  positive_values = positive_values[order(date, -value)]
  positive_values[date=="2017-07-01"] #Visual check before graph
  
  ggplot(positive_values[date=="2017-07-01"], aes(x=date, y=value, fill=element_eng))+
    geom_bar(stat="identity", position="dodge") + 
    theme_bw() + 
    labs(title="Comparing overlap in PLW HIV+ variables for the month of July, 2017\n(selected for high reporting completeness)", x="Date", y="Value, summed by month")
  
  
  #Now, start with groups that are likely to have overlap, and compare totals when split out by subpopulation. 
  #1. Groups that were attended to in the delivery room. 
  delivery = dt[element_id%in%c('CyZ9wMA7iSi', 'H0skpxsq2U3', 'NIC3bM4fv4v', 'XHzJ8g7KOvb', 'gHBcPOF5y3z', 'iKekGMwvtVy', 'zH4glMMjhcX'), .(value=sum(value, na.rm=T)), by=c('year', 'element_eng')]
  ggplot(delivery[year<2019], aes(x=year, y=value, fill=element_eng))+
    geom_bar(stat="identity", position="dodge") + 
    theme_bw() + 
    labs(title="Comparing overlap in PLW delivery/facility treatment variables", x="Date", y="Value, summed by month")
  
  #Is "HIV+, receiving ART and gave birth in the maternity ward" always a subset of "HIV+ and gave birth in the maternity ward?"
  delivery2 = dt[element_id%in%c('H0skpxsq2U3', "CyZ9wMA7iSi"), .(value=sum(value, na.rm=T)), by=c('date', 'element_eng')]
  ggplot(delivery2, aes(x=date, y=value, fill=element_eng))+
    geom_bar(stat="identity", position="dodge") + 
    theme_bw() + 
    labs(title="Comparing overlap in PLW delivery/facility treatment variables", x="Date", y="Value, summed by month")
  
  #Also do through calculation 
  subset_check('H0skpxsq2U3', "CyZ9wMA7iSi")

  #DECISION - Unsure. there are some cases where you don't have full reporting overlap. 
  
  #Is HIV+ received in the facility during the month, and already receiving ART a subset of HIV+ received inthe facility during the month? 
  delivery3 = dt[element_id%in%c('iKekGMwvtVy', "NIC3bM4fv4v"), .(value=sum(value, na.rm=T)), by=c('date', 'element_eng')]
  ggplot(delivery3, aes(x=date, y=value, fill=element_eng))+
    geom_bar(stat="identity", position="dodge") + 
    theme_bw() + 
    labs(title="Comparing overlap in PLW delivery/facility treatment variables", x="Date", y="Value, summed by month")
  
  #Subset calculation 
  subset_check('iKekGMwvtVy', "NIC3bM4fv4v")
  #DECISION - Also seems likely. 
  final_positive_vars = final_positive_vars[final_positive_vars!="iKekGMwvtVy"]
  
  #Is HIV+ and received in the facility during the month always a subset of "Knew their HIV status before arriving at the facility for ART?"
  subset_check("NIC3bM4fv4v", "urDEeghmDdM")
  
  #2. Received ART 
  positive_subpops = dt[element_id%in%positive_vars & subpop=="plw", .(value=sum(value, na.rm=T)), by=c('date', 'element_eng', 'maternity', 'sex', 'case', 'age')]
  
  
}


#---------------------------------------------------------------------
# Infant variable overlap analysis
#---------------------------------------------------------------------
unique(dt[subpop=="exposed_infant", .(element_eng, element_id)])