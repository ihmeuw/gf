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
dt = dt[order(element_eng)]
dt[, element_no:=.GRP, by='element_eng']

#----------------------------------------------------------
# Define some useful functions for checks below 
#----------------------------------------------------------
#Given 2 element IDs, does the first one always have less than the second for date and org unit ID? 
subset_check = function(element1, element2){
  subset1 = dt[element_no==element1, .(value1=sum(value, na.rm=T)), by=c('org_unit_id', 'date')]
  subset2 = dt[element_no==element2, .(value2=sum(value, na.rm=T)), by=c('org_unit_id', 'date')]
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
plw = unique(dt[subpop=="plw", .(element_eng, element_no)])


#Generate a variable that represents the total number of PLW counseled. 
{
  counseled_vars = c()
}

#Generate a variable that represents the total number of PLW tested. 
{
  tested_vars = plw[grep("tested", tolower(element_eng)), unique(element_no)]
  plw[element_no%in%tested_vars, unique(element_eng)]
  
  #Do a visual check to make sure you haven't missed any 
  plw[!element_no%in%tested_vars, .(element_eng)] #All are included EKL 6/21
  
  #Compare the two variables that are included. 
  plw_values = dt[element_no%in%tested_vars & subpop=="plw", .(value=sum(value, na.rm=T)), by=c('date', 'element_eng')]
  ggplot(plw_values, aes(x=date, y=value, color=element_eng))+
    geom_line() + geom_point() + 
    theme_bw() + 
    labs(title="Comparing overlap in PLW testing variables", x="Date", y="Value, summed by month")
  
  #It looks like "Tested HIV+ in the delivery room and were put on ART after delivery" is very low; what are the unique values?
  unique(dt[element_no=="zH4glMMjhcX", .(value)]) #The highest number is 21.
  
  #DECISION: Should we just drop these? There are so few. 
}

#Generate a variable that represents the total number of PLW that are HIV+. 
{
  positive_vars = plw[grep("HIV\\+", element_eng), unique(element_no)]
  unique(plw[element_no%in%positive_vars, .(element_eng, element_no)])
  final_positive_vars = copy(positive_vars)
  
  #Do a visual check to make sure you haven't missed any 
  plw[!element_no%in%positive_vars, .(element_eng)] #All are included EKL 6/21
  
  #Make a subset of just HIV+ variables, and generate a key to make graphs prettier
  x = dt[element_no %in% positive_vars]
  # x[, short_element:=substr(element_eng, 1, 30)]
  x = x[order(element_eng)]
  x[, key:=.GRP, by='element_eng'] #Make a key character for graphing
  x[, element_eng:=paste0(element_eng, " (", key, ")")]
  x = x[ ,.(value=sum(value, na.rm=T)), by=.(date, element_eng, key)]
  
  #Make a spaghetti plot of all variables 
  spaghetti1 = ggplot(x, aes(x=date, y=value, color=element_eng)) +
    geom_point() +
    geom_line() + 
    theme_bw() + 
    labs(title = "All HIV+ variables, summed by date", x="Date", y="Value", color="Element")

  #Restrict the y-scales to break apart the bottom lines a bit. 
  spaghetti2 = ggplot(x, aes(x=date, y=value, color=element_eng)) +
    geom_point() +
    geom_line() + 
    theme_bw() + 
    scale_y_continuous(limits=c(0, 4000))+
    labs(title = "All HIV+ variables, summed by date", subtitle="*Y-axis limited to 4000", 
         x="Date", y="Value", color="Element")
  
  #-------------------------------------------
  #Do a reporting completeness graph 
  #-------------------------------------------
  FacsReportingByDate = dt[, .(facilities=length(unique(org_unit_id))), by=date]
  FacsReportingDrugByDate = dt[subpop=="plw" & element_no%in%positive_vars, .(facs_reporting_on_drug=as.numeric(length(unique(org_unit_id)))), by=date]
  
  #Did they report for the specific drug you're targeting?
  report = merge(FacsReportingByDate, FacsReportingDrugByDate, by='date', all.x=TRUE)
  
  report[is.na(facs_reporting_on_drug), facs_reporting_on_drug:=0]
  report[ , drug_reporting_ratio:=100*(facs_reporting_on_drug/facilities)]
  
  # convert to numerics and round
  report[, drug_reporting_ratio:=round(drug_reporting_ratio, 1)]
  
  report1 = ggplot(report, aes(x=date, y=drug_reporting_ratio)) +
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
  positive_values = dt[element_no%in%positive_vars & subpop=="plw", .(value=sum(value, na.rm=T)), by=c('date', 'element_eng')]
  positive_values = positive_values[order(date, -value)]
  positive_values[date=="2017-07-01"] #Visual check before graph
  
  
  #Now, start with groups that are likely to have overlap, and compare totals when split out by subpopulation. 
  #Is "HIV+, receiving ART and gave birth in the maternity ward" always a subset of "HIV+ and gave birth in the maternity ward?"
  delivery2 = dt[element_no%in%c(9, 17), .(value=sum(value, na.rm=T)), by=c('date', 'element_eng')]
  compare1 = ggplot(delivery2, aes(x=date, y=value, fill=element_eng))+
    geom_bar(stat="identity", position="dodge") + 
    theme_bw() + 
    labs(title="Comparing overlap in PLW delivery/facility treatment variables", caption="*Subset test not true for all observations", 
         x="Date", y="Value, summed by month", fill='Element')
  
  #Also do through calculation 
  subset_check(9, 17) #NOT TRUE FOR EVERY OBSERVATION. 

  #DECISION - Unsure. there are some cases where you don't have full reporting overlap. 
  
  #Is HIV+ received in the facility during the month, and already receiving ART a subset of HIV+ received inthe facility during the month? 
  delivery3 = dt[element_no%in%c(21, 22), .(value=sum(value, na.rm=T)), by=c('date', 'element_eng')]
  compare2 = ggplot(delivery3, aes(x=date, y=value, fill=element_eng))+
    geom_bar(stat="identity", position="dodge") + 
    theme_bw() + 
    labs(title="Comparing overlap in PLW delivery/facility treatment variables", caption="*Subset test not true for all observations", 
         x="Date", y="Value, summed by month", fill='Element')
  
  #Subset calculation 
  subset_check(21, 22) #NOT TRUE FOR EVERY OBSERVATION. 
  
  #Is HIV+ and received in the facility during the month always a subset of "Knew their HIV status before arriving at the facility for ART?"
  subset_check(21, 26) #NOT TRUE FOR EVERY OBSERVATION. 
  
  #2. Received ART 
  positive_subpops = dt[element_no%in%positive_vars & subpop=="plw", .(value=sum(value, na.rm=T)), by=c('date', 'element_eng', 'maternity', 'sex', 'case', 'age')]
  
  #--------------------------------------------
  # Plot all graphs 
  #--------------------------------------------
  outDir = "J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pnls/"
  pdf(paste0(outDir, "pmtct_quality_checks.pdf"), height=6, width=12)
  
  print(report1)
  print(spaghetti1)
  print(spaghetti2)
  print(compare1)
  print(compare2)
  dev.off()
  
  
}


#---------------------------------------------------------------------
# Infant variable overlap analysis
#---------------------------------------------------------------------
unique(dt[subpop=="exposed_infant", .(element_eng, element_no)])