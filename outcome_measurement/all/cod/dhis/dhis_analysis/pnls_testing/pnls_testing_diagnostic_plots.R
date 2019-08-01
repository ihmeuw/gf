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

# minor outlier screen
dt[ , max(value), by=variable]
dt = dt[value!=769776 & value!=29841 & value!=10000 & !(variable=='HIV+' & value==510)]

#------------------------------------
#dianostic plots
  
  og_counts = dt[ ,.(value=length(unique(org_unit_id))), by=.(date, variable)]
  
  ggplot(og_counts, aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    labs(x="Date", y="Facilities reporting",
         title='Total facilities reporting by variable') +
    theme_bw() +
    theme(text=element_text(size=18))
  
  og_counts2 = dt[ ,.(value=sum(value)), by=.(date, variable)]
  
  ggplot(og_counts2, aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    labs(x="Date", y="Count") +
    theme_bw() +
    theme(text=element_text(size=18))
  
  dt[ ,.(value=sum(value)), by=variable][order(value)]
  
  
  test = dt[grepl('Tested', variable) | grepl('HIV+', variable) | grepl('Counseled', variable)]
  test_sum = test[ ,.(value=sum(value)), by=.(date, variable)]
  
  ggplot(test_sum, aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(text=element_text(size=18))
  
  
  dt[ ,.(value=sum(value)), by=variable][order(value)]
  
  hiv = test_sum[grep("HIV", variable)]
  
  ggplot(hiv, aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(text=element_text(size=18))
  
  couns = test_sum[grep("Counseled", variable)]
  
  ggplot(couns, aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(text=element_text(size=18))
  
  tested = test_sum[grepl('Tested', variable)]
  
  ggplot(tested, aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(text=element_text(size=18))
  
  
  pts = og_counts2[grepl('Patient', variable)]
  
  ggplot(pts, aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(text=element_text(size=18))
  
  
  case = dt[variable=='Enrolled in case management' ,.(value=sum(value)), by=.(date, variable)]
  
  ggplot(case, aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    labs(x="Date", y="Count") +
    theme_bw() +
    theme(text=element_text(size=18))
  
  
  dt[variable=='Tested', sum(value), by=subpop]
  pos = dt[variable=='HIV+', sum(value), by=subpop]
  pos[ , customer:=(subpop=='csw_client')]
  
  
  bar = dt[variable=='HIV+' | variable=='HIV+ and informed of their results']
  bar = bar[ ,.(value=sum(value)), by=.(subpop, variable=variable)]
  
  ggplot(bar[subpop!='patients'], aes(x=subpop, y=value, fill=variable)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw() +
    labs(fill='Variable name')
  
  # drop it out
  ggplot(bar, aes(x=subpop, y=value, fill=variable)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw() +
    labs(fill='Variable name')
  
  
  bar2 = dt[variable=='New patients who received a treatment consultation' | variable=='Tested']
  bar2 = bar2[ ,.(value=sum(value)), by=variable]
  
  ggplot(bar2, aes(x=variable, y=value, fill=variable)) +
    geom_bar(stat="identity", position="fill") +
    theme_bw() +
    labs(fill='Variable name')
  
  
  vec = dt[date=='2017-01-01', unique(org_unit_id)]
  
  dt2 = dt[(org_unit_id %in% vec) & variable=='Counseled and tested',.(value=sum(value)), by=.(date, subpop)]
 
   ggplot(key1, aes(x=date, y=value, fill=subpop)) +
        geom_bar(stat='identity', position='fill') 
   
   vec = dt[date=='2017-01-01', unique(org_unit_id)]
   
   dt2 = dt[(org_unit_id %in% vec)]
   
   dt2 = dt2[variable=='Counseled and tested' | variable=='Tested',.(value=sum(value)), by=.(date, variable) ]

   ggplot(dt2, aes(x=date, y=value, color=variable)) +
     geom_line() +
     geom_point()
  
  
  bar3 = dt[variable=='Tested' | variable=='Tested and informed of their results']
  bar3 = bar3[ ,.(value=sum(value)), by=.(variable=variable)]
  
  ggplot(bar3, aes(x=subpop, y=value, fill=variable)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw() +
    labs(fill='Variable name') 
  
  
  
  tested = dt[variable=="New patients who received a treatment consultation" | variable=='Tested' | variable=='Counseled' ,
              .(value=sum(value)), by=.(date, variable)]
  
  ggplot(tested, aes(x=date, y=value, color=variable)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_color_manual(values=test_colors) +
    labs(color="", x='Date', y='Count', 
         title='Patients who received test counseling and/or an HIV test') +
    theme(text = element_text(size=20))
  
  # mean tests per facility
  
  mean_tests = dt[variable=='Tested', .(value=mean(value)), by=date]
  
  
  ggplot(mean_tests, aes(x=date, y=value)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_color_manual(values=test_colors) +
    labs(color="", x='Date', y='Count', 
         title='Mean HIV tests per facility per month') +
    theme(text = element_text(size=20))
  
  
  mean_tests2 = dt[variable=='Tested', .(value=sum(value), facilities=length(unique(org_unit_id))), by=date]
  mean_tests2[ ,mean_tests:=value/facilities]
  
  ggplot(mean_tests2, aes(x=date, y=mean_tests)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_color_manual(values=test_colors) +
    labs(color="", x='Date', y='Count', 
         title='Mean HIV tests per facility per month') +
    theme(text = element_text(size=20))
  
  
