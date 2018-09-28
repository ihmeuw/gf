# Outlier screening on DHIs data 

# ----------------------------------------------
# Caitlin O'Brien-Carelli
# 9/4/2018

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
# --------------------

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1527 -s 20 -P snis_prep

# --------------------
# set working directories 

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#--------------------
# upload the prepped data set

file <- 'prepped/base_services_drc_01_2015_07_2018_prepped'

# import the data set for cleaning and prep
dt <- readRDS(paste0(dir, file, '.rds'))

# subset to the relevant years
dt <- dt[year=='2017' | year=='2018']

#--------------------
# install quant reg and load
install.packages("quantreg", lib=paste0(dir, 'quantreg_5.36'))
library(SparseM, lib.loc=paste0(dir, '/quantreg_5.36/'))
library(quantreg, lib.loc=paste0(dir, '/quantreg_5.36/'))
?rq 

#--------------------------
# check that value is a numeric

dt[ ,value:=as.numeric(as.character(value))]

#---------------------------

#-------------------------------------------------------------------

# 
dt[element_eng=="A 1.4 Presumed malaria", tableau:=1]
dt[element_eng=="A 1.4 Presumed malaria treated", tableau:=1]

dt[element_eng=="A 1.4 Suspected malaria case", tableau:=1]
dt[element_eng=="A 1.4 Severe malaria", tableau:=1]
dt[element_eng=="A 1.4 Severe malaria treated", tableau:=1]

dt[element_eng=="A 1.5 Severe malaria - pregnant woman", tableau:=1]
dt[element_eng=="A 1.5 Severe malaria treated - pregnant woman", tableau:=1]

tabl <- dt[tableau==1]

for (e in unique(tabl$element_eng)) {
  for (d in unique(tabl$dps)) {

quantFit <- rq(value~date+factor(org_unit), data=tabl[element_eng==e & dps==d], tau=0.5)
r <- resid(quantFit)

tabl[ , resid:=r]  
tabl[ , resid_lim:=(median(r)+(4*sd(r)))]
print(e)
print(d)

}}
  
  

# export a pdf of the data points that will be eliminated
outlier_orgs <- sub[resid > (median(r)+(4*sd(r))), unique(org_unit)] 
sub <- sub[sub$org_unit %in% outlier_orgs]

print(sub) }}

# create plots for each outlier

list_of_plots = NULL
i=1

for (f in (unique(sub$org_unit))) {
  
  name <- unique(sub[org_unit==f]$org_unit)
  
  list_of_plots[[i]] <- ggplot(sub[org_unit==f], aes(x=date, y=value)) + 
    geom_point() +
    geom_line(alpha=0.5) + 
    labs(title=name, subtitle=e, x="Date", y="Count") +
    theme_bw() +
    scale_y_continuous(labels = scales::comma)
  
  i=i+1
}

pdf(paste0(dir,'/outliers/', e , '_2017_2018.pdf'), height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

}}




#-------------------------




i=1
for (e in unique(subset$element_eng)) {
  for(p in unique(subset$org_unit)) { 
    
    # run the quantile regression and list the residuals
    system(paste0('qsub -0. -e. -cwd -N ', quantreg_e_p, ' ./r_shell.sh ./quantregScript.r ', e, ' ', p))
    i=i+1
  }
}

# wait for files to be done
i = i-1
numFiles = length(list.files('/share/temp/sadfsafdsa/quantreg_output/'))
while(numFiles<i) { 
  Sys.sleep(5)
  print(paste0(numFiles, ' of ', i, 'jobs complete, waiting 5 seconds...'))
  numFiles = length(list.files('/share/temp/sadfsafdsa/quantreg_output/'))
}


# collect all output into one dataa table
j=1
for (e in unique(subset$element)) {
  for(p in unique(subset$dps)) { 
    tmp = readRDS(paste0('/share/temp/quantreg_output/file',e,p,'.rds'))
    if(j==1) fulLData = tmp
    if(j>1) fullData = rbind(fullData, tmp)
    j=j+1
  }
}


# view a histogram of the residuals to determine if some are very large

#--------------------------------------------------------
# save a data set for tableau

# subset to only the relevant elements and recent years
tabl <- dt[tableau==1]
tabl[year==2017 | year==2018]

# save the file 
name <- strsplit(file, '_')[[1]][1]
saveRDS(tabl, paste0('prepped/tabl_', name, '.rds'))

#----------------------------------------------------------

