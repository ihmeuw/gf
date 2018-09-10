# Outlier screening on Tableau SNIS data

# ----------------------------------------------
# Caitlin O'Brien-Carelli
# 9/10/2018

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

file <- 'prepped/tabl_base'

# import the data set for cleaning and prep
dt <- readRDS(paste0(dir, file, '.rds'))

#--------------------
# install quant reg and load
install.packages("quantreg", lib=paste0(dir, 'quantreg_5.36'))
library(SparseM, lib.loc=paste0(dir, '/quantreg_5.36/'))
library(quantreg, lib.loc=paste0(dir, '/quantreg_5.36/'))
?rq 

#--------------------------
# check that value is a numeric

dt[ ,value:=as.numeric(as.character(value))]

#---------------------------------------------------

test <- dt[element_eng=="A 1.5 Severe malaria - pregnant woman"]
test <- test[ ,.(value=sum(value)), by=.(date, org_unit) ]

quantFit <- rq(value~date+factor(org_unit), data=test, tau=0.5)
r <- resid(quantFit)Resources

test[ , resid:=r]  
test[(median(r)+(4*sd(r)))]

#----------------------------

# export a pdf of the data points that will be eliminated
outlier_orgs <- test[resid > (median(r)+(4*sd(r))), unique(org_unit)] 
sub <- test[test$org_unit %in% outlier_orgs]


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

pdf(paste0(dir,'/outliers/facilities_2017_2018.pdf'), height=6, width=9)

for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 

dev.off()

