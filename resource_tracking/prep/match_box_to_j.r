#-----------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Match Box and J:drive file systems so you can 
# re-run resource tracking on the cluster. 
# DATE: September 2019 
#-----------------------------------------------------------------

rm(list=ls())
# Set up R
if (Sys.info()[1]=='Windows'){
  setwd("C:/Users/elineb/Documents/gf/") #Change to the root of your repository
} else {
  setwd("/ihme/homes/elineb/gf/")
}
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/_common/load_master_list.r", encoding="UTF-8")

verbose=FALSE

#Read in file list. This is your "promise"- the files here should match between Dropbox and the J:drive. 
file_list = load_master_list(purpose="financial")

#Subset down to the columns you need, and 

