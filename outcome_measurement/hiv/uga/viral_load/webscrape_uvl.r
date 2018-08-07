# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/25/2018
# Loop to download data from the Uganda Viral Load Dashboard
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(plyr)

# --------------------
# detect if on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# ----------------------------------------------
# Files and directories

# whether or not to re-download everything (or just new data)
reload_everything = FALSE

# output directory
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape/sex_tb')

# ----------------------------------------------

# ----------------------------------------------
# Load/prep data

#loop over years - can be altered to run years separately
y <- c('14', '15', '16', '17', '18')
m <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
t <- c('y','n','x')
s <- c('m', 'f', 'x')

arguments <- expand.grid(y=y, m=m, t=t, s=s)

build_url <- function(page_specs){
  
  print(page_specs)
  
  y <- page_specs$y
  m <- page_specs$m
  t <- page_specs$t
  s <- page_specs$s
  
  outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_', s,'.rds')
  
  # tb status file
  outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_', s,'_',t, '_','.rds')
  
  check = file.exists(outFile)
  
  print(check)
  
  # only download if it doesn't already exist
  if (check==FALSE | reload_everything==TRUE) {
    
    # sex filter url
    #url = paste0('https://vldash.cphluganda.org/live?age_ids=%5B%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=20', y, m,'&genders=%5B%22',s,'%22%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%5D&to_date=20', y, m)
    
    # tb status url
    url = paste0('https://vldash.cphluganda.org/live?age_ids=%5B%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=20', y, m,'&genders=%5B%22',s,'%22%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%22', t, '%22%5D&to_date=20',y,m)
    
    # to determine where errors occur
    
    # load
    existence <- tryCatch(fromJSON(url), error = function(x){return("Does not exist")})
    
    # save raw output
    if (existence != 'Does not exist'){
      saveRDS(existence, file=outFile)
      existence <- "Successfully saved"
    }

    results <- data.table("page_specs" = paste(y, m, t , s),
                          "url" = url,
                         "existence" = existence)
    
    return(results)
  }
}

urls <- adply(arguments, 1, build_url)


# 
# 
# #-------------------------
# # old for loop
# 
# 
#  for(y in c('15', '16', '17', '18')) {
#  
# # 2018 only
#   #for(y in c('18')) {
#   
#   # loop over months
#   for(m in c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')) {
#     
#     # 2014 only - loop breaks on months with no data (before August 2014)
#     #for(y in c('14')) {
#      # for(m in c('08', '09', '10', '11', '12')) {
#         
#       
#       # loop over age groups
#       # add this filter to the link/outFile if you want to run with the age filter
#       #for(a in c('0,1', '1,2,3,4', '5,6,7,8,9,10', '11, 12, 13, 14, 15',
#       # '16, 17, 18, 19, 20', '21, 22, 23, 24, 25', '26, 27, 28, 29, 30', '31, 32, 33, 34, 35', '36, 
#       #   37, 38, 39', '40,41, 42, 43, 44, 45', '46, 47, 48, 49, 50', '51, 52, 53, 54, 55', '56, 57, 58, 59, 60', 
#       #   '61,62,63,64,65', '66,67,68,69,70', '71,72,73,74,75', '76,77,78,79,80', '81,82,83,84,85', '86,87,88,89,90', 
#       #   '91,92,93,94,95', '96,97,98,99')) { 
#       
#     
#      # loop over tb groups - includes "unknown" option
#        for(t in c('y','n','x')) { 
#       
#         # loop over sexes - includes 'unknown' option
#        for(s in c('m', 'f', 'x')) {
#    
#           # check if file exists 
#          
#          # sex file
#          outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_', s,'.rds')
#          
#          # tb status file
#           outFile = paste0(dir, '/facilities_suppression_', m,'_','20', y,'_', s,'_',t, '_','.rds')
#           
#           check = file.exists(outFile)
#       
#           # only download if it doesn't already exist
#           if (check==FALSE | reload_everything==TRUE) {
#             
#             # sex filter url
#             #url = paste0('https://vldash.cphluganda.org/live?age_ids=%5B%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=20', y, m,'&genders=%5B%22',s,'%22%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%5D&to_date=20', y, m)
#                          
#             # tb status url
#             url = paste0('https://vldash.cphluganda.org/live?age_ids=%5B%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=20', y, m,'&genders=%5B%22',s,'%22%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%22', t, '%22%5D&to_date=20',y,m)
#             
#             # to determine where errors occur
#             print(url)
#             
#             # load
#             data = fromJSON(url)
#             
#             # save raw output
#             saveRDS(data, file=outFile)
#           }
#         }
#     
#       }}}
#       
# # ----------------------------------------------
