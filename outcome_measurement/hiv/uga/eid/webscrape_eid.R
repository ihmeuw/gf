# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 1/10/2019
# Loop to download data from the Uganda Early Infant Diagnosis Dashboard
# Downloads all years with sex and PCR filters applied
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

# shell script to run on the cluster
# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 20 -P snis_download

# ----------------------------------------------
# Files and directories

# whether or not to re-download everything (or just new data)
reload_everything = FALSE

# output directory
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/eid/sex_data')

outDir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/eid')

# ----------------------------------------------

# ----------------------------------------------
# Load/prep data

# loop over years - can be altered to run years separately
y = c('14', '15', '16', '17', '18', '19')
m = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
s = c('m', 'f', 'UNKNOWN')
p = c("FIRST", "SECOND", "UNKNOWN" )

# input arguments - if not including tb, drop t
arguments = expand.grid(y=y, m=m, s=s, p=p)


build_url = function(page_specs) {
  
  print(page_specs)
  
  y = page_specs$y
  m = page_specs$m
  s = page_specs$s
  p = page_specs$p
  
  # set output file name
  outFile = paste0(dir, '/eid_', m,'_20', y,'_', s,'_', p, 'pcr_.rds')
  
  check = file.exists(outFile)
  
  # only download if it doesn't already exist
  if (check==FALSE | reload_everything==TRUE) {
    
    url = paste0('https://edash.cphluganda.org/live?age_ids=%5B%5D&care_levels=%5B%5D&districts=%5B%5D&fro_date=20', y, m, '&genders=%5B%22',s,'%22%5D&hubs=%5B%5D&pcrs=%5B%22', p,'%22%5D&regions=%5B%5D&to_date=20', y, m)
    
    
    # to determine where errors occur
    
    # load
    existence = tryCatch(fromJSON(url), error = function(x){return("Does not exist")})
    
    # save raw output
    if (existence != 'Does not exist'){
      saveRDS(existence, file=outFile)
      existence = "Successfully saved"
    } 
    
    results = data.table("page_specs" = paste(y, m, s, p), 
                         "url" = url,
                         "existence" = existence)
    return(results)
  }
}

# run the function
urls = adply(arguments, 1, build_url)

#--------------------------------------------------
# download the meta data on districts and facilities

# downloads meta data and saves automatically
src_dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/eid/dist_facilities_eid.R')

# download the facilities data 
source(src_dir)
#--------------------------------------------------
