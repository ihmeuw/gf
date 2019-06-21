#------------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: ACtively find the files we haven't classified 
#   for the RT database, and classify as best you can. 
# DATE: June 2019 
#-------------------------------------------------
repo_root = "C:/Users/elineb/Documents/gf"
setwd(repo_root)
source("./resource_tracking/prep/_common/set_up_r.r")

#Make a list of all of the files that live in each country's resource tracking universe, 
# using recursive search functions 
countries = c('cod', 'gtm', 'sen', 'uga')
base_dirs = c()
for (country in countries){
  active = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", country, "/raw_data/active")
  not_active = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", country, "/raw_data/not_active")
  unclassified = paste0("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/", country, "/unclassified_files_to_review")

  base_dirs = c(base_dirs, active, not_active, unclassified)
} 

terminal_dirs = character()
file_list = character() #What columns do you want to have here?
for (path in base_dirs){
  new_dirs = get_dirs(path, character())
  terminal_dirs = append(terminal_dirs, new_dirs)
  for (dir in terminal_dirs){
    file_list = append(file_list, get_files(dir))
  }
}

#--------------------------------------------
# Now, merge together your documented file lists, and remove these files 
#-----------------------------------------------------