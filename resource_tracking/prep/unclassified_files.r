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
cod_files = fread("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/cod/raw_data/cod_budget_filelist.csv")
gtm_files = fread("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/gtm/raw_data/gtm_budget_filelist.csv")
sen_files = fread("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/sen/raw_data/sen_budget_filelist.csv")
uga_files = fread("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/uga/raw_data/uga_budget_filelist.csv")

all_files = rbind(cod_files, gtm_files, sen_files, uga_files, fill=T)

#Add full file paths to file names
dir = "J:/Project/Evaluation/GF/resource_tracking/"
all_files[, folder:=ifelse(data_source == "pudr", "pudrs", "budgets")]
all_files[, version:=ifelse(file_iteration == "initial", "iterations", "")]
all_files[, master_file_dir:=paste0(dir, "_gf_files_gos/", loc_name, "/raw_data/")]

all_files[, file_dir:=paste0(master_file_dir, grant_status, "/", grant, "/", folder, "/")]
all_files[version!="", file_dir:=paste0(file_dir, version, "/")]

all_files[, inFile:=paste0(file_dir, file_name)]

#Only keep the vector of built up file names; that's all you need. 
processed_files = all_files$inFile

#Now, drop out the files from the list before that you're processing with resource tracking. 
file_list = unique(file_list) #Remove duplicate files (?) 
all_files = unique(all_files) #Remove duplicate files(?)
before_drop = length(file_list)
unprocessed_files = file_list[!file_list%in%processed_files]

if (length(unprocessed_files) + length(processed_files) != nrow(all_files)) {
  print("Some file paths not being filtered correctly - unprocessed + processed does not equal all files.")
}

#---------------------------------------------
# Pull some information out of unprocessed file path names to search for 
#   files you're missing! 
#--------------------------------------------
unprocessed_files = data.table(file_name = unprocessed_files)
unprocessed_files[, country:=tstrsplit(file_name, "/", keep=7)]
unprocessed_files[, grant:=tstrsplit(file_name, "/", keep=10)]
unprocessed_files[, data_source:=tstrsplit(file_name, "/", keep=11)]
