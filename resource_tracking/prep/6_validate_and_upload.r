#--------------------------------------------------------------
# PURPOSE: Validate final datasets, and upload to Basecamp
# AUTHOR: Emily Linebarger 
# DATE: March 2019. 
# -------------------------------------------------------------

#--------------------------------------------------------------
# READ IN DATA 
#--------------------------------------------------------------

all_budgets = readRDS(paste0(combined_output_dir, "final_budgets.rds"))
all_expenditures = readRDS(paste0(combined_output_dir, "final_expenditures.rds"))

cod_budgets = readRDS(paste0(dir, "_gf_files_gos/cod/prepped_data/final_budgets.rds"))
cod_expenditures = readRDS(paste0(dir, "_gf_files_gos/cod/prepped_data/final_expenditures.rds"))

gtm_budgets = readRDS(paste0(dir, "_gf_files_gos/gtm/prepped_data/final_budgets.rds"))
gtm_expenditures = readRDS(paste0(dir, "_gf_files_gos/gtm/prepped_data/final_expenditures.rds"))

uga_budgets = readRDS(paste0(dir, "_gf_files_gos/uga/prepped_data/final_budgets.rds"))
uga_expenditures = readRDS(paste0(dir, "_gf_files_gos/uga/prepped_data/final_expenditures.rds"))

#Read in the codebook for comparison
codebook = read.xlsx(paste0(dir, "documentation/RT_Codebook.xlsx"))

all_files = list(all_budgets, all_expenditures, cod_budgets, cod_expenditures, gtm_budgets, gtm_expenditures, uga_budgets, uga_expenditures)
#--------------------------------------------------------------
# VALIDATE DATA 
#--------------------------------------------------------------
for (file in all_files){
  #print(as.character(file))
  #Make sure all variables are of the type expected, and have the values you would expect.

  #Make sure there are no NAs where you're not expecting them.

  # Make sure you have all the columns you expect, and that all variables are represented in the codebook.

  stopifnot(sort(names(file))==sort(codebook$Variable))
}

all_budgets = all_budgets[, names(all_budgets)%in%codebook$Variable, with = FALSE]
saveRDS(all_budgets, paste0(combined_output_dir, "final_budgets.rds"))
write.csv(all_budgets, paste0(combined_output_dir, "final_budgets.csv"), row.names = FALSE)

all_expenditures = all_expenditures[, names(all_expenditures)%in%codebook$Variable, with = FALSE]
saveRDS(all_expenditures, paste0(combined_output_dir, "final_expenditures.rds"))
write.csv(all_expenditures, paste0(combined_output_dir, "final_expenditures.csv"), row.names = FALSE)


#--------------------------------------------------------------
# AUTOMATICALLY UPLOAD FILES TO BASECAMP
#--------------------------------------------------------------
# upload_data = function(){
#   upload = readline(prompt="Review data. Would you like to upload to Basecamp? (y/n): ")
#   
#   while (upload != 'y' & upload != 'n'){
#     print("Invalid input. Please enter either 'y' or 'n'. ")
#     upload = readline(prompt="Review data. Would you like to upload to Basecamp? (y/n): ")
#   }
#   if (upload == 'y'){
#     print("Upload successful.")
#   system("curl -H \"Authorization: Bearer $ACCESS_TOKEN\" ")
#   system("-H 'Content-Type: application/json' ")
#   system("-H 'User-Agent: Global Fund PCE (elineb@uw.edu)'")
#   system("-d '{ 'name': 'My new project!' }'")
#   system("https://3.basecampapi.com/999999999/projects.json")
# 
# 
#   #GLOBAL LEVEL PCE FILES
#   #Example curl for uploading a document
#   system("curl -H 'Authorization: Bearer $ACCESS_TOKEN' \
#          -H 'Content-Type: application/json' \
#          -H 'User-Agent: MyApp (yourname@example.com)' \
#          -d '{ 'name': 'My new project!' }' \
#          https://3.basecampapi.com/999999999/projects.json")
# 
#   } else if (upload == 'n'){
#     print("Files not uploaded.")
#   } 
# }
# 
# 
# upload_data()
