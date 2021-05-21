# prepare four data sets for analysis on changes between FR and GM with
# different keyword search specifications
library(data.table)

# set up
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  setwd(paste0("C:/Users/",user,"/Documents/gf/")) #Change to the root of your repository
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}

# source files with other functions and common resource tracking filepaths
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
source("./resource_tracking/prep/gf_files_prep_functions/id_keywords_for_extension_comments.R")

# read in data set
all_data <- as.data.table(fread("C:\\Users\\frc2\\Box Sync\\Global Fund Files\\tableau_data\\guatemala_nfm3_budgets_all.csv"))

dt <- all_data[,.(loc_name, gf_module, gf_intervention, activity_description, orig_module, orig_intervention, cost_category, comments, equity, file_name, grant, budget_version, budget)]
write.csv(dt, file = paste0(dir, "other/specialized_datasets/gtm/incap_nfm_nfm3_budgets_18may2021.csv"))

# set up keyword search
id_keywords_for_extension_comments(country="Guatemala", inFile=paste0(dir, "other/specialized_datasets/gtm/incap_nfm_nfm3_budgets_18may2021.csv"))

# read in keyword search data set
kw_data <- fread(paste0(mapping_dir,"/keyword_search/GTM/2021_extension/test_guatemala_keyword_search_5_18_2021.csv"))

# subset the data by keyword topic
prt <- kw_data[topicAreaDesc=="PRT"]
write.csv(prt, file = paste0(dir,"/other/specialized_datasets/gtm/extension_datasets/gtm_hiv_prt.csv"))

reprogramacion <- kw_data[topicAreaDesc=="RE"]
reprogramacion[topicAreaDesc=="RE", topicAreaDesc:="reprogramacion/recalendarizacion"]
write.csv(reprogramacion, file = paste0(dir,"/other/specialized_datasets/gtm/extension_datasets/gtm_hiv_reprogramacion.csv"))

paar <- kw_data[topicAreaDesc=="PAAR"]
write.csv(paar, file = paste0(dir, "/other/specialized_datasets/gtm/extension_datasets/gtm_hiv_paar.csv"))

otras_partidas <- kw_data[keyword_topic_area==FALSE]
write.csv(otras_partidas, file = paste0(dir,"/other/specialized_datasets/gtm/extension_datasets/gtm_hiv_otras_partidas.csv"))