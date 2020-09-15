#############################################################
##Title: uga_focus_topic_secondary_analysis_2020
##Purpose: Incorporating CEP feedback on secondary focus topic activities
##Author: Matthew Schneider
##Date: 9/10/2020
#############################################################
##command for array qsub -w e -N raking_preds -t 1:2 -P proj_fgh -l intel=TRUE -l m_mem_free=35G -l h_rt=001:00:00 -l fthread=25 -q long.q  -o /ihme/homes/mts24/phc_project/output/raked -e /ihme/homes/mts24/phc_project/errors/raked /ihme/singularity-images/rstudio/shells/r_shell_singularity_3530.sh /homes/mts24/phc_code/phc/2_estimation/03_predictions_raking_uis.R
rm(list = ls()) #clear memory

if (Sys.info()[1] == "Linux"){
  j <- "/home/j"
  h <- paste0("/homes/",Sys.info()[7])
  s <- paste0("/share/resource_tracking/phc/data/nha/int/")
  f <- paste0("/share/resource_tracking/phc/data/nha/fin/")
  k <- paste0("/ihme/cc_resources/libraries/")
}else if (Sys.info()[1] == "Windows"){
  j <- "J:"
  h <- "H:"
  k <- "K:"
}


if (Sys.info()[1] == "Linux"){
  #load libraries
  .libPaths(c("/share/code/mts24",.libPaths()))
  #install.packages(c("brms", "bayesplot","rstanarm","fastDummies","mipfp"),lib = "/share/code/mts24", INSTALL_opts = c('--no-lock'))
  library(fastDummies, lib.loc = "/share/code/mts24")
  library(readstata13)
  library(data.table)
  library(dplyr)
  library(parallel)
  library(doParallel)
  library(feather)
  library(reshape2)
  library(foreach)
  library(readxl)
  library(ggplot2)
}else if (Sys.info()[1] == "Windows"){
  pacman::p_load(readstata13, magrittr, ISwR, rstan, rstanarm, reshape2, data.table, devtools, ggplot2, ggpubr, plyr, dplyr, bayesplot, brms,parallel,fastDummies,merTools,mipfp, reshape2)
  install_github("norimune/glmmstan", force = TRUE)
  library(shinystan)
}

##insheeting data from Audrey - this is for all budgets
data_budget <- fread(paste0("C:/Users/abatzel/Box Sync/Global Fund Files/tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv"))

uga_data_budget <- data_budget[loc_name=="Uganda" & budget_version!="funding_request20_CT" & !is.na(budget_version)]
uga_data_budget[budget_version=="approved",budget_version:="approved_in_grant_making"]

##insheeting CEP decision for focus topic activities based on approved and revised budgets
uga <- fread(paste0(j,"/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/keyword_search/UGA/uganda_final_keyword_search_review_cep_090820.csv"))

#removing white space and line breaks from merging variables
cols_trim <- c("loc_name","gf_module","gf_intervention","activity_description",
               "cost_category","budget_version", "grant", "disease", "fr_disease", "file_name", "pr")

uga_data_budget[,(cols_trim) :=lapply(.SD,trimws),.SDcols = cols_trim] ##removing white spaces from beginning and ends
uga[,(cols_trim) :=lapply(.SD,trimws),.SDcols = cols_trim]
uga[,(cols_trim) :=lapply(.SD,function(x) gsub("\r?\n|\r", "",x)),.SDcols = cols_trim] ##removing '\r' and '\n'
uga_data_budget[,(cols_trim) :=lapply(.SD,function(x) gsub("\r?\n|\r", "",x)),.SDcols = cols_trim] ##removing '\r' and '\n'


##merging to add budget information to data
uga_focus_topic <- merge(uga,
                         uga_data_budget[,c("loc_name","gf_module","gf_intervention","activity_description",
                                            "cost_category","budget_version", "grant", "disease", "fr_disease",
                                            "file_name", "pr", "budget")],
                         all = TRUE,
                         by = c("loc_name","gf_module","gf_intervention","activity_description","cost_category",
                                "budget_version", "grant", "disease", "fr_disease", "file_name", "pr"))

##removing any duplicates
uga_focus_topic <-unique(uga_focus_topic)

fwrite(uga_focus_topic, paste0(j,"/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/keyword_search/UGA/uganda_final_keyword_search_review_090820_budget.csv"))
