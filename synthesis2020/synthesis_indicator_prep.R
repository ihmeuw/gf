#############################################################
##Title: pce_synthesis_indicators_data
##Purpose: Prepping indicator data for synthesis report for all IHME/PATH PCE countries
##Author: Matthew Schneider
##Date: 11/3/2020, last updated and reran - 1/28/21
##Input Files:
##           - C:\Users\mts24\Box Sync\Global Fund Files\tableau_data
##                                                                    \budgetRevisions_with_frBudgets_activityLevel.csv
##Output Files:
##          1. draft_synthesis_indicators_quant.xlsx - absorption for NFM2 and NFM3 total, rssh, hrg-equity across all IHME/PATH countries
#############################################################

rm(list = ls()) #clear memory

if (Sys.info()[1] == "Linux"){
  j <- "/home/j"
  h <- paste0("/homes/",Sys.info()[7])
  s <- paste0("/share/resource_tracking/phc/data/nha/int/")
  f <- paste0("/share/resource_tracking/phc/data/nha/fin/")
  k <- paste0("/ihme/cc_resources/libraries/")
}else if (Sys.info()[1] == "Windows"){
  c <- "C:"
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
  pacman::p_load(readstata13, magrittr, 
                 ISwR,data.table, devtools, ggplot2, ggpubr, 
                 plyr, dplyr, parallel,
                 fastDummies, reshape2, readxl,xlsx,
                 dependencies = TRUE)
}

#path to save files
#path <- paste0(c,"/Users/mts24/Documents/PCE/Synthesis")
user = as.character(Sys.info()[7])
path <- paste0("C:/Users/",user,"/Box Sync/Global Fund Files/synthesis/data")

##reading in latest budget data - includes NFM2 funding requests, approved for grant making budgets, all revisions, and 
##                                NFM3 funding requests and grant making budgets
##this dataset if budgets down to activities and cost categories
all_ind_data <- fread(input = paste0(c,"/Users/mts24/Box Sync/Global Fund Files/tableau_data/all_perf_indic_data.csv")) #absorption by intervention and grant for each year (semester)

#GTM's grant GTM-M-MSPAS reported both semester 1 and semester 1-2 PUDRs, so we will drop semester 1
all_ind_data <- all_ind_data[!(grant=="GTM-M-MSPAS" & semester=="Semester 1")]
#SEN's grants reported semester 3 but then a joint semester 3-4 PUDRs, so we will drop semester 3 for senegal grants
#all_ind_data <- all_ind_data[!(loc_name=="cod" & semester=="Semester 3")]

all_ind_data[(semester=="Semester 1" | semester=="Semester 2" | semester=="Semester 1-2"),year:="1-2"]
all_ind_data[(semester=="Semester 3" | semester=="Semester 4"| semester=="Semester 3-4"),year:="3-4"]
all_ind_data[(semester=="Semester 5"),year:="5"]


all_ind_data_yr12 <- all_ind_data[grant_status=="active",c("loc_name","grant","semester","year","grant_period","module",
                                                          "gf_indicator", "indicator_code","original_indicator",
                                                       "category","sub_category",
                                                       "target_n","target_d", "target_pct",
                                                       "result_n","result_d", "result_pct",
                                                       "result_value","achievement_ratio_result","result_source")]

write.xlsx2(all_ind_data_yr12, file = paste0(path,"/draft_synthesis_indicators_quant.xlsx"), sheetName = "All ind ach by grant cntry yr",
            col.names = TRUE, row.names = TRUE, append = FALSE)

rssh_ind_data <- all_ind_data[grant_status=="active" & strategicObjective=="RSSH",c("loc_name","semester","year","gf_indicator",
                                                           "indicator_code","original_indicator", "grant",
                                                           "category","sub_category",
                                                           "target_n","target_d", "target_pct",
                                                           "result_n","result_d", "result_pct",
                                                           "result_value","result_source",
                                                           "achievement_ratio_result","reported_achievement_ratio")]

rssh_ind_data_yr12 <- rssh_ind_data[,.(sum_result_n=sum(result_n, na.rm = TRUE), sum_result_d=sum(result_d, na.rm = TRUE), 
                                         sum_target_n=sum(target_n, na.rm = TRUE), sum_target_d=sum(target_d, na.rm = TRUE),
                                         avg_reported_result_pct=mean(result_pct,na.rm = TRUE),avg_reported_target_pct=mean(target_pct,na.rm = TRUE)), 
                                         by = c("loc_name","year","gf_indicator",
                                                "indicator_code","original_indicator")]

rssh_ind_data_yr12[,sum_result_pct:=sum_result_n/sum_result_d]
rssh_ind_data_yr12[,sum_target_pct:=sum_target_n/sum_target_d]

rssh_ind_data_yr12[,calc_achiv_ratio:=sum_result_pct/sum_target_pct] #verified that achievement ratios are calculated as a pct over pct
rssh_ind_data_yr12[is.na(calc_achiv_ratio),calc_achiv_ratio:=avg_reported_result_pct/avg_reported_target_pct] #verified that achievement ratios are calculated as a pct over pct

rssh_ind_data_yr <- rssh_ind_data_yr12[order(loc_name,gf_indicator,year),c("loc_name","year","gf_indicator","indicator_code",
                                                                               "original_indicator",
                                                                               "sum_result_n","sum_result_d","sum_target_n",
                                                                               "sum_target_d","avg_reported_result_pct","avg_reported_target_pct",
                                                                               "calc_achiv_ratio")]

write.xlsx2(rssh_ind_data_yr, file = paste0(path,"/draft_synthesis_indicators_quant.xlsx"), sheetName = "RSSH ind ach by cntry yr",
            col.names = TRUE, row.names = TRUE, append = TRUE)

##RSSH with grant level details
rssh_ind_data_yr12_grant <- rssh_ind_data[,.(sum_result_n=sum(result_n, na.rm = TRUE), sum_result_d=sum(result_d, na.rm = TRUE), 
                                       sum_target_n=sum(target_n, na.rm = TRUE), sum_target_d=sum(target_d, na.rm = TRUE),
                                       avg_reported_result_pct=mean(result_pct,na.rm = TRUE),avg_reported_target_pct=mean(target_pct,na.rm = TRUE)), 
                                    by = c("loc_name","year","gf_indicator","grant",
                                           "indicator_code","original_indicator")]

rssh_ind_data_yr12_grant[,sum_result_pct:=sum_result_n/sum_result_d]
rssh_ind_data_yr12_grant[,sum_target_pct:=sum_target_n/sum_target_d]

rssh_ind_data_yr12_grant[,calc_achiv_ratio:=sum_result_pct/sum_target_pct] #verified that achievement ratios are calculated as a pct over pct
rssh_ind_data_yr12_grant[is.na(calc_achiv_ratio),calc_achiv_ratio:=avg_reported_result_pct/avg_reported_target_pct] #verified that achievement ratios are calculated as a pct over pct

rssh_ind_data_yr_grant <- rssh_ind_data_yr12_grant[order(loc_name,grant,gf_indicator,year),c("loc_name","year","grant","gf_indicator","indicator_code",
                                                                           "original_indicator",
                                                                           "sum_result_n","sum_result_d","sum_target_n",
                                                                           "sum_target_d","avg_reported_result_pct","avg_reported_target_pct",
                                                                           "calc_achiv_ratio")]

write.xlsx2(rssh_ind_data_yr_grant, file = paste0(path,"/draft_synthesis_indicators_quant.xlsx"), sheetName = "RSSH ind ach by grant cntry yr",
            col.names = TRUE, row.names = TRUE, append = TRUE)


##HRG-Equity
equity_ind_data <- all_ind_data[grant_status=="active" & strategicObjective=="Equity",c("loc_name","semester","year","gf_indicator",
                                                                                     "indicator_code","original_indicator",
                                                                                     "category","sub_category",
                                                                                     "target_n","target_d", "target_pct",
                                                                                     "result_n","result_d", "result_pct",
                                                                                     "result_value","result_source",
                                                                                     "achievement_ratio_result","reported_achievement_ratio")]

equity_ind_data_yr12 <- equity_ind_data[,.(sum_result_n=sum(result_n, na.rm = TRUE), sum_result_d=sum(result_d, na.rm = TRUE), 
                                         sum_target_n=sum(target_n, na.rm = TRUE), sum_target_d=sum(target_d, na.rm = TRUE),
                                         avg_reported_result_pct=mean(result_pct,na.rm = TRUE),avg_reported_target_pct=mean(target_pct,na.rm = TRUE)), 
                                      by = c("loc_name","year","gf_indicator",
                                             "indicator_code","original_indicator","category","sub_category")]

equity_ind_data_yr12[,sum_result_pct:=sum_result_n/sum_result_d]
equity_ind_data_yr12[,sum_target_pct:=sum_target_n/sum_target_d]

equity_ind_data_yr12[,calc_achiv_ratio:=sum_result_pct/sum_target_pct] #verified that achievement ratios are calculated as a pct over pct
equity_ind_data_yr12[is.na(calc_achiv_ratio),calc_achiv_ratio:=avg_reported_result_pct/avg_reported_target_pct] #verified that achievement ratios are calculated as a pct over pct
equity_ind_data_yr12[is.na(calc_achiv_ratio),calc_achiv_ratio:=sum_result_n/sum_target_n] #calculating acheivement ratio using result/target numerators
equity_ind_data_yr <- equity_ind_data_yr12[order(loc_name,gf_indicator,year),c("loc_name","year","gf_indicator","indicator_code",
                                                               "original_indicator","category","sub_category",
                                                               "sum_result_n","sum_result_d","sum_target_n",
                                                               "sum_target_d","avg_reported_result_pct","avg_reported_target_pct",
                                                               "calc_achiv_ratio")]
write.xlsx2(equity_ind_data_yr, file = paste0(path,"/draft_synthesis_indicators_quant.xlsx"), sheetName = "HRG-Equity ind ach by cntry year",
            col.names = TRUE, row.names = TRUE, append = TRUE)



##HRG-Equity by grant
equity_ind_data <- all_ind_data[grant_status=="active" & strategicObjective=="Equity",c("loc_name","semester","year","gf_indicator",
                                                                                        "indicator_code","original_indicator", "grant",
                                                                                        "category","sub_category",
                                                                                        "target_n","target_d", "target_pct",
                                                                                        "result_n","result_d", "result_pct",
                                                                                        "result_value","result_source",
                                                                                        "achievement_ratio_result","reported_achievement_ratio")]

equity_ind_data_yr12_grant <- equity_ind_data[,.(sum_result_n=sum(result_n, na.rm = TRUE), sum_result_d=sum(result_d, na.rm = TRUE), 
                                           sum_target_n=sum(target_n, na.rm = TRUE), sum_target_d=sum(target_d, na.rm = TRUE),
                                           avg_reported_result_pct=mean(result_pct,na.rm = TRUE),avg_reported_target_pct=mean(target_pct,na.rm = TRUE)), 
                                        by = c("loc_name","year", "grant","gf_indicator",
                                               "indicator_code","original_indicator","category","sub_category")]

equity_ind_data_yr12_grant[,sum_result_pct:=sum_result_n/sum_result_d]
equity_ind_data_yr12_grant[,sum_target_pct:=sum_target_n/sum_target_d]

equity_ind_data_yr12_grant[,calc_achiv_ratio:=sum_result_pct/sum_target_pct] #verified that achievement ratios are calculated as a pct over pct
equity_ind_data_yr12_grant[is.na(calc_achiv_ratio),calc_achiv_ratio:=avg_reported_result_pct/avg_reported_target_pct] #verified that achievement ratios are calculated as a pct over pct
equity_ind_data_yr12_grant[is.na(calc_achiv_ratio),calc_achiv_ratio:=sum_result_n/sum_target_n] #calculating acheivement ratio using result/target numerators
equity_ind_data_yr_grant <- equity_ind_data_yr12_grant[order(loc_name,grant,gf_indicator,year),c("loc_name","year", "grant","gf_indicator","indicator_code",
                                                                               "original_indicator","category","sub_category",
                                                                               "sum_result_n","sum_result_d","sum_target_n",
                                                                               "sum_target_d","avg_reported_result_pct","avg_reported_target_pct",
                                                                               "calc_achiv_ratio")]
write.xlsx2(equity_ind_data_yr_grant, file = paste0(path,"/draft_synthesis_indicators_quant.xlsx"), sheetName = "HRG-Equity ind ach by grant cntry yr",
            col.names = TRUE, row.names = TRUE, append = TRUE)

