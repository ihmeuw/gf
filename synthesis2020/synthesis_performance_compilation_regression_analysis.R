#############################################################
##Title: pce_synthesis_abs_perf_revision_data
##Purpose: Analyze revision, absorption and performance data for synthesis report - not in the end used
##Author: Matthew Schneider
##Date: 11/11/2020
##Input Files:
##           - C:\Users\mts24\Box Sync\Global Fund Files\tableau_data
##                                                                    \budgetRevisions_with_frBudgets_activityLevel.csv
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
  library(readstata13)
  library(data.table)
  library(dplyr)
  library(ggpubr)
  library(parallel)
  #library(doParallel)
  library(feather)
  library(reshape2)
  library(foreach)
  library(readxl)
  library(ggplot2)
  library(devtools)
  library(readxl)
  library(xlsx)
  library(ggpubr)
}

#path to save files
#path <- paste0(c,"/Users/mts24/Documents/PCE/Synthesis")
path <- paste0("C:/Users/mts24/Box Sync/Global Fund Files/synthesis/data")

##ihme indicator data
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

##limiting the indicators we are interested in to active and coverage indicators
ind_data_yr12 <- all_ind_data[grant_status=="active" & indicator_type=="Coverage",c("loc_name","grant","semester","year","grant_period","module",
                                                           "gf_indicator", "indicator_code","original_indicator",
                                                           "category","sub_category",
                                                           "target_n","target_d", "target_pct",
                                                           "result_n","result_d", "result_pct",
                                                           "result_value","achievement_ratio_result","result_source")]

ind_data_yr12 <- ind_data_yr12[,.(sum_result_n=sum(result_n, na.rm = TRUE), sum_result_d=sum(result_d, na.rm = TRUE), 
                             sum_target_n=sum(target_n, na.rm = TRUE), sum_target_d=sum(target_d, na.rm = TRUE),
                             avg_reported_result_pct=mean(result_pct,na.rm = TRUE),avg_reported_target_pct=mean(target_pct,na.rm = TRUE)), 
                          by = c("loc_name","year","grant","module","gf_indicator","category","sub_category",
                                 "indicator_code","original_indicator")]

ind_data_yr12[,sum_result_pct:=sum_result_n/sum_result_d]
ind_data_yr12[,sum_target_pct:=sum_target_n/sum_target_d]

ind_data_yr12[,performance:=sum_result_pct/sum_target_pct] #verified that achievement ratios are calculated as a pct over pct
ind_data_yr12[is.na(performance) | is.infinite(performance),performance:=avg_reported_result_pct/avg_reported_target_pct] #verified that achievement ratios are calculated as a pct over pct
ind_data_yr12[is.na(performance) | is.infinite(performance),performance:=sum_result_n/sum_target_n]

ind_data_yr12[,c("country","disease","partner") := tstrsplit(grant,"-", fixed=TRUE)]
ind_data_yr12[,value:= sum_result_pct*100]
ind_data_yr12[,target:=sum_target_pct*100]

##figure this out!
ind_data_yr12[is.na(value) | is.infinite(value),value:= avg_reported_result_pct]
ind_data_yr12[is.na(target) | is.infinite(target),target:= avg_reported_target_pct]
ind_data_yr12[is.na(value)| is.infinite(value),value:= sum_result_n]
ind_data_yr12[is.na(target)| is.infinite(target),target:=sum_target_n]
ind_data_yr12[,indicator:=paste0(indicator_code,": ",gf_indicator)]

ind_data_yr12[,performance_rating:="A1 (>=100%)"]
ind_data_yr12[,performance:=as.numeric(as.character(performance))]
ind_data_yr12[performance>=0.9 & performance<1 ,performance_rating:="A2 (90-99%)"]
ind_data_yr12[performance>=0.6 & performance<0.9 ,performance_rating:="B1 (60-89%)"]
ind_data_yr12[performance>=0.2 & performance<0.6 ,performance_rating:="B2 (30-59%)"]
ind_data_yr12[performance>=0 & performance<0.3 ,performance_rating:="C (<30%)"]
ind_data_yr12[is.na(performance) | is.infinite(performance),performance_rating:="No related \n indicators"]

ind_data_yr12[,target:=as.numeric(as.character(target))]
ind_data_yr12[,value:=as.numeric(as.character(value))]
ind_data_yr12[,performance:=as.numeric(as.character(performance))]

##cutting off performance at 1.2 as GF does
ind_data_yr12[,performance_cutoff:=performance]
ind_data_yr12[performance>1.2,performance_cutoff:=1.2]

ind_data_yr12[,country:="DRC"]
ind_data_yr12[loc_name=="KHM",country:="Cambodia"]
ind_data_yr12[loc_name=="MMR",country:="Myanmar"]
ind_data_yr12[loc_name=="MOZ",country:="Mozambique"]
ind_data_yr12[loc_name=="Moz",country:="Mozambique"]
ind_data_yr12[loc_name=="uga",country:="Uganda"]
ind_data_yr12[loc_name=="sen",country:="Senegal"]
ind_data_yr12[loc_name=="gtm",country:="Guatemala"]

##reading in latest budget data - includes NFM2 funding requests, approved for grant making budgets, all revisions, and 
##                                NFM3 funding requests and grant making budgets
##this dataset if budgets down to activities and cost categories
all_abs_data <- fread(input = paste0(c,"/Users/mts24/Box Sync/Global Fund Files/tableau_data/all_absorption.csv")) #absorption by intervention and grant for each year (semester)
##need to update RSSH interventions we are considering also part of HRG-Equity
rssh_equity_int <- c("Supportive policy and programmatic environment")
all_abs_data[gf_module=="Community responses and systems" | 
               gf_module=="Community systems strengthening", equity:="TRUE"]
all_abs_data[gf_intervention=="Supportive policy and programmatic environment", equity:="TRUE"]

all_abs_data <- all_abs_data[grant_period!="2016-2019"]

##creating an indicator variable for grants that are Government PRs or Non-Government PRs
unique(all_abs_data[,c("iso3","grant_disease","pr"):=tstrsplit(grant,"-")])

all_abs_data[, pr_type:="NGO"]
all_abs_data[pr %in% c("MOH", "MoFPED", "MSPAS","PNLP","CNLS"), pr_type:="Government"]


mofped <- all_abs_data[(grant=="UGA-H-MoFPED" | grant=="UGA-M-MoFPED" | grant=="UGA-T-MoFPED") & (semester=="Semester 1" | semester=="Semester 2")]
mofped_1_module <- mofped[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","grant","gf_module")] #created the semester 1-2 expenditure and budget for Mofped grants
mofped_1_module[,semester:="Semester 1-2"]

abs_yr12_module <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6"),.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","grant","semester","gf_module")]
abs_yr12_module <- rbind(abs_yr12_module,mofped_1_module) #appending Uganda's MoFPED semester 1-2 budget and expenditures to other funds to then sum
abs_yr12_module <- abs_yr12_module[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","grant","gf_module")]
abs_yr12_module[,absorption:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco
abs_yr12_module <- abs_yr12_module[order(loc_name,gf_module,semester),c("loc_name","grant","gf_module","semester","budget","expenditure","absorption")]

##merging absorption and indicator performance
abs_yr12_module[,c("year1","year"):= tstrsplit(semester," ")]
colnames(abs_yr12_module)[colnames(abs_yr12_module)=="gf_module"] <- "module"
abs_yr12_module[,country:=loc_name]


##fixing some differences in the module names
abs_yr12_module[module=="Comprehensive prevention programs for men who have sex with men", module:="Comprehensive prevention programs for MSM"]
abs_yr12_module[module=="Comprehensive prevention programs for people who inject drugs and their partners", module:="Comprehensive prevention programs for people who inject drugs (PWID) and their partners"]
abs_yr12_module[module=="Comprehensive prevention programs for sex workers and their clients", module:="Comprehensive prevention programs for sex workers and their clients"]
abs_yr12_module[module=="Comprehensive prevention programs for transgender people", module:="Comprehensive prevention programs for TGs"]
abs_yr12_module[module=="Health management information system and monitoring and evaluation", module:="Health management information systems and M&E"]
abs_yr12_module[module=="Multidrug-resistant TB", module:="MDR-TB"]   
abs_yr12_module[module=="Prevention of mother-to-child transmission", module:="PMTCT"]
                      
ind_data_yr12[module=="RSSH: Health management information systems and M&amp;E", module:="Health management information systems and M&E"]
ind_data_yr12[module=="RSSH: Integrated service delivery and quality improvement", module:="Integrated service delivery and quality improvement"]
ind_data_yr12[module=="RSSH: Procurement and supply chain management systems", module:="Procurement and supply chain management systems"]
ind_data_yr12[module=="Specific prevention interventions (SPI)", module:="Specific prevention interventions"]

ind_module_yr12 <- ind_data_yr12[,.(performance=mean(performance,na.rm = TRUE),
                                                          performance_cutoff=mean(performance_cutoff,na.rm = TRUE)),
                                 by = c("country","module","disease","grant","year")]
ind_module_yr12[,performance_rating:="A1 (>=100%)"]
ind_module_yr12[,performance:=as.numeric(as.character(performance))]
ind_module_yr12[performance>=0.9 & performance<1 ,performance_rating:="A2 (90-99%)"]
ind_module_yr12[performance>=0.6 & performance<0.9 ,performance_rating:="B1 (60-89%)"]
ind_module_yr12[performance>=0.2 & performance<0.6 ,performance_rating:="B2 (30-59%)"]
ind_module_yr12[performance>=0 & performance<0.3 ,performance_rating:="C (<30%)"]
ind_module_yr12[is.na(performance) | is.infinite(performance),performance_rating:="No related \n indicators"]

ind_module_cum <- ind_data_yr12[is.finite(performance),.(performance=mean(performance,na.rm = TRUE),
                                                         performance_cutoff=mean(performance_cutoff,na.rm = TRUE)),
                                by = c("country","module","disease","grant")]

ind_module_cum[,performance_rating:="A1 (>=100%)"]
ind_module_cum[,performance:=as.numeric(as.character(performance))]
ind_module_cum[performance>=0.9 & performance<1 ,performance_rating:="A2 (90-99%)"]
ind_module_cum[performance>=0.6 & performance<0.9 ,performance_rating:="B1 (60-89%)"]
ind_module_cum[performance>=0.2 & performance<0.6 ,performance_rating:="B2 (30-59%)"]
ind_module_cum[performance>=0 & performance<0.3 ,performance_rating:="C (<30%)"]
ind_module_cum[is.na(performance) | is.infinite(performance),performance_rating:="No related \n indicators"]


ind_cum <- ind_data_yr12[is.finite(performance) & !is.na(module),.(performance=mean(performance,na.rm = TRUE),
                                                                   performance_cutoff=mean(performance_cutoff,na.rm = TRUE)),
                         by = c("country","disease","grant", "year")]


##pulling in revision data
revision <- data.table(read.xlsx2(file = paste0(path,"/draft_synthesis_budget_quant.xlsx"), sheetName = "National Program"))
revision[,pct_revision:= (as.numeric(as.character(nfm2_most_recent_revision)) - as.numeric(as.character(nfm2_approved)))/as.numeric(as.character(nfm2_approved))]
revision[is.infinite(pct_revision),pct_revision:=1]

revision[,pct_revision_cmf:= (as.numeric(as.character(nfm2_most_recent_revision)) - as.numeric(as.character(nfm2_approved_catalytic_funds)))/as.numeric(as.character(nfm2_approved_catalytic_funds))]
revision[is.infinite(pct_revision_cmf),pct_revision_cmf:=1]

rev_merge <- revision[grant!="",c("loc_name","grant","gf_module","nfm2_approved","nfm2_approved_catalytic_funds","nfm2_most_recent_revision","pct_revision","pct_revision_cmf")]
rev_merge[,nfm2_approved:=as.numeric(as.character(nfm2_approved))]
rev_merge[,nfm2_most_recent_revision:=as.numeric(as.character(nfm2_most_recent_revision))]
rev_merge[,nfm2_approved_catalytic_funds:=as.numeric(as.character(nfm2_approved_catalytic_funds))]
rev_merge[,nfm2_revision:= nfm2_most_recent_revision-nfm2_approved]
rev_merge[,nfm2_revision_cmf:= nfm2_most_recent_revision-nfm2_approved_catalytic_funds]
rev_merge[,ln_nfm2_revision:= log(nfm2_revision)]
rev_merge[,ln_nfm2_most_recent_revision:= log(nfm2_most_recent_revision)]
rev_merge[,ln_nfm2_approved:= log(nfm2_approved)]

rev_merge[,country:=loc_name]
##fixing some differences in the module names
rev_merge[,module:=as.character(gf_module)]
rev_merge[gf_module=="Comprehensive prevention programs for men who have sex with men", module:="Comprehensive prevention programs for MSM"]
rev_merge[gf_module=="Comprehensive prevention programs for people who inject drugs and their partners", module:="Comprehensive prevention programs for people who inject drugs (PWID) and their partners"]
rev_merge[gf_module=="Comprehensive prevention programs for sex workers and their clients", module:="Comprehensive prevention programs for sex workers and their clients"]
rev_merge[gf_module=="Comprehensive prevention programs for transgender people", module:="Comprehensive prevention programs for TGs"]
rev_merge[gf_module=="Health management information system and monitoring and evaluation", module:="Health management information systems and M&E"]
rev_merge[gf_module=="Multidrug-resistant TB", module:="MDR-TB"]   
rev_merge[gf_module=="Prevention of mother-to-child transmission", module:="PMTCT"]
rev_merge[gf_module=="Specific prevention interventions (SPI)", module:="Specific prevention interventions"]


##cumulative absorption - based on year 1 & 2 data
cum_abs_module <- abs_yr12_module[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("country","grant","module")]
cum_abs_module[,absorption:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco



abs_module <- unique(abs_yr12_module[,module])
ind_module <- unique(ind_data_yr12[,module])
rev_module <- unique(rev_merge[,module])
setdiff(abs_module,ind_module)
setdiff(ind_module,rev_module)

dt <- merge(abs_yr12_module[,c("country","year","grant","module","budget","expenditure","absorption")],
      ind_data_yr12[,c("country","year","grant","module","gf_indicator","indicator_code","category",
                           "sub_category","performance","performance_cutoff","performance_rating")], by = c("country","year","grant","module"), all = TRUE)

dt <- merge(dt, rev_merge, by = c("country","grant","module"),all=TRUE)

dt_abs_rev <- merge(abs_yr12_module[,c("country","year","grant","module","budget","expenditure","absorption")],
            rev_merge, by = c("country","grant","module"),all=TRUE)

dt_abs_rev <- merge(dt_abs_rev, ind_module_yr12, by = c("country","year","grant","module"),all=TRUE)
dt_abs_rev[is.na(performance) | is.infinite(performance),performance_rating:="No related \n indicators"]

##looking at the relationship at a cumulative level for absorption and revisions - NONE FOUND
dt_cum_abs_rev <- merge(cum_abs_module, rev_merge, by = c("country","grant","module"),all=TRUE)

dt_cum_abs_rev <- merge(dt_cum_abs_rev, ind_module_cum, by = c("country","grant","module"),all=TRUE)
dt_cum_abs_rev[is.na(performance) | is.infinite(performance),performance_rating:="No related \n indicators"]

dt_cum_abs_rev[,cor.test(absorption,pct_revision)] ##cumulative absorption related to cumulative revision including Matching Funds - not signficiant
dt_cum_abs_rev[,cor.test(absorption,pct_revision_cmf)] ##cumulative absorption related to cumulative revision INCLUDING MATCHING FUNDS - signficiant
dt_cum_abs_rev[,cor.test(performance,pct_revision)] ##cumulative absorption related to cumulative revision - not signficiant
dt_cum_abs_rev[,cor.test(performance,pct_revision_cmf)] ##cumulative absorption related to cumulative revision - not signficiant

dt_cum_abs_rev[,cor.test(performance,absorption)] ##cumulative absorption related to cumulative revision - not signficiant

cum_abs_rev_model1 <- lm(pct_revision_cmf ~ absorption + country + module, data = dt_cum_abs_rev)
summary(cum_abs_rev_model1) # signficant relationship between revisions and absorption at the cumulative level

cum_abs_rev_model2 <- lm(performance ~ pct_revision_cmf + country + module, data = dt_cum_abs_rev)
summary(cum_abs_rev_model2) # no relationship between revisions and performance at the cumulative level


##Looking at the relationship at an indicator, grant indicator level
dt[performance<4 & absorption<4,cor.test(absorption,performance)]

dt[performance<7,cor.test(performance,pct_revision)]
dt[performance<7,cor.test(performance,pct_revision_cmf)]

dt[performance<7,cor.test(absorption,pct_revision)]
dt[performance<7,cor.test(absorption,pct_revision_cmf)]

dt_abs_rev[absorption<4 & pct_revision<4 & year=="1-2",cor.test(absorption,pct_revision_cmf)]
dt_abs_rev[absorption<4 & pct_revision<4 & year=="3-4",cor.test(absorption,pct_revision_cmf)]
dt_abs_rev[performance<4 & pct_revision<4 & year=="1-2",cor.test(performance,pct_revision_cmf)]
dt_abs_rev[performance<4 & pct_revision<4,cor.test(performance,pct_revision_cmf)]

figs <- paste0("C:/Users/mts24/Box Sync/Global Fund Files/synthesis/figures")

ggplot(dt[performance<4 & absorption<4],aes(x = absorption,y = performance)) + 
  geom_point(aes(color = country,shape = year, size = nfm2_revision_cmf)) +
  geom_smooth(method="lm") +
  xlab("Module Absorption") +
  ylab("Module Performance")

dt_abs_rev[absorption<2 & pct_revision_cmf<4 & year=="1-2",cor.test(absorption, pct_revision_cmf)]
ggplot(dt_abs_rev[absorption<2 & pct_revision_cmf<4 & year=="1-2"],aes(x = absorption,y = pct_revision_cmf)) + 
  geom_point(aes(shape = country, color = performance_rating)) +
  geom_smooth(method="lm") +
  stat_cor(method = "pearson", label.x = 1.25, label.y = .75) +
  ggtitle("Relationship between absorption and budget changes") +
  xlab("Absorption by grant-module (end of first year)") +
  ylab("Percent change in budget by grant-module") +
  labs(shape="Country", color="Grant-module \n avg. indicator \n performance rating")
ggsave(paste0(figs,"/module_revision_abs.png"),width = 8.5, height = 5, units = "in")

dt_abs_rev[absorption<2 & pct_revision_cmf<2 & year=="1-2",cor.test(absorption, nfm2_revision_cmf)]
ggplot(dt_abs_rev[absorption<2 & pct_revision_cmf<2 & year=="1-2"],aes(x = absorption,y = nfm2_revision_cmf)) + 
  geom_point(aes(shape = country, color = performance_rating)) +
  geom_smooth(method="lm") +
  stat_cor(method = "pearson", label.x = 1.25, label.y = .75) +
  ggtitle("Relationship between absorption and budget changes") +
  xlab("Absorption by grant-module (end of first year)") +
  ylab("Change in budget by grant-module") +
  labs(shape="Country", color="Grant-module \n avg. indicator \n performance rating")

dt_abs_rev[,rssh:=FALSE]
rssh_modules <- c("Procurement and supply chain management system",
                  "Health management information systems and M&E",
                  "Human resources for health, including community health workers",
                  "Integrated service delivery and quality improvement",
                  "Community responses and systems",
                  "Financial management systems",
                  "Community systems strengthening",
                  "Health products management systems",
                  "National health strategies")
dt_abs_rev[module %in% rssh_modules,rssh:=TRUE]

ggplot(dt_abs_rev[rssh==TRUE & absorption<2 & pct_revision_cmf<4 & year=="1-2"],aes(x = absorption,y = pct_revision_cmf)) + 
  geom_point(aes(shape = country, color = performance_rating)) +
  geom_smooth(method="lm") +
  stat_cor(method = "pearson", label.x = 1.25, label.y = .75) +
  ggtitle("Relationship between absorption and budget changes - RSSH Modules") +
  xlab("Absorption by grant-module (end of first year)") +
  ylab("Percent change in budget by grant-module") +
  labs(shape="Country", color="Grant-module \n avg. indicator \n performance rating")
ggsave(paste0(figs,"/rssh_module_revision_abs.png"),width = 8.5, height = 5, units = "in")


##radar chart
library(fmsb)
radarchart(dt_abs_rev[rssh==TRUE & country=="Uganda" &
                        absorption<2 & pct_revision_cmf<4 & year=="1-2",
                      c("module","absorption","grant","pct_revision_cmf")],
           seg = length(unique(dt_abs_rev[rssh==TRUE & country=="Uganda",module])))

ggplot(dt[performance<4 & absorption<4],aes(x = absorption,y = performance)) + facet_wrap(~module, scales = "free_y")  + geom_point(aes(color = country,shape = year)) +
  geom_smooth(method="lm") +
  xlab("Module Absorption") +
  ylab("Module Performance")

ggplot(dt_abs_rev[absorption<4 & pct_revision<4 & year=="1-2" & nfm2_approved!=0 & performance<4],aes(x = performance,y = pct_revision)) + facet_wrap(~module, scales = "free_y") + geom_point(aes(color = country,shape = year)) +
  geom_smooth(method="lm") +
  xlab("Module Absorption") +
  ylab("Module Percent Change Revision")

abs_rev_model1 <- lm(pct_revision_cmf ~ absorption, data = dt_abs_rev[absorption<2 & pct_revision<4 & year=="1-2"])
summary(abs_rev_model1)

abs_rev_model2 <- lm(pct_revision_cmf ~ absorption + country, data = dt_abs_rev[absorption<2 & pct_revision<4 & year=="1-2"])
summary(abs_rev_model2)

abs_rev_model3 <- lm(pct_revision_cmf ~ absorption + country + module, data = dt_abs_rev[absorption<4 & pct_revision<4 & year=="1-2"])
summary(abs_rev_model3)


perf_abs_rev_model1 <- lm(pct_revision_cmf ~ performance + country, data = dt_abs_rev[absorption<4 & pct_revision<4 & performance<4 & year=="1-2"])
summary(perf_abs_rev_model1)

perf_abs_rev_model4 <- lm(nfm2_revision_cmf ~ absorption + country, data = dt_abs_rev[absorption<4 & pct_revision<4 & performance<4 & year=="1-2"])
summary(perf_abs_rev_model4)


ols_model1 <- lm(performance ~ absorption + year, data = dt[country=="Senegal" & performance<7 & absorption<4])
ols_model1 <- lm(performance ~ absorption, data = dt[performance<7 & absorption<4])
ols_model2 <- lm(performance ~ absorption + country, data = dt[performance<7 & absorption<4])
ols_model3 <- lm(performance ~ absorption + country + module + as.character(year), data = dt[performance<4 & absorption<4])
ols_model4 <- lm(performance ~ absorption + country + module + as.character(year), data = dt[performance<4 & absorption<4 & year!=5])
ols_model4 <- lm(absorption ~ performance + country + module + as.character(year), data = dt[performance<4 & absorption<4])
ols_model6 <- lm(absorption ~ country + year, data = dt[performance<7 & absorption<4])

ols_model5 <- lm(performance ~ country + year, data = dt[performance<7 & absorption<4])
ols_model6 <- lm(performance ~ absorption + country + year, data = dt[performance<7 & absorption<4])
ols_model7 <- lm(performance ~ absorption + country + year + module, data = dt[performance<7 & absorption<4])


library(plm)
#making id_var
dt[,id:=paste0(module,"_",gf_indicator)]
panel <- plm(performance ~ absorption + country + year, 
             data = dt[!is.na(performance) & performance<7 & absorption<4 & !is.na(absorption)], 
            index = c("id"), model= "random")

panel <- plm(absorption ~ performance + country + year, 
             data = dt[!is.na(performance) & performance<7 & absorption<4 & !is.na(absorption)], 
             index = c("id"), model= "random")
summary(ols_model1)
summary(ols_model2)
summary(ols_model3)
summary(ols_model4)
summary(ols_model5)
summary(ols_model6)
summary(ols_model7)

summary(ols_model)
##saving absolute expenditure, budget, and absorption at end of each year (different from PUDR reported cumulative absorption)
#abs_yr12 <- read.xlsx2(file = paste0(path,"/draft_synthesis_absorption_quant.xlsx"), sheetName = "Absorption by cntry year module")