#############################################################
##Title: pce_synthesis_absorption_data
##Purpose: Prepping absorption data for synthesis report for all IHME/PATH PCE countries
##Author: Matthew Schneider
##Date: 10/30/2020, last updated and reran - 1/28/21
##Input Files:
##           - C:\Users\mts24\Box Sync\Global Fund Files\tableau_data
##                                                                    \budgetRevisions_with_frBudgets_activityLevel.csv
##Output Files:
##          1. draft_synthesis_absorption_quant.xlsx - absorption for NFM2 and NFM3 total, rssh, hrg-equity across all IHME/PATH countries
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
user = as.character(Sys.info()[7])
path <- paste0("C:/Users/",user,"/Box Sync/Global Fund Files/synthesis/data")

##reading in latest budget data - includes NFM2 funding requests, approved for grant making budgets, all revisions, and 
##                                NFM3 funding requests and grant making budgets
##this dataset if budgets down to activities and cost categories
all_abs_data <- fread(input = paste0(c,"/Users/", user, "/Box Sync/Global Fund Files/tableau_data/all_absorption.csv")) #absorption by intervention and grant for each year (semester)
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

##reportetd cumulative budget data
##notice that Uganda has an updated PUDR and is getting double counted (either need to drop newest data or older data as to not double count)
cum_abs_data <- fread(input = paste0(c,"/Users/", user, "/Box Sync/Global Fund Files/tableau_data/cumulative_absorption.csv")) #cumulcative (first 2 years for most grants) absorption by intervention and grant

cum_abs_data <- cum_abs_data[end_date=="2019-12-31"]

#########################################################################
##creating average absorption across countries and grants for end of year 1 and end of year 2
##this created object doesn't include Uganda MoFPED grant for the first year due to reporting
mofped <- all_abs_data[(grant=="UGA-H-MoFPED" | grant=="UGA-M-MoFPED" | grant=="UGA-T-MoFPED") & (semester=="Semester 1" | semester=="Semester 2")]
mofped_1 <- mofped[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name")] #created the semester 1-2 expenditure and budget for Mofped grants
mofped_1[,semester:="Semester 1-2"]

mofped_1_module <- mofped[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","gf_module")] #created the semester 1-2 expenditure and budget for Mofped grants
mofped_1_module[,semester:="Semester 1-2"]


abs_yr12 <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6"),.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester")]
abs_yr12 <- rbind(abs_yr12,mofped_1) #appending Uganda's MoFPED semester 1-2 budget and expenditures to other funds to then sum
abs_yr12 <- abs_yr12[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester")]
abs_yr12[,absorption:=expenditure/budget] 
abs_yr12 <- abs_yr12[order(loc_name,semester),c("loc_name","semester","budget","expenditure","absorption")]

##saving absolute expenditure, budget, and absorption at end of each year (different from PUDR reported cumulative absorption)
write.xlsx2(abs_yr12, file = paste0(path,"/draft_synthesis_absorption_quant.xlsx"), sheetName = "Absorption by cntry year",
            col.names = TRUE, row.names = TRUE, append = FALSE)

##The same calculation but including a breakdown by PR Types - Governmental or NGO
mofped <- all_abs_data[(grant=="UGA-H-MoFPED" | grant=="UGA-M-MoFPED" | grant=="UGA-T-MoFPED") & (semester=="Semester 1" | semester=="Semester 2")]
mofped_pr <- mofped[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","pr_type")] #created the semester 1-2 expenditure and budget for Mofped grants
mofped_pr[,semester:="Semester 1-2"]

abs_yr12_pr <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6"),.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","pr_type")]
abs_yr12_pr <- rbind(abs_yr12_pr,mofped_pr) #appending Uganda's MoFPED semester 1-2 budget and expenditures to other funds to then sum
abs_yr12_pr <- abs_yr12_pr[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","pr_type")]
abs_yr12_pr[,absorption:=expenditure/budget] 
abs_yr12_pr <- abs_yr12_pr[order(loc_name,semester),c("loc_name","pr_type","semester","budget","expenditure","absorption")]

##saving absolute expenditure, budget, and absorption at end of each year (different from PUDR reported cumulative absorption)
write.xlsx2(abs_yr12_pr, file = paste0(path,"/draft_synthesis_absorption_quant.xlsx"), sheetName = "Absorption by cntry year pr",
            col.names = TRUE, row.names = TRUE, append = TRUE)


abs_yr12_module <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6"),.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","gf_module")]
abs_yr12_module <- rbind(abs_yr12_module,mofped_1_module) #appending Uganda's MoFPED semester 1-2 budget and expenditures to other funds to then sum
abs_yr12_module <- abs_yr12_module[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","gf_module")]
abs_yr12_module[,absorption:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco
abs_yr12_module <- abs_yr12_module[order(loc_name,gf_module,semester),c("loc_name","gf_module","semester","budget","expenditure","absorption")]
write.xlsx2(abs_yr12_module, file = paste0(path,"/draft_synthesis_absorption_quant.xlsx"), sheetName = "Absorption by cntry year module",
            col.names = TRUE, row.names = TRUE, append = TRUE)


###################################################################################################################
##caculating cumulative absorption from the reported cumulative expenditure and budgeted data reported within PURDs
cum_abs_module <- cum_abs_data[,.(cumulative_budget=sum(cumulative_budget),cumulative_expenditure=sum(cumulative_expenditure)),by=c("loc_name","gf_module")]
cum_abs_module[,cumulative_absorption:=cumulative_expenditure/cumulative_budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco

cum_abs_country_grant <- cum_abs_data[,.(cumulative_budget=sum(cumulative_budget),cumulative_expenditure=sum(cumulative_expenditure)),by=c("loc_name","grant")]
cum_abs_country_grant[,cumulative_absorption:=cumulative_expenditure/cumulative_budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco

##
##need to calcualte cumulative absorption for DRC's M-MOH grant as it isn't not provided in the PUDRs
abs_yr12_grant <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6"),.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","grant")]
abs_yr12_grant[,absoprtion:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco
cod_m_moh_12_abs <- abs_yr12_grant[grant=="COD-M-MOH"]
cod_m_moh_cum_abs <- cod_m_moh_12_abs[,.(cumulative_budget=sum(budget),cumulative_expenditure=sum(expenditure)),by=c("loc_name","grant")]
cod_m_moh_cum_abs[,cumulative_absorption:=cumulative_expenditure/cumulative_budget]

##appending the calcualted cumulative absorption for COD-M-MOH
cum_abs_country_grant_codfix <- rbind(cum_abs_country_grant[grant!="COD-M-MOH"],cod_m_moh_cum_abs)

##Cumlative absorption by country - with the cumulative absorption for the COD-M-MOH grant calculated based on end of year 1 and 2
cum_abs_country_codfix <- cum_abs_country_grant_codfix[,.(cumulative_budget=sum(cumulative_budget),cumulative_expenditure=sum(cumulative_expenditure)),by=c("loc_name")]
cum_abs_country_codfix[,cumulative_absorption:=cumulative_expenditure/cumulative_budget]



#########################################################################
##RSSH
##creating average absorption across countries and grants for end of year 1 and end of year 2
##this created object doesn't include Uganda MoFPED grant for the first year due to reporting
mofped_rssh <- all_abs_data[(grant=="UGA-H-MoFPED" | grant=="UGA-M-MoFPED" | grant=="UGA-T-MoFPED") & (semester=="Semester 1" | semester=="Semester 2") & rssh=="TRUE"]
mofped_1_rssh <- mofped_rssh[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name")] #created the semester 1-2 expenditure and budget for Mofped grants
mofped_1_rssh[,semester:="Semester 1-2"]

abs_yr12_rssh <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6") & rssh=="TRUE",.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester")] #this drops the MoFPED PUDR data that is seperated 1 and 2, which is then appended below
abs_yr12_rssh <- rbind(abs_yr12_rssh,mofped_1_rssh) #appending Uganda's MoFPED semester 1-2 budget and expenditures to other funds to then sum
abs_yr12_rssh <- abs_yr12_rssh[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester")]
abs_yr12_rssh[,absorption:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco
abs_yr12_rssh <- abs_yr12_rssh[order(loc_name,semester),c("loc_name","semester","budget","expenditure","absorption")]

###################
write.xlsx2(abs_yr12_rssh, file = paste0(path,"/draft_synthesis_absorption_quant.xlsx"), sheetName = "RSSH Absorption by cntry year",
            col.names = TRUE, row.names = TRUE, append = TRUE)


################################################
##RSSH doing the same calculations by modules
###############################################
mofped_rssh_module <- all_abs_data[(grant=="UGA-H-MoFPED" | grant=="UGA-M-MoFPED" | grant=="UGA-T-MoFPED") & (semester=="Semester 1" | semester=="Semester 2") & rssh=="TRUE"]
mofped_1_rssh_module <- mofped_rssh_module[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","gf_module")] #created the semester 1-2 expenditure and budget for Mofped grants
mofped_1_rssh_module[,semester:="Semester 1-2"]

abs_yr12_rssh_module <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6") & rssh=="TRUE",.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","gf_module")]
abs_yr12_rssh_module <- rbind(abs_yr12_rssh_module,mofped_1_rssh_module) #appending Uganda's MoFPED semester 1-2 budget and expenditures to other funds to then sum
abs_yr12_rssh_module <- abs_yr12_rssh_module[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","gf_module")]
abs_yr12_rssh_module[,absorption:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco
abs_yr12_rssh_module <- abs_yr12_rssh_module[order(loc_name,gf_module,semester),c("loc_name","gf_module","semester","budget","expenditure","absorption")]
##################################################
write.xlsx2(abs_yr12_rssh_module, file = paste0(path,"/draft_synthesis_absorption_quant.xlsx"), sheetName = "RSSH Absorption by cntry year module",
            col.names = TRUE, row.names = TRUE, append = TRUE)


abs_yr12_rssh_module_all <- abs_yr12_rssh_module[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("semester","gf_module")]
abs_yr12_rssh_module_all[,absoprtion:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco


#creating a unique list of RSSH interventions to capture these fromt the cumulative absorption reported
rssh_modules <- data.table(unique(all_abs_data[rssh=="TRUE",gf_module]))
colnames(rssh_modules) <- "gf_module"
rssh_interventions <- data.table(unique(all_abs_data[rssh=="TRUE",gf_intervention]))
colnames(rssh_interventions) <- "gf_intervention"

###################################################################################################################
##caculating cumulative absorption from the reported cumulative expenditure and budgeted data reported within PURDs
cum_abs_module_rssh <- cum_abs_data[,.(cumulative_budget=sum(cumulative_budget),cumulative_expenditure=sum(cumulative_expenditure)),by=c("loc_name","gf_module")]
cum_abs_module_rssh <- merge(cum_abs_module_rssh,rssh_modules, by = "gf_module")
cum_abs_module_rssh[,cumulative_absoprtion:=cumulative_expenditure/cumulative_budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco

cum_abs_country_grant_rssh <- cum_abs_data[,.(cumulative_budget=sum(cumulative_budget),cumulative_expenditure=sum(cumulative_expenditure)),by=c("loc_name","grant","gf_module")]
cum_abs_country_grant_rssh <- merge(cum_abs_country_grant_rssh,rssh_modules, by = "gf_module")
cum_abs_country_grant_rssh <- cum_abs_country_grant_rssh[,.(cumulative_budget=sum(cumulative_budget),cumulative_expenditure=sum(cumulative_expenditure)),by=c("loc_name","grant")]
cum_abs_country_grant_rssh[,cumulative_absoprtion:=cumulative_expenditure/cumulative_budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco

##
##need to calcualte RSSH cumulative absorption for DRC's M-MOH grant as it isn't not provided in the PUDRs
abs_yr12_grant_rssh <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6") & rssh=="TRUE",.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","grant")]
abs_yr12_grant_rssh[,absoprtion:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco
cod_m_moh_12_abs_rssh <- abs_yr12_grant_rssh[grant=="COD-M-MOH"]
cod_m_moh_cum_abs_rssh <- cod_m_moh_12_abs_rssh[,.(cumulative_budget=sum(budget),cumulative_expenditure=sum(expenditure)),by=c("loc_name","grant")]
cod_m_moh_cum_abs_rssh[,cumulative_absoprtion:=cumulative_expenditure/cumulative_budget]

##appending the calcualted cumulative absorption for COD-M-MOH
cum_abs_country_grant_codfix_rssh <- rbind(cum_abs_country_grant_rssh[grant!="COD-M-MOH"],cod_m_moh_cum_abs_rssh)

##Cumlative absorption by country - with the cumulative absorption for the COD-M-MOH grant calculated based on end of year 1 and 2
cum_abs_country_codfix_rssh <- cum_abs_data[,.(cumulative_budget=sum(cumulative_budget),cumulative_expenditure=sum(cumulative_expenditure)),by=c("loc_name")]

#########################################################################
##HRG-Equity
##creating average absorption across countries and grants for end of year 1 and end of year 2
##this created object doesn't include Uganda MoFPED grant for the first year due to reporting
mofped_equity <- all_abs_data[(grant=="UGA-H-MoFPED" | grant=="UGA-M-MoFPED" | grant=="UGA-T-MoFPED") & (semester=="Semester 1" | semester=="Semester 2") & equity=="TRUE"]
mofped_1_equity <- mofped_equity[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name")] #created the semester 1-2 expenditure and budget for Mofped grants
mofped_1_equity[,semester:="Semester 1-2"]

abs_yr12_equity <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6") & equity=="TRUE",.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester")] #this drops the MoFPED PUDR data that is seperated 1 and 2, which is then appended below
abs_yr12_equity <- rbind(abs_yr12_equity,mofped_1_equity) #appending Uganda's MoFPED semester 1-2 budget and expenditures to other funds to then sum
abs_yr12_equity <- abs_yr12_equity[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester")]
abs_yr12_equity[,absoprtion:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco
abs_yr12_equity <- abs_yr12_equity[order(loc_name,semester),c("loc_name","semester","budget","expenditure","absorption")]

###################
write.xlsx2(abs_yr12_equity, file = paste0(path,"/draft_synthesis_absorption_quant.xlsx"), sheetName = "HRG-E Absorption by cntry year",
            col.names = TRUE, row.names = TRUE, append = TRUE)


################################################
##equity doing the same calculations by modules
###############################################
mofped_equity_module <- all_abs_data[(grant=="UGA-H-MoFPED" | grant=="UGA-M-MoFPED" | grant=="UGA-T-MoFPED") & (semester=="Semester 1" | semester=="Semester 2") & equity=="TRUE"]
mofped_1_equity_module <- mofped_equity_module[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","gf_module")] #created the semester 1-2 expenditure and budget for Mofped grants
mofped_1_equity_module[,semester:="Semester 1-2"]

abs_yr12_equity_module <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6") & equity=="TRUE",.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","gf_module")]
abs_yr12_equity_module <- rbind(abs_yr12_equity_module,mofped_1_equity_module) #appending Uganda's MoFPED semester 1-2 budget and expenditures to other funds to then sum
abs_yr12_equity_module <- abs_yr12_equity_module[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","gf_module")]
abs_yr12_equity_module[,absorption:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco
abs_yr12_equity_module <- abs_yr12_equity_module[order(loc_name,gf_module,semester),c("loc_name","gf_module","semester","budget","expenditure","absorption")]
##################################################
write.xlsx2(abs_yr12_equity_module, file = paste0(path,"/draft_synthesis_absorption_quant.xlsx"), sheetName = "HRG-E Absorption by cntry year module",
            col.names = TRUE, row.names = TRUE, append = TRUE)



################################################
##equity doing the same calculations by categories HR, KP, other
###############################################
#making categories for HRG-Equity

##generating indicator variable for modules and interventions that the GF CRG count as "Opt-In" activities for Human Rights
hr_modules <- c("Programs to reduce human rights-related barriers to HIV services","Reducing human rights-related barriers to HIV/TB services",
                "Removing human rights and gender related barriers to TB services")
hr_interventions <- c("Addressing stigma","Removing human rights") #this will catch all interventions with the phrase "Addressing stigma"

all_abs_data[gf_module %in% hr_modules,crg_hr:= "TRUE"]
all_abs_data[gf_intervention %like% hr_interventions[1],crg_hr:= "TRUE"]
all_abs_data[gf_intervention %like% hr_interventions[2],crg_hr:= "TRUE"]
all_abs_data[is.na(crg_hr),crg_hr:= "FALSE"]

table(all_abs_data$crg_hr) #this identified 60 interventions across our 4 countries

mofped_equity_int <- all_abs_data[(grant=="UGA-H-MoFPED" | grant=="UGA-M-MoFPED" | grant=="UGA-T-MoFPED") & (semester=="Semester 1" | semester=="Semester 2") & equity=="TRUE"]
mofped_1_equity_int <- mofped_equity_int[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","crg_hr","kp")] #created the semester 1-2 expenditure and budget for Mofped grants
mofped_1_equity_int[,semester:="Semester 1-2"]

abs_yr12_equity_int <- all_abs_data[(semester=="Semester 1-2" | semester=="Semester 3-4" | semester=="Semester 5" | semester=="Semester 5-6") & equity=="TRUE",.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","crg_hr","kp")]
abs_yr12_equity_int <- rbind(abs_yr12_equity_int,mofped_1_equity_int) #appending Uganda's MoFPED semester 1-2 budget and expenditures to other funds to then sum
abs_yr12_equity_int <- abs_yr12_equity_int[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","semester","crg_hr","kp")]
abs_yr12_equity_int[,absorption:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco
abs_yr12_equity_int[crg_hr=="TRUE",label:="HRG Funds"]
abs_yr12_equity_int[crg_hr=="FALSE" & kp=="TRUE",label:="KP Funds"]
abs_yr12_equity_int[crg_hr=="FALSE" & kp=="FALSE",label:="Other Vulnerable Populations & \n HRG-Equity Realted Investments"]

abs_yr12_equity_int <- abs_yr12_equity_int[order(loc_name,label,semester),c("loc_name","label","semester","budget","expenditure","absorption")]

##collapsing across semesters
abs_yr12_equity_int_collapsed <- abs_yr12_equity_int[,.(budget=sum(budget),expenditure=sum(expenditure)),by=c("loc_name","label")]
abs_yr12_equity_int_collapsed[,absorption:=expenditure/budget] #Guatemala seems a little strange - different grant periods - consider excluding certain grants - speak with Francisco
##################################################
write.xlsx2(abs_yr12_equity_int, file = paste0(path,"/draft_synthesis_absorption_quant.xlsx"), sheetName = "HRG-E Absorption by cntry yr cats",
            col.names = TRUE, row.names = TRUE, append = TRUE)

write.xlsx2(abs_yr12_equity_int_collapsed, file = paste0(path,"/draft_synthesis_absorption_quant.xlsx"), sheetName = "HRG-E Absorption by cntry cats",
            col.names = TRUE, row.names = TRUE, append = TRUE)

