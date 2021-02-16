#############################################################
##Title: pce_synthesis_budget_data
##Purpose: Prepping budgetary data for synthesis report
##Author: Matthew Schneider
##Date: 10/29/2020, last updated and reran - 1/28/21 
##Input Files:
##           - C:\Users\mts24\Box Sync\Global Fund Files\tableau_data
##                                                                    \budgetRevisions_with_frBudgets_activityLevel.csv
##Output Files:
##          1. draft_synthesis_budget_quant.xlsx - budgets for NFM2 and NFM3 total, rssh, hrg-equity across all IHME/PATH countries
##          2. draft_hrgequity_related_modules_interventions.xlsx - list of HRG-Equity related investments
#############################################################

rm(list = ls()) #clear memory

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
                 scales,RColorBrewer)
}

if (Sys.info()[1] == "Linux"){
  j <- "/home/j"
  h <- paste0("/homes/",Sys.info()[7])
  k <- paste0("/ihme/cc_resources/libraries/")
}else if (Sys.info()[1] == "Windows"){
  c <- "C:"
  j <- "J:"
  h <- "H:"
  k <- "K:"
}

user = as.character(Sys.info()[7])

#path to save files
path <- paste0("C:/Users/",user,"/Documents/PCE/Synthesis")

##reading in latest budget data - includes NFM2 funding requests, approved for grant making budgets, all revisions, and 
##                                NFM3 funding requests and grant making budgets
##this dataset if budgets down to activities and cost categories
budget_data <- fread(input = paste0(c,"/Users/mts24/Box Sync/Global Fund Files/tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv"))

##fixing that Senegal's CNLS grant doesn't have a most revision marked - so doing it by hand
budget_data[file_name=="SEN-H-CNLS_Revised Budget_FC_FINAL_28Feb2019.xlsx",isMostRecentRevision:=TRUE]

#including differentiated testing in the equity grouping
budget_data[gf_intervention %like% "Differentiated HIV",equity := TRUE]
budget_data[gf_intervention %like% "Differentiated HIV",kp := TRUE]

##need to update RSSH interventions we are considering also part of HRG-Equity
rssh_equity_int <- c("Supportive policy and programmatic environment")
budget_data[gf_intervention=="Community-led advocacy" | 
              gf_intervention=="Community-led advocacy and research" |
              gf_intervention=="Social mobilization, building community linkages, collaboration and coordination" |
              gf_intervention=="Social mobilization, building community linkages, and coordination", equity:="TRUE"]
budget_data[gf_intervention=="Supportive policy and programmatic environment", equity:="TRUE"]


##subsetting to only important variables that will be used with compiled data across both consortia (IHME/PATH and EHG)
##we keep only grants that are active and summing up to interventions
budget_data_fr_disease <- copy(budget_data)
budget_data_fr_disease[(fr_disease=="hiv" | fr_disease=="tb"), fr_disease:="hiv/tb"]

budget_data_fr_disease[,nfm:="nfm2"]
budget_data_fr_disease[grant_period=="2021-2023",nfm:="nfm3"]
budget_data_fr_disease[grant_period=="2016-2019",nfm:="nfm1"]

#dropping any grants from Window 1 (NFM1)
budget_data_fr_disease<-budget_data_fr_disease[grant_period!="2016-2019"]

###############################################################
##Section 1. national program level data - total budgets by countries and diseases (malaria, HIV/TB, RSSH is RSSH specific grant for funding request and approved budgets)
dt_fr_gm_country_disease <- budget_data_fr_disease[!is.na(budget_version) & (budget_version=="approved" | budget_version=="funding_request17" | budget_version=="funding_request20"),
                                                                  .(budget=sum(budget,na.rm = TRUE)),
                                                                  by = c("loc_name","grant","disease","gf_module","budget_version","nfm")]

##pulling most recent revision data to them append
dt_mostrecentrevision_country_disease <- budget_data_fr_disease[!is.na(budget_version) & isMostRecentRevision=="TRUE",
                                        .(budget=sum(budget,na.rm = TRUE)),
                                        by = c("loc_name","grant","disease","gf_module","isMostRecentRevision","nfm")]
dt_mostrecentrevision_country_disease[,budget_version:="most_recent_revision"]
dt_mostrecentrevision_country_disease<- dt_mostrecentrevision_country_disease[, !"isMostRecentRevision"]

##pulling revisions that include any catalytic matching funds - note some cases where this is the same as the most recent revision data to them append
budget_data_fr_disease[,cat_funds:=FALSE]
budget_data_fr_disease[(revision_type %like% "Matching" | revision_type %like% "Catalytic"),cat_funds:=TRUE]

cat_fund_grants <- unique(budget_data_fr_disease[(revision_type %like% "Matching" | revision_type %like% "Catalytic"),grant])

dt_cat_fund_revisions_country_disease <- budget_data_fr_disease[(revision_type %like% "Matching" | revision_type %like% "Catalytic") |
                                                                (!(grant %in% cat_fund_grants) & budget_version=="approved"),
                                                                .(budget=sum(budget,na.rm = TRUE)),
                                                                by = c("loc_name","grant","disease","gf_module","nfm")]

dt_cat_fund_revisions_country_disease[,budget_version:="approved_catalytic_funds"]


##This is the draft dataset with country, disease, module level budgets for NFM2 FR, Approved for GM, and Most Recent Revisions;& NFM3 FR and GM
dt_fr_gm_fr_country_disease <- rbind(dt_fr_gm_country_disease,
                                     dt_mostrecentrevision_country_disease,
                                     dt_cat_fund_revisions_country_disease)

dt_fr_gm_fr_country_disease_wide <- data.table(dcast(dt_fr_gm_fr_country_disease,loc_name + grant + gf_module + disease  ~ nfm + budget_version,value.var = "budget", fun = sum))

############################################################
##Section 2. RSSH data - budgets by countries and modules
dt_fr_gm_rssh <- budget_data_fr_disease[rssh=="TRUE" & !is.na(budget_version) & (budget_version=="approved" | budget_version=="funding_request17" | budget_version=="funding_request20"),
                                                   .(budget=sum(budget,na.rm = TRUE)),
                                                   by = c("loc_name","grant","fr_disease","disease","gf_module","budget_version","nfm")]

dt_mostrecentrevision_rssh <- budget_data_fr_disease[rssh=="TRUE" & !is.na(budget_version) & isMostRecentRevision=="TRUE",
                                                                .(budget=sum(budget,na.rm = TRUE)),
                                                                by = c("loc_name","grant","fr_disease","disease","gf_module","isMostRecentRevision","nfm")]
dt_mostrecentrevision_rssh[,budget_version:="most_recent_revision"]
dt_mostrecentrevision_rssh<- dt_mostrecentrevision_rssh[, !"isMostRecentRevision"]

##pulling revisions that include any catalytic matching funds - note some cases where this is the same as the most recent revision data to them append
dt_cat_fund_rssh <- budget_data_fr_disease[rssh=="TRUE" & ((revision_type %like% "Matching" | revision_type %like% "Catalytic") |
                                                             (!(grant %in% cat_fund_grants) & budget_version=="approved")),
                                                                .(budget=sum(budget,na.rm = TRUE)),
                                                                by = c("loc_name","grant","fr_disease","disease","gf_module","nfm")]
dt_cat_fund_rssh[,budget_version:="approved_catalytic_funds"]


##This is the draft dataset with country, disease, module for RSSH level budgets for NFM2 FR, Approved for GM, and Most Recent Revisions;& NFM3 FR and GM
dt_fr_gm_fr_rssh <- rbind(dt_fr_gm_rssh,dt_mostrecentrevision_rssh,dt_cat_fund_rssh)

dt_fr_gm_fr_rssh_wide <- data.table(dcast(dt_fr_gm_fr_rssh,loc_name + grant + fr_disease + disease + gf_module  ~ nfm + budget_version,value.var = "budget", fun = sum))

#############################################################
##Section 3. Equity data - budgets by countries, modules, interventions
dt_fr_gm_equity <- budget_data_fr_disease[equity=="TRUE" & !is.na(budget_version) & (budget_version=="approved" | budget_version=="funding_request17" | budget_version=="funding_request20"),
                                        .(budget=sum(budget,na.rm = TRUE)),
                                        by = c("loc_name","grant","fr_disease","disease","gf_module","gf_intervention","budget_version","nfm","kp")]

dt_mostrecentrevision_equity <- budget_data_fr_disease[equity=="TRUE" & !is.na(budget_version) & isMostRecentRevision=="TRUE",
                                                     .(budget=sum(budget,na.rm = TRUE)),
                                                     by = c("loc_name","grant","fr_disease","disease","gf_module","gf_intervention","isMostRecentRevision","nfm","kp")]
dt_mostrecentrevision_equity[,budget_version:="most_recent_revision"]
dt_mostrecentrevision_equity<- dt_mostrecentrevision_equity[, !"isMostRecentRevision"]


##pulling revisions that include any catalytic matching funds - note some cases where this is the same as the most recent revision data to them append
dt_cat_fund_equity <- budget_data_fr_disease[equity=="TRUE" & ((revision_type %like% "Matching" | revision_type %like% "Catalytic") |
                                                              (!(grant %in% cat_fund_grants) & budget_version=="approved")),
                                           .(budget=sum(budget,na.rm = TRUE)),
                                           by = c("loc_name","grant","fr_disease","disease","gf_module","gf_intervention","budget_version","nfm","kp")]

dt_cat_fund_equity[,budget_version:="approved_catalytic_funds"]

##This is the draft dataset with country, disease, module for Equity level budgets for NFM2 FR, Approved for GM, and Most Recent Revisions;& NFM3 FR and GM
dt_fr_gm_fr_equity <- rbind(dt_fr_gm_equity,dt_mostrecentrevision_equity,dt_cat_fund_equity)

dt_fr_gm_fr_equity_wide <- data.table(dcast(dt_fr_gm_fr_equity,loc_name + grant + fr_disease +disease + gf_module + gf_intervention + kp  ~ nfm + budget_version,value.var = "budget", fun = sum))

##generating indicator variable for modules and interventions that the GF CRG count as "Opt-In" activities for Human Rights
hr_modules <- c("Programs to reduce human rights-related barriers to HIV services","Reducing human rights-related barriers to HIV/TB services",
                "Removing human rights and gender related barriers to TB services")
hr_interventions <- c("Addressing stigma","Removing human rights") #this will catch all interventions with the phrase "Addressing stigma"

dt_fr_gm_fr_equity_wide[gf_module %in% hr_modules,crg_hr:= "TRUE"]
dt_fr_gm_fr_equity_wide[gf_intervention %like% hr_interventions[1],crg_hr:= "TRUE"]
dt_fr_gm_fr_equity_wide[gf_intervention %like% hr_interventions[2],crg_hr:= "TRUE"]
dt_fr_gm_fr_equity_wide[is.na(crg_hr),crg_hr:= "FALSE"]

table(dt_fr_gm_fr_equity_wide$crg_hr) #this identified 60 interventions across our 4 countries
dt_fr_gm_fr_equity_wide[crg_hr=="TRUE",label:="Human rights related investments"]
dt_fr_gm_fr_equity_wide[crg_hr=="FALSE" & kp=="TRUE",label:="KVP related investments"]
dt_fr_gm_fr_equity_wide[crg_hr=="FALSE" & kp=="FALSE",label:="Other equity realted investments"]

dt_fr_gm_fr_equity_wide[,sum(nfm2_most_recent_revision),by=crg_hr] #we see that $11.6m of the previously identified HRG-Equity Bubdgets are identified by the "Opt-In" 60 interventions
dt_fr_gm_fr_equity_wide[,sum(nfm2_approved),by=crg_hr] #we see that $5.8m of the previously identified HRG-Equity Bubdgets are identified by the "Opt-In" 60 interventions

dt_fr_gm_fr_equity_wide[loc_name=='Uganda',sum(nfm2_most_recent_revision),by=crg_hr] #we see that $7.8m of the previously identified HRG-Equity Bubdgets are identified by the "Opt-In" 60 interventions
dt_fr_gm_fr_equity_wide[loc_name=='Uganda',sum(nfm2_approved),by=crg_hr] #we see that $4.5m of the previously identified HRG-Equity Bubdgets are identified by the "Opt-In" 60 interventions

##Generating indicator variable for modules and interventions that GF CRG use search terms to identify non-"Opt-In" HR funding
hr_2_interventions <- c("Interventions for young key populations","Gender-based violence",
                        "Counselling and psycho-social support","Orphans and vulnerable children",
                        "Engaging all care providers","prisoners","mobile populations",
                        "Key populations: others", "Community-based monitoring","Community-led advocacy and research",
                        "Social mobilization","Institutional capacity building, planning and leadership development",
                        "Community TB care delivery","Community TB care delivery","Community TB/HIV care delivery","Key populations - ",
                        "Key populations (TB/HIV)","Key populations","Interventions for young")

for (c in 1:19) { #length(hr_2_interventions)){
  search <- hr_2_interventions[c]
  print(search)
  dt_fr_gm_fr_equity_wide[gf_intervention %like% search,crg_hr_2:= "TRUE"]  
}
dt_fr_gm_fr_equity_wide[is.na(crg_hr_2),crg_hr_2:= "FALSE"]  
table(dt_fr_gm_fr_equity_wide$crg_hr_2)
unique_int <- data.table(unique(dt_fr_gm_fr_equity_wide[crg_hr_2!="TRUE" & crg_hr!="TRUE",gf_intervention]))

ihme_pce_hrge_module_interventions <- unique(dt_fr_gm_fr_equity_wide[,c("gf_module","gf_intervention","crg_hr","kp")])
ihme_pce_hrge_module_interventions[crg_hr==FALSE & kp==FALSE,other_equity:=TRUE]
ihme_pce_hrge_module_interventions[crg_hr==TRUE & kp==TRUE,kp:=FALSE]

#saving dataset of equity related modules and interventions (by CRG HR groups and KVP)
path <- paste0("C:/Users/mts24/Documents/PCE/Synthesis")
write.xlsx2(ihme_pce_hrge_module_interventions, file = paste0(data_path,"/draft_hrgequity_related_modules_interventions.xlsx"))

country_equity_subanalyses <- dt_fr_gm_fr_equity_wide[,.(fr17_hr=sum(nfm2_funding_request17), approved_hr=sum(nfm2_approved),
                                                         approved_catalytic_hr=sum(nfm2_approved_catalytic_funds),mostrecent_hr=sum(nfm2_most_recent_revision), 
                                                         fr20_hr=sum(nfm3_funding_request20)),
                                                      by=c("loc_name","crg_hr","crg_hr_2","kp")] #we see that $5.8m of the previously identified HRG-Equity Bubdgets are identified by the "Opt-In" 60 interventions
country_equity_subanalyses[crg_hr=="TRUE",label:="CRG Opt-In Interventions"]
country_equity_subanalyses[crg_hr_2=="TRUE",label:="CRG non-Opt-In Interventions (total)"]
country_equity_subanalyses[crg_hr=="FALSE" & crg_hr_2=="FALSE",label:="Other HRG-Equity Realted Investments"]

country_equity_subanalyses_2 <- dt_fr_gm_fr_equity_wide[,.(fr17_hr=sum(nfm2_funding_request17), 
                                                           approved_catalytic_hr=sum(nfm2_approved_catalytic_funds),
                                                           approved_hr=sum(nfm2_approved),
                                                           mostrecent_hr=sum(nfm2_most_recent_revision), 
                                                           fr20_hr=sum(nfm3_funding_request20)),
                                                        by=c("loc_name","crg_hr","kp")] #we see that $5.8m of the previously identified HRG-Equity Bubdgets are identified by the "Opt-In" 60 interventions
country_equity_subanalyses_2[crg_hr=="TRUE",label:="HRG Funds"]
country_equity_subanalyses_2[crg_hr=="FALSE" & kp=="TRUE",label:="KP Funds"]
country_equity_subanalyses_2[crg_hr=="FALSE" & kp=="FALSE",label:="Other Vulnerable Populations & \n HRG-Equity Realted Investments"]

fig <- melt(country_equity_subanalyses_2[,!c("crg_hr","kp")], id.vars = c("loc_name","label"))

fig[variable=="fr17_hr",variable_desc:="NFM2: \n Funding Request"]
fig[variable=="approved_catalytic_hr",variable_desc:="NFM2: \n Grant Making Budget \n with Catalytic Funds"]
fig[variable=="approved_hr",variable_desc:="NFM2: \n Grant Making Budget"]
fig[variable=="mostrecent_hr",variable_desc:="NFM2: \n Most Recent Budget"]
fig[variable=="fr20_hr",variable_desc:="NFM3: \n Funding Request"]

#Figure Global Expenditures absolute values
ggplot(fig, aes(fill=forcats::fct_rev(label), y=value, x=variable_desc)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = rev(colorRampPalette(brewer.pal(5,"RdYlBu"))(7)), guide = guide_legend(reverse = TRUE, nrow = 4)) +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-6)) + 
  ylab("Budget (millions $)") + xlab("Budget")  +
  theme(legend.position = "bottom", axis.text=element_text(size=14),axis.title=element_text(size=16)) +
  #legend.text = element_text(12)) + #,legend.title = element_text(8,face = "bold"))
  labs(fill = "", title ="HRG-Equity Budget Changes") + facet_wrap("loc_name", scales = "free_y") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

#ggsave(paste0(figs,"global_phc_gdp_nohi_062520.png"),width = 18, height = 8.25, units = "in")

dt_fr_gm_fr_equity_wide[loc_name=='Uganda',sum(nfm2_most_recent_revision),by=c("crg_hr","crg_hr_2")] #we see that $7.8m of the previously identified HRG-Equity Bubdgets are identified by the "Opt-In" 60 interventions
dt_fr_gm_fr_equity_wide[loc_name=='Uganda',sum(nfm2_approved),by=c("crg_hr","crg_hr_2")] #we see that $4.5m of the previously identified HRG-Equity Bubdgets are identified by the "Opt-In" 60 interventions


###################################
#Saving Output
data_path <- paste0("C:/Users/",user,"/Box Sync/Global Fund Files/synthesis/data")

dt_fr_gm_fr_country_disease_wide <- dt_fr_gm_fr_country_disease_wide[,c("loc_name","grant","disease","gf_module","nfm2_funding_request17",
                                                                           "nfm2_approved","nfm2_approved_catalytic_funds","nfm2_most_recent_revision",
                                                                           "nfm3_funding_request20","nfm3_approved")]

write.xlsx2(dt_fr_gm_fr_country_disease_wide, file = paste0(data_path,"/draft_synthesis_budget_quant.xlsx"), sheetName = "National Program",
            col.names = TRUE, row.names = TRUE, append = FALSE)


dt_fr_gm_fr_rssh_wide <- dt_fr_gm_fr_rssh_wide[,c("loc_name","grant","fr_disease","disease","gf_module","nfm2_funding_request17",
                                                  "nfm2_approved","nfm2_approved_catalytic_funds","nfm2_most_recent_revision",
                                                  "nfm3_funding_request20","nfm3_approved")]

write.xlsx2(dt_fr_gm_fr_rssh_wide, file = paste0(data_path,"/draft_synthesis_budget_quant.xlsx"), sheetName = "RSSH",
            col.names = TRUE, row.names = TRUE, append = TRUE)

dt_fr_gm_fr_equity_wide <- dt_fr_gm_fr_equity_wide[,c("loc_name","grant","label","fr_disease","disease","gf_module","gf_intervention",
                                                      "nfm2_funding_request17","nfm2_approved",
                                                      "nfm2_approved_catalytic_funds","nfm2_most_recent_revision",
                                                      "nfm3_funding_request20","nfm3_approved")]

write.xlsx2(dt_fr_gm_fr_equity_wide, file = paste0(data_path,"/draft_synthesis_budget_quant.xlsx"), sheetName = "HRG-Equity",
            col.names = TRUE, row.names = TRUE, append = TRUE)

##removing certain continuation grants for the NFM2 FR to GM analysis
no_fr17_grants <- c("COD-M-MOH","COD-M-SANRU","SEN-H-ANCS","SEN-H-CNLS","SEN-M-PNLP")
dt_fr_gm_fr_country_disease_wide_fr_gm <- dt_fr_gm_fr_country_disease_wide[!(grant %in% no_fr17_grants)]
dt_fr_gm_fr_rssh_wide_fr_gm <- dt_fr_gm_fr_rssh_wide[!(grant %in% no_fr17_grants)]
dt_fr_gm_fr_equity_wide_fr_gm <- dt_fr_gm_fr_equity_wide[!(grant %in% no_fr17_grants)]

dt_fr_gm_fr_country_disease_wide_fr_gm <- dt_fr_gm_fr_country_disease_wide_fr_gm[,
                                             .(nfm2_funding_request17=sum(nfm2_funding_request17,na.rm = TRUE),
                                               nfm2_approved=sum(nfm2_approved,na.rm = TRUE),
                                               nfm2_approved_catalytic_funds=sum(nfm2_approved_catalytic_funds,na.rm = TRUE),
                                               nfm2_most_recent_revision=sum(nfm2_most_recent_revision,na.rm = TRUE),
                                               nfm3_funding_request20=sum(nfm3_funding_request20,na.rm = TRUE),
                                               nfm3_approved=sum(nfm3_approved,na.rm = TRUE)),
                                             by = ,c("loc_name","gf_module")]


write.xlsx2(dt_fr_gm_fr_country_disease_wide_fr_gm, file = paste0(data_path,"/draft_synthesis_budget_quant.xlsx"), sheetName = "FR-GM National Program",
            col.names = TRUE, row.names = TRUE, append = TRUE)


dt_fr_gm_fr_rssh_wide_fr_gm <- dt_fr_gm_fr_rssh_wide_fr_gm[,
                                          .(nfm2_funding_request17=sum(nfm2_funding_request17,na.rm = TRUE),
                                          nfm2_approved=sum(nfm2_approved,na.rm = TRUE),
                                          nfm2_approved_catalytic_funds=sum(nfm2_approved_catalytic_funds,na.rm = TRUE),
                                          nfm2_most_recent_revision=sum(nfm2_most_recent_revision,na.rm = TRUE),
                                          nfm3_funding_request20=sum(nfm3_funding_request20,na.rm = TRUE),
                                          nfm3_approved=sum(nfm3_approved,na.rm = TRUE)),
                                          by = ,c("loc_name","gf_module")]


write.xlsx2(dt_fr_gm_fr_rssh_wide_fr_gm, file = paste0(data_path,"/draft_synthesis_budget_quant.xlsx"), sheetName = "FR-GM RSSH",
            col.names = TRUE, row.names = TRUE, append = TRUE)

dt_fr_gm_fr_equity_wide_fr_gm <- dt_fr_gm_fr_equity_wide_fr_gm[,
                              .(nfm2_funding_request17=sum(nfm2_funding_request17,na.rm = TRUE),
                              nfm2_approved=sum(nfm2_approved,na.rm = TRUE),
                              nfm2_approved_catalytic_funds=sum(nfm2_approved_catalytic_funds,na.rm = TRUE),
                              nfm2_most_recent_revision=sum(nfm2_most_recent_revision,na.rm = TRUE),
                              nfm3_funding_request20=sum(nfm3_funding_request20,na.rm = TRUE),
                              nfm3_approved=sum(nfm3_approved,na.rm = TRUE)),
                              by = ,c("loc_name","gf_module","gf_intervention","label")]

write.xlsx2(dt_fr_gm_fr_equity_wide_fr_gm, file = paste0(data_path,"/draft_synthesis_budget_quant.xlsx"), sheetName = "FR-GM HRG-Equity",
            col.names = TRUE, row.names = TRUE, append = TRUE)
