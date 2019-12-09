# --------------------------------------
# Sample financial analyses 
# Emily Linebarger, November 22 2019 
# *Please copy and use wherever needed!*
#---------------------------------------

rm(list=ls())

# load libraries, and set up Box filepath 
library(data.table)

user = Sys.info()[[7]]
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/") #Change to your Box location if saved in another place
repoRoot = paste0("C:/Users/", user, "/Documents/gf/") # Change to the root of your repository

source(paste0(repoRoot, "resource_tracking/analysis/graphing_functions.r"))

#-----------------------------------------
# Helpful links 
# 2018-2020 Modular Framework: "J:\Project\Evaluation\GF\resource_tracking\modular_framework_mapping\Internal Module Map_04.2019.xlsx"
# Resource Tracking Codebook: "J:\Project\Evaluation\GF\resource_tracking\documentation\RT_Codebook.xlsx"
# RT File Finder (Shiny app) "~gf\resource_tracking\analysis\rtFileFinder\app.R" (in repository)
# All of my recent analyses are stored by folder - there's tons of starter code, and please feel free to copy it. ~gf\resource_tracking\analysis\deliverables
#-----------------------------------------

#---------------
# ABSORPTION 
#--------------
# Read in prepped absorption dataset from each country, and bind them together. 
# Be careful with grant periods and semesters when doing analyses! I would always specify what grant period and semester you want to subset to in any calculation. 
cod = readRDS(paste0(box, "COD/prepped_data/absorption_cod.rds"))
gtm = readRDS(paste0(box, "GTM/prepped_data/absorption_gtm.rds"))
sen = readRDS(paste0(box, "SEN/prepped_data/absorption_sen.rds"))
uga = readRDS(paste0(box, "UGA/prepped_data/absorption_uga.rds"))

all_absorption = rbindlist(list(cod, gtm, sen, uga))

#To calculate absorption, sum budget and expenditure by the variables you want to stratify by, and then calculate absorption (expenditure/budget). 
# Be very careful with the PUDR period you're tagging, and whether you want one semester of absorption or cumulative! 
head(all_absorption)

# What has human rights absorption been in S1 2019 for the UGA-C-TASO grant? 
c_taso_csw = all_absorption[grant_period=="2018-2020" & semester=="Semester 3" & 
                            grant=="UGA-C-TASO" & gf_module=="Programs to reduce human rights-related barriers to HIV services", 
                            .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T))]
c_taso_csw[, absorption:=round((expenditure/budget)*100, 1)]
c_taso_csw

# Can you do this by intervention? 
c_taso_csw1 = all_absorption[grant_period=="2018-2020" & semester=="Semester 3" & 
                              grant=="UGA-C-TASO" & gf_module=="Programs to reduce human rights-related barriers to HIV services", 
                            .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by='gf_intervention']
c_taso_csw1[, absorption:=round((expenditure/budget)*100, 1)]
c_taso_csw1

#What if you wanted cumulative absorption (first 18 months of the grant?)
c_taso_csw_cumulative = all_absorption[grant_period=="2018-2020" & semester=="Semester 3" & 
                              grant=="UGA-C-TASO" & gf_module=="Programs to reduce human rights-related barriers to HIV services", 
                            .(cumulative_budget=sum(cumulative_budget, na.rm=T), cumulative_expenditure=sum(cumulative_expenditure, na.rm=T))]
c_taso_csw_cumulative[, cumulative_absorption:=round((cumulative_expenditure/cumulative_budget)*100, 1)]
c_taso_csw_cumulative

# There's also a helper function to get cumulative data, and graph it in a standard format. 
# This function will ONLY give you 2018-2020 grants, for comparability - so many Guatemala grants will not be included. 
all_cumulative = get_cumulative_absorption(byVars=c('abbrev_mod'), grantSubset="UGA-C-TASO", currency="USD") # 'abbrev_mod' is a shortened version of 'gf_module' which is nicer for graphing. You can also use 'abbrev_int' 
                                                                                # in place of 'gf_intervention'. 
graph = budget_exp_bar(all_cumulative, altTitle="Cumulative absorption for UGA-C-TASO")

#---------------
# FINAL BUDGETS
#--------------
# These are the final, approved budgets from the grant-making process. They show the initial plan for the grants, but there may have been revisions since this point. 
# This one is much simpler, just sum the budget across the variables you'd like to stratify by.
# Again, would always start with the grant period you'd like to subset to. 
budgets = readRDS(paste0(box, "tableau_data/final_budgets.rds"))

# How much was budgeted for human rights in the UGA-C-TASO grant? 
budgets[grant_period=="2018-2020" & grant=="UGA-C-TASO" & gf_module=="Programs to reduce human rights-related barriers to HIV services", .(budget=sum(budget, na.rm=T))]

#-------------------
# BUDGET REVISIONS 
#-------------------
# These files show the final, approved budget compared to any subsequent iterations by module and intervention. This only includes budgets that have had at least 
# one revision, and we've been able to get the new detailed budget. 
# If there are questions about which version corresponds to which file, please use the RT file finder, subset to file_iteration == "revision" and compare the 'update_date' column. 
cod = readRDS(paste0(box, "COD/prepped_data/budget_revisions.rds"))
gtm = readRDS(paste0(box, "GTM/prepped_data/budget_revisions.rds"))
sen = readRDS(paste0(box, "SEN/prepped_data/budget_revisions.rds"))
uga = readRDS(paste0(box, "UGA/prepped_data/budget_revisions.rds"))

all_revisions = rbindlist(list(cod, gtm, sen, uga), fill=T)

# How has the budget for human rights changed in the UGA-C-TASO grant? 
all_revisions[grant_period=="2018-2020" & grant=="UGA-C-TASO" & gf_module=="Programs to reduce human rights-related barriers to HIV services", 
              .(v0=sum(v0, na.rm=T), v1=sum(v1, na.rm=T), v2=sum(v2), v3=sum(v3))]
# If a total is zero here, it means that we don't have that number of revisions, and the columns will be NA in the original dataset. 
# You can check the Shiny app to make sure. 

#-------------------------
# FUNDING LANDSCAPES 
#-------------------------
# Just use the helper function in this situation, no need to work with the raw data. 
p1 = funding_landscape("ribbon", "gtm", "tb", 2010, 2017)
p2 = funding_landscape("proportion", "gtm", "tb", 2010, 2017)
p3 = funding_landscape("proportion", "gtm", "tb", 2010, 2017, includeGHE=TRUE)

#---------------------------------------
# OTHER HELPER FUNCTIONS
#---------------------------------------
get_modules() # Shows which modules are available by disease 
get_interventions(diseaseName="rssh") #Shows you which interventions are available for a specific disease. 


#The two other prepped files that aren't included here are the absorption data by cost category, and the final expenditures data. 
# I don't think you'll need these, but just in case, please use these files. 
# Absorption by Cost Category: paste0(box, "tableau_data\all_cost_categories.rds")
# Final Expenditures: paste0(box, "tableau_data\final_expenditures.rds")