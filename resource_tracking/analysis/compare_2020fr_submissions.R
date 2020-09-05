# -----------------------------------------------
# AUTHOR: Audrey Batzel
# PURPOSE: Assess differences in the FRs submitted to CT versus those submitted to TRP
# DATE: September 2020

# The current working directory should be the root of this repository

# TO DO: 
  # -Calculate percent change for each of these sub-sections and see which had the largest in either direction
  # -Add in a legend somehow so we know which are which

# ******Note: This would probably all be fairly easy to put into Tableau - if we want to expand much more off of these, 
# we should just go ahead and put it in Tableau because in the end it will be a lot more efficient. 
# -----------------------------------------------
rm(list=ls())
# -----------------------------------------------
# SET UP R
# -----------------------------------------------
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  if(user == 'abatzel'){ 
    repo_root = "C:/local/gf/"
  } else {
    repo_root = paste0("C:/Users/", user, "/Documents/gf/")} #Change to the root of your repository
  setwd(repo_root)
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}
# source files with other functions and common resource tracking filepaths
source("./resource_tracking/prep/_common/set_up_r.R", encoding="UTF-8")
# -----------------------------------------------

# -----------------------------------------------
# read in data
# -----------------------------------------------
dt = as.data.table(read.csv(paste0(box, "tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv")))
dt = dt[budget_version %in% c('funding_request20', 'funding_request20_CT')]

key_cols = c('loc_name', 'budget_version', 'fr_disease')

# set levels to order the bars in the plot
dt[, budget_version := as.character(budget_version)]
dt[, budget_version := factor(budget_version, levels = c('funding_request20_CT', 'funding_request20'))]
# -----------------------------------------------

# -----------------------------------------------
# comparison at module level
# -----------------------------------------------
mod_sum = dt[, .(budget= sum(budget)), by = c(key_cols, 'gf_module')]

i = 1
by_mod_plots = NULL
for( x in unique(dt$loc_name) ){
  by_mod_plots[[i]] = ggplot(mod_sum[loc_name == x,], aes(x=budget_version, y=budget, fill = gf_module)) + 
        geom_bar(stat = "identity") +
        #coord_flip() +
        facet_grid(loc_name ~ fr_disease) +
        theme(legend.position="none") +
        ggtitle(x)
  i = i + 1
}
for(i in seq(length(by_mod_plots))) { 
  print(by_mod_plots[[i]])
} 
# -----------------------------------------------

# -----------------------------------------------
# comparison at intervention level - # TO DO: loop through by module as well? 
# -----------------------------------------------
int_sum = dt[, .(budget= sum(budget)), by = c(key_cols, 'gf_module', 'gf_intervention')]
i = 1
by_int_plots = NULL
for( x in unique(dt$loc_name) ){
  by_int_plots[[i]] = ggplot(int_sum[loc_name == x,], aes(x=budget_version, y=budget, fill = gf_intervention)) + 
    geom_bar(stat = "identity") +
    #coord_flip() +
    facet_grid(loc_name ~ fr_disease) +
    theme(legend.position="none") +
    ggtitle(x)
  i = i + 1
}
for(i in seq(length(by_int_plots))) { 
  print(by_int_plots[[i]])
} 
# -----------------------------------------------

# -----------------------------------------------
# comparison for focus topics
# -----------------------------------------------
# entire focus topic-level
ft_sum = dt[isTopicArea == TRUE, .(budget= sum(budget)), by = c(key_cols, 'topicAreaDesc')]
i = 1
by_ft_plots = NULL
for( x in unique(dt$loc_name) ){
  by_ft_plots[[i]] = ggplot(ft_sum[loc_name == x,], aes(x=budget_version, y=budget, fill = topicAreaDesc)) + 
    geom_bar(stat = "identity") +
    #coord_flip() +
    facet_grid(loc_name ~ fr_disease) +
    #theme(legend.position="none") +
    ggtitle(x)
  i = i + 1
}
for(i in seq(length(by_ft_plots))) { 
  print(by_ft_plots[[i]])
} 

# focus topic intervention-level
ft_int_sum = dt[isTopicArea == TRUE, .(budget= sum(budget)), by = c(key_cols, 'topicAreaDesc', 'gf_intervention')]
i = 1
by_ft_int_plots = NULL
for( x in unique(dt$loc_name) ){
  by_ft_int_plots[[i]] = ggplot(ft_int_sum[loc_name == x,], aes(x=budget_version, y=budget, fill = gf_intervention)) + 
    geom_bar(stat = "identity") +
    #coord_flip() +
    facet_grid(topicAreaDesc ~ fr_disease) +
    theme(legend.position="none") +
    ggtitle(x)
  i = i + 1
}
for(i in seq(length(by_ft_int_plots))) { 
  print(by_ft_int_plots[[i]])
} 
# -----------------------------------------------

# -----------------------------------------------
# RSSH/Equity (equity is on hold because we need to revist equity decisions with EHG)
# -----------------------------------------------
# RSSH interventions
rssh_int_sum =  dt[SO == 'rssh', .(budget= sum(budget)), by = c(key_cols, 'gf_module', 'gf_intervention')]
i = 1
by_rssh_int_plots = NULL
for( x in unique(dt$loc_name) ){
  by_rssh_int_plots[[i]] = ggplot(rssh_int_sum[loc_name == x,], aes(x=budget_version, y=budget, fill = gf_intervention)) + 
    geom_bar(stat = "identity") +
    #coord_flip() +
    facet_grid(loc_name ~ fr_disease) +
    theme(legend.position="none") +
    ggtitle(x)
  i = i + 1
}
for(i in seq(length(by_rssh_int_plots))) { 
  print(by_rssh_int_plots[[i]])
} 
# -----------------------------------------------


