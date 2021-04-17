#########################################################
# Audrey Batzel
# look at changes in HRG/equity across countries

# clear
rm(list=ls())

# Set up
library(ggplot2)
library(data.table)
library(readxl)
library(scales)
library(grid)
library(lattice)
library(RColorBrewer)

# -------------------------------------------------------------------
# Files and directories
setwd('C:/local/gf/')

user=as.character(Sys.info()[7])

# input file
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/tableau_data/")
inFile = paste0(box, 'budgetRevisions_with_frBudgets_activityLevel.csv')

# -------------------------------------------------------------------
# read in data
data = as.data.table(read.csv(inFile))

# sum by HRG equity, grant, country
dt = data[grant_period == '2018-2020',]
dt = dt[ isMostRecentRevision == TRUE | budget_version %in% c('Approved and Catalytic/Matching Funds', 'approved'),]          
          
dt = dt[, .(budget=sum(budget, na.rm = TRUE)), by = .(loc_name, grant, budget_version, equity)]

wide = dcast.data.table(dt, loc_name + grant + budget_version ~ equity, value.var = 'budget')
setnames(wide, 'TRUE', 'equity_budget')
setnames(wide, 'FALSE', 'non_equity_budget')

wide[, budget_total := equity_budget + non_equity_budget]
wide[, percent_equity_budget := round((equity_budget/budget_total)*100)]

wide[grepl(budget_version, pattern='revision'), budget_version := 'most_recent_revision']
wide[budget_version == 'Approved and Catalytic/Matching Funds', budget_version := 'approved_catalytic_matching_funds']

# wide[budget_version == 'approved', budget_version_graph := 'Approved']
# wide[budget_version == 'approved_with_catalytic', budget_version_graph := 'Approved and Catalytic/Matching Funds']
# wide[budget_version == 'most_recent_revision', budget_version_graph := 'Most Recent Revision']

wide[, budget_version := as.factor(budget_version)]
wide$budget_version = ordered(wide$budget_version, c('most_recent_revision', 'approved_catalytic_matching_funds', 'approved' ))

colors = c('#D55E00','#56B4E9','#C378A2')
names(colors) = levels(wide$budget_version)

# plot bar chart showing percentage
g1 = ggplot(wide[loc_name != 'Guatemala'], aes(x = grant, y = percent_equity_budget, fill = budget_version)) + 
  theme_bw() + 
  geom_bar(stat = 'identity', position=position_dodge()) +
  facet_grid(rows = 'loc_name', scales = 'free_y') +
  labs(fill = 'Budget Version', x = 'Grant', y = 'Percentage of the grant', title = 'Percent of budget related to HRG-Equity') +
  theme(legend.position="bottom") + 
  theme(text=element_text(size=18)) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,100) +
  coord_flip() +
  geom_text(aes(x = grant, y = percent_equity_budget, label = paste0(percent_equity_budget, '%'), group = budget_version), 
            hjust = -0.3, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE) +
  scale_fill_manual(name = 'Budget Version', values = colors)

g2 = ggplot(wide[loc_name != 'Guatemala' & budget_version %in% c('most_recent_revision', 'approved_catalytic_matching_funds')], aes(x = grant, y = percent_equity_budget, fill = budget_version)) + 
  theme_bw() + 
  geom_bar(stat = 'identity', position=position_dodge()) +
  facet_grid(rows = 'loc_name', scales = 'free_y') +
  labs(fill = 'Budget Version', x = 'Grant', y = 'Percentage of the grant', title = 'Percent of budget related to HRG-Equity') +
  theme(legend.position="bottom") + 
  theme(text=element_text(size=18)) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  geom_text(aes(x = grant, y = percent_equity_budget, label = paste0(percent_equity_budget, '%'), group = budget_version), 
            hjust = -0.3, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE)  +
  scale_fill_manual(name = 'Budget Version', values = colors)


g3 = ggplot(wide[loc_name != 'Guatemala', ], aes(x = grant, y = equity_budget/1000000, fill = budget_version)) + 
  theme_bw() + 
  geom_bar(stat = 'identity', position=position_dodge()) +
  facet_grid(rows = 'loc_name', scales = 'free_y') +
  labs(fill = 'Budget Version', x = 'Grant', y = 'Budget (millions USD)', title = 'Total budget related to HRG-Equity') +
  theme(legend.position="bottom") + 
  theme(text=element_text(size=18)) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  geom_text(aes(x = grant, y = equity_budget/1000000, label = paste0('$', round((equity_budget/1000000), 2), ' million'), group = budget_version), 
            hjust = -0.1, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE) +
  scale_fill_manual(name = 'Budget Version', values = colors)


g4 = ggplot(wide[loc_name != 'Guatemala' & budget_version %in% c('most_recent_revision', 'approved_catalytic_matching_funds')], aes(x = grant, y = equity_budget/1000000, fill = budget_version)) + 
  theme_bw() + 
  geom_bar(stat = 'identity', position=position_dodge()) +
  facet_grid(rows = 'loc_name', scales = 'free_y') +
  labs(fill = 'Budget Version', x = 'Grant', y = 'Budget (millions USD)', title = 'Total budget related to HRG-Equity') +
  theme(legend.position="bottom") + 
  theme(text=element_text(size=18)) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  geom_text(aes(x = grant, y = equity_budget/1000000, label = paste0('$', round((equity_budget/1000000), 2), ' million'), group = budget_version), 
            hjust = -0.1, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE) +
  scale_fill_manual(name = 'Budget Version', values = colors)

  
outFile = 'J:/Project/Evaluation/GF/resource_tracking/visualizations2020/hrg_equity_across_budget_versions.pdf'
pdf(outFile, height = 11, width = 16)
print(g1)
print(g3)
#print(g2)
#print(g4)
dev.off()

outFile_png = 'J:/Project/Evaluation/GF/resource_tracking/visualizations2020/hrg_equity_across_budget_versions_percentage.png'

png(outFile_png, height = 9, width = 12, units = "in", res = 300)
print(g1)
dev.off()

# -------------------------------------------------------------------
# Calculate percent change
values = wide[loc_name != 'Guatemala', .(grant, budget_version, equity_budget)]
values = dcast.data.table(values, grant ~ budget_version)
values = values[!is.na(approved_catalytic_matching_funds)]
  sum(values$approved_catalytic_matching_funds) - sum(values$approved)
  ((sum(values$approved_catalytic_matching_funds) - sum(values$approved)) / sum(values$approved))*100
values = values[!is.na(most_recent_revision )]
  sum(values$most_recent_revision) - sum(values$approved_catalytic_matching_funds)
  ((sum(values$most_recent_revision) - sum(values$approved_catalytic_matching_funds)) / sum(values$approved_catalytic_matching_funds))*100

# -------------------------------------------------------------------
# INCLUDE FUNDING REQUESTS:  

# sum by HRG equity, grant, country
dt2 = data[grant_period == '2018-2020',]
dt2 = dt2[ budget_version == 'funding_request17',]          
dt2 = dt2[, .(budget=sum(budget, na.rm = TRUE)), by = .(loc_name, equity, budget_version)]
dt2_wide = dcast.data.table(dt2, loc_name + budget_version ~ equity)

setnames(dt2_wide, 'TRUE', 'equity_budget')
setnames(dt2_wide, 'FALSE', 'non_equity_budget')

dt2_wide[, budget_total := equity_budget + non_equity_budget]
dt2_wide[, percent_equity_budget := round((equity_budget/budget_total)*100)]

dt_w_fr = rbindlist(list(wide, dt2_wide), use.names = TRUE, fill = TRUE)
dt_w_fr[, c('percent_equity_budget') := NULL]

sd = c('non_equity_budget', 'equity_budget', 'budget_total')
check = dt_w_fr[, lapply(.SD, sum), by = c('loc_name', 'budget_version'), .SDcols = sd]
check[, percent_equity_budget := round((equity_budget/budget_total)*100)]

colors2 = c('#D55E00','#56B4E9','#C378A2', '#AED581')
names(colors2) = levels(check$budget_version)

g5 = ggplot(check, aes(x = loc_name, y = percent_equity_budget, fill = budget_version)) + 
  theme_bw() + 
  geom_bar(stat = 'identity', position=position_dodge()) +
  labs(fill = 'Budget Version', x = 'Country', y = 'Percentage', title = 'Percent of budget related to HRG-Equity across grants') +
  # theme(legend.position="bottom") + 
  theme(text=element_text(size=18)) +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,100) +
  coord_flip() +
  geom_text(aes(x = loc_name, y = percent_equity_budget, label = paste0(percent_equity_budget, '%'), group = budget_version), 
            hjust = -0.3, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE) +
  scale_fill_manual(name = 'Budget Version', values = colors2)

outFile_png2 = 'J:/Project/Evaluation/GF/resource_tracking/visualizations2020/hrg_equity_across_budget_versions_percentage_wFRs.png'
png(outFile_png2, height = 8, width = 10, units = "in", res = 300)
print(g5)
dev.off()


# -------------------------------------------------------------------
# read in data
data = as.data.table(read.csv(inFile))
dt = data[grant_period == '2021-2023',]
dt = dt[ isMostRecentRevision == TRUE | budget_version %in% c('funding_request20', 'approved'),]          

loc = 'Uganda'
# HRG-Equity
# modules
hrge = dt[loc_name == loc & equity == TRUE, .(budget=sum(budget, na.rm = TRUE)), by = .(loc_name, budget_version, gf_module, equity)]
hrge_wide = dcast.data.table(hrge, loc_name + gf_module ~ budget_version, value.var = 'budget')
hrge_wide[, percent_change := ((approved-funding_request20)/funding_request20)*100]
# interventions
i_hrge = dt[loc_name == loc & equity == TRUE, .(budget=sum(budget, na.rm = TRUE)), by = .(loc_name, budget_version, gf_module, gf_intervention, equity)]
i_hrge_wide = dcast.data.table(i_hrge, loc_name + gf_module + gf_intervention ~ budget_version, value.var = 'budget')
i_hrge_wide[, percent_change := ((approved-funding_request20)/funding_request20)*100]
# RSSH
# modules
rssh = dt[loc_name == loc & rssh == TRUE, .(budget=sum(budget, na.rm = TRUE)), by = .(loc_name, budget_version, gf_module, rssh)]
rssh_wide = dcast.data.table(rssh, loc_name + gf_module ~ budget_version, value.var = 'budget')
rssh_wide[, percent_change := ((approved-funding_request20)/funding_request20)*100]
# interventions
i_rssh = dt[loc_name == loc & rssh == TRUE, .(budget=sum(budget, na.rm = TRUE)), by = .(loc_name, budget_version, gf_module, gf_intervention, rssh)]
i_rssh_wide = dcast.data.table(i_rssh, loc_name + gf_module + gf_intervention ~ budget_version, value.var = 'budget')
i_rssh_wide[, percent_change := ((approved-funding_request20)/funding_request20)*100]
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# save a data set for the UGA team to use
data = as.data.table(read.csv(inFile))
dt = data[grant_period == '2021-2023',]
out_dt = dt[ loc_name == 'Uganda' & budget_version %in% c('funding_request20', 'approved'),]          
setnames(out_dt, 'SO', 'strategicObjective')
out_dt = out_dt[, .(loc_name, gf_module, gf_intervention, activity_description, cost_category, budget_version, file_name, grant, disease, fr_disease, rssh, equity, isStrategicObjective, strategicObjective, budget)]
out_dt[, grant_cycle := 'NFM3']
out_dt_SO = out_dt[isStrategicObjective == TRUE, ]
write.csv(out_dt_SO, paste0("C:/Users/abatzel/Box Sync/Global Fund Files/UGA/data_for_idrc/nfm3_frgm_data_uganda_equity_rssh.csv"), row.names = FALSE)
write.csv(out_dt, paste0("C:/Users/abatzel/Box Sync/Global Fund Files/UGA/data_for_idrc/nfm3_frgm_data_uganda.csv"), row.names = FALSE)

# -------------------------------------------------------------------