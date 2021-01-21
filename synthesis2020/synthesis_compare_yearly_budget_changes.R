# -------------------------------------------------------------------
# Audrey Batzel
# Synthesis 2020 Data
# Compare absolute budget changes (module level) across budget versions looking at yearly budgeted amounts

# To do - Matt said something about including/visualizing the relative changes 
#       - figure out what to do with GTM years? 
#       - fix COD data - why are there years 2028-2031 for COD-H-MOH? 
#       - NAs in budget_version for COD-M-MOH and COD-M-SANRU
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# set up:
# -------------------------------------------------------------------
rm(list=ls())

# set working directory to the root of the repo
setwd('C:/local/gf/')

# Set up
library(ggplot2)
library(data.table)
library(readxl)
library(scales)
library(grid)
library(lattice)
library(RColorBrewer)

# -------------------------------------------------------------------
# files and directories
# -------------------------------------------------------------------
# input file
user = as.character(Sys.info()[7])
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/")

# output file
out_dir = 'J:/Project/Evaluation/GF/resource_tracking/visualizations2020/Synthesis/'
outFile1 = paste0(out_dir, "synthesis_barplot_yearly_by_module_and_grant_check.pdf")
outFile2 = paste0(out_dir, "synthesis_barplot_yearly_by_module_and_country.pdf")
outFile3 = paste0(out_dir, "synthesis_barplot_yearly_by_country_rsshequity.pdf")
  
# -------------------------------------------------------------------
# read in and set up data
# -------------------------------------------------------------------
dt = as.data.table(read.csv(paste0(box, 'UGA/prepped_data/all_budget_revisions_yearlyLevel_uga_2021-01-13.csv')))

# subset to just budgets for active grants, excluding file_iteration = initial
dt = dt[file_iteration %in% c('approved_gm', 'revision')]

# fix problems with 'year' variable... 
check = unique(dt[,.(year, grant_period, grant)])

# subset to NFM2
dt = dt[grant_period %in% c('2018-2020')]

# # *****
# # subset out the problematic grants for now...
# dt = dt[!grant %in% c('COD-H-MOH')]
# dt = dt[!country %in% c('GTM')]
# # *****

# create simplified modules for HIV, TB, and malaria modules
dt[, gf_module := as.character(gf_module)]
dt[, simplified_mod := gf_module]
dt[grepl(gf_module, pattern = 'Comprehensive prevention'), simplified_mod := 'Prevention']
dt[grepl(gf_module, pattern = 'Comprehensive programs'), simplified_mod := 'Prevention']
dt[grepl(gf_module, pattern = 'Prevention'), simplified_mod := 'Prevention']
dt[grepl(gf_module, pattern = 'PMTCT'), simplified_mod := 'Prevention']
dt[grepl(gf_module, pattern = 'PMTCT'), simplified_mod := 'Prevention']
dt[grepl(gf_module, pattern = 'human rights'), simplified_mod := 'Human Rights']
dt[grepl(gf_module, pattern = 'Multidrug-resistant TB'), simplified_mod := 'MDR-TB']
dt[grepl(gf_module, pattern = "Specific prevention interventions"), simplified_mod := 'Specific prevention interventions']
dt[grepl(gf_module, pattern = "Condoms"), simplified_mod := 'Prevention']
# create simplified rssh modules
dt[grepl(gf_module, pattern = 'information system'), simplified_mod := 'HMIS and M&E']
dt[grepl(gf_module, pattern = 'Human resources'), simplified_mod := 'Human resources']
dt[grepl(gf_module, pattern = 'Integrated service delivery'), simplified_mod := 'Integr Service Del']
dt[grepl(gf_module, pattern = 'Financial management'), simplified_mod := 'Finance Manag Sys']
dt[grepl(gf_module, pattern = 'Community responses and systems'), simplified_mod := 'Comm Resp | Comm Sys Str'] # revised names
dt[grepl(gf_module, pattern = 'Community systems strengthening'), simplified_mod := 'Comm Resp | Comm Sys Str'] # revised names
dt[grepl(gf_module, pattern = 'Health sector governance and planning'), simplified_mod := 'Natl Strategies | Governance'] # revised names
dt[grepl(gf_module, pattern = 'National health strategies'), simplified_mod := 'Natl Strategies | Governance'] # revised names
dt[grepl(gf_module, pattern = 'Laboratory systems'), simplified_mod := 'Lab systems'] # new NFM3 module
dt[grepl(gf_module, pattern = 'Procurement'), simplified_mod := 'Procurement | Health products'] # revised names
dt[grepl(gf_module, pattern = 'Health products management systems'), simplified_mod := 'Procurement | Health products']
# -------------------------------------------------------------------

# # -----------------------------------------------------------------
# # Grant level figures:
# # -----------------------------------------------------------------
# # *** Since it's just UGA for now, add country
# dt[, country := 'UGA']
#
# # sum to yearly level, simplified module level
# dt_yearly = dt[, .(budget=sum(budget, na.rm=TRUE)), by=.(country, grant, grant_period, budget_version, module, year, file_name)]
# 
# # order budget versions
# dt_yearly[budget_version == 'Approved and Catalytic/Matching Funds', budget_version := 'approved_catalytic']
# dt_yearly[, budget_version := factor(budget_version, levels = c('approved', 'approved_catalytic', 'revision1',
#                                                                 'revision2', 'revision3'))]
# 
# # create a list of plots
# list_of_plots = NULL
# i=1
# for (g in unique(dt_yearly$grant)) {
#   # bar plots
#   list_of_plots[[i]] = ggplot(dt_yearly[grant == g, ], aes(x = budget_version, y = (budget/1000000), fill = module)) +
#     geom_bar(position = "stack", stat = "identity") +
#     facet_grid(~year) +
#     labs(title = paste0('Grant: ', g), x = 'Budget Version', y = 'Budget (Millions USD)', fill = 'Module') +
#     theme_bw()
#   
#   i=i+1
# }
# # print out the list of plots into a pdf
# pdf(outFile1, height = 8, width = 15)
# for(i in seq(length(list_of_plots))) { 
#   print(list_of_plots[[i]])
# } 
# dev.off()
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# set up for country level figures (incl rssh + equity)
# -------------------------------------------------------------------
# subset to just approved, approved + catalytic, and most recent revision
dt[budget_version == 'Approved and Catalytic/Matching Funds', budget_version := 'approved_catalytic']

dt2 = dt[ isMostRecentRevision == TRUE | budget_version %in% c('approved', 'approved_catalytic')]
dt2[ isMostRecentRevision == TRUE, budget_version := 'most_recent_revision']

# for grants that don't have approved + catalytic, we will copy over the approved amounts
# so make note of which grants don't have approved+catalytic
grants = unique(dt2$grant)
grants_with_catalytic = dt2[budget_version == 'approved_catalytic', unique(grant)]
grants_wo_catalytic = grants[!grants %in% grants_with_catalytic]

# sum to yearly level by grant -- # add loc_name when we have multiple countries added - we will use the same 
# for rssh/equity figures
dt_yearly = dt2[, .(budget=sum(budget, na.rm=TRUE)), by=.(grant, grant_period, budget_version, gf_module, simplified_mod, gf_intervention, rssh, equity, year)]

# for grants that don't have approved + catalytic, copy over the approved amounts
dt_wide = dcast.data.table(dt_yearly, grant + grant_period + gf_module + simplified_mod + gf_intervention + rssh + equity + year ~ budget_version, value.var = 'budget')
dt_wide[grant %in% grants_wo_catalytic, approved_catalytic := approved]

# sum by country -- # add loc_name when we have multiple countries added
ids = names(dt_wide)[!names(dt_wide) %in% c('approved', 'approved_catalytic', 'most_recent_revision')]
dt_long = melt.data.table(dt_wide, id.vars = ids, variable.name = 'budget_version', value.name = 'budget')
# temporary addition before we have all countries
dt_long[, loc_name := 'UGA']

# add a variable for displaying the budget version name in the figures
dt_long[budget_version == 'approved', figure_budget_version := 'NFM2 Award']
dt_long[budget_version == 'approved_catalytic', figure_budget_version := 'NFM2 Award+CMF']
dt_long[budget_version == 'most_recent_revision', figure_budget_version := 'NFM2 Revision']

# change colors for the figure
# colorblindness-friendly colors 
colors = c('#D55E00', '#C378A2', '#56B4E9', '#009E73', '#0072B2', '#565656', '#DEAE43', '#F0E442') #*Add more
colors = c(colors, colors, colors)
names(colors) = levels(dt_country$simplified_mod)
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Country-level figures:
# -------------------------------------------------------------------
dt_country = dt_long[, .(budget=sum(budget, na.rm=TRUE)), by=.(loc_name, figure_budget_version, simplified_mod, year)]

# take out 0 budget so that 2021 doesn't show up blank in the figure
dt_country = dt_country[budget!=0,]

# create a list of plots
list_of_plots = NULL
i=1
for (country in unique(dt_country$loc_name)) {
  # bar plots
  list_of_plots[[i]] = ggplot(dt_country[loc_name == country, ], aes(x = figure_budget_version, y = (budget/1000000), fill = simplified_mod)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(~year) +
    scale_fill_manual(name = 'Module', values = colors) +
    labs(title = paste0('Country: ', country), x = 'Budget Version', y = 'Budget (Millions USD)') +
    theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1))
  
  i=i+1
}
# print out the list of plots into a pdf
pdf(outFile2, height = 9, width = 12)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 
dev.off()
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# RSSH and HRG Equity Country-level figures:
# -------------------------------------------------------------------
# subset to HRG and equity interventions
dt_rssh = dt_long[rssh==TRUE, .(budget=sum(budget, na.rm=TRUE)), by=.(loc_name, figure_budget_version, simplified_mod, year)]
dt_equity = dt_long[equity==TRUE, .(budget=sum(budget, na.rm=TRUE)), by=.(loc_name, figure_budget_version, simplified_mod, year)]

# take out 0 budget so that 2021 doesn't show up blank in the figure
dt_rssh = dt_rssh[budget!=0,]
dt_equity = dt_equity[budget!=0,]

# create a list of plots
list_of_plots = NULL
i=1

for (country in unique(dt_rssh$loc_name)) {
  # bar plots
  list_of_plots[[i]] = ggplot(dt_rssh[loc_name == country, ], aes(x = figure_budget_version, y = (budget/1000000), fill = simplified_mod)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(~year) +
    scale_fill_manual(name = 'Module', values = colors) +
    labs(title = paste0('Country: ', country, '; RSSH'), x = 'Budget Version', y = 'Budget (Millions USD)') +
    theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1))
  
  i=i+1
}
for (country in unique(dt_equity$loc_name)) {
  # bar plots
  list_of_plots[[i]] = ggplot(dt_equity[loc_name == country, ], aes(x = figure_budget_version, y = (budget/1000000), fill = simplified_mod)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(~year) +
    scale_fill_manual(name = 'Module', values = colors) +
    labs(title = paste0('Country: ', country, '; HRG/equity'), x = 'Budget Version', y = 'Budget (Millions USD)') +
    theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1))
  
  i=i+1
}

# print out the list of plots into a pdf
pdf(outFile3, height = 9, width = 12)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 
dev.off()

# -------------------------------------------------------------------
