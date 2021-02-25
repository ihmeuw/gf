# -------------------------------------------------------------------
# Audrey Batzel
# Synthesis 2020 Data
# Compare absolute budget changes (module level) across budget versions looking at yearly budgeted amounts

# To do - Matt said something about including/visualizing the relative changes 
#       - figure out what to do with GTM years? 
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
library(janitor)
# -------------------------------------------------------------------
# files and directories
# -------------------------------------------------------------------
# input file
user = as.character(Sys.info()[7])
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/")

# output file
out_dir = 'J:/Project/Evaluation/GF/resource_tracking/visualizations2020/Synthesis/'
outFile1 = paste0(out_dir, "synthesis_barplot_yearly_by_module_and_grant_check.pdf")
outFile2 = paste0(out_dir, "synthesis_barplot_yearly_by_module_and_country2.pdf")
outFile3 = paste0(out_dir, "synthesis_barplot_yearly_by_country_rsshequity2.pdf")

# -------------------------------------------------------------------
# read in and set up data
# -------------------------------------------------------------------
dt = as.data.table(read.csv(paste0(box, "tableau_data/all_budget_revisions_yearlyLevel.csv")))

# subset to just budgets for active grants, excluding file_iteration = initial
dt = dt[file_iteration %in% c('approved_gm', 'revision')]

# # fix problems with 'year' variable... 
# check = unique(dt[,.(year, grant_period, grant)])
# fix years in the COD-H-MOH grant
dt[grant == 'COD-H-MOH' & year > 2021, year := (year - 10)]

# only keep GTM H-INCAP grant
dt = dt[loc_name != 'Guatemala' | grant == 'GTM-H-INCAP']
# revision 3 starts at Q1 2018, but that data is really Q4 2018, and so on: Q2 2018 here is actually Q4 2019
gtm_quarter_fix = unique(dt[loc_name == 'Guatemala' & budget_version == 'approved', .(year, quarter)])
gtm_quarter_fix2 = unique(dt[loc_name == 'Guatemala' & budget_version == 'revision3', .(loc_name, budget_version, year, quarter)])
setnames(gtm_quarter_fix, 'year', 'year_fix')
setnames(gtm_quarter_fix, 'quarter', 'qtr_fix')
gtm_quarter_fix = cbind(gtm_quarter_fix, gtm_quarter_fix2)

dt = merge(dt, gtm_quarter_fix, by = c('loc_name', 'budget_version', 'year', 'quarter'), all.x = TRUE)
dt[loc_name == 'Guatemala' & budget_version == 'revision3', year := year_fix]
dt[loc_name == 'Guatemala' & budget_version == 'revision3', quarter := qtr_fix]
dt[, c('year_fix', 'qtr_fix'):= NULL]

# # standardize GTM data to the yearly data in other countries
# gtm_quarters = unique(dt[loc_name == 'Guatemala', .(loc_name, year, quarter)])
# setorderv(gtm_quarters, c('year', 'quarter'))
# gtm_quarters[, quarter_ext := .I]
# gtm_quarters[quarter_ext %in% 1:4, gtm_year := 'Year1']
# gtm_quarters[quarter_ext %in% 5:8, gtm_year := 'Year2']
# gtm_quarters[quarter_ext == 9, gtm_year := 'Year3']
# 
# dt = merge(dt, gtm_quarters, by = c('loc_name','year','quarter'), all.x = TRUE)

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

# fix guatemala years to show the comparable time periods:
dt2[, year := as.character(year)]
#dt2[loc_name == 'Guatemala', year:=gtm_year]
dt2 = dt2[!is.na(year), ]
dt2$year = factor(dt2$year, levels = c('2018', '2019', '2020', '2021', 'Year1', 'Year2', 'Year3'))

# sum to yearly level by grant 
# for rssh/equity figures
dt_yearly = dt2[, .(budget=sum(budget, na.rm=TRUE)), by=.(loc_name, grant, grant_period, budget_version, gf_module, simplified_mod, gf_intervention, cost_category, rssh, equity, year)]

# for grants that don't have approved + catalytic, copy over the approved amounts
dt_wide = dcast.data.table(dt_yearly, loc_name + grant + grant_period + gf_module + simplified_mod + gf_intervention + cost_category + rssh + equity + year ~ budget_version, value.var = 'budget')
dt_wide[grant %in% grants_wo_catalytic, approved_catalytic := approved]

# sum by country -- # add loc_name when we have multiple countries added
ids = names(dt_wide)[!names(dt_wide) %in% c('approved', 'approved_catalytic', 'most_recent_revision')]
dt_long = melt.data.table(dt_wide, id.vars = ids, variable.name = 'budget_version', value.name = 'budget')

# add a variable for displaying the budget version name in the figures
dt_long[budget_version == 'approved', figure_budget_version := 'NFM2 Award']
dt_long[budget_version == 'approved_catalytic', figure_budget_version := 'NFM2 Award+CMF']
dt_long[budget_version == 'most_recent_revision', figure_budget_version := 'NFM2 Revision']

# change colors for the figure
# colorblindness-friendly colors 
colors = c('#D55E00', '#C378A2', '#56B4E9', '#009E73', '#0072B2', '#565656', '#DEAE43', '#F0E442') #*Add more
colors = c(colors, colors, colors)
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Country-level figures:
# -------------------------------------------------------------------
dt_country = dt_long[, .(budget=sum(budget, na.rm=TRUE)), by=.(loc_name, figure_budget_version, simplified_mod, year)]
# take out 0 budget so that 2021 doesn't show up blank in the figure
dt_country = dt_country[budget!=0,]

dt_country_cc = dt_long[, .(budget=sum(budget, na.rm=TRUE)), by=.(loc_name, figure_budget_version, cost_category, year)]
# take out 0 budget so that 2021 doesn't show up blank in the figure
dt_country_cc = dt_country_cc[budget!=0,]
# simplify cost category
dt_country_cc[, cost_category := as.character(cost_category)]
dt_country_cc[, simplified_cost_category := unlist(lapply(cost_category, function (x) {unlist(strsplit(x, ".", fixed=TRUE))[1] }))]

dt_country_cc = dt_country_cc[, .(budget=sum(budget, na.rm=TRUE)), by=.(loc_name, figure_budget_version, simplified_cost_category, year)]
dt_country_cc[, simplified_cost_category := as.integer(simplified_cost_category)]

# read in the spreadsheet I made to merge on cost grouping names
ccs = as.data.table(read.csv(paste0('J:/Project/Evaluation/GF/resource_tracking/documentation/Global_Fund_cost_groupings.csv')))
dt_country_cc = merge(dt_country_cc, ccs[, .(cost_category_number, cost_category_label)], by.x = 'simplified_cost_category', by.y = 'cost_category_number')

# colors for cost groupings:
colors_set1 = brewer.pal(8, 'Set1')
colors_set2 = brewer.pal(8, 'Set2')
colors2 = c(colors_set1, colors_set2)
names(colors2) = levels(dt_country_cc$cost_category_label)

# create a list of plots
list_of_plots = NULL
i=1
for (country in unique(dt_country$loc_name)) {
  # bar plots
  list_of_plots[[i]] = ggplot(dt_country[loc_name == country & figure_budget_version!="NFM2 Award", ], aes(x = year, y = (budget/1000000), fill = simplified_mod)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(~figure_budget_version) +
    scale_fill_manual(name = 'Module', values = rev(colorRampPalette(brewer.pal(8, "Dark2"))(19))) +
    labs(title = paste0(country, '; entire 2018-2020 portfolio by Module'), x = 'Budget Version', y = 'Budget (Millions USD)') +
    theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 24))
    i=i+1
    
    list_of_plots[[i]] = ggplot(dt_country_cc[loc_name == country & figure_budget_version!="NFM2 Award", ], aes(x = year, y = (budget/1000000), fill = cost_category_label)) +
      geom_bar(position = "stack", stat = "identity") +
      facet_grid(~figure_budget_version) +
      scale_fill_manual(name = 'Cost Category', values = colorRampPalette(brewer.pal(8, "Dark2"))(13)) +
      labs(title = paste0(country, '; entire 2018-2020 portfolio by Cost Grouping'), x = 'Budget Version', y = 'Budget (Millions USD)') +
      theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 24))
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
  list_of_plots[[i]] = ggplot(dt_rssh[loc_name == country & figure_budget_version!="NFM2 Award", ], aes(x = year, y = (budget/1000000), fill = simplified_mod)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(~figure_budget_version) +
    scale_fill_manual(name = 'Module', values = colors) +
    labs(title = paste0(country,'; RSSH'), x = 'Budget Version', y = 'Budget (Millions USD)') +
    theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 24))
  
  i=i+1
}
for (country in unique(dt_equity$loc_name)) {
  # bar plots
  list_of_plots[[i]] = ggplot(dt_equity[loc_name == country & figure_budget_version!="NFM2 Award", ], aes(x = year, y = (budget/1000000), fill = simplified_mod)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(~figure_budget_version) +
    scale_fill_manual(name = 'Module', values = colors) +
    labs(title = paste0(country, '; HRG/equity'), x = 'Budget Version', y = 'Budget (Millions USD)') +
    theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 24))
  
  i=i+1
}

# print out the list of plots into a pdf
pdf(outFile3, height = 9, width = 12)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 
dev.off()
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# group figures by country
# -------------------------------------------------------------------
for (country in unique(dt_country$loc_name)) {

  outFile_country = paste0(out_dir, "synthesis_barplot_yearly_", country, ".pdf")

  if (country == 'Guatemala'){
    if( 'Year1' %in% dt_country[loc_name == 'Guatemala', unique(year)] ){
      gtm_caption = 'Note: Year 1 includes 2018 Q4 - 2019 Q3, \nYear 2 includes 2019 Q4 - 2020 Q3, and Year 3 is 2020 Q4.'
      outFile_country = paste0(out_dir, "synthesis_barplot_yearly_", country, "_grantYear.pdf")
    } else{
      gtm_caption = 'Note: 2018 includes just the last quarter of 2018.'
      outFile_country = paste0(out_dir, "synthesis_barplot_yearly_", country, "_calendarYear.pdf")
    }}
  
  pdf(outFile_country, height = 9, width = 12)
    g1 = ggplot(dt_country[loc_name == country & figure_budget_version!="NFM2 Award", ], aes(x = year, y = (budget/1000000), fill = simplified_mod)) +
            geom_bar(position = "stack", stat = "identity") +
            facet_grid(~figure_budget_version) +
            scale_fill_manual(name = 'Module', values = rev(colorRampPalette(brewer.pal(8, "Dark2"))(19))) +
            labs(title = paste0(country, '; annual budget across grants by module'), x = 'Budget Version', y = 'Budget (Millions USD)') +
            theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 24))
    if(country == 'Guatemala') g1 = g1 + labs(title = paste0('GTM-H-INCAP; annual budget by module'), subtitle = gtm_caption)
    print(g1)
    
    g2 = ggplot(dt_country_cc[loc_name == country & figure_budget_version!="NFM2 Award", ], aes(x = year, y = (budget/1000000), fill = cost_category_label)) +
      geom_bar(position = "stack", stat = "identity") +
      facet_grid(~figure_budget_version) +
      scale_fill_manual(name = 'Cost Category', values = colorRampPalette(brewer.pal(8, "Dark2"))(13)) +
      labs(title = paste0(country, '; annual budget across grants by cost grouping'), x = 'Budget Version', y = 'Budget (Millions USD)') +
      theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 24))
    if(country == 'Guatemala') g2 = g2 + labs(title = paste0('GTM-H-INCAP; annual budget by cost grouping'), subtitle = gtm_caption)
    print(g2)
    
    g3 = ggplot(dt_rssh[loc_name == country & figure_budget_version!="NFM2 Award", ], aes(x = year, y = (budget/1000000), fill = simplified_mod)) +
      geom_bar(position = "stack", stat = "identity") +
      facet_grid(~figure_budget_version) +
      scale_fill_manual(name = 'Module', values = colors) +
      labs(title = paste0(country,'; annual budget for RSSH interventions across grants by module'), x = 'Budget Version', y = 'Budget (Millions USD)') +
      theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 24))
    if(country == 'Guatemala') g3 = g3 + labs(title = paste0('GTM-H-INCAP; annual budget for RSSH interventions across \ngrants by module'), subtitle = gtm_caption)
    print(g3)
    
    g4 = ggplot(dt_equity[loc_name == country & figure_budget_version!="NFM2 Award", ], aes(x = year, y = (budget/1000000), fill = simplified_mod)) +
      geom_bar(position = "stack", stat = "identity") +
      facet_grid(~figure_budget_version) +
      scale_fill_manual(name = 'Module', values = colors) +
      labs(title = paste0(country, '; annual budget for HRG/equity interventions across grants by module'), x = 'Budget Version', y = 'Budget (Millions USD)') +
      theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 24))
    if(country == 'Guatemala') g4 = g4 + labs(title = paste0('GTM-H-INCAP; annual budget for HRG/equity interventions \nacross grants by module'), subtitle = gtm_caption)
    print(g4)
    
  dev.off()
}
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Make and save data tables for each country/figure
# -------------------------------------------------------------------
for (country in unique(dt_country$loc_name)) {
  
  table = dt_country[loc_name == country & figure_budget_version %in% c("NFM2 Award+CMF", "NFM2 Revision"), ]
  table[, budget := round(budget)]
  table = data.table(dcast(table, loc_name + simplified_mod ~ figure_budget_version + year, value.var = "budget", fun.aggregate = sum ))
  table = table %>% adorn_totals('row')
  setnames(table, 'loc_name', 'Country')
  setnames(table, 'simplified_mod', 'GF Module')
  outrTable = paste0(out_dir, 'tables_for_annual_budget_comparisons/', country, '_table_modules_year.csv')
  write.csv(table, outrTable, row.names = FALSE)
  
  percent_table = dt_country[loc_name == country & figure_budget_version %in% c("NFM2 Award+CMF", "NFM2 Revision"), ]
  percent_table[, budget := round(budget)]
  # cast wide just the budget version to be able to calculate percentage difference (among years too)
  percent_table = data.table(dcast(percent_table, loc_name + simplified_mod + year ~ figure_budget_version, value.var = "budget", fun.aggregate = sum ))
  # calculate percentage change
  percent_table[, percent_change := round(((`NFM2 Revision`-`NFM2 Award+CMF`)/`NFM2 Award+CMF`)*100)]
  percent_table = percent_table[,.(loc_name, simplified_mod, year, percent_change)]
  # cast the year wide now:
  percent_table = data.table(dcast(percent_table, loc_name + simplified_mod ~ year, value.var = "percent_change"))
  setnames(percent_table, 'loc_name', 'Country')
  setnames(percent_table, 'simplified_mod', 'GF Module')
  outrTable = paste0(out_dir, 'tables_for_annual_budget_comparisons/', country, '_table_modules_year_percentChange.csv')
  write.csv(percent_table, outrTable, row.names = FALSE)

  #--------------------------------  
  table = dt_country_cc[loc_name == country & figure_budget_version %in% c("NFM2 Award+CMF", "NFM2 Revision"), ]
  table[, simplified_cost_category := NULL]
  table[, budget := round(budget)]
  table = data.table(dcast(table, loc_name + cost_category_label ~ figure_budget_version + year, value.var = "budget", fun.aggregate = sum ))
  table = table %>% adorn_totals('row')
  setnames(table, 'loc_name', 'Country')
  setnames(table, 'cost_category_label', 'Cost Grouping')
  outrTable = paste0(out_dir, 'tables_for_annual_budget_comparisons/', country, '_table_costGroupings_year.csv')
  write.csv(table, outrTable, row.names = FALSE)
  
  percent_table = dt_country_cc[loc_name == country & figure_budget_version %in% c("NFM2 Award+CMF", "NFM2 Revision"), ]
  percent_table[, simplified_cost_category := NULL]
  percent_table[, budget := round(budget)]
  # cast wide just the budget version to be able to calculate percentage difference (among years too)
  percent_table = data.table(dcast(percent_table, loc_name + cost_category_label + year ~ figure_budget_version, value.var = "budget", fun.aggregate = sum ))
  # calculate percentage change
  percent_table[, percent_change := round(((`NFM2 Revision`-`NFM2 Award+CMF`)/`NFM2 Award+CMF`)*100)]
  percent_table = percent_table[,.(loc_name, cost_category_label, year, percent_change)]
  # cast the year wide now:
  percent_table = data.table(dcast(percent_table, loc_name + cost_category_label ~ year, value.var = "percent_change"))
  setnames(percent_table, 'loc_name', 'Country')
  setnames(percent_table, 'cost_category_label', 'Cost Grouping')
  outrTable = paste0(out_dir, 'tables_for_annual_budget_comparisons/', country, '_table_costGroupings_year_percentChange.csv')
  write.csv(percent_table, outrTable, row.names = FALSE)
  
  #--------------------------------  
  table = dt_rssh[loc_name == country & figure_budget_version %in% c("NFM2 Award+CMF", "NFM2 Revision"), ]
  table[, budget := round(budget)]
  table = data.table(dcast(table, loc_name + simplified_mod ~ figure_budget_version + year, value.var = "budget", fun.aggregate = sum ))
  table = table %>% adorn_totals('row')
  setnames(table, 'loc_name', 'Country')
  setnames(table, 'simplified_mod', 'GF Module')
  outrTable = paste0(out_dir, 'tables_for_annual_budget_comparisons/', country, '_table_rssh_modules_year.csv')
  write.csv(table, outrTable, row.names = FALSE)
  
  percent_table = dt_rssh[loc_name == country & figure_budget_version %in% c("NFM2 Award+CMF", "NFM2 Revision"), ]
  percent_table[, budget := round(budget)]
  # cast wide just the budget version to be able to calculate percentage difference (among years too)
  percent_table = data.table(dcast(percent_table, loc_name + simplified_mod + year ~ figure_budget_version, value.var = "budget", fun.aggregate = sum ))
  # calculate percentage change
  percent_table[, percent_change := round(((`NFM2 Revision`-`NFM2 Award+CMF`)/`NFM2 Award+CMF`)*100)]
  percent_table = percent_table[,.(loc_name, simplified_mod, year, percent_change)]
  # cast the year wide now:
  percent_table = data.table(dcast(percent_table, loc_name + simplified_mod ~ year, value.var = "percent_change"))
  setnames(percent_table, 'loc_name', 'Country')
  setnames(percent_table, 'simplified_mod', 'GF Module')
  outrTable = paste0(out_dir, 'tables_for_annual_budget_comparisons/', country, '_table_rssh_modules_year_percentChange.csv')
  write.csv(percent_table, outrTable, row.names = FALSE)
  
  #--------------------------------  
  table = dt_equity[loc_name == country & figure_budget_version %in% c("NFM2 Award+CMF", "NFM2 Revision"), ]
  table[, budget := round(budget)]
  table = data.table(dcast(table, loc_name + simplified_mod ~ figure_budget_version + year, value.var = "budget", fun.aggregate = sum ))
  table = table %>% adorn_totals('row')
  setnames(table, 'loc_name', 'Country')
  setnames(table, 'simplified_mod', 'GF Module')
  outrTable = paste0(out_dir, 'tables_for_annual_budget_comparisons/', country, '_table_hrg_modules_year.csv')
  write.csv(table, outrTable, row.names = FALSE)  
  
  percent_table = dt_equity[loc_name == country & figure_budget_version %in% c("NFM2 Award+CMF", "NFM2 Revision"), ]
  percent_table[, budget := round(budget)]
  # cast wide just the budget version to be able to calculate percentage difference (among years too)
  percent_table = data.table(dcast(percent_table, loc_name + simplified_mod + year ~ figure_budget_version, value.var = "budget", fun.aggregate = sum ))
  # calculate percentage change
  percent_table[, percent_change := round(((`NFM2 Revision`-`NFM2 Award+CMF`)/`NFM2 Award+CMF`)*100)]
  percent_table = percent_table[,.(loc_name, simplified_mod, year, percent_change)]
  # cast the year wide now:
  percent_table = data.table(dcast(percent_table, loc_name + simplified_mod ~ year, value.var = "percent_change"))
  setnames(percent_table, 'loc_name', 'Country')
  setnames(percent_table, 'simplified_mod', 'GF Module')
  outrTable = paste0(out_dir, 'tables_for_annual_budget_comparisons/', country, '_table_hrg_modules_year_percentChange.csv')
  write.csv(percent_table, outrTable, row.names = FALSE)
  
  #--------------------------------  
}
# -------------------------------------------------------------------
