#########################################################
# Synthesis 2020 Data
# Visualize RSSH data as a heatmap showing breakdown of % of total RSSH spending by module and country
# Audrey Batzel

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
library(tidyr)
# -------------------------------------------------------------------
# Files and directories
setwd('C:/local/gf/')

user=as.character(Sys.info()[7])

# input file
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/")
inFile = paste0(box, 'synthesis/data/draft_synthesis_budget_quant.xlsx')
ehgFile = paste0(box, 'synthesis/data/Synthesis Budget Variance 181120.xlsx')
inFile_nfm3 = paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv')

# output file
#outFile = "J:/Project/Evaluation/GF/resource_tracking/visualizations2020/Synthesis/synthesis_ch3_rssh_heatmap.pdf"
out_dir = "J:/Project/Evaluation/GF/resource_tracking/visualizations2021/heatmaps_rssh_indicators/"
# -------------------------------------------------------------------
# read IHME and EGH rssh data
rssh_data = as.data.table(read_xlsx(path = inFile, sheet = "RSSH"))
ehg_rssh_data = as.data.table(read_xlsx(path = ehgFile, sheet = "RSSH"))

# subset to necessary columns and rows
rssh_data = rssh_data[,.(loc_name, gf_module, nfm2_funding_request17, nfm2_approved,nfm2_approved_catalytic_funds,
                          nfm2_most_recent_revision, nfm3_funding_request20,nfm3_approved)]
ehg_rssh_data = ehg_rssh_data[loc_name %in% c('Myanmar', 'Cambodia', 'Mozambique', 'Sudan')
                               ,.(loc_name, gf_module, nfm2_funding_request17, nfm2_approved, nfm2_most_recent_revision, nfm3_funding_request20)]

# bind data together into cross consortia sheet
cc_rssh_data = rbind(rssh_data, ehg_rssh_data, fill=TRUE)

# -------------------------------------------------------------------
# reshape long for plotting
plot_rssh = melt(cc_rssh_data, id.vars = c("loc_name","gf_module"),
                  measure.vars = c("nfm2_funding_request17","nfm2_approved","nfm2_approved_catalytic_funds",
                                   "nfm2_most_recent_revision", "nfm3_funding_request20","nfm3_approved"),
                  variable.name = "version", value.name = "budget")

# sum across unnecessary variables
plot_rssh = plot_rssh[, .(budget=sum(budget, na.rm=T)), by=c('loc_name', 'gf_module', 'version')]

# -------------------------------------------------------------------
# subset to nfm3
dt = plot_rssh[ version == 'nfm3_funding_request20', ]

# rename duplicate modules
dt[gf_module == 'Health management information system and monitoring and evaluation', gf_module := 'Health management information systems and M&E']
dt[gf_module == 'Community responses and systems', gf_module := 'Community systems strengthening']
dt[gf_module == 'Procurement and supply chain management systems', gf_module := 'Health products management systems']

# calculate each module % of total
dt[, rssh_total_by_country := sum(budget), by = c('loc_name')]
dt[, module_percent_of_total_rssh := round((budget/rssh_total_by_country)*100)]
dt = dt[budget > 0]

# -------------------------------------------------------------------
# add in table of indicators for captions: 
indicators = as.data.table(read_xlsx('J:/Project/Evaluation/GF/resource_tracking/visualizations2020/Synthesis/table in ch3 on rssh indicators.xlsx'))

# reshape
indicators = melt.data.table(indicators, id.vars = c('Module', 'RSSH_indicator_NFM3'), variable.name = 'country', value.name = 'indicator_present' )
indicators = indicators[indicator_present == 1, ]

mod_country = unique(indicators[, .(Module, country)])
mod_country[, indicators := '']

# can probably make this into an lapply one-liner? 
for(i in 1:nrow(mod_country)){
  mod= mod_country[i, Module]
  c= mod_country[i, country]
  
  list_of_indicators = indicators[Module == mod & country == c, unique(RSSH_indicator_NFM3)]
  mod_country[Module == mod & country == c, indicators := paste(list_of_indicators, collapse = ',\n')]
}

# merge with dt of budgets
mod_country[, country := as.character(country)]
plot_dt = merge(dt, mod_country, by.x = c('loc_name', 'gf_module'), by.y = c('country', 'Module'), all = TRUE)
plot_dt[is.na(indicators), indicators := '']

# shorten column names - is there a better way to do this with data table?
plot_dt[gf_module == 'Health management information systems and M&E', plot_module := 'HMIS and M&E']
plot_dt[gf_module == 'Health products management systems', plot_module := 'Health products\nmanagement systems']
plot_dt[gf_module == 'Integrated service delivery and quality improvement', plot_module := 'Int. service delivery\nand QE']
plot_dt[gf_module == 'Community systems strengthening', plot_module := 'CSS']
plot_dt[gf_module == 'Health sector governance and planning', plot_module := 'Health sector gov.\nand planning']
plot_dt[gf_module == 'Human resources for health, including community health workers', plot_module := 'HRH, incl. CHWs']
plot_dt[gf_module == 'Laboratory systems', plot_module := 'Laboratory systems']
plot_dt[gf_module == 'Financial management systems', plot_module := 'Financial management\nsystems']

# order to match what is in the synthesis report
plot_dt[, plot_module := factor(plot_module, levels = c('HMIS and M&E', 
                                                    'Health products\nmanagement systems',
                                                    'Int. service delivery\nand QE',
                                                    'CSS',
                                                    'Health sector gov.\nand planning',
                                                    'HRH, incl. CHWs',
                                                    'Laboratory systems',
                                                    'Financial management\nsystems'))]
# # quick fix for cambodia hmis 
# plot_dt[is.na(module_percent_of_total_rssh), module_percent_of_total_rssh := 0]
# plot_dt[is.na(version), version := 'nfm3_funding_request20']

# -------------------------------------------------------------------
# plot into a heatmap
g = ggplot(plot_dt, aes(loc_name, plot_module, fill= module_percent_of_total_rssh)) + geom_tile() + theme_bw() + 
  scale_x_discrete(position = 'top') + labs( x = '', y = "", fill = "% of Country's Total \nRSSH Spending") + 
  scale_fill_continuous(high = "#132B43", low = "#9fd4fc") +
  scale_y_discrete(limits = rev(levels(plot_dt$plot_module))) +
  theme(axis.text=element_text(size=12), legend.text = element_text(size=11), legend.title = element_text(size = 13)) +
  theme(axis.text.x = element_text(angle = 45, hjust = -0.01)) +
  geom_text(aes(label= paste0(plot_dt$module_percent_of_total_rssh, '%', '\n', plot_dt$indicators)), size = 3.5, color = '#FFFFFF')
  # scale_fill_continuous(type = 'viridis') 
  # 56B1F7 - alternate/close to default color 
  
pdf(outFile, height = 8, width = 11)
print(g)
dev.off()

pdf(paste0('C:/Users/', user, '/Box Sync/Global Fund Files/synthesis/figures/synthesis_ch3_rssh_heatmap.pdf'), height = 8, width = 11)
print(g)
dev.off()

##########################################################################################

# -------------------------------------------------------------------
# read in NFM3 approved data
data = as.data.table(read.csv(inFile_nfm3))
data = data[grant_period == '2021-2023' & budget_version == 'approved' & rssh == TRUE, .(budget = sum(budget, na.rm=TRUE)), 
            by = .(loc_name, gf_module, grant)]
data[, rssh_total_by_grant := sum(budget), by = c('grant')]
data[, module_percent_of_total_rssh := round((budget/rssh_total_by_grant)*100)]
data = data[budget > 0]
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# # add in table of indicators for captions: 
# indicators = as.data.table(read_xlsx('J:/Project/Evaluation/GF/resource_tracking/visualizations2020/Synthesis/table in ch3 on rssh indicators - nfm3.xlsx'))
# 
# # make this data so it makes more sense...
# indicators = melt.data.table(indicators, id.vars = c('Module', 'RSSH_indicator_NFM3'), variable.name = 'grant', value.name = 'indicator_present' )
# indicators = indicators[indicator_present == 1, ] 
# # save this version...
# write.csv(indicators, 'J:/Project/Evaluation/GF/resource_tracking/visualizations2021/Synthesis/table in ch3 on rssh indicators_nfm3_long.csv', row.names = FALSE)

# add in table of indicators for captions: 
indicators = as.data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/Synthesis/table in ch3 on rssh indicators_nfm3_long.csv'))

mod_grant = unique(indicators[, .(Module, grant)])
mod_grant[, indicators := '']

# can probably make this into an lapply one-liner? 
for(i in 1:nrow(mod_grant)){
  mod= mod_grant[i, Module]
  c= mod_grant[i, grant]
  
  list_of_indicators = indicators[Module == mod & grant == c, unique(RSSH_indicator_NFM3)]
  mod_grant[Module == mod & grant == c, indicators := paste(list_of_indicators, collapse = ',\n')]
}

# merge with budget data
mod_grant[, grant := as.character(grant)]
# # for now - check this later*******
# mod_grant[grant != 'COD-C-CORDAID', ]
mod_grant[grepl('COD', grant), loc_name := 'DRC']
mod_grant[grepl('UGA', grant), loc_name := 'Uganda']
mod_grant[grepl('SEN', grant), loc_name := 'Senegal']
mod_grant[grepl('GTM', grant), loc_name := 'Guatemala']

data[gf_module == 'Health management information system and monitoring and evaluation', gf_module := 'Health management information systems and M&E']
plot_dt = merge(data, mod_grant, by.x = c('loc_name', 'grant', 'gf_module'), by.y = c('loc_name','grant', 'Module'), all = TRUE)

# shorten column names - is there a better way to do this with data table?
plot_dt[gf_module == 'Health management information systems and M&E', plot_module := 'HMIS and M&E']
plot_dt[gf_module == 'Health products management systems', plot_module := 'Health products\nmanagement systems']
plot_dt[gf_module == 'Integrated service delivery and quality improvement', plot_module := 'Int. service delivery\nand QE']
plot_dt[gf_module == 'Community systems strengthening', plot_module := 'CSS']
plot_dt[gf_module == 'Health sector governance and planning', plot_module := 'Health sector gov.\nand planning']
plot_dt[gf_module == 'Human resources for health, including community health workers', plot_module := 'HRH, incl. CHWs']
plot_dt[gf_module == 'Laboratory systems', plot_module := 'Laboratory systems']
plot_dt[gf_module == 'Financial management systems', plot_module := 'Financial management\nsystems']

# order to match what is in the synthesis report
plot_dt[, plot_module := factor(plot_module, levels = c('HMIS and M&E', 
                                                        'Health products\nmanagement systems',
                                                        'Int. service delivery\nand QE',
                                                        'CSS',
                                                        'Health sector gov.\nand planning',
                                                        'HRH, incl. CHWs',
                                                        'Laboratory systems',
                                                        'Financial management\nsystems'))]

# -------------------------------------------------------------------
## make data table square
# grants = unique(plot_dt$grant)
# plot_modules = unique(plot_dt$plot_module)
# merge_dt = expand_grid(grants, plot_modules)
# 
# plot_dt = merge(plot_dt, merge_dt, by.x = c('grant', 'plot_module'), by.y = c('grants', 'plot_modules'), all = TRUE)

plot_dt[grepl('COD', grant), loc_name := 'DRC']
plot_dt[grepl('UGA', grant), loc_name := 'Uganda']
plot_dt[grepl('SEN', grant), loc_name := 'Senegal']
plot_dt[grepl('GTM', grant), loc_name := 'Guatemala']
plot_dt[is.na(indicators), indicators := '']

plot_dt[!is.na(module_percent_of_total_rssh), plot_label := paste0("$", (round(budget/1000000,1)),' million (', module_percent_of_total_rssh, '%)', '\n', indicators)]
plot_dt[is.na(module_percent_of_total_rssh), plot_label := paste0("$0 (0%)\n", indicators)]
plot_dt[budget < 1000000, plot_label := paste0("$", (round(budget/1000000,2)),' million (', module_percent_of_total_rssh, '%)', '\n', indicators)]

for (c in unique(plot_dt$loc_name)){
  graph_dt = plot_dt[loc_name == c, ]
  # plot into a heatmap
  g = ggplot(graph_dt, aes(grant, plot_module, fill= module_percent_of_total_rssh)) + geom_tile() + theme_classic() + 
    scale_x_discrete(position = 'top') + labs( x = '', y = "", fill = "% of grant's \nRSSH Funding") + 
    scale_fill_continuous(high = "#132B43", low = "#9fd4fc") +
    scale_y_discrete(limits = rev(levels(graph_dt$plot_module))) +
    theme(axis.text=element_text(size=12), legend.text = element_text(size=11), legend.title = element_text(size = 13)) +
    theme(axis.text.x = element_text(angle = 45, hjust = -0.01)) +
    geom_text(aes(label= graph_dt$plot_label), size = 3, color = '#FFFFFF')
  # scale_fill_continuous(type = 'viridis') 
  # 56B1F7 - alternate/close to default color 
  file = paste0(out_dir, 'rssh_indicator_heatmaps_by_country_grant_', c, '.png')
  png(file, height = 8, width = 11, units = "in", res = 300)
  print(g)
  dev.off()
}

pdf(paste0(out_dir, 'rssh_indicator_heatmaps_by_country_grant.pdf'), height = 8, width = 11)
for (c in unique(plot_dt$loc_name)){
  graph_dt = plot_dt[loc_name == c, ]
  # plot into a heatmap
  print(ggplot(graph_dt, aes(grant, plot_module, fill= module_percent_of_total_rssh)) + geom_tile() + 
         scale_x_discrete(position = 'top') + labs( x = '', y = "", fill = "% of grant's \nRSSH Funding") + 
         scale_fill_continuous(high = "#132B43", low = "#9fd4fc") +
         scale_y_discrete(limits = rev(levels(graph_dt$plot_module))) +
         theme(axis.text=element_text(size=12), legend.text = element_text(size=11), legend.title = element_text(size = 13)) +
         theme(axis.text.x = element_text(angle = 45, hjust = -0.01)) +
         geom_text(aes(label= graph_dt$plot_label), size = 3, color = '#FFFFFF') + theme_classic())
}
dev.off()

# pdf(paste0('C:/Users/', user, '/Box Sync/Global Fund Files/synthesis/figures/synthesis_ch3_rssh_heatmap.pdf'), height = 8, width = 11)
# print(g)
# dev.off()
# -------------------------------------------------------------------

##########################################################################################
# bar graph figures by country
# -------------------------------------------------------------------
# read in NFM3 approved data
data = as.data.table(read.csv(inFile_nfm3))
data = data[grant_period == '2021-2023' & budget_version %in% c('approved', 'funding_request20') & rssh == TRUE, .(budget = sum(budget, na.rm=TRUE)), 
            by = .(loc_name, gf_module, budget_version)]

# add in table of indicators for captions: 
indicators = as.data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/Synthesis/table in ch3 on rssh indicators_nfm3_long.csv'))
indicators_fr = as.data.table(read_xlsx('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/Synthesis/rssh_indicators_nfm3_fr_long.xlsx'))

indicators = unique(indicators[, .(module, rssh_indicator_nfm3, loc_name)])
indicators = indicators[ loc_name %in% c('DRC', 'Uganda', 'Senegal', 'Guatemala')]
indicators[, version:='approved']
indicators_fr[, version:='funding_request20']
indicators_fr[, in_fr := NULL]
indicators = rbindlist(list(indicators, indicators_fr))

mod_loc = unique(indicators[, .(module, loc_name, version)])
mod_loc[, indicators := '']

# can probably make this into an lapply one-liner? 
for(i in 1:nrow(mod_loc)){
  mod= mod_loc[i, module]
  c= mod_loc[i, loc_name]
  v = mod_loc[i, version]
  
  list_of_indicators = indicators[module == mod & loc_name == c & version == v, unique(rssh_indicator_nfm3)]
  mod_loc[module == mod & loc_name == c & version == v, indicators := paste(list_of_indicators, collapse = ',\n')]
}

num_indicators = indicators[!rssh_indicator_nfm3 %in% c('WPTM(s)'), .(num_inds = .N), by = .(module, loc_name, version)]
mod_loc = merge(num_indicators, mod_loc, all = TRUE)
mod_loc[, inds_lab := as.character(num_inds)]
mod_loc[grepl('Custom', indicators), inds_lab := paste0(inds_lab, '*')]
mod_loc[grepl('WPTM', indicators) & !is.na(inds_lab), inds_lab := paste0(inds_lab, '^')]
mod_loc[grepl('WPTM', indicators) & is.na(inds_lab), inds_lab := paste0('0^')]

# -------------------------------------------------------------------
# merge indicators with budget data
data[gf_module == 'Health management information system and monitoring and evaluation', gf_module := 'Health management information systems and M&E']
plot_dt = merge(data, mod_loc, by.x = c('loc_name', 'gf_module', 'budget_version'), by.y = c('loc_name', 'module', 'version'), all = TRUE)

# shorten column names - is there a better way to do this with data table?
plot_dt[gf_module == 'Health management information systems and M&E', plot_module := 'HMIS and M&E']
plot_dt[gf_module == 'Health products management systems', plot_module := 'Health products\nmanagement systems']
plot_dt[gf_module == 'Integrated service delivery and quality improvement', plot_module := 'Int. service delivery\nand QE']
plot_dt[gf_module == 'Community systems strengthening', plot_module := 'CSS']
plot_dt[gf_module == 'Health sector governance and planning', plot_module := 'Health sector gov.\nand planning']
plot_dt[gf_module == 'Human resources for health, including community health workers', plot_module := 'HRH, incl. CHWs']
plot_dt[gf_module == 'Laboratory systems', plot_module := 'Laboratory systems']
plot_dt[gf_module == 'Financial management systems', plot_module := 'Financial management\nsystems']

# -------------------------------------------------------------------
# make figures
plot_dt[is.na(indicators), indicators := '']
plot_dt[is.na(inds_lab), inds_lab := '']
plot_dt[, plot_label := inds_lab]

# order to match what is in the synthesis report
plot_dt[, plot_module := factor(plot_module, levels = c('HMIS and M&E', 
                                                        'Health products\nmanagement systems',
                                                        'Int. service delivery\nand QE',
                                                        'CSS',
                                                        'Health sector gov.\nand planning',
                                                        'HRH, incl. CHWs',
                                                        'Laboratory systems',
                                                        'Financial management\nsystems'))]

plot_dt[budget_version == 'approved', budget_version := 'Approved Budget']
plot_dt[budget_version == 'funding_request20', budget_version := 'Funding Request']

plot_dt[, budget_version := factor(budget_version, levels = c('Funding Request', 
                                                        'Approved Budget'))]

# plot into a bar graph by country
for (c in unique(plot_dt$loc_name)){
  graph_dt = plot_dt[loc_name == c, ]
  
  g = ggplot(graph_dt, aes(x=plot_module, y=round(budget/1000000), fill=plot_module)) + 
    geom_bar(stat="identity") + theme_bw() + 
    facet_wrap(~budget_version) + 
    labs(title = paste0(c, ': RSSH indicators and allocations by module, comparing funding request to \napproved budgets/PFs'), x = "Module", y = "Budget (Millions USD)", caption = "* = Custom coverage indicators included in the total, ^ = Module also had WPTM(s), not included in total", fill = "Module") +
    theme(legend.position = 'none') +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
    theme(text=element_text(size=14)) +
    geom_text(aes(label=plot_label), size = 6, vjust = -0.3)

  if( c == 'DRC'){
    # order to match what is in the synthesis report
    graph_dt[, plot_module := factor(plot_module, levels = c('HMIS and M&E', 
                                                            'HRH, incl. CHWs',
                                                            'Int. service delivery\nand QE',
                                                            'CSS',
                                                            'Health products\nmanagement systems',
                                                            'Health sector gov.\nand planning',
                                                            'Laboratory systems',
                                                            'Financial management\nsystems'))]
    
    g = ggplot(graph_dt[budget_version == 'Approved Budget'], aes(x=plot_module, y=round(budget/1000000), fill=plot_module)) + 
      geom_bar(stat="identity") + theme_bw() + 
      labs(title = paste0(c, ': RSSH indicators and allocations by module, comparing funding request to \napproved budgets/PFs'), x = "Module", y = "Budget (Millions USD)", caption = "* = Custom coverage indicators included in the total, ^ = Module also had WPTM(s), not included in total", fill = "Module") +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
      theme(text=element_text(size=14)) +
      geom_text(aes(label=plot_label), size = 6, vjust = -0.3)
    
  }
  
  file = paste0('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/bargraphs_rssh_indicators/bargraphs_rssh_indicators_', c, '.png')
  png(file, height = 8, width = 11, units = "in", res = 300)
  print(g)
  dev.off()
}

##########################################################################################
# 2021 synthesis level figure

# -------------------------------------------------------------------
# read in NFM3 approved data -IHME
data_ihme = as.data.table(read.csv(inFile_nfm3))
data_ihme = data_ihme[grant_period == '2021-2023' & budget_version == 'approved' & rssh == TRUE, .(budget = sum(budget, na.rm=TRUE)), 
            by = .(loc_name, gf_module)]

# read in data - EHG
data_ehg = as.data.table(read_excel('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/Synthesis/ehg_nfm3_rssh_budget_data.xlsx'))
setnames(data_ehg, 'module', 'gf_module')
data_ehg = data_ehg[, .(budget = sum(budget, na.rm=TRUE)), 
            by = .(loc_name, gf_module)]

data = rbind(data_ihme, data_ehg)

data[, rssh_total_by_loc := sum(budget), by = c('loc_name')]
data[, module_percent_of_total_rssh := round((budget/rssh_total_by_loc)*100, 1)]
data = data[budget > 0]

# -------------------------------------------------------------------
# add in table of indicators for captions: 
indicators = as.data.table(read.csv('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/Synthesis/table in ch3 on rssh indicators_nfm3_long.csv'))
indicators = unique(indicators[,.(module, rssh_indicator_nfm3, loc_name)])

mod_loc = unique(indicators[, .(module, loc_name)])
mod_loc[, indicators := '']

# can probably make this into an lapply one-liner? 
for(i in 1:nrow(mod_loc)){
  mod= mod_loc[i, module]
  c= mod_loc[i, loc_name]
  
  list_of_indicators = indicators[module == mod & loc_name == c, unique(rssh_indicator_nfm3)]
  mod_loc[module == mod & loc_name == c, indicators := paste(list_of_indicators, collapse = ',\n')]
}

num_indicators = indicators[!rssh_indicator_nfm3 %in% c('WPTM(s)'), .(num_inds = .N), by = .(module, loc_name)]
mod_loc = merge(num_indicators, mod_loc, all = TRUE)
mod_loc[, inds_lab := as.character(num_inds)]
mod_loc[grepl('Custom', indicators), inds_lab := paste0(inds_lab, '*')]
mod_loc[grepl('WPTM', indicators) & !is.na(inds_lab), inds_lab := paste0(inds_lab, '\n+WPTM(s)')]
mod_loc[grepl('WPTM', indicators) & is.na(inds_lab), inds_lab := paste0('WPTM(s)')]

# -------------------------------------------------------------------
# merge indicators with budget data
data[gf_module == 'Health management information system and monitoring and evaluation', gf_module := 'Health management information systems and M&E']
plot_dt = merge(data, mod_loc, by.x = c('loc_name', 'gf_module'), by.y = c('loc_name', 'module'), all = TRUE)

# shorten column names - is there a better way to do this with data table?
plot_dt[gf_module == 'Health management information systems and M&E', plot_module := 'HMIS and M&E']
plot_dt[gf_module == 'Health products management systems', plot_module := 'Health products\nmanagement systems']
plot_dt[gf_module == 'Integrated service delivery and quality improvement', plot_module := 'Int. service delivery\nand QE']
plot_dt[gf_module == 'Community systems strengthening', plot_module := 'CSS']
plot_dt[gf_module == 'Health sector governance and planning', plot_module := 'Health sector gov.\nand planning']
plot_dt[gf_module == 'Human resources for health, including community health workers', plot_module := 'HRH, incl. CHWs']
plot_dt[gf_module == 'Laboratory systems', plot_module := 'Laboratory systems']
plot_dt[gf_module == 'Financial management systems', plot_module := 'Financial management\nsystems']

# -------------------------------------------------------------------
# make heatmap
plot_dt[is.na(indicators), indicators := '']
plot_dt[is.na(inds_lab), inds_lab := '']

# plot_dt[!is.na(module_percent_of_total_rssh), plot_label := paste0(round(module_percent_of_total_rssh), '%', '\n', inds_lab)]
# plot_dt[is.na(module_percent_of_total_rssh), plot_label := paste0("(0%)\n", inds_lab)]
# plot_dt[module_percent_of_total_rssh < 1 | budget < 1000000, plot_label := paste0(module_percent_of_total_rssh, '%', '\n', inds_lab)]

plot_dt[, plot_label := inds_lab]

# # get number of possible indicators
# all_inds = as.data.table(read_xlsx('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/Synthesis/available_rssh_indicators.xlsx'))
# all_inds = all_inds[, .(num_avail_inds = .N) , by = c('module')]
# plot_dt = merge(plot_dt, all_inds, by.x = 'gf_module', by.y = 'module', all = TRUE)
# plot_dt[, plot_module := paste0(plot_module, '\n(', num_avail_inds,')')]
# # order to match what is in the synthesis report
# plot_dt[, plot_module := factor(plot_module, levels = c('HMIS and M&E\n(5)', 
#                                                         'Health products\nmanagement systems\n(5)',
#                                                         'Int. service delivery\nand QE\n(4)',
#                                                         'CSS\n(2)',
#                                                         'Health sector gov.\nand planning\n(1)',
#                                                         'HRH, incl. CHWs\n(3)',
#                                                         'Laboratory systems\n(1)',
#                                                         'Financial management\nsystems\n(1)'))]

# order to match what is in the synthesis report
plot_dt[, plot_module := factor(plot_module, levels = c('HMIS and M&E', 
                                                        'Health products\nmanagement systems',
                                                        'Int. service delivery\nand QE',
                                                        'CSS',
                                                        'Health sector gov.\nand planning',
                                                        'HRH, incl. CHWs',
                                                        'Laboratory systems',
                                                        'Financial management\nsystems'))]


# plot into a heatmap
g = ggplot(plot_dt, aes(loc_name, plot_module, fill= module_percent_of_total_rssh)) + geom_tile() + theme_classic() + 
  scale_x_discrete(position = 'top') + labs(x = "", y = "", caption = "* = Custom coverage indicators included in the total", fill = "% of country's \nRSSH Funding") + 
  scale_fill_continuous(high = "#132B43", low = "#9fd4fc") +
  scale_y_discrete(limits = rev(levels(plot_dt$plot_module))) +
  theme(axis.text=element_text(size=16), legend.text = element_text(size=11), legend.title = element_text(size = 13), plot.caption = element_text(size = 11)) +
  theme(axis.text.x = element_text(angle = 45, hjust = -0.01)) +
  geom_text(aes(label= plot_dt$plot_label), size = 4.75, color = '#FFFFFF') +
  geom_text(data = plot_dt[module_percent_of_total_rssh <15, ], aes(label= plot_dt[module_percent_of_total_rssh <15, plot_label]), size = 4.75, color = '#132B43')

file = paste0('J:/Project/Evaluation/GF/resource_tracking/visualizations2021/Synthesis/heatmap_RSSHindicators_byModuleCountry.png')
png(file, height = 8, width = 11, units = "in", res = 300)
print(g)
dev.off()

# -------------------------------------------------------------------

