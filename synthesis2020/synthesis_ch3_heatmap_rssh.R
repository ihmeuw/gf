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
# -------------------------------------------------------------------
# Files and directories
setwd('C:/local/gf/')

user=as.character(Sys.info()[7])

# input file
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/")
inFile = paste0(box, 'synthesis/data/draft_synthesis_budget_quant.xlsx')
ehgFile = paste0(box, 'synthesis/data/Synthesis Budget Variance 181120.xlsx')

# output file
outFile = "J:/Project/Evaluation/GF/resource_tracking/visualizations2020/Synthesis/synthesis_ch3_rssh_heatmap.pdf"

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
