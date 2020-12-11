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
dt[, module_percent_of_total_rssh := round((budget/rssh_total_by_country)*100, 2)]
dt = dt[module_percent_of_total_rssh > 0, ]

# order and limit to match what is in the synthesis report
dt[, gf_module := factor(gf_module, levels = c('Health management information systems and M&E', 
                                 'Health products management systems',
                                 'Integrated service delivery and quality improvement',
                                 'Community systems strengthening',
                                 'Health sector governance and planning',
                                 'Human resources for health, including community health workers',
                                 'Laboratory systems',
                                 'Financial management systems'))]

# plot into a heatmap
g = ggplot(dt, aes(loc_name, gf_module, fill= module_percent_of_total_rssh)) + geom_tile() + theme_bw() + 
  scale_x_discrete(position = 'top') + labs( x = 'Country', y = "RSSH-related Module", fill = "% of Country's Total \nRSSH Spending") + 
  scale_fill_continuous(high = "#132B43", low = "#9fd4fc") +
  scale_y_discrete(limits = rev(levels(dt$gf_module))) +
  geom_text(aes(label= paste0(dt$module_percent_of_total_rssh, '%')), color = '#FFFFFF')
  # scale_fill_continuous(type = 'viridis') 
  # 56B1F7 - alternate/close to default color 
  
pdf(outFile, height = 9, width = 14)
print(g)
dev.off()

##########################################################################################
