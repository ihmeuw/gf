#########################################################
# Audrey Batzel
# Synthesis 2020 Data
# Compare absolute budget changes (module level) across budget versions looking at yearly budgeted amounts

# To do - Matt said something about including/visualizing the relative changes 
#       - summarize across grants/countries? how? 
#       - figure out what to do with GTM years? 
#       - fix COD data - why are there years 2028-2031 for COD-H-MOH? 
#       - NAs in budget_version for COD-M-MOH and COD-M-SANRU

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

# input file
user=as.character(Sys.info()[7])
box = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/")

# output file
outFile = "J:/Project/Evaluation/GF/resource_tracking/visualizations2020/Synthesis/synthesis_barplot_yearly_by_module_and_grant.pdf"

# -------------------------------------------------------------------
dt = data.table()
# read in country data and bind together
for (c in c('UGA', 'SEN', 'COD', 'GTM')) {
  dt_c = as.data.table(readRDS(paste0(box, c, '/prepped_data/raw_bound_gf_files.rds')))
  dt_c[, country := c]
  dt = rbind(dt, dt_c)
}

# subset to just budgets for active grants, excluding file_iteration = initial
dt = dt[data_source == 'budget' & grant_status == 'active' & file_iteration %in% c('approved_gm', 'revision')]

# fix problems with 'year' variable... 
check = unique(dt[,.(year, grant_period, grant)])

# *****
# subset out the problematic grants for now...
dt = dt[!grant %in% c('COD-H-MOH')]
dt = dt[!country %in% c('GTM')]
# *****

# subset to NFM2
dt = dt[grant_period %in% c('2018-2020')]

# sum to yearly level, module level
dt_yearly = dt[, .(budget=sum(budget, na.rm=TRUE)), by=.(country, grant, grant_period, budget_version, module, year, file_name)]

# -------------------------------------------------------------------
# order budget versions
dt_yearly[budget_version == 'Approved and Catalytic/Matching Funds', budget_version := 'approved_catalytic']
dt_yearly[, budget_version := factor(budget_version, levels = c('approved', 'approved_catalytic', 'revision1',
                                                                'revision2', 'revision3'))]
# create a list of plots
list_of_plots = NULL
i=1
for (g in unique(dt_yearly$grant)) {
  # bar plots
  list_of_plots[[i]] = ggplot(dt_yearly[grant == g, ], aes(x = budget_version, y = (budget/1000000), fill = module)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(~year) +
    labs(title = paste0('Grant: ', g), x = 'Budget Version', y = 'Budget (Millions USD)', fill = 'Module') +
    theme_bw()
  
  i=i+1
}
#--------------------------------

#--------------------------------
# print out the list of plots into a pdf
#--------------------------------
# save figures
pdf(outFile, height = 8, width = 15)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
} 
dev.off()

##########################################################################################
