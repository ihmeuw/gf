# graphs for shifts in RSSH between NFM3 Funding Request and Grant making

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
user = as.character(Sys.info()[7])

# input file
box = paste0("C:/Users/",user,"/Box Sync/Global Fund Files/")
inFile = paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv')

# output files
outDir = paste0(box, 'synthesis/2021_extension/figures/')

all_data <- as.data.table(read.csv(file = inFile))

rssh_data <- all_data[rssh==TRUE & grant_period=="2021-2023" & budget_version%in%c("approved", "funding_request20"), .(budget=sum(budget, na.rm=T)), 
                      by=c('loc_name', 'gf_module', 'gf_intervention', 'budget_version')]

# create dataset at the modular level
rssh_modules = rssh_data[,.(budget=sum(budget, na.rm = TRUE)), by=c('loc_name', 'gf_module', 'budget_version')]

# create simplified modules
rssh_modules[,simplified_mod:=gf_module]

rssh_modules[grepl(gf_module, pattern = 'Health management information'), simplified_mod := 'HMIS/M&E']
rssh_modules[grepl(gf_module, pattern = 'community health workers'), simplified_mod := 'Human resources, incl CHWs']

plot1 <- ggplot(rssh_modules, aes(y=budget, x=budget_version, fill=simplified_mod)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0("Changes in RSSH Modules, NFM3 Grantmaking"),
       y='Budget (Millions)',
       x='Budget Versions',
       fill = "Modules") +
  scale_y_continuous(labels = function(x)x/1000000, limits = c(0,65000000))+
  facet_wrap(~loc_name, scales = "free")+
  scale_fill_brewer(palette = "Paired")+
  theme_minimal(base_size = 14)+
  # geom_text(data=plot_equity_2,
  # aes(label=paste0(round(hrg_prop*100),'%')),
  # y=65000000)+ # this helps calculate the total percentage of equity funds in each country
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top", reverse = TRUE,nrow=4)) +
  facet_grid(loc_name ~ ., switch = "y", scales = 'free')

outFile1 = paste0(outDir, '/nfm3_grantmaking_rssh_modules.png')

png(outFile1, height = 11, width = 10, units = "in", res = 300)
plot1
dev.off()