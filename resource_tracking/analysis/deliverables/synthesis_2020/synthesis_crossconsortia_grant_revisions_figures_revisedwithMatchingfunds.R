# Synthesis 2020 Figures_version2
# To address Grant Revisions
# compare increases across data source
# Francisco Rios 


# To do:

# 
rm(list=ls())


# Set up
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(readxl)

# -------------------------------------------------------------------
# Files and directories

# input file
box = paste0("C:/Users/frc2/Box Sync/Global Fund Files/")
inFile = paste0(box, 'synthesis/data/draft_synthesis_budget_quant.xlsx')
ehgFile = paste0(box, 'synthesis/data/Synthesis Budget Variance 181120.xlsx')


# output files
out.path = paste0(box, '/synthesis/figures/')

# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Load/prep data

# load
data = as.data.table(read_xlsx(inFile))
ehg_data = as.data.table(read_xlsx(ehgFile))

data <- data[,.(loc_name, gf_module, nfm2_approved_catalytic_funds, nfm2_most_recent_revision)]
ehg_data <- ehg_data[loc_name %in% c('Cambodia', 'Myanmar', 'Sudan', 'Mozambique'),.(loc_name, gf_module, nfm2_approved, nfm2_most_recent_revision)]

setnames(data, old=c('nfm2_approved_catalytic_funds'), new=c('nfm2_approved'))

# bind two data sources together
cc_data <- rbind(data, ehg_data, fill=TRUE)

# # reshape the data long for graphing
all_data <- melt(cc_data, id.vars = c("loc_name", "gf_module"),
                 measure.vars = c("nfm2_approved", "nfm2_most_recent_revision"),
                 variable.name = "version", value.name = "budget")

# replace missing values with zeros
all_data[is.na(budget), budget:=0]

# sum data to the country level
all_data <- all_data[,.(budget=sum(budget, na.rm=TRUE)), by=c('loc_name', 'version')]

# add type of data
all_data$type <- "All modules"

##### ##### ##### ##### 
##### second dataset: rssh funds
##### ##### ##### ##### ##### 

# read IHME and EGH rssh data
rssh_data <- as.data.table(read_xlsx(path = inFile, sheet = "RSSH"))
ehg_rssh_data <- as.data.table(read_xlsx(path = ehgFile, sheet = "RSSH"))

# subset to necessary columns and rows
rssh_data <- rssh_data[,.(loc_name, gf_module, nfm2_approved_catalytic_funds, nfm2_most_recent_revision)]
ehg_rssh_data <- ehg_rssh_data[loc_name %in% c('Myanmar', 'Cambodia', 'Mozambique', 'Sudan')
                               ,.(loc_name, gf_module, nfm2_approved, nfm2_most_recent_revision)]

setnames(rssh_data, old=c('nfm2_approved_catalytic_funds'), new=c('nfm2_approved'))

# bind data together into cross consortia sheet
cc_rssh_data <- rbind(rssh_data, ehg_rssh_data, fill=TRUE)

# reshape long for plotting
rssh_data <- melt(cc_rssh_data, id.vars = c("loc_name","gf_module"),
                  measure.vars = c("nfm2_approved", "nfm2_most_recent_revision"),
                  variable.name = "version", value.name = "budget")

# sum across unnecessary variables
rssh_data <- rssh_data[, .(budget=sum(budget, na.rm=T)), by=c('loc_name', 'version')]

# add indicator variable for type of breakdown
rssh_data$type <- "RSSH"

##### ##### ##### ##### 
##### third dataset: equity funds
##### ##### ##### ##### ##### 

# read IHME and EGH equity data
equity_data <- as.data.table(read_xlsx(path = inFile, sheet = "HRG-Equity"))
ehg_equity_data <- as.data.table(read_xlsx(path = ehgFile, sheet = "HRG-Equity"))

equity_data <- equity_data[,.(loc_name, gf_module, nfm2_approved_catalytic_funds, nfm2_most_recent_revision)]
ehg_equity_data <- ehg_equity_data[loc_name %in% c('Cambodia', 'Myanmar', 'Sudan', 'Mozambique'),.(loc_name, gf_module, nfm2_approved, nfm2_most_recent_revision)]

setnames(equity_data, old=c('nfm2_approved_catalytic_funds'), new=c('nfm2_approved'))

cc_equity_data <- rbind(equity_data, ehg_equity_data, fill=TRUE)

cc_equity_data <- melt(cc_equity_data, id.vars = c("loc_name", "gf_module"),
                       measure.vars = c("nfm2_approved", "nfm2_most_recent_revision"),
                       variable.name = "version", value.name = "budget")

# sum across modules
equity_data <- cc_equity_data[, .(budget=sum(budget, na.rm=T)), by=c('loc_name', 'version')]

# add indicator variable for type of breakdown
equity_data$type <- "HRG-Equity"

# bind all three datasets together
plot_data <- rbind(all_data, rssh_data, equity_data, fill=TRUE)

# format data for plotting
# cast wide
plot_data <- dcast(plot_data, loc_name + type ~ version, value.var = "budget")

# calculate variables that will be used in plotting
plot_data$difference <- plot_data$nfm2_most_recent_revision-plot_data$nfm2_approved
plot_data$percent_change <- plot_data$difference/plot_data$nfm2_approved

plot_data$type <- factor(plot_data$type, 
                         levels = c("HRG-Equity", "RSSH", "All modules"))

######################## First OutFile #################################
#
# show how funds changes between approved and most recent revision (absolute terms)
#
##########################################################################

# make synthesis graphic
p1 <- ggplot(plot_data, aes(y=difference, x=type, fill=type)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title="Difference between Approved + Match Funds and Most Recent Budget Revision",
       y='Budget difference (in Millions)',
       x='',
       caption = "",
       fill = "") +
  scale_y_continuous(labels = function(x)x/10^6)+
  facet_grid(loc_name ~ ., switch = "y")+
  theme_minimal(base_size=14)+
  theme(axis.text.y=element_blank())+
  scale_fill_brewer(palette = "Set2")+
  guides(fill = guide_legend(reverse=TRUE))

p1
ggsave("absolute_differences_nfm2award_MF_revision.png", path = out.path, plot=p1, width = 9.5, height=9, units = "in")
# ggsave("difference_award_revisionsynthesis_figure.png", path = out.path, plot = p1, width = 10, height = 9, units = "in")

# make second figure with percent change
p2 <- ggplot(plot_data, aes(y=percent_change, x=type, fill=type)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title="Difference between Approved + Match Funds and Most Recent Budget Revision",
       y='Budget difference (%)',
       x='',
       caption = "",
       fill = "") +
  scale_y_continuous(labels = function(x)x*100)+
  facet_grid(loc_name ~ ., switch = "y")+
  theme_minimal(base_size=14)+
  theme(axis.text.y=element_blank())+
  scale_fill_brewer(palette = "Set2")+
  guides(fill = guide_legend(reverse=TRUE))

p2
ggsave("percent_differences_nfm2award_MF_revision.png", path = out.path, plot=p2, width = 9.5, height=9, units = "in")

# save plot data for use in other graphics
write.csv(plot_data, file = paste0(box,"/synthesis/data/cc_revisions_data_forplots.csv"))
          