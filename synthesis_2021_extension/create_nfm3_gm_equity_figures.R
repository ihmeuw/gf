# graphs for shifts in KVP-Equity for synthesis
# Francisco

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

# # the tables to be saved
# outTable1 = paste0(outDir, "tables/hiv_data_table.csv")

all_data <- as.data.table(read.csv(file = inFile))

# subset data

equity_data = all_data[equity==TRUE & grant_period=="2021-2023" & budget_version%in%c("approved", "funding_request20"), .(budget=sum(budget, na.rm=T)), 
              by=c('loc_name', 'gf_module', 'gf_intervention', 'budget_version')]

##adding total country-funding to calculate proportion of total HRG-Equity
total_grant_budget <- all_data[grant_period=="2021-2023" & budget_version%in%c("approved", "funding_request20"),.(total_budget=sum(budget,na.rm = TRUE)), by=c("loc_name","budget_version")]

# create dataset at the modular level
equity_modules = equity_data[,.(budget=sum(budget, na.rm = TRUE)), by=c('loc_name', 'gf_module', 'budget_version')]

# # create dataset at equity category level
# plot_equity_2 <- merge(equity_modules,total_grant_budget,by = c("loc_name","budget_version"))
# 
# plot_equity_3 <- plot_equity_2[,hrg_budget_total:=sum(budget,na.rm = TRUE), by = c("loc_name","budget_version")]

# plot_equity_3[,hrg_prop_total:= hrg_budget_total/total_budget]
# plot_equity_3[,hrg_prop:= budget/hrg_budget_total]
# 
# plot_equity_3 <- plot_equity_3[,.(loc_name, budget_version, total_budget, hrg_budget_total, hrg_prop_total, hrg_prop)]
# plot_equity_3 <- unique(plot_equity_3)

# create simplified modules
equity_modules[,simplified_mod:=gf_module]
equity_modules[grepl(gf_module, pattern = 'Comprehensive prevention'), simplified_mod := 'Prevention']
equity_modules[grepl(gf_module, pattern = 'Comprehensive programs'), simplified_mod := 'Prevention']
equity_modules[grepl(gf_module, pattern = 'Prevention'), simplified_mod := 'Prevention']
equity_modules[grepl(gf_module, pattern = 'PMTCT'), simplified_mod := 'Prevention']
equity_modules[grepl(gf_module, pattern = 'PMTCT'), simplified_mod := 'Prevention']
equity_modules[grepl(gf_module, pattern = 'human rights'), simplified_mod := 'Human Rights']
equity_modules[grepl(gf_module, pattern = 'Multidrug-resistant TB'), simplified_mod := 'MDR-TB']
equity_modules[grepl(gf_module, pattern = "Specific prevention interventions"), simplified_mod := 'Specific prevention interventions']
# -------------------------------------------------------------------
# Define the number of colors you want
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)

plot1 <- ggplot(equity_modules, aes(y=budget, x=budget_version, fill=simplified_mod)) +
         geom_bar(stat = 'identity') +
         coord_flip()+
         labs(title=paste0("Changes in Equity Related Modules, NFM3 Grantmaking"),
              y='Budget (Millions)',
              x='Budget Versions',
              fill = "Modules") +
         scale_y_continuous(labels = function(x)x/1000000, limits = c(0,65000000))+
         facet_wrap(~loc_name, scales = "free")+
         scale_fill_manual(values=mycolors)+
         theme_minimal(base_size = 14)+
         # geom_text(data=plot_equity_2,
                  # aes(label=paste0(round(hrg_prop*100),'%')),
                  # y=65000000)+ # this helps calculate the total percentage of equity funds in each country
         theme(legend.position = "bottom") +
         guides(fill = guide_legend(title.position = "top", reverse = TRUE,nrow=4)) +
         facet_grid(loc_name ~ ., switch = "y", scales = 'free')

outFile1 = paste0(outDir, '/nfm3_grantmaking_equity_modules.png')
       
png(outFile1, height = 11, width = 10, units = "in", res = 300)
plot1
dev.off()


# create second dataset with equity_categories 
equity_mapping <- as.data.table(read.xlsx(paste0(box, "synthesis/data/final_hrgequity_related_modules_interventions.xlsx")))

setnames(equity_mapping, old=c("Module", "Intervention"),
         new = c("gf_module", "gf_intervention"))

equity_mapping <- equity_mapping[,X1:=NULL]
         
equity_categories <- merge(equity_data, equity_mapping, by=c("gf_module", "gf_intervention"), all.x = TRUE)

# sum data by categories
equity_categories[Human.rights.funding==TRUE, category:="human rights"]
equity_categories[Key.and.vulnerable.populations.funding==TRUE, category:="KVP"]
equity_categories[Other.equity.related.investments==TRUE, category:="other equity investments"]

# sum by category
equity_categories <- equity_categories[,.(budget=sum(budget, na.rm = TRUE)), by=c('loc_name', 'budget_version', 'category')]

plot2 <- ggplot(equity_categories, aes(y=budget, x=budget_version, fill=category)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0("Changes in Equity Related Modules, NFM3 Grantmaking"),
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

outFile2= paste0(outDir, '/nfm3_grantmaking_equity_categories.png')

png(outFile2, height = 11, width = 9, units = "in", res = 300)
plot2
dev.off()


# code that i would like to use to create similar looking graphs
# r3 <- ggplot(plot_rssh_3[version_short=="NFM2 Award" | version_short=="NFM2 Revision*"],
#              aes(y=budget, x=version_short, fill=simplified_mod)) + 
#   geom_bar(stat = 'identity') +
#   coord_flip()+
#   labs(title=paste0("RSSH"),
#        y='Budget (Millions)',
#        x='Budget Versions',
#        caption = "*Revision is the most recent official budget revision.",
#        fill = "Modules") +
#   scale_y_continuous(labels = function(x)x/1000000, limits = c(0,65000000))+
#   facet_wrap(~loc_name, scales = "free")+
#   scale_fill_brewer(palette = "Paired")+
#   theme_minimal(base_size = 14)+
#   geom_text(data=plot_rssh_3[simplified_mod=="HMIS and M&E" & (version_short=="NFM2 Award" | version_short=="NFM2 Revision*")],
#             aes(label=paste0(round(rssh_prop_total*100),'%')),
#             y=65000000)+
#   theme(legend.position = "bottom") +
#   guides(fill = guide_legend(title.position = "top", reverse = TRUE,nrow=4)) +
#   facet_grid(loc_name ~ ., switch = "y", scales = 'free')

# # old code that made equity graphs
# e <- ggplot(plot_equity_3, aes(y=budget, x=simple_version, fill=label)) + 
#   geom_bar(stat = 'identity') +
#   coord_flip()+
#   theme(legend.position = "none") +
#   labs(title=paste0("HRG-Equity related investments"),
#        y='Budget (Millions)',
#        x='Budget Versions',
#        #caption = "*Revision is the most recent official budget revision. Percentages are HRG-Equity/total grant investments",
#        fill = "HRG-Equity Categories") +
#   theme_bw() +
#   theme(plot.caption = element_text(hjust = 0),
#         plot.caption.position = "plot", 
#         text = element_text(size = 19),
#         legend.position = "bottom",legend.title = element_blank()) +
#   scale_y_continuous(labels = function(x)x/1000000)+
#   scale_fill_brewer(palette = "Dark2")+
#   facet_grid(loc_name ~ ., switch = "y",scales = "free_y")+
#   geom_text(data=plot_equity_3[label=="Human rights related investments"],
#             aes(label=paste0(round(hrg_prop_total*100),'%')),
#             y=119200000, size = 6)+
#   guides(fill = guide_legend(reverse=TRUE))
# 
# ggplot(plot_equity_3, aes(y=budget, x=simple_version, fill=label)) + 
#     geom_bar(stat = 'identity') +
#     coord_flip()+
#     theme(legend.position = "none") +
#     labs(title=paste0("HRG-Equity related investments"),
#          y='Budget (Millions)',
#          x='Budget Versions',
#          #caption = "*Revision is the most recent official budget revision. Percentages are HRG-Equity/total grant investments",
#          fill = "HRG-Equity Categories") +
#     theme_bw()
