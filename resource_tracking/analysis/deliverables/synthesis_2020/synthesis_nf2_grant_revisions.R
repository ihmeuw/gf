# Synthesis 2020 Figures
# To address Grant Revisions
# Francisco Rios 


# To do:


# Set up
library(ggplot2)
library(data.table)
library(readxl)
library(scales)
library(gridExtra)
library(grid)
library(lattice)
library(RColorBrewer)
library(gghighlight)
# -------------------------------------------------------------------
# Files and directories

# input file
box = paste0("C:/Users/frc2/Box Sync/Global Fund Files/")
inFile = paste0(box, 'synthesis/data/draft_synthesis_budget_quant.xlsx')

# output files
outDir = paste0(box, '/synthesis/figures/')

# the files without the legend
outFileg = paste0(outDir, 'revisions_disease_breakdown.png')
outFilem = paste0(outDir, 'revisions_mal_modules.png')
outFileh = paste0(outDir, 'revisions_hiv_modules.png')
outFilet = paste0(outDir, 'revisions_tb_modules.png')

outFilee = paste0(outDir, 'revisions_equity_modules.png')
outFilee2 = paste0(outDir, 'revisions_equity_grid.png')

outFiler = paste0(outDir, 'revisions_rssh_modules.png')
outFiler2 = paste0(outDir, 'revisons_rssh_grid.png')
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Load/prep data

# load
data = as.data.table(read_xlsx(inFile))
data <- data[,.(loc_name, disease, gf_module, nfm2_approved, nfm2_most_recent_revision)]

# reshape the data long for graphing
plot_data <- melt(data, id.vars = c("loc_name", "disease", "gf_module"), 
                      measure.vars = c("nfm2_approved", "nfm2_most_recent_revision"),
                      variable.name = "version", value.name = "budget") 

# sort the file version as a factor
plot_data$version <- factor(plot_data$version, 
                                levels = c("nfm2_most_recent_revision",
                                           "nfm2_approved"))

# create simplified modules
plot_data[grepl(gf_module, pattern = 'Comprehensive prevention'), simplified_mod := 'Prevention']
plot_data[grepl(gf_module, pattern = 'Comprehensive programs'), simplified_mod := 'Prevention']
plot_data[grepl(gf_module, pattern = 'Prevention'), simplified_mod := 'Prevention']
plot_data[grepl(gf_module, pattern = 'PMTCT'), simplified_mod := 'Prevention']
plot_data[grepl(gf_module, pattern = 'PMTCT'), simplified_mod := 'Prevention']
plot_data[grepl(gf_module, pattern = 'human rights'), simplified_mod := 'Human Rights']
plot_data[grepl(gf_module, pattern = 'Multidrug-resistant TB'), simplified_mod := 'MDR-TB']
plot_data[grepl(gf_module, pattern = "Specific prevention interventions"), simplified_mod := 'Specific prevention interventions']

plot_data[is.na(simplified_mod), simplified_mod := gf_module]

######################## First OutFile #################################
#
# show how disease funds have changed over time during grant revisions (including catalytic funds)
#
##########################################################################

g <- ggplot(plot_data, aes(y=budget, x=version, fill=disease)) + 
    geom_bar(stat = 'identity')+
    coord_flip()+
    scale_fill_manual(values= c("#CE171E", "#FBAB18", "#6E2C6C", "#0066B3"))+
    labs(title=paste0('Disease-level Revisions'),
         y='Budget (Millions)',
         x='Budget Versions') +
    scale_y_continuous(labels = function(x)x/1000000)+
    facet_wrap(~loc_name, scales = "free")+
  theme_bw(base_size=12)

png(outFileg, height = 8, width = 10, units = "in", res = 300)
g
dev.off()

######################## Second OutFiles #################################
#
# show how simplified modules have changed over time during grant revisions
#
##########################################################################

# plot disease breakdown by modules
m <- ggplot(plot_data[disease=="malaria"], aes(y=budget, x=version, fill=simplified_mod)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0("Revisions in malaria modules"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_wrap(~loc_name, scales = "free")+
  scale_fill_brewer(palette = "Paired")+
  theme_bw(base_size=12)
  
h <- ggplot(plot_data[disease=="hiv"], aes(y=budget, x=version, fill=simplified_mod)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0("Revisions in HIV modules"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_wrap(~loc_name, scales = "free")+
  scale_fill_brewer(palette = "Paired")+
  theme_bw(base_size=12)

t <- ggplot(plot_data[disease=="tb"], aes(y=budget, x=version, fill=simplified_mod)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0("Revisions in TB modules"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_wrap(~loc_name, scales = "free")+
  scale_fill_brewer(palette = "Paired")+
  theme_bw(base_size=12)

png(outFilem, height = 8, width = 10, units = "in", res = 300)
m
dev.off()

png(outFileh, height = 8, width = 10, units = "in", res = 300)
h
dev.off()

png(outFilet, height = 8, width = 10, units = "in", res = 300)
t
dev.off()

######################## Third OutFiles #################################
#
# show how equity modules have changed over time during grant revisions
#
##########################################################################
equity_data <- as.data.table(read_xlsx(path = inFile, sheet = "HRG-Equity"))
equity_data <- equity_data[,.(loc_name, label, fr_disease, gf_module, gf_intervention, nfm2_approved, nfm2_most_recent_revision)]

plot_equity <- melt(equity_data, id.vars = c("loc_name", "gf_module", "label", "fr_disease"),
                    measure.vars = c("nfm2_approved", "nfm2_most_recent_revision"),
                    variable.name = "version", value.name = "budget")

plot_equity <- plot_equity[, .(budget=sum(budget, na.rm=T)), by=c('loc_name', 'label', 'version')]

plot_equity[grepl(version, pattern = "nfm2_approved"), simple_version := "award"]
plot_equity[grepl(version, pattern = "nfm2_most_recent_revision"), simple_version := "revision"]

plot_equity$simple_version <- factor(plot_equity$simple_version, 
                                   levels = c("award",
                                              "revision"))

# sort the file version as a factor
plot_equity$version <- factor(plot_equity$version, 
                            levels = c("nfm2_most_recent_revision",
                                       "nfm2_approved"))

e <- ggplot(plot_equity, aes(y=budget, x=version, fill=label)) + 
  geom_bar(stat = 'identity') +
  coord_flip()+
  theme(legend.position = "none") +
  labs(title=paste0("Equity revisions"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_wrap(~loc_name, scales = "free")

e2 <- ggplot(plot_equity, aes(y=budget, x=simple_version)) + 
  geom_bar(stat = 'identity') +
  labs(title=paste0("Changes in Equity/HRG Funds between Grant Award and Most Recent Revision"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  theme_bw()+
  facet_grid(rows=vars(loc_name), cols = vars(label), scales = "free")

png(outFilee, height = 8, width = 10, units = "in", res = 300)
e
dev.off()

png(outFilee2, height = 8, width = 10, units = "in", res = 300)
e2
dev.off()

######################## Fourth OutFiles #################################
#
# show how RSSH modules have changed over time during grant revisions
#
##########################################################################
rssh_data <- as.data.table(read_xlsx(path = inFile, sheet = "RSSH"))
rssh_data <- rssh_data[,.(loc_name, fr_disease, gf_module, nfm2_approved, nfm2_most_recent_revision)]
plot_rssh <- melt(rssh_data, id.vars = c("loc_name","fr_disease", "gf_module"),
                    measure.vars = c("nfm2_approved", "nfm2_most_recent_revision"),
                    variable.name = "version", value.name = "budget")

plot_rssh <- plot_rssh[, .(budget=sum(budget, na.rm=T)), by=c('loc_name', 'gf_module', 'version')]

# sort the file version as a factor
plot_rssh$version <- factor(plot_rssh$version, 
                            levels = c("nfm2_most_recent_revision",
                                       "nfm2_approved"))

plot_rssh[grepl(version, pattern = "nfm2_approved"), simple_version := "award"]
plot_rssh[grepl(version, pattern = "nfm2_most_recent_revision"), simple_version := "revision"]

plot_rssh$simple_version <- factor(plot_rssh$simple_version, 
                            levels = c("award",
                                       "revision"))

# create an indicator to show whether value increased or decreased between versions


plot_rssh[grepl(gf_module, pattern = 'information system'), simplified_mod := 'HMIS \n and M&E']
plot_rssh[grepl(gf_module, pattern = 'Human resources'), simplified_mod := 'Human \n resources']
plot_rssh[grepl(gf_module, pattern = 'Financial management'), simplified_mod := 'Finance \n Mng sys']
plot_rssh[grepl(gf_module, pattern = 'Integrated service delivery'), simplified_mod := 'Integr \n Svc Del']
plot_rssh[grepl(gf_module, pattern = 'Procurement'), simplified_mod := 'Procur & \n Supply Chain']
plot_rssh[grepl(gf_module, pattern = 'Community responses and systems'), simplified_mod := 'Comm Resp \n and Sys']
plot_rssh[grepl(gf_module, pattern = 'National health strategies'), simplified_mod := 'Natl \n Health Strg']
plot_rssh[grepl(gf_module, pattern = 'Health sector governance and planning'), simplified_mod := 'Health \n Sec Gov']
plot_rssh[grepl(gf_module, pattern = 'Health products management systems'), simplified_mod := 'Health Prod \n Mng Sys']
plot_rssh[grepl(gf_module, pattern = 'Community systems strengthening'), simplified_mod := 'Comm Sys \n Streng']
plot_rssh[is.na(simplified_mod), simplified_mod := gf_module]

# drop certain rows
plot_rssh <- plot_rssh[gf_module!='Community systems strengthening']
plot_rssh <- plot_rssh[gf_module!='Health sector governance and planning']
plot_rssh <- plot_rssh[gf_module!='Community systems strengthening']
plot_rssh <- plot_rssh[gf_module!='Health products management systems']
plot_rssh <- plot_rssh[gf_module!='Laboratory systems']

r <- ggplot(plot_rssh, aes(y=budget, x=simple_version, fill=simplified_mod)) + 
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0("RSSH revisions"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_wrap(~loc_name, scales = "free")+
  scale_fill_brewer(palette = "Paired")+
  theme_bw(base_size = 11)
  
r2 <- ggplot(plot_rssh, aes(y=budget, x=simple_version)) + 
  geom_bar(stat = 'identity') +
  labs(title=paste0("Changes in RSSH modules between Grant Award and Most Recent Revision"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  theme_bw()+
  facet_grid(rows=vars(loc_name), cols = vars(simplified_mod), scales = "free")+
  theme_bw(base_size = 11)

png(outFiler, height = 8, width = 11.5, units = "in", res = 300)
r
dev.off()

png(outFiler2, height = 8, width = 11.5, units = "in", res = 300)
r2
dev.off()

