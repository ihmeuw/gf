# Synthesis 2020 Figures
# To address Comparison between NFM2 and NFM3
# This file aims to produce visuals that include EHG data as well
# Francisco Rios 

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

# input file
box = paste0("C:/Users/frc2/Box Sync/Global Fund Files/")
inFile = paste0(box, 'synthesis/data/draft_synthesis_budget_quant.xlsx')
ehgFile = paste0(box, 'synthesis/data/Synthesis Budget Variance 181120.xlsx')

# output files
outDir = paste0(box, '/synthesis/figures/cross_consortia_nfm2_nfm3_comparisons/')

# the files to be saved
outFileg = paste0(outDir, 'cc_fr_comparisons_disease_breakdown.png')
outFilem = paste0(outDir, 'cc_fr_comparisons_mal_modules.png')
outFileh = paste0(outDir, 'cc_fr_comparisons_hiv_modules.png')
outFilet = paste0(outDir, 'cc_fr_comparisons_tb_modules.png')

outFilee = paste0(outDir, 'cc_fr_comparisons_equity_modules.png')
outFilee2 = paste0(outDir, 'cc_fr_comparisons_equity_grid.png')

outFilee3 = paste0(outDir, 'cc_fr17_comparisons_equity.png')

outFiler = paste0(outDir, 'cc_fr_comparisons_rssh_grid.png')
outFiler2 = paste0(outDir, 'cc_fr_comparions_rssh_grip.png')
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Load/prep data for first file

# load
data = as.data.table(read_xlsx(inFile))
ehg_data = as.data.table(read_xlsx(ehgFile))

data <- data[,.(loc_name, disease, gf_module, nfm2_funding_request17, nfm2_approved, nfm2_most_recent_revision, nfm3_funding_request20)]

ehg_data <- ehg_data[loc_name %in% c('Cambodia', 'Myanmar', 'Sudan', 'Mozambique'),.(loc_name, gf_module, nfm2_funding_request17, nfm2_approved, nfm2_most_recent_revision, nfm3_funding_request20)]

# # there is no disease column in the EHG data--add that in
map <- readRDS("//ihme.washington.edu/ihme/snfs/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/2018_2020_MF.rds")
map[substring(code, 1, 1)=='C', disease:='covid-19']
map <- unique(map[,.(gf_module, disease)])

# add some disease info in manually
ehg_data <- merge(ehg_data, map, by = "gf_module", all.x = TRUE)
ehg_data[grepl(gf_module, pattern = 'Comprehensive prevention'), disease := 'hiv']
ehg_data[grepl(gf_module, pattern = 'HIV'), disease := 'hiv']
ehg_data[grepl(gf_module, pattern = 'TB'), disease := 'tb']
ehg_data[grepl(gf_module, pattern = 'PMTCT'), disease := 'hiv']
ehg_data[grepl(gf_module, pattern = 'COVID'), disease := 'covid-19']
ehg_data[grepl(gf_module, pattern = 'Condoms'), disease := 'hiv']
ehg_data[grepl(gf_module, pattern = 'Prevention'), disease := 'hiv']
ehg_data[grepl(gf_module, pattern = 'Program management'), disease := 'Unspecified (program management)']

# bind two data sources together
cc_data <- rbind(data, ehg_data, fill=TRUE)

# # reshape the data long for graphing
plot_data <- melt(cc_data, id.vars = c("loc_name", "disease", "gf_module"),
                  measure.vars = c("nfm2_funding_request17", "nfm2_approved", "nfm2_most_recent_revision", "nfm3_funding_request20"),
                  variable.name = "version", value.name = "budget")

# change the names of the variables to shorten
plot_data[version=="nfm3_funding_request20", version_short:="NFM3 FR"]
plot_data[version=="nfm2_most_recent_revision", version_short:="NFM2 Revision*"]
plot_data[version=="nfm2_approved", version_short:="NFM2 Approved"]
plot_data[version=="nfm2_funding_request17", version_short:="NFM2 FR"]

# sort the file version as a factor
plot_data$version_short <- factor(plot_data$version_short,
                            levels = c("NFM3 FR",
                                       "NFM2 Revision*",
                                       "NFM2 Approved",
                                       "NFM2 FR"))

# create simplified modules
plot_data[grepl(gf_module, pattern = 'Comprehensive prevention'), simplified_mod := 'Prevention']
plot_data[grepl(gf_module, pattern = 'Comprehensive programs'), simplified_mod := 'Prevention']
plot_data[grepl(gf_module, pattern = 'Prevention'), simplified_mod := 'Prevention']
plot_data[grepl(gf_module, pattern = 'PMTCT'), simplified_mod := 'Prevention']
plot_data[grepl(gf_module, pattern = 'PMTCT'), simplified_mod := 'Prevention']
plot_data[grepl(gf_module, pattern = 'human rights'), simplified_mod := 'Human Rights']
plot_data[grepl(gf_module, pattern = 'Multidrug-resistant TB'), simplified_mod := 'MDR-TB']
plot_data[grepl(gf_module, pattern = "Specific prevention interventions"), simplified_mod := 'Specific prevention interventions']
plot_data[grepl(gf_module, pattern = "Condoms"), simplified_mod := 'Prevention']

plot_data[is.na(simplified_mod), simplified_mod := gf_module]

# replace missing values with zeros
plot_data[is.na(budget), budget:=0]

######################## First OutFile #################################
#
# show how disease funds have changed over time during grant revisions (including catalytic funds)
#
##########################################################################

g <- ggplot(plot_data, aes(y=budget, x=version_short, fill=disease)) + 
  geom_bar(stat = 'identity')+
  coord_flip()+
  scale_fill_manual(values= c("#BFBFBF", "#CE171E", "#FBAB18", "#6E2C6C", "#0066B3", "#000000"))+
  labs(title=paste0('Disease-level Revisions'),
       y='Budget (Millions)',
       x='Budget Versions',
       caption = "*Revision is the most recent official budget revision.",
       fill = "Simplified Module Categories") +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_grid(loc_name ~ ., switch = "y")+
  theme_minimal(base_size=12)

png(outFileg, height = 8, width = 10, units = "in", res = 300)
g
dev.off()

######################## Second OutFiles #################################
#
# show how simplified modules have changed over time during grant revisions
#
##########################################################################

# plot disease breakdown by modules
m <- ggplot(plot_data[disease=="malaria"], aes(y=budget, x=version_short, fill=simplified_mod)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0("Revisions in malaria modules"),
       y='Budget (Millions)',
       x='Budget Versions',
       caption = "*Revision is the most recent official budget revision.",
       fill = "Simplified Module Categories") +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_grid(loc_name ~ ., switch = "y")+
  scale_fill_brewer(palette = "Paired")+
  theme_minimal(base_size=12)

h <- ggplot(plot_data[disease=="hiv"], aes(y=budget, x=version_short, fill=simplified_mod)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0("Revisions in HIV modules"),
       y='Budget (Millions)',
       x='Budget Versions',
       caption = "*Revision is the most recent official budget revision.",
       fill = "Simplified Module Categories") +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_grid(loc_name ~ ., switch = "y")+
  scale_fill_brewer(palette = "Paired")+
  theme_minimal(base_size=12)

t <- ggplot(plot_data[disease=="tb"], aes(y=budget, x=version_short, fill=simplified_mod)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0("Revisions in TB modules"),
       y='Budget (Millions)',
       x='Budget Versions',
       caption = "*Revision is the most recent official budget revision.",
       fill = "Simplified Module Categories") +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_grid(loc_name ~ ., switch = "y")+
  scale_fill_brewer(palette = "Paired")+
  theme_minimal(base_size=12)

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
ehg_equity_data <- as.data.table(read_xlsx(path = ehgFile, sheet = "HRG-Equity"))

equity_data <- equity_data[,.(loc_name, label, gf_module, gf_intervention, nfm2_funding_request17, nfm2_approved, nfm2_most_recent_revision, nfm3_funding_request20)]
ehg_equity_data <- ehg_equity_data[loc_name %in% c('Cambodia', 'Myanmar', 'Sudan', 'Mozambique'),.(loc_name, gf_module, gf_intervention, nfm2_funding_request17, nfm2_approved, nfm2_most_recent_revision, nfm3_funding_request20)]

# label is missing from the EHG Equity data source
ehg_equity_data[grepl(gf_module, pattern = 'prevention programs'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Prevention programs'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Comprehensive programs'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_module, pattern = 'PMTCT'), label := 'Other equity related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Differentiated HIV'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Multidrug-resistant'), label := 'Other equity related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Prevention for'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_module, pattern = 'KVPs'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Prevention'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Case management'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Prevention of mother-to-child transmission'), label := 'Other equity related investments']
ehg_equity_data[grepl(gf_module, pattern = 'human rights-'), label := 'Human rights related investments']
ehg_equity_data[grepl(gf_module, pattern = 'human rights'), label := 'Human rights related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Community response'), label := 'Other equity related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Community systems'), label := 'Other equity related investments']
ehg_equity_data[grepl(gf_module, pattern = 'Integrated service delivery '), label := 'Other equity related investments']

ehg_equity_data[grepl(gf_intervention, pattern = 'community case management'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'Addressing stigma'), label := 'Human rights related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'Integration into national'), label := 'Other equity related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'Intermittent preventive treatment'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'Key populations'), label := 'KVP related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'Prevention and management of co-infections '), label := 'KVP related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'human rights'), label := 'Human rights related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'human rights-'), label := 'Human rights related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'Community TB care delivery'), label := 'Other equity related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'Community MDR-TB care delivery'), label := 'Other equity related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'Community TB/HIV care delivery'), label := 'Other equity related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'Living support'), label := 'Other equity related investments']
ehg_equity_data[grepl(gf_intervention, pattern = 'Differentiated HIV testing services'), label := 'Other equity related investments']

cc_equity_data <- rbind(equity_data, ehg_equity_data, fill=TRUE)

plot_equity <- melt(cc_equity_data, id.vars = c("loc_name", "gf_module", "label"),
                    measure.vars = c("nfm2_funding_request17", "nfm2_approved", "nfm2_most_recent_revision", "nfm3_funding_request20"),
                    variable.name = "version", value.name = "budget")

# fix typo in label
plot_equity <- plot_equity[label=="Other equity realted investments", label:="Other equity related investments"]

# sum across modules
plot_equity <- plot_equity[, .(budget=sum(budget, na.rm=T)), by=c('loc_name', 'label', 'version')]

plot_equity[grepl(version, pattern = "nfm2_funding_request17"), simple_version := "NFM2FR"]
plot_equity[grepl(version, pattern = "nfm2_approved"), simple_version := "Award"]
plot_equity[grepl(version, pattern = "nfm2_most_recent_revision"), simple_version := "OBR"]
plot_equity[grepl(version, pattern = "nfm3_funding_request20"), simple_version := "NFM3FR"]

plot_equity$simple_version <- factor(plot_equity$simple_version, 
                                     levels = c("NFM2FR",
                                                "Award",
                                                "OBR",
                                                "NFM3FR"))

# sort the file version as a factor
plot_equity$version <- factor(plot_equity$version, 
                              levels = c("nfm3_funding_request20",
                                         "nfm2_most_recent_revision",
                                         "nfm2_approved",
                                         "nfm2_funding_request17"))

e <- ggplot(plot_equity, aes(y=budget, x=version, fill=label)) + 
  geom_bar(stat = 'identity') +
  coord_flip()+
  theme(legend.position = "none") +
  labs(title=paste0("Equity revisions"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_grid(loc_name ~ ., switch = "y")+
  theme_bw(base_size = 12)

e2 <- ggplot(plot_equity, aes(y=budget, x=simple_version)) + 
  geom_bar(stat = 'identity') +
  labs(title=paste0("Changes in Equity/Human rights related investments between Grant Award and Most Recent Revision"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  theme_bw()+
  facet_grid(rows=vars(loc_name), cols = vars(label), scales = "free")

e3 <- ggplot(plot_equity[version%in%c("nfm2_approved",
                                      "nfm2_funding_request17")], aes(y=budget, x=version, fill=label)) + 
  geom_bar(stat = 'identity') +
  coord_flip()+
  theme(legend.position = "none") +
  labs(title=paste0("Equity revisions"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_grid(loc_name ~ ., switch = "y")+
  theme_bw(base_size = 12)

png(outFilee, height = 8, width = 10, units = "in", res = 300)
e
dev.off()

png(outFilee2, height = 8, width = 10, units = "in", res = 300)
e2
dev.off()

png(outFilee3, height = 8, width = 10, units = "in", res = 300)
e3
dev.off()


######################## Fourth OutFiles #################################
#
# show how RSSH modules have changed over time during grant revisions
#
##########################################################################
# read IHME and EGH rssh data
rssh_data <- as.data.table(read_xlsx(path = inFile, sheet = "RSSH"))
ehg_rssh_data <- as.data.table(read_xlsx(path = ehgFile, sheet = "RSSH"))

# subset to necessary columns and rows
rssh_data <- rssh_data[,.(loc_name, gf_module, nfm2_funding_request17, nfm2_approved, nfm2_most_recent_revision, nfm3_funding_request20)]
ehg_rssh_data <- ehg_rssh_data[loc_name %in% c('Myanmar', 'Cambodia', 'Mozambique', 'Sudan')
                           ,.(loc_name, gf_module, nfm2_funding_request17, nfm2_approved, nfm2_most_recent_revision, nfm3_funding_request20)]

# bind data together into cross consortia sheet
cc_rssh_data <- rbind(rssh_data, ehg_rssh_data, fill=TRUE)

# reshape long for plotting
plot_rssh <- melt(cc_rssh_data, id.vars = c("loc_name","gf_module"),
                  measure.vars = c("nfm2_funding_request17","nfm2_approved", "nfm2_most_recent_revision", "nfm3_funding_request20"),
                  variable.name = "version", value.name = "budget")

# sum across unnecessary variables
plot_rssh <- plot_rssh[, .(budget=sum(budget, na.rm=T)), by=c('loc_name', 'gf_module', 'version')]

# create new variable "version_short" to help with labeling of budget versions
plot_rssh[grepl(version, pattern = "nfm2_funding_request17"), version_short := "NFM2 FR"]
plot_rssh[grepl(version, pattern = "nfm2_approved"), version_short := "NFM2 Approved"]
plot_rssh[grepl(version, pattern = "nfm2_most_recent_revision"), version_short := "NFM2 Revision*"]
plot_rssh[grepl(version, pattern = "nfm3_funding_request20"), version_short := "NFM3 FR"]

# sort the version_short variable as a factor for plotting
plot_rssh$version <- factor(plot_rssh$version_short, 
                            levels = c("NFM3 FR",
                                       "NFM2 Revision*",
                                       "NFM2 Approved",
                                       "NFM2 FR"))

# create simple_version variable to use for graphing of grid figure
plot_rssh[grepl(version, pattern = "nfm2_funding_request17"), simple_version := "NFM2FR"]
plot_rssh[grepl(version, pattern = "nfm2_approved"), simple_version := "GA"]
plot_rssh[grepl(version, pattern = "nfm2_most_recent_revision"), simple_version := "OBR"]
plot_rssh[grepl(version, pattern = "nfm3_funding_request20"), simple_version := "NFM3FR"]

plot_rssh$simple_version <- factor(plot_rssh$simple_version, 
                                   levels = c("NFM2FR",
                                              "GA",
                                              "OBR",
                                              "NFM3FR"))

# create simplified rssh modules for comparing across NFM versions
plot_rssh[grepl(gf_module, pattern = 'information system'), simplified_mod := 'HMIS and M&E']
plot_rssh[grepl(gf_module, pattern = 'Human resources'), simplified_mod := 'Human resources']
plot_rssh[grepl(gf_module, pattern = 'Integrated service delivery'), simplified_mod := 'Integr Service Del']
plot_rssh[grepl(gf_module, pattern = 'Financial management'), simplified_mod := 'Finance Manag Sys']

plot_rssh[grepl(gf_module, pattern = 'Community responses and systems'), simplified_mod := 'Comm Resp | Comm Sys Str'] # revised names
plot_rssh[grepl(gf_module, pattern = 'Community systems strengthening'), simplified_mod := 'Comm Resp | Comm Sys Str'] # revised names

plot_rssh[grepl(gf_module, pattern = 'Health sector governance and planning'), simplified_mod := 'Natl Strategies | Governance'] # revised names
plot_rssh[grepl(gf_module, pattern = 'National health strategies'), simplified_mod := 'Natl Strategies | Governance'] # revised names

plot_rssh[grepl(gf_module, pattern = 'Laboratory systems'), simplified_mod := 'Lab systems'] # new NFM3 module

plot_rssh[grepl(gf_module, pattern = 'Procurement'), simplified_mod := 'Procurement | Health products'] # revised names
plot_rssh[grepl(gf_module, pattern = 'Health products management systems'), simplified_mod := 'Procurement | Health products']

# 
# plot_rssh[is.na(simplified_mod), simplified_mod := gf_module]

# factor the simplified_mod variable
# plot_rssh$simplified_mod <- factor(plot_rssh$simplified_mod,
#                                    levels = c("Comm Resp & Comm Sys Str",
#                                               "Finance Manag Sys",
#                                               "HMIS and M&E",
#                                               "Human resources",
#                                               "Integr Service Del",
#                                               "Natl Health Strg",
#                                               "Procurement & Health products",
#                                               "New NFM3 RSSH Mods"))
# 
# # # drop certain rows
# # plot_rssh <- plot_rssh[gf_module!='Community systems strengthening']
# # plot_rssh <- plot_rssh[gf_module!='Health sector governance and planning']
# # plot_rssh <- plot_rssh[gf_module!='Health products management systems']
# # plot_rssh <- plot_rssh[gf_module!='Laboratory systems']

r <- ggplot(plot_rssh, aes(y=budget, x=version, fill=simplified_mod)) + 
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0("RSSH revisions"),
       y='Budget (Millions)',
       x='Budget Versions',
       caption = "*Revision is the most recent official budget revision.",
       fill = "Modules") +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_wrap(~loc_name, scales = "free")+
  scale_fill_brewer(palette = "Paired")+
  theme_minimal(base_size = 11)+
  facet_grid(loc_name ~ ., switch = "y")

r2 <- ggplot(plot_rssh, aes(y=budget, x=simple_version)) + 
  geom_bar(stat = 'identity') +
  labs(title=paste0("Changes in RSSH modules between NFM2 FR, Grant Award, Most Recent Revision (OBR), and NFM3 FR"),
       y='Budget (Millions)',
       x='Budget Versions') +
  scale_y_continuous(labels = function(x)x/1000000)+
  facet_grid(rows=vars(loc_name), cols = vars(simplified_mod), scales = "free")+
  theme_bw(base_size = 11)

png(outFiler, height = 8, width = 11.5, units = "in", res = 300)
r
dev.off()

png(outFiler2, height = 8, width = 12, units = "in", res = 300)
r2
dev.off()

