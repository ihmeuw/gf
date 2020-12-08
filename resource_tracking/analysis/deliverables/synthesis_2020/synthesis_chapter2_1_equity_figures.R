# make figures that will be included in the synthesis report

# clear workspace
rm(list=ls())

# set up
library(data.table)
library(ggplot2)
library(readxl)

# input file
box = paste0("C:/Users/frc2/Box Sync/Global Fund Files/")
inFile = paste0(box, 'synthesis/data/draft_synthesis_budget_quant.xlsx')
ehgFile = paste0(box, 'synthesis/data/Synthesis Budget Variance 181120.xlsx')
out.path = paste0(box, 'synthesis/figures/')

# read in IHME and EHG data
ihme_equity_data <- as.data.table(read_xlsx(path = inFile, sheet = "FR-GM HRG-Equity"))
ehg_equity_data <- as.data.table(read_xlsx(path = ehgFile, sheet = "HRG-Equity"))

# subset to relevant columns and countries
frgm_ihme_equity_data <- ihme_equity_data[,.(loc_name, gf_module, gf_intervention, label, nfm2_funding_request17, nfm2_approved)]
frgm_ehg_equity_data <- ehg_equity_data[loc_name %in% c('Cambodia', 'Myanmar', 'Sudan', 'Mozambique'),.(loc_name, gf_module, gf_intervention, nfm2_funding_request17, nfm2_approved)]

# # subset IHME data that were continuation grants-- malaria and HIV in Senegal and malaria for DRC
# frgm_ihme_equity_data <- frgm_ihme_equity_data[!grant%in%c('SEN-H-ANCS', 'SEN-H-CNLS', 'SEN-M-PNLP')] # Senegal grants without funding request data
# frgm_ihme_equity_data <- frgm_ihme_equity_data[!grant%in%c('COD-M-MOH', 'COD-M-MOH')] # DRC grants without funding request data

# label is missing from the EHG Equity data source
frgm_ehg_equity_data[grepl(gf_module, pattern = 'prevention programs'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Prevention programs'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Comprehensive programs'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'PMTCT'), label := 'Other equity related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Differentiated HIV'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Multidrug-resistant'), label := 'Other equity related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Prevention for'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'KVPs'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Prevention'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Case management'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Prevention of mother-to-child transmission'), label := 'Other equity related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'human rights-'), label := 'Human rights related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'human rights'), label := 'Human rights related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Community response'), label := 'Other equity related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Community systems'), label := 'Other equity related investments']
frgm_ehg_equity_data[grepl(gf_module, pattern = 'Integrated service delivery '), label := 'Other equity related investments']

frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'community case management'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'Addressing stigma'), label := 'Human rights related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'Integration into national'), label := 'Other equity related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'Intermittent preventive treatment'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'Key populations'), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'Prevention and management of co-infections '), label := 'KVP related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'human rights'), label := 'Human rights related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'human rights-'), label := 'Human rights related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'Community TB care delivery'), label := 'Other equity related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'Community MDR-TB care delivery'), label := 'Other equity related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'Community TB/HIV care delivery'), label := 'Other equity related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'Living support'), label := 'Other equity related investments']
frgm_ehg_equity_data[grepl(gf_intervention, pattern = 'Differentiated HIV testing services'), label := 'Other equity related investments']

# merge cross consortia data together
cc_frgm_equity_data <- rbind(frgm_ihme_equity_data, frgm_ehg_equity_data, fill=TRUE)


# reshape the data for plotting
plot_equity <- melt(cc_frgm_equity_data, id.vars = c("loc_name", "gf_module", "label"),
                    measure.vars = c("nfm2_funding_request17", "nfm2_approved"),
                    variable.name = "version", value.name = "budget")

# fix typo in label
plot_equity <- plot_equity[label=="Other equity realted investments", label:="Other equity related investments"]

# sum data by module, intervention, disease, label
plot_equity <- plot_equity[,.(budget=sum(budget, na.rm = TRUE)), by=c('loc_name', 'gf_module', 'label', 'version')] 

# recreated figure on HRG-Equity Percent Change NFM2 FR-GM
data1 <- plot_equity[,.(budget=sum(budget, na.rm=TRUE)), by=c('loc_name', 'version')]
data1 <- dcast(data1, loc_name ~ version, value.var = "budget")

# calculate percent change in data1
data1$difference <- data1$nfm2_approved-data1$nfm2_funding_request17
data1$percent_change <- (data1$difference/data1$nfm2_funding_request17)*100
data1 <- data1[loc_name!="Sudan"]

data1$loc_name <- factor(data1$loc_name,
                         levels = c('Uganda',
                                    'Senegal',
                                    'Myanmar',
                                    'Mozambique',
                                    'Guatemala',
                                    'DRC',
                                    'Cambodia'))

# special synthesis figure for chapter 2.1  FR to GM shifts in equity
p0 <- ggplot(data1, aes(y=percent_change, x=loc_name)) +
  geom_bar(stat = 'identity', color="#66C2A5", fill="#66C2A5") +
  coord_flip()+
  labs(title=paste0("HRG-Equity Percent Change NFM2 FR-GM"),
       y='Percent change',
       x='',
       fill = '')+
  theme_minimal(base_size=14)

p0
ggsave("HRG-Equity_summary_figure.png", plot = p0, path = out.path, width = 7.5, height = 5, units = "in")


# create figure that shows change in various subcategories of Equity investements
data2 <- plot_equity[,.(budget=sum(budget, na.rm=TRUE)), by=c('loc_name', 'label', 'version')]
data2 <- dcast(data2, loc_name + label ~ version, value.var = "budget")
data2$difference <- data2$nfm2_approved-data2$nfm2_funding_request17
data2$percent_change <- data2$difference/data2$nfm2_funding_request17
data2 <- data2[loc_name!="Sudan"]
data2$percent_change <- data2$percent_change*100

data2$loc_name <- factor(data2$loc_name,
                         levels = c('Uganda',
                                    'Senegal',
                                    'Myanmar',
                                    'Mozambique',
                                    'Guatemala',
                                    'DRC',
                                    'Cambodia'))


p1 <- ggplot(data2, aes(y=percent_change, x=loc_name)) +
  geom_bar(stat = 'identity', color="#66C2A5", fill="#66C2A5") +
  coord_flip()+
  labs(title=paste0("HRG-Equity Percent Change NFM2 FR-GM"),
       y='Percent change',
       x='',
       fill = '',
       caption = "Exact percent increase in Human Rights cannot be calculated for Senegal and DRC since funds in NFM2 FR were 0. ")+
  facet_grid(~label)+
  theme_minimal(base_size=12)
p1
ggsave("HRG-Equity_percent_change_frgm.png", plot = p1, path = out.path, width = 7.5, height = 5, units = "in")


# #### CODE BELOW WAS TAKEN FROM FILE PREPPING OTHER EQUITY FIGURES--might need revising
# 
# 
# 
# 
# # sum across modules
# plot_equity <- plot_equity[, .(budget=sum(budget, na.rm=T)), by=c('loc_name', 'label', 'version')]
# 
# plot_equity[grepl(version, pattern = "nfm2_funding_request17"), simple_version := "NFM2FR"]
# plot_equity[grepl(version, pattern = "nfm2_approved"), simple_version := "Award"]
# plot_equity[grepl(version, pattern = "nfm2_most_recent_revision"), simple_version := "OBR"]
# plot_equity[grepl(version, pattern = "nfm3_funding_request20"), simple_version := "NFM3FR"]
# 
# plot_equity$simple_version <- factor(plot_equity$simple_version, 
#                                      levels = c("NFM2FR",
#                                                 "Award",
#                                                 "OBR",
#                                                 "NFM3FR"))
# 
# # sort the file version as a factor
# plot_equity$version <- factor(plot_equity$version, 
#                               levels = c("nfm3_funding_request20",
#                                          "nfm2_most_recent_revision",
#                                          "nfm2_approved",
#                                          "nfm2_funding_request17"))