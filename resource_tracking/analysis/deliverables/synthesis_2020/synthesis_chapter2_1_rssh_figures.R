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
ihme_rssh_data <- as.data.table(read_xlsx(path = inFile, sheet = "FR-GM RSSH"))
ehg_rssh_data <- as.data.table(read_xlsx(path = ehgFile, sheet = "RSSH"))

# subset to relevant columns and countries
frgm_ihme_rssh_data <- ihme_rssh_data[,.(loc_name, gf_module, nfm2_funding_request17, nfm2_approved)]
frgm_ehg_rssh_data <- ehg_rssh_data[loc_name %in% c('Cambodia', 'Myanmar', 'Sudan', 'Mozambique'),.(loc_name, gf_module, nfm2_funding_request17, nfm2_approved)]

# merge cross consortia data together
cc_frgm_rssh_data <- rbind(frgm_ihme_rssh_data, frgm_ehg_rssh_data, fill=TRUE)

# reshape the data for plotting
plot_rssh <- melt(cc_frgm_rssh_data, id.vars = c("loc_name", "gf_module"),
                    measure.vars = c("nfm2_funding_request17", "nfm2_approved"),
                    variable.name = "version", value.name = "budget")


# sum data by module, intervention, disease, label
plot_rssh <- plot_rssh[,.(budget=sum(budget, na.rm = TRUE)), by=c('loc_name', 'gf_module','version')]

# recreated figure on RSSH Percent Change NFM2 FR-GM
data1 <- plot_rssh[,.(budget=sum(budget, na.rm=TRUE)), by=c('loc_name', 'version')]
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

# special synthesis figure for chapter 2.1  FR to GM shifts in RSSH
p0 <- ggplot(data1, aes(y=percent_change, x=loc_name)) +
  geom_bar(stat = 'identity', color="#FC8D62", fill="#FC8D62") +
  coord_flip()+
  labs(title=paste0("RSSH Percent Change NFM2 FR-GM"),
       y='Percent change',
       x='',
       fill = '')+
  theme_minimal(base_size=14)

p0
ggsave("RSSH_percent_change_frgm.png", plot = p0, path = out.path, width = 7.5, height = 5, units = "in")

# keep disaggregation to the label level
# recreated figure on RSSH Percent Change NFM2 FR-GM
data2 <- plot_rssh[,.(budget=sum(budget, na.rm=TRUE)), by=c('loc_name', 'version', 'gf_module')]
data2 <- dcast(data2, loc_name + gf_module ~ version, value.var = "budget")

# calculate percent change in data1
data2$difference <- data2$nfm2_approved-data2$nfm2_funding_request17
data2$percent_change <- (data2$difference/data2$nfm2_funding_request17)*100
data2 <- data2[gf_module!="Laboratory systems"]
# data2 <- data2[nfm2_funding_request17!=0 & nfm2_approved!=0]

# # add in label
data2[grepl(gf_module, pattern = 'information system'), label := 'HMIS and M&E']
data2[grepl(gf_module, pattern = 'Human resources'), label := 'Human resources']
data2[grepl(gf_module, pattern = 'Integrated service delivery'), label := 'Integr Service Del']
data2[grepl(gf_module, pattern = 'Financial management'), label := 'Finance Manag Sys']
data2[grepl(gf_module, pattern = 'Community responses and systems'), label := 'Comm Resp & Sys'] # revised names
data2[grepl(gf_module, pattern = 'National health strategies'), label := 'Natl Health Strategies'] # revised names
data2[grepl(gf_module, pattern = 'Procurement'), label := 'Procuremnet & Supply Chain']


data2 <- data2[!is.na(label)]
data2$loc_name <- factor(data2$loc_name,
                         levels = c('Uganda',
                                    'Sudan',
                                    'Senegal',
                                    'Myanmar',
                                    'Mozambique',
                                    'Guatemala',
                                    'DRC',
                                    'Cambodia'))

data2 <- data2[loc_name!="Sudan"]

# special synthesis figure for chapter 2.1  FR to GM shifts in RSSH module categories
p1 <- ggplot(data2, aes(y=percent_change, x=loc_name)) +
  geom_bar(stat = 'identity', color="#FC8D62", fill="#FC8D62") +
  coord_flip()+
  labs(title=paste0("Change in RSSH modules NFM2 FR-GM"),
       y='Percent change',
       x='',
       fill = '')+
  theme_minimal(base_size=14)+
  facet_wrap(~label)

p1

ggsave("RSSH_percent_change_modules_frgm.png", plot = p1, path = out.path, width = 11, height = 7, units = "in")
