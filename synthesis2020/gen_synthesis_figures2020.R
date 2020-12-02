##====== PCE Synthesis 2020 Figures ======##
# Authors: Thomas Glucksman
# Code to generate each set of figures will be organized according to the synthesis workshop slide deck
# Standard output to pdf, though figure dimensions may vary
#   ggsave(filename, plot, device = "pdf", path = "figures/...", width, height, units = "in")


## ====== SETUP ====== ##
# clear env
rm(list = ls())

# packages
library(ggplot2)
library(xlsx)
library(dplyr)
library(reshape2)
library(stringr)
library(RColorBrewer)

# set directories
user = as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  data.path = paste0("C:/Users/", user, "/Box Sync/Global Fund Files/synthesis/data/")
} else {
  #... set path yourself if not using windows
  data.path = ""
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
out.path <- "figures/"

##====== LOAD DATA ======##

# budgets
budget_total = read.xlsx(paste0(data.path, "draft_synthesis_budget_quant.xlsx"), sheetName = "National Program")
budget_rssh = read.xlsx(paste0(data.path, "draft_synthesis_budget_quant.xlsx"), sheetName = "RSSH")
budget_hrg = read.xlsx(paste0(data.path, "draft_synthesis_budget_quant.xlsx"), sheetName = "HRG-Equity")

# absorption
# ...

# indicators
# ...

#Notes
# For DRC, remove all FR and GM funds that the variable disease/fr_disease = "malaria". 
# For Senegal remove all FR and GM funds that the variable disease = "hiv" or "malaria"
# Remove grants from budget figures that did not have funding requests
#   SEN-H-CNLS, SEN-H-ANCS, SEN-M-PNLP, COD-M-MOH, COD-M-SANRU 

no_fr <- c("SEN-H-CNLS", 'SEN-H-ANCS', 'SEN-M-PNLP', 'COD-M-MOH', 'COD-M-SANRU')

budget_total = budget_total %>%
  filter(!(grant %in% no_fr))

budget_rssh = budget_rssh %>%
  filter(!(grant %in% no_fr))

budget_hrg = budget_hrg %>%
  filter(!(grant %in% no_fr))

##====== PLOTTING ======##



## FR to GM: Total Budgets
# 2017 total budgets by country, disease, FR vs GM
# possibly by simplified module

df1 = select(budget_total, loc_name, disease, nfm2_funding_request17, nfm2_approved) %>% 
  group_by(loc_name, disease) %>%
  summarise(nfm2_FR = sum(nfm2_funding_request17), nfm2_Approved = sum(nfm2_approved)) %>%
  melt(id.vars = c("loc_name", "disease"), variable_name = "budget_version") %>%
  mutate(value = value/(10^6)) %>% # convert to millions
  filter(value != 0)
  
p1 <- ggplot(df1, aes(x = variable, y = value, fill = disease)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = round(value)), position = position_stack(vjust = 0.5), size = 2) +
  coord_flip() +
  theme_bw() +
  facet_grid(loc_name~., switch = "y") +
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), type= "qual", palette = "Set2") +
  scale_x_discrete(limits = rev(levels(df1$variable))) + 
  scale_y_continuous(expand = expansion(add = 1))
  p1
ggsave("fr2gm_total_budget.png", plot = p1, path = out.path, width = 7, height = 5, units = "in")

## FR to GM: RSSH
# 2017 RSSH budgets by country, disease, FR vs GM
df2 = select(budget_rssh, loc_name, gf_module, nfm2_funding_request17, nfm2_approved) %>% 
  group_by(loc_name, gf_module) %>%
  summarise(nfm2_FR = sum(nfm2_funding_request17), nfm2_Approved = sum(nfm2_approved)) %>%
  melt(id.vars = c("loc_name", "gf_module"), variable_name = "budget_version") %>%
  mutate(value = value/(10^6)) %>%# convert to millions
  filter(value != 0) %>%
  mutate(gf_module = str_wrap(gf_module, width = 15))

# facet by loc
p2 <- ggplot(df2, aes(x = variable, y = value, fill = gf_module)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_grid(loc_name~., switch = "y") +
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), type = "qual", palette = "Set3") +
  scale_x_discrete(limits = rev(levels(df1$variable))) +
  scale_y_continuous(breaks = seq(0,30,5), expand = expansion(add = 0.1))
  p2

# facet by module 
p2.1 <- ggplot(df2, aes(x = variable, y = value, fill = loc_name)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_grid(gf_module~., switch = "y") +
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), type = "qual", palette = "Set3") +
  scale_x_discrete(limits = rev(levels(df1$variable))) +
  scale_y_continuous(breaks = seq(0,20,2.5), expand = expansion(add = 0.1))
  p2.1
  
ggsave("ft2gm_rssh_budget_fac_loc.png", plot = p2, path = out.path, width = 10, height = 5, units = "in")
ggsave("ft2gm_rssh_budget_fac_module.png", plot = p2.1, path = out.path, width = 10, height = 10, units = "in")

## FR to GM: HRG-Equity
# 2017 HRG budgets by country, module, FR vs GM
df3 = select(budget_hrg, loc_name, label, fr_disease, nfm2_funding_request17, nfm2_approved) %>% 
  group_by(loc_name, label, fr_disease) %>%
  summarise(nfm2_FR = sum(nfm2_funding_request17), nfm2_Approved = sum(nfm2_approved)) %>%
  melt(id.vars = c("loc_name", "label", "fr_disease"), variable_name = "budget_version") %>%
  mutate(value = value/(10^6)) # convert to millions

# facet loc
p3 <- ggplot(df3, aes(x = variable, y = value, fill = label)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_grid(loc_name~., switch = "y") +
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), palette = "Set2") +
  scale_x_discrete(limits = rev(levels(df1$variable))) +
  scale_y_continuous(breaks = seq(0,30,5),expand = expansion(add = 0.1))
p3

# facet fr_disease
p3.1 <- ggplot(filter(df3, value != 0), aes(x = variable, y = value, fill = label)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_grid(fr_disease~., switch = "y") +
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), palette = "Set2") +
  scale_x_discrete(limits = rev(levels(df1$variable))) +
  scale_y_continuous(breaks = seq(0,40,5), expand = expansion(add = 0.1))
p3.1

ggsave("ft2gm_hrg_budget_fac_loc.png", plot = p3, path = out.path, width = 10, height = 5, units = "in")
ggsave("fg2gm_hrg_budget_fac_disease.png", plot = p3.1, path = out.path, width = 10, height = 5, units = "in")


## Changing trajectory: Budgets
# change in overall budgets: NFM2 FR -> NFM2 GM -> NFM2 OBR -> NFM3 FR
# stack by disease, facet module
# same figures for RSSH/Equity

# total budgets trajectory
df4 = budget_total %>%
  select(loc_name, disease, nfm2_funding_request17, nfm2_approved, nfm2_most_recent_revision, nfm3_funding_request20) %>%
  group_by(loc_name, disease) %>%
  summarise(nfm2_FR = sum(nfm2_funding_request17), nfm2_approved = sum(nfm2_approved), nfm2_revision = sum(nfm2_most_recent_revision),
            nfm3_FR = sum(nfm3_funding_request20)) %>%
  melt(id.vars = c("loc_name", 'disease'), variable_name = "budget_version") %>%
  mutate(value = value/10^6) %>%
  filter(value != 0)

p4 <- ggplot(df4, aes(x = variable, y = value, fill = disease)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = round(value)), position = position_stack(vjust = 0.5), size = 2) +
  coord_flip() +
  theme_bw() +
  facet_grid(loc_name~., switch = "y")+
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), type= "qual", palette = "Set2") +
  scale_x_discrete(limits = rev(levels(df4$variable))) + 
  scale_y_continuous(breaks = seq(0,700,100), expand = expansion(add = 1))
p4

ggsave("trajectory_total_budget.png", plot = p4, path = out.path, width = 10, height = 7, units = "in")

# rssh budegets trajectory
df5 = budget_rssh %>%
  select(loc_name, fr_disease, gf_module, nfm2_funding_request17, nfm2_approved, nfm2_most_recent_revision, nfm3_funding_request20) %>%
  group_by(loc_name, fr_disease, gf_module) %>%
  summarise(nfm2_FR = sum(nfm2_funding_request17), nfm2_approved = sum(nfm2_approved), nfm2_revision = sum(nfm2_most_recent_revision),
            nfm3_FR = sum(nfm3_funding_request20)) %>%
  melt(id.vars = c("loc_name", "fr_disease", "gf_module"), variable_name = "budget_version") %>%
  mutate(value = value/10^6) %>%
  filter(value != 0) %>%
  mutate(gf_module_wrap = str_wrap(gf_module, width = 10))

# facet country, stack module
p5 <- ggplot(df5, aes(x = variable, y = value, fill = gf_module)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_grid(loc_name~., switch = "y")+
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), type= "qual", palette = "Set3") +
  scale_x_discrete(limits = rev(levels(df5$variable))) +
  scale_y_continuous(expand = expansion(add = 1))
  p5
  ggsave("trajectory_rssh_fac_loc1.png", path = out.path, plot = p5, width = 16, height = 10, units = "in")
  
# facet country, stack disease  
p5.1 <- ggplot(df5, aes(x = variable, y = value, fill = fr_disease)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_grid(loc_name~., switch = "y")+
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), type= "qual", palette = "Set2") +
  scale_x_discrete(limits = rev(levels(df5$variable))) +
  scale_y_continuous(expand = expansion(add = 1))
  p5.1
  ggsave("trajectory_rssh_fac_loc2.png", path = out.path, plot = p5.1, width = 15, height = 10, units = "in")
  
# facet disease, stack module
  p5.2 <- ggplot(df5, aes(x = variable, y = value, fill = gf_module)) +
    geom_bar(position = "stack", stat = "identity") +
    coord_flip() +
    theme_bw() +
    facet_grid(fr_disease~., switch = "y")+
    ylab("Millions (USD)") +
    xlab("") +
    theme(legend.position = "right",
          legend.title = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          panel.grid.major.y = element_blank()) +
    scale_fill_brewer(guide = guide_legend(reverse=TRUE), type= "qual", palette = "Set3") +
    scale_x_discrete(limits = rev(levels(df5$variable))) +
    scale_y_continuous(expand = expansion(add = 1))
  p5.2
  ggsave("trajectory_rssh_fac_disease.png", plot = p5.2, path = out.path, width = 16, height = 10, units = "in")
  
  # facet module/loc, stack disease
  p5.3 <- ggplot(df5, aes(x = variable, y = value, fill = fr_disease)) +
    geom_bar(position = "stack", stat = "identity") +
    coord_flip() +
    theme_bw() +
    facet_grid(gf_module_wrap~loc_name, switch = "y")+
    ylab("Millions (USD)") +
    xlab("") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          panel.grid.major.y = element_blank()) +
    scale_fill_brewer(guide = guide_legend(reverse=TRUE), type= "qual", palette = "Set2") +
    scale_x_discrete(limits = rev(levels(df5$variable))) +
    scale_y_continuous(expand = expansion(add = 1))
  p5.3
  
  ggsave("trajectory_rssh_fac_mod_loc.png", plot = p5.3, path = out.path, width = 10, height = 12, units = "in")
  
  
# hrg budegets trajectory
df6 = budget_hrg %>%
  select(loc_name, fr_disease, label, nfm2_funding_request17, nfm2_approved, nfm2_most_recent_revision, nfm3_funding_request20) %>%
  group_by(loc_name, fr_disease, label) %>%
  summarise(nfm2_FR = sum(nfm2_funding_request17), nfm2_approved = sum(nfm2_approved), nfm2_revision = sum(nfm2_most_recent_revision),
            nfm3_FR = sum(nfm3_funding_request20)) %>%
  melt(id.vars = c("loc_name", "fr_disease", "label"), variable_name = "budget_version") %>%
  mutate(value = value/10^6) %>%
  filter(value != 0) %>%
  mutate(label_wrap = str_wrap(label, width = 10))

# facet country, stack module
p6 <- ggplot(df6, aes(x = variable, y = value, fill = label)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_grid(loc_name~., switch = "y")+
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), type= "qual", palette = "Set2") +
  scale_x_discrete(limits = rev(levels(df6$variable))) +
  scale_y_continuous(expand = expansion(add = 1))
p6
ggsave("trajectory_hrg_fac_loc1.png", path = out.path, plot = p6, width = 15, height = 10, units = "in")

# facet country, stack disease
p6.1 <- ggplot(df6, aes(x = variable, y = value, fill = fr_disease)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_grid(loc_name~., switch = "y")+
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), type= "qual", palette = "Set2") +
  scale_x_discrete(limits = rev(levels(df6$variable))) +
  scale_y_continuous(expand = expansion(add = 1))
  p6.1
  ggsave("trajectory_hrg_fac_loc2.png", path = out.path, plot = p6.1, width = 15, height = 10, units = "in")
  
# facet disease, stack module
p6.2 <- ggplot(df6, aes(x = variable, y = value, fill = label)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_grid(fr_disease~., switch = "y")+
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), type= "qual", palette = "Set2") +
  scale_x_discrete(limits = rev(levels(df6$variable))) +
  scale_y_continuous(expand = expansion(add = 1))
  p6.2
  ggsave("trajectory_hrg_fac_disease.png", path = out.path, plot = p6.2, width = 15, height = 10, units = "in")
  
# facet module/loc, stack disease
p6.3 <- ggplot(df6, aes(x = variable, y = value, fill = fr_disease)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_grid(label~loc_name, switch = "y") +
  ylab("Millions (USD)") +
  xlab("") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        panel.grid.major.y = element_blank()) +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE), type= "qual", palette = "Set2") +
  scale_x_discrete(limits = rev(levels(df6$variable))) +
  scale_y_continuous(expand = expansion(add = 1))
p6.3
ggsave("trajectory_hrg_fac_mod_loc.png", path = out.path, plot = p6.3, width = 12, height = 10, units = "in")
