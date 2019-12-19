# ------------------------------------------------------
# AUTHOR: Emily Linebarger , Francisco Rios Casas
# PURPOSE: Review absorption/indicator data INC. EHG countries
# DATE: Last updated December 2019 for synthesis Report
# ------------------------------------------------------

library(data.table) 
library(ggplot2) 
library(scales) 
library(ggpubr)

dt = readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/analysis/subset_data/absorption_indicators_combined_synthesis.rds")

#----------------------------------------------------
# DATA QUALITY CORRECTIONS 
#----------------------------------------------------
#Where absorption is bigger than 200%, cap it. 
dt[absorption>200, absorption:=200]
#Where absorption is below 0, replace with 0. 
dt[absorption<0, absorption:=0]
# Where achievement ratio is above 2, cap at 2. 
dt[achievement_ratio>2, achievement_ratio:=2]

dt = dt[!is.na(achievement_ratio) & !is.na(absorption)]

# fix reverse coding as well


#Fix disease labeling 
#dt[disease=="hiv", disease:="HIV"]
#dt[disease=="tb", disease:="TB"]
#dt[disease=="malaria", disease:="Malaria"]

#Fix country labeling 
#dt[loc_name=="cod", loc_name:="DRC"]
#dt[, loc_name:=toupper(loc_name)]


#-----------------------------------
# PLOTS 
#-----------------------------------
pdf("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/analysis/visualizations/absorption_indicators_comparison3.pdf", width=10, height=8)
#First, run a scatter plot of achivement ratio vs. absorption percentage. 
ggplot(dt[], aes(x=absorption, y=achievement_ratio)) + 
  geom_point() + 
  theme_bw(base_size=18) +
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm")+
  stat_cor()

# stratify by disease
ggplot(dt[], aes(x=absorption, y=achievement_ratio, color=grant_disease)) + 
  geom_point() + 
  theme_bw(base_size=18)+
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm", se=FALSE)+
  stat_cor()

# include outcome indicators
ggplot(dt[], aes(x=absorption, y=achievement_ratio, color=loc_name)) + 
  geom_point() + 
  theme_bw(base_size=18)+
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm", se=FALSE)

# 
ggplot(dt[], aes(x=absorption, y=achievement_ratio, color=disease)) + 
  geom_point() + 
  theme_bw(base_size=18) + 
  facet_wrap(~start_date) + 
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm", se=FALSE)

#Now, look at how targets have changed over time for the same indicator code. 
dt[, grant_code:=paste0(grant, grant_period, indicator_code)]
ggplot(dt[start_date>="2018-01-01" & ihme_target_n<100], aes(x=start_date, y=ihme_target_n, group=grant_code, color=grant_code)) + 
  geom_point() + 
  geom_line() + 
  theme_bw(base_size=18) + 
  theme(legend.position = "none") + 
  labs(title="Change in targets over time", x="Date", y="Target", caption="*Each color represents a given grant and indicator code")
#Setting target less than 100 based on the median, which is 89. 

#Now, run for each disease, with panels representing country. 
for (c in c('DRC', 'SEN', 'UGA')){
  for (d in unique(dt$disease)) {
    subset = dt[start_date>="2018-01-01" & disease==d & loc_name==c]
    if (nrow(subset)>0){
      p = ggplot(subset, aes(x=absorption, y=achievement_ratio)) + 
        geom_point() + 
        theme_bw(base_size=18) + 
        facet_wrap(~start_date) + 
        labs(title=paste0("Absorption vs. Achievement Ratio for ", c, " ", d), x="Absorption (%)", y="Achievement Ratio", color="")
      print(p) 
    } 
  } 
} 

dev.off() 
