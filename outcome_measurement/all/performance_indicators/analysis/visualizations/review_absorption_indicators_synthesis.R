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


### Additional changes to fix:
# * stratify by data quality ???? [subset to those that are most complete??? or just ihme-path vs EHG]
# * wrap by semester 1, 2, 3 ** this can be done for IHME grants only
# * add a delay on the budget period
# * can regional grants be added to this?
# * subset to countries or indicators where effect would be immediate 
# * include outcome indicators


# fix reverse coding as well
#dt$achievement_ratio <- NA
#dt$achievement_ratio_final <- ifelse(dt$reverse_indicator_final=="no", dt$achievement_ratio, 1/dt$achievement_ratio)

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
pdf("J:/Project/Evaluation/GF/outcome_measurement/multi_country/performance_indicators/pudr_indicator_extraction/analysis/visualizations/absorption_indicators_comparison_synthesis.pdf", width=10, height=8)
#First, run a scatter plot of achivement ratio vs. absorption percentage. 
ggplot(dt[], aes(x=absorption, y=achievement_ratio)) + 
  geom_point() + 
  theme_bw(base_size=18) +
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm")+
  stat_cor()

# plot one without the outcome indicators
ggplot(dt[type_desc=="Coverage"], aes(x=absorption, y=achievement_ratio)) + 
  geom_point() + 
  theme_bw(base_size=18) +
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm")+
  stat_cor()

#First, run a scatter plot of achivement ratio vs. absorption percentage. scale the point sizes 
ggplot(dt[], aes(x=absorption, y=achievement_ratio, size=cumulative_budget)) + 
  geom_point() + 
  theme_bw(base_size=18) +
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm")+
  stat_cor()+
  scale_size("Budget Total")

# add labels to the points and scale size of points
ggplot(dt[], aes(x=absorption, y=achievement_ratio, size=cumulative_budget)) + 
  geom_point() + 
  theme_bw(base_size=18) +
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm")+
  stat_cor(label.x=125, label.y = 1.75)+
  geom_text(nudge_y = .01,size=2.25, aes(label=ifelse(achievement_ratio>1.5 | achievement_ratio<0.5 | absorption > 99,paste(indicator_code," ",loc_name),"")))+
  scale_size("Budget Total")

# stratify by disease
ggplot(dt[], aes(x=absorption, y=achievement_ratio, color=disease)) + 
  geom_point() + 
  theme_bw(base_size=18)+
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm", se=FALSE)+
  stat_cor()

# stratify by budget category
ggplot(dt[], aes(x=absorption, y=achievement_ratio, color=abbrev_mod)) + 
  geom_point() + 
  theme_bw(base_size=18)+
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm", se=FALSE)+
  stat_cor()

# stratify by type of variable (outcome, coverage, impact)
ggplot(dt[], aes(x=absorption, y=achievement_ratio, color=type_desc)) + 
  geom_point() + 
  theme_bw(base_size=18)+
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm", se=FALSE)+
  stat_cor()

# stratify by budget category
ggplot(dt[], aes(x=absorption, y=achievement_ratio, color=abbrev_mod)) + 
  geom_point() + 
  theme_bw(base_size=18)+
  labs(title="Absorption vs. Achievement Ratio", x="Absorption (%)", y="Achievement Ratio", color="")+
  geom_smooth(method = "lm", se=FALSE)+
  stat_cor()# subset only to modules that might see an effect more quickly than others

dev.off() 

write.csv(dt, "C:/Users/frc2/Desktop/indic_absorb.csv")
