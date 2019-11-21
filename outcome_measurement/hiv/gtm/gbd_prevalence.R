# Guatemala HIV prevalence graoh, sex stratified
# For the HIV testing slide deck
# Caitlin O'Brien-Carelli
# 10/29/2019

library(data.table)
library(ggplot2)
#----------------------
# import the data 

dir = 'J:/Project/Evaluation/GF/outcome_measurement/gtm/hiv/'
dt = fread(paste0(dir, 'gbd_prevalence/IHME_GBD_2017_prevalence_by_sex.csv'))

#----------------------
# convert the percentages for labelling

setnames(dt, 'year', 'Year')
dt[ , val:=100*val]
dt[ , upper:=100*upper]
dt[ , lower:=100*lower]

#----------------------
# graph the data

ggplot(dt[sex!='Both'], aes(x=Year, y=val, color=sex))+
  geom_ribbon(aes(ymin=lower, ymax=upper, alpha=0.1), fill = '#d9d9d9', show.legend = FALSE)+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c('#b2182b', '#4575b4'))+
  theme_bw()+
  labs(y='HIV prevalence (%)',
       title = 'HIV prevalence in Guatemala, 1990 - 2017', 
       color='Sex', caption = 'Source: IHME GBD study')+
  theme(text = element_text(size=18))


#----------------------