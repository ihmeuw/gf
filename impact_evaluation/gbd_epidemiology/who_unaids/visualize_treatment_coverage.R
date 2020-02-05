# visualize coverage estimates as percentages
# create a table of treatment coverage changes
# Caitlin O'Brien-Carelli
# 1/15/2020
# Updated by Emily Linebarger on 1/24/2020 to run on WHO/UNAIDS data 


rm(list = ls())
library(data.table)
library(ggplot2)
library(stringr)
library(colormap)
#-----------------------
# set the directories
dir = 'J:/Project/Evaluation/GF/outcome_measurement/multi_country/coverage/'

# load the malaria data 
dt = fread(paste0(dir, "raw_data/anti_malaria_1131.csv"))

tb = fread(paste0(dir,"raw_data/tb_tx_coverage.csv"))

# multiply by 100
art[ ,mean:=100*mean]
art[ ,lower:=100*lower]
art[ ,upper:=100*upper]

# bind them together
dt = rbind(dt, art)

#--------------------------------------
# prep tb 

# reset names and remove percent signs
setnames(tb, c('country', 'year', 'mean', 'lower', 'upper'))
tb[ , mean:=as.numeric(gsub('%', '', mean))]
tb[ , lower:=as.numeric(gsub('%', '',lower))]
tb[ , upper:=as.numeric(gsub('%', '', upper))]

# drop cameroon...
tb = tb[country!='Cameroon']

# merge into the data 
tb[ ,indicator:='TB treatment coverage']
dt = rbind(dt, tb)

#--------------------------------------
# prepare for display 

# replace drc
dt[country=='Democratic Republic of the Congo', country:='DRC']

#--------------------------------------
treat[cause=="HIV/AIDS", indicator:="ART Coverage"]
treat[cause=="Malaria", indicator:="Effective antimalarial coverage"]
treat[cause=="Tuberculosis", indicator:="TB treatment coverage"]

# pdf(paste0(dir, 'synthesis_graphs.pdf'), width=12, height=9)

# set colors
colors = c('ART Coverage'=colormap()[1], 'Effective antimalarial coverage'=colormap()[30], 
           'TB treatment coverage'=colormap()[61])

ggplot(dt, aes(y=mean, x=year, ymin=lower, ymax=upper, color=cause, fill=indicator)) + 
  geom_ribbon(alpha=.2, colour=NA) + 
  geom_line(size=0.8) + 
  facet_wrap(~country, scales='free_y') +
  scale_y_continuous(limits=c(0,NA)) + 
  scale_color_manual('', values=colors) + 
  scale_fill_manual('', values=colors) + 
  labs(title=paste0('Treatment coverage for HIV, TB and malaria, 2000 - 2017'),
       y='Percent (%)', x='Year', caption='Source: IHME Global Burden of Disease; TB coverage from the WHO') + 
  theme_minimal(base_size=14) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        plot.title=element_text(size=14), plot.subtitle=element_text(size=11), 
        axis.title.y=element_text(size=11), plot.caption=element_text(size=8)) 

#--------------------------------------
# visualize the data 

# # malaria on the same scale
# ggplot(dt[indicator=='Effective antimalarial coverage'], aes(x=year, y=mean))+
#   geom_point()+
#   geom_line()+
#   facet_wrap(~country)+
#   geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
#   labs(x='Year', y='Percent (%)', 
#        title='Effective antimalarial coverage',
#        subtitle = 'Percentage of malaria cases treated with effective antimalarial drugs')+
#       theme_bw()
# 
# # on individual scales
# ggplot(dt[indicator=='Effective antimalarial coverage'], aes(x=year, y=mean))+
#   geom_point()+
#   geom_line()+
#   geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
#   facet_wrap(~country, scales='free_y')+
#   labs(x='Year', y='Percent (%)', 
#        title='Effective antimalarial coverage (distinct scales)',
#        subtitle = 'Percentage of malaria cases treated with effective antimalarial drugs')+
#   theme_bw()
# 
# #--------------------------------------
# # ART coverage
# 
# # ART on the same scale
# ggplot(dt[indicator=='ART Coverage'], aes(x=year, y=mean))+
#   geom_point()+
#   geom_line()+
#   facet_wrap(~country)+
#   geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
#   labs(x='Year', y='Percent (%)', 
#        title='Percentage of PLHIV on ART')+
#   theme_bw()
# 
# # on individual scales
# ggplot(dt[indicator=='ART Coverage'], aes(x=year, y=mean))+
#   geom_point()+
#   geom_line()+
#   geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
#   facet_wrap(~country, scales='free_y')+
#   labs(x='Year', y='Percent (%)', 
#        title='Percentage of PLHIV on ART')+
#   theme_bw()
# 
# #--------------------------------------
# # tb on the same scale
# ggplot(dt[indicator=='TB treatment coverage'], aes(x=year, y=mean))+
#   geom_point()+
#   geom_line()+
#   facet_wrap(~country)+
#   geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
#   labs(x='Year', y='Percent (%)', 
#        title='TB treatment coverage',
#        subtitle = 'Percentage of notified TB cases treated, 2000 - 2018')+
#   theme_bw()
# 
# # tb on individual scales
# ggplot(dt[indicator=='TB treatment coverage'], aes(x=year, y=mean))+
#   geom_point()+
#   geom_line()+
#   geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
#   facet_wrap(~country, scales='free_y')+
#   labs(x='Year', y='Percent (%)', 
#        title='Percentage of notified TB cases treated, 2000 - 2018')+
#   theme_bw()
# 
# #--------------------------------------
# # all three variables on the same graph 
# 
# ggplot(dt, aes(x=year, y=mean, color=indicator))+
#   geom_point()+
#   geom_line()+
#   geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)+
#   facet_wrap(~country, scales='free_y')+
#   labs(x='Year', y='Percent (%)', color = "",
#        'Treatment coverage for HIV, TB and malaria, 2000 - 2017')+
#   theme_bw()
# 
# #--------------------------------------
# 
# dev.off()
# 
# #------------------------------------------------------
# # table calculating rate changes
# 
# #---------------------------------
# # calculate rocs
# 
# # subset and shape wide to calculate rates of change 
# roc = dt[year==2000 | year==2010 | year==2017]
# roc[ ,c('lower', 'upper'):=NULL]
# roc = dcast(roc, indicator+country~year)
# 
# # rename columns
# setnames(roc, c('2000', '2010', '2017'), c('y2000', 'y2010', 'y2017'))
# 
# # 2000 rates of change
# roc[ , roc2000:=round((log(y2017/y2000)/17), 3)]
# roc[  ,roc2000:=roc2000*100]
# 
# # 2010 rates of change
# roc[ , roc2010:=round((log(y2017/y2010)/7), 3)]
# roc[ , roc2010:=roc2010*100]
# 
# #---------------------------------
# # create the table and format the results for visualization
# 
# # round percentages to a single decimal and fix column names
# roc = roc[ ,lapply(.SD, round, 1), .SDcols=3:7, by=.(indicator, country)]
# 
# setnames(roc, c('Indicator', 'Country', '2000', '2010', '2017',
#                 '2000 - 2017', '2010 - 2017'))
# 
# #--------------------------
# # export to copy into report
# 
# write.csv(roc, paste0(dir, 'rocs_table.csv'))
# 
# #--------------------------
# 
# 
# 
# 
# 
