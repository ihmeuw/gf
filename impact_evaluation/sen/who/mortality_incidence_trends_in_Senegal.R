# ----------------------------------------------------
# Francisco Rios Casas
# 
# 11/9/2019
# Make a beautiful WHO_Unaids graph for report
# ----------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(colormap)
# --------------------


# ------------------------------------------------------------------------
# Files and directories

# root directory
dir = 'J:/Project/Evaluation/GF/impact_evaluation/'

# input files downlaoded from WHo/UNAIDS
inFile = paste0(dir, 'synthesis_epidemiology/prepped_data/who_unaids_prepped.rds')

# output file
outFile = paste0(dir, '/sen/visualizations/who/mortality_incidence_trends_in_Senegal.pdf')
# ------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# load/prep WHO/UNAIDS mortality and incidence estimates data

# load data
data = readRDS(inFile) # all data

# subset to senegal
data = data[location=="Senegal"]


# subset to age-standardized rates
a = 'Age-standardized'
#a = 'All Ages'
data = data[age==a & metric=='Rate']

# subset to both sexes combined
data = data[sex=='Both']

# subset to incidence and mortality
data = data[measure %in% c('Incidence','Deaths')]

# subset to 2000+
data = data[year>=2010]

# display incidence rates per 1000 for malaria
data[cause=='Malaria' & measure=='Incidence', val:=val/100]
data[cause=='Malaria' & measure=='Incidence', lower:=lower/100]
data[cause=='Malaria' & measure=='Incidence', upper:=upper/100]
# ---------------------------------------------------------------------------------


# Compute rates of change # -------------------------

# subset to first and last years
min = min(data$year)
mid = floor(median(data$year))
max = max(data$year)
rocs = data[year %in% c(min, mid, max)]

# reshape wide
rocs = dcast(rocs, measure+location+sex+age+cause+metric~year, value.var='val')
setnames(rocs, as.character(c(min, mid, max)), c(paste0('val', min), paste0('val', mid), paste0('val',max)))

# compute % change for each variable
rocs[, roc:=log(get(paste0('val',max))/get(paste0('val', min)))/(max-min)]

# look up specific country ROCs
# rocs[location=='Myanmar']

# ----------------------------------------------------------------------
# Aesthetic formatting for graphs

# relabel "deaths" to "mortality"
data[measure=='Deaths', measure:='Mortality']
rocs[measure=='Deaths', measure:='Mortality']





# add nice ROC labels
rocs[roc<0, roc_lab:=paste0(round(-roc*100, 1), '% decrease\nper year')]
rocs[roc>=0, roc_lab:=paste0(round(roc*100, 1), '% increase\nper year')]
rocs[, roc_lab_short:=paste0(round(roc*100, 1), '%')]

# set label coordinates manually (don't hate)
rocs[location=='Senegal' & measure=='Incidence' & cause=='Tuberculosis', label_x:=2013]
rocs[location=='Senegal' & measure=='Incidence' & cause=='Tuberculosis', label_y:=135]
rocs[location=='Senegal' & measure=='Incidence' & cause=='HIV/AIDS', label_x:=2015]
rocs[location=='Senegal' & measure=='Incidence' & cause=='HIV/AIDS', label_y:=16.5]
rocs[location=='Senegal' & measure=='Incidence' & cause=='Malaria', label_x:=2015.2]
rocs[location=='Senegal' & measure=='Incidence' & cause=='Malaria', label_y:=70]

rocs[location=='Senegal' & measure=='Mortality' & cause=='Tuberculosis', label_x:=2013]
rocs[location=='Senegal' & measure=='Mortality' & cause=='Tuberculosis', label_y:=20]
rocs[location=='Senegal' & measure=='Mortality' & cause=='HIV/AIDS', label_x:=2015]
rocs[location=='Senegal' & measure=='Mortality' & cause=='HIV/AIDS', label_y:=4]
rocs[location=='Senegal' & measure=='Mortality' & cause=='Malaria', label_x:=2015]
rocs[location=='Senegal' & measure=='Mortality' & cause=='Malaria', label_y:=32]

# add iso codes for short labels on cross-country graph
data[location=='Senegal', iso3:='SEN']

# set colors
colors = c('HIV/AIDS'=colormap()[1], 'Malaria'=colormap()[30], 'Tuberculosis'=colormap()[61])

# captions
c1 = 'Source: WHO/UNAIDS 2019\n*Malaria incidence rate displayed per 1,000 population'
c2 = 'Source: WHO 2019\nRates are age-standardized'
if (a!='Age-standardized') c1 = gsub('\nRates are age-standardized', '', c1) 
if (a!='Age-standardized') c2 = gsub('\nRates are age-standardized', '', c2) 
# ----------------------------------------------------------------------

# open pdf
pdf(outFile, height=5.5, width=9)

# graph national trends
p = ggplot(data, aes(y=val, x=year, ymin=lower, ymax=upper, color=cause, fill=cause)) + 
  geom_ribbon(alpha=.2, colour=NA) + 
  geom_line(size=1.5) + 
  geom_text(data=rocs, aes(y=label_y, x=label_x, label=roc_lab, color=cause), 
            inherit.aes=FALSE, show.legend=FALSE, vjust=0, hjust=0) + 
  facet_wrap(~measure, scales='free_y') + 
  scale_y_continuous(limits=c(0,NA)) + 
  scale_color_manual('', values=colors) + 
  scale_fill_manual('', values=colors) + 
  labs(title='National Trends in Mortality and Incidence', 
       subtitle="Senegal", y='Rate per 100,000 Population*',
       x='', caption=c1) + 
  theme_minimal(base_size=14) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        plot.title=element_text(size=14), plot.subtitle=element_text(size=11), 
        axis.title.y=element_text(size=11), plot.caption=element_text(size=8))

print(p)

# close pdf
dev.off()
# ----------------------------------------------------
