# ----------------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
# 
# 12/11/2019
# Make a beautiful GBD graphic for reports
# ----------------------------------------------------

# --------------------
# Set up R
library(data.table)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(colormap)
# --------------------
# set the files to export pdfs 

outFile_report = paste0(dir, 'outputs/inc_mort_for_synthesis.pdf')
outFile_report2 = paste0(dir, 'outputs/inc_mort_all_pce_countries.pdf')
# ---------------------------------------
# load/prep GBD mortality estimates data

# load data
data = copy(dt)

# subset to age-standardized rates
a = 'Age-standardized'
data = data[age==a & metric=='Rate']

# subset to both sexes combined
data = data[sex=='Both']

# subset to incidence and mortality
data = data[measure %in% c('Incidence','Deaths')]

# subset to 2000+
data = data[year>=2000]

# relabel "deaths" to "mortality"
data[measure=='Deaths', measure:='Mortality']

#---------------------
# display incidence rates per 1000 for malaria for scale 
data[cause=='Malaria' & measure=='Incidence', val:=val/100]
data[cause=='Malaria' & measure=='Incidence', lower:=lower/100]
data[cause=='Malaria' & measure=='Incidence', upper:=upper/100]
# --------------------------------------------------------------
# --------------------------------------------------------------
# set rates of change
rocs = data[ , .(roc = roc), by=.(measure, location, cause, year)]

# ----------------------------------------------------------------------
# Aesthetic formatting for graphs

# add nice ROC labels
rocs[roc<0, roc_lab:=paste0(round(-roc, 1), '% decrease per year')]
rocs[roc>=0, roc_lab:=paste0(round(roc, 1), '% increase per year')]
rocs[, roc_lab_short:=paste0(round(roc, 1), '%')]
#-------------------
# set label coordinates manually (don't hate)
rocs[location=='Guatemala' & measure=='Incidence' & cause=='Tuberculosis', label_x:=2009]
rocs[location=='Guatemala' & measure=='Incidence' & cause=='Tuberculosis', label_y:=20]
rocs[location=='Guatemala' & measure=='Incidence' & cause=='HIV/AIDS', label_x:=2009]
rocs[location=='Guatemala' & measure=='Incidence' & cause=='HIV/AIDS', label_y:=12]
rocs[location=='Guatemala' & measure=='Incidence' & cause=='Malaria', label_x:=1999]
rocs[location=='Guatemala' & measure=='Incidence' & cause=='Malaria', label_y:=3]

rocs[location=='Guatemala' & measure=='Mortality' & cause=='Tuberculosis', label_x:=2000]
rocs[location=='Guatemala' & measure=='Mortality' & cause=='Tuberculosis', label_y:=2.75]
rocs[location=='Guatemala' & measure=='Mortality' & cause=='HIV/AIDS', label_x:=2011]
rocs[location=='Guatemala' & measure=='Mortality' & cause=='HIV/AIDS', label_y:=5.5]
rocs[location=='Guatemala' & measure=='Mortality' & cause=='Malaria', label_x:=2008]
rocs[location=='Guatemala' & measure=='Mortality' & cause=='Malaria', label_y:=0.5]

rocs[location=='Senegal' & measure=='Incidence' & cause=='Tuberculosis', label_x:=2009]
rocs[location=='Senegal' & measure=='Incidence' & cause=='Tuberculosis', label_y:=220]
rocs[location=='Senegal' & measure=='Incidence' & cause=='HIV/AIDS', label_x:=2001]
rocs[location=='Senegal' & measure=='Incidence' & cause=='HIV/AIDS', label_y:=5]
rocs[location=='Senegal' & measure=='Incidence' & cause=='Malaria', label_x:=2008]
rocs[location=='Senegal' & measure=='Incidence' & cause=='Malaria', label_y:=110]

rocs[location=='Senegal' & measure=='Mortality' & cause=='Tuberculosis', label_x:=2011.5]
rocs[location=='Senegal' & measure=='Mortality' & cause=='Tuberculosis', label_y:=40]
rocs[location=='Senegal' & measure=='Mortality' & cause=='HIV/AIDS', label_x:=2003]
rocs[location=='Senegal' & measure=='Mortality' & cause=='HIV/AIDS', label_y:=10]
rocs[location=='Senegal' & measure=='Mortality' & cause=='Malaria', label_x:=2004]
rocs[location=='Senegal' & measure=='Mortality' & cause=='Malaria', label_y:=65]

rocs[location=='DRC' & measure=='Incidence' & cause=='Tuberculosis', label_x:=2009]
rocs[location=='DRC' & measure=='Incidence' & cause=='Tuberculosis', label_y:=510]
rocs[location=='DRC' & measure=='Incidence' & cause=='HIV/AIDS', label_x:=2009]
rocs[location=='DRC' & measure=='Incidence' & cause=='HIV/AIDS', label_y:=70]
rocs[location=='DRC' & measure=='Incidence' & cause=='Malaria', label_x:=2002]
rocs[location=='DRC' & measure=='Incidence' & cause=='Malaria', label_y:=330]

rocs[location=='DRC' & measure=='Mortality' & cause=='Tuberculosis', label_x:=2003]
rocs[location=='DRC' & measure=='Mortality' & cause=='Tuberculosis', label_y:=100]
rocs[location=='DRC' & measure=='Mortality' & cause=='HIV/AIDS', label_x:=2002]
rocs[location=='DRC' & measure=='Mortality' & cause=='HIV/AIDS', label_y:=30]
rocs[location=='DRC' & measure=='Mortality' & cause=='Malaria', label_x:=2011]
rocs[location=='DRC' & measure=='Mortality' & cause=='Malaria', label_y:=140]

rocs[location=='Uganda' & measure=='Incidence' & cause=='Tuberculosis', label_x:=2011.5]
rocs[location=='Uganda' & measure=='Incidence' & cause=='Tuberculosis', label_y:=420]
rocs[location=='Uganda' & measure=='Incidence' & cause=='HIV/AIDS', label_x:=2008]
rocs[location=='Uganda' & measure=='Incidence' & cause=='HIV/AIDS', label_y:=190]
rocs[location=='Uganda' & measure=='Incidence' & cause=='Malaria', label_x:=2003]
rocs[location=='Uganda' & measure=='Incidence' & cause=='Malaria', label_y:=500]

rocs[location=='Uganda' & measure=='Mortality' & cause=='Tuberculosis', label_x:=2002]
rocs[location=='Uganda' & measure=='Mortality' & cause=='Tuberculosis', label_y:=30]
rocs[location=='Uganda' & measure=='Mortality' & cause=='HIV/AIDS', label_x:=2011]
rocs[location=='Uganda' & measure=='Mortality' & cause=='HIV/AIDS', label_y:=210]
rocs[location=='Uganda' & measure=='Mortality' & cause=='Malaria', label_x:=2000]
rocs[location=='Uganda' & measure=='Mortality' & cause=='Malaria', label_y:=130]

#--------------------------
# add iso codes for short labels on cross-country graph
data[location=='Guatemala', iso3:='GTM']
data[location=='DRC', iso3:='DRC']
data[location=='Senegal', iso3:='SEN']
data[location=='Uganda', iso3:='UGA']
data[location=='Myanmar', iso3:='MMR']
data[location=='Cambodia', iso3:='KHM']
data[location=='Sudan', iso3:='SDN']
data[location=='Mozambique', iso3:='MOZ']

# set colors
colors = c('HIV/AIDS'=colormap()[1], 'Malaria'=colormap()[30], 'Tuberculosis'=colormap()[61])

# captions
c1 = 'Source: GBD 2018\n*Malaria incidence rate displayed per 1,000 population'
c2 = 'Source: GBD 2018\nRates are age-standardized'
if (a!='Age-standardized') c1 = gsub('\nRates are age-standardized', '', c1) 
if (a!='Age-standardized') c2 = gsub('\nRates are age-standardized', '', c2) 
# ----------------------------------------------------------------------

# ----------------------------------------------------
# graphs for report 

pdf(outFile_report, height=5.5, width=9)

#-----------------------
# graphs with all countries for report

for (m in unique(data$measure)) {

  if (m=='Incidence') {ytitle='Rate per 100,000 Population*'
  cap = c1} else { ytitle='Rate per 100,000 Population'
    cap = c2 }

  p1 = ggplot(data[measure==m], aes(y=val, x=year, ymin=lower, ymax=upper, color=cause, fill=cause)) + 
    geom_ribbon(alpha=.2, colour=NA) + 
    geom_line(size=0.8) + 
    facet_wrap(~location, scales='free_y') +
    scale_y_continuous(limits=c(0,NA)) + 
    scale_color_manual('', values=colors) + 
    scale_fill_manual('', values=colors) + 
    labs(title=paste0('National Trends in ', m , ', 2000 - 2017'),
         y=ytitle, x='', caption=cap) + 
    theme_minimal(base_size=14) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
          plot.title=element_text(size=14), plot.subtitle=element_text(size=11), 
          axis.title.y=element_text(size=11), plot.caption=element_text(size=8)) 
  
  # print the graph
  print(p1) }

countries = data[ ,unique(location)]

dev.off()

#-----------------------
# graph national trends for each country 

pdf(outFile_report2, height=5.5, width=9)

for(c in countries) { 
  p2 = ggplot(data[location==c], aes(y=val, x=year, ymin=lower, ymax=upper, color=cause, fill=cause)) + 
    geom_ribbon(alpha=.2, colour=NA) + 
    geom_line(size=1.5) + 
    facet_wrap(~measure, scales='free_y') + 
    geom_text(data=rocs[location==c], aes(y=label_y, x=label_x, label=roc_lab, color=cause), 
              inherit.aes=FALSE, show.legend=FALSE, vjust=0, hjust=0) + 
    scale_y_continuous(limits=c(0,NA)) + 
    scale_color_manual('', values=colors) + 
    scale_fill_manual('', values=colors) + 
    labs(title='National Trends in Mortality and Incidence', 
         subtitle=c, y='Rate per 100,000 Population*',
         x='', caption=c1) + 
    theme_minimal(base_size=14) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
          plot.title=element_text(size=14), plot.subtitle=element_text(size=11), 
          axis.title.y=element_text(size=11), plot.caption=element_text(size=8))
  print(p2)
}


dev.off()

#-------------------------------
# # # one last graph with everything
# # plots = list()
# # i=1
# # for(m in c('Incidence','Mortality')) { 
# #   for(d in c('HIV/AIDS', 'Tuberculosis', 'Malaria')) {
# #     
# #     # dynamic titles
# #     if (d=='HIV/AIDS' & m=='Incidence') ytitle='Incidence Rate\nper 100,000 Population'
# #     if (d=='HIV/AIDS' & m=='Mortality') ytitle='Mortality Rate\nper 100,000 Population'
# #     if (d!='HIV/AIDS') ytitle = ''
# #     if (m=='Incidence') subtitle=d
# #     if (m!='Incidence') subtitle=''
# #     
# #     plots[[i]] = ggplot(data[cause==d & measure==m], aes(y=val, x=year, color=cause, group=interaction(location, cause))) + 
# #       geom_line(size=1, show.legend=FALSE) + 
# #       geom_text_repel(data=data[year==min & cause==d & measure==m], aes(x=year-1, label=iso3), 
# #                       color='black', size=2, show.legend=FALSE, box.padding=0, direction='y', segment.color='transparent') + 
# #       geom_text_repel(data=rocs[cause==d & measure==m], aes(y=val2017, x=max+1, label=roc_lab_short), 
# #                       color='black', size=2, show.legend=FALSE, box.padding=0, direction='y', segment.color='transparent') + 
# #       scale_y_continuous(limits=c(0,NA)) + 
# #       scale_color_manual('', values=colors) + 
# #       labs(title=subtitle, y=ytitle, x='') + 
# #       theme_minimal(base_size=14) + 
# #       theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
# #             plot.title=element_text(size=11), axis.title.y=element_text(size=11), plot.caption=element_text(size=8))
# #     i=i+1
# #   }
# # }
# # grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=3)
# # 
# # # close pdf
# # dev.off()
# 
