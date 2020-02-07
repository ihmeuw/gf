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

outFile_report = paste0(dir, 'outputs/', set, '_inc_mort_for_synthesis.pdf')
outFile_report2 = paste0(dir, 'outputs/', set, '_inc_mort_all_pce_countries.pdf')
outFile_report3 = paste0(dir, 'outputs/', set, '_coverage_all_pce_countries.pdf')

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
if (set=='gbd') data = data[year>=2000]
if (set=='who_unaids') data = data[year>=2010]

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

#--------------------------
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
  
  if (set=='gbd') years = '2000 - 2017'
  if (set=='who_unaids') years = '2010 - 2018'
  
  if (set=='who_unaids' & m=='Incidence') {
     cap = 'Source: WHO (TB/malaria); UNAIDS (HIV/AIDS)\n*Malaria incidence rate displayed per 1,000 population' 
  } else { cap = 'Source: WHO (TB/malaria); UNAIDS (HIV/AIDS)'}
  
  m_lite = tolower(m)

  p1 = ggplot(data[measure==m], aes(y=val, x=year, ymin=lower, ymax=upper, color=cause, fill=cause)) + 
    geom_ribbon(alpha=.2, colour=NA) + 
    geom_line(size=0.8) + 
    facet_wrap(~location, scales='free_y') +
    scale_y_continuous(limits=c(0,NA)) + 
    scale_color_manual('', values=colors) + 
    scale_fill_manual('', values=colors) + 
    labs(title=paste0('National trends in ', m_lite , ', ', years),
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
    scale_y_continuous(limits=c(0,NA)) + 
    scale_color_manual('', values=colors) + 
    scale_fill_manual('', values=colors) + 
    labs(title='National trends in mortality and incidence', 
         subtitle=c, y='Rate per 100,000 Population*',
         x='', caption=c1) + 
    theme_minimal(base_size=14) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
          plot.title=element_text(size=14), plot.subtitle=element_text(size=11), 
          axis.title.y=element_text(size=11), plot.caption=element_text(size=8))
  print(p2)
}


dev.off()

#------------------------------
# coverage figure

cov = readRDS(paste0(dir, 
      'prepped_data/', set, '_coverage_prepped.rds'))

# set the years depending on the data set
if (set=='gbd') years = '2000 - 2017'
if (set=='who_unaids') years = '2010 - 2018'
if (set=='who_unaids') cov = cov[2010 <= year]
if (set=='who_unaids') cov_cap = 'Sources: WHO (TB); UNAIDS (HIV/AIDS); IHME (malaria)'
cov[location=="Democratic Republic of the Congo", location:='DRC']

# drop global as we only have it for one disease
cov = cov[location!='Global']

pdf(outFile_report3, height=5.5, width=9)

ggplot(cov, aes(y=mean, x=year, ymin=lower, 
                ymax=upper, color=cause, fill=cause)) + 
    geom_ribbon(alpha=.2, colour=NA) + 
    geom_line(size=0.8) + 
    facet_wrap(~location, scales='free_y') +
    scale_y_continuous(limits=c(0,NA)) + 
    scale_color_manual('', values=colors) + 
    scale_fill_manual('', values=colors) + 
    labs(title=paste0('National trends in treatment coverage',
                      ', ', years),
         y='Percent (%)', x='', caption=cov_cap) + 
    theme_minimal(base_size=14) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
          plot.title=element_text(size=14), plot.subtitle=element_text(size=11), 
          axis.title.y=element_text(size=11), plot.caption=element_text(size=8)) 

dev.off()

#-------------------------------------





