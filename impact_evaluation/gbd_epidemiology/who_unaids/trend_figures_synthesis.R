# ----------------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
# 
# 12/11/2019
# Make a beautiful GBD graphic for reports
# ----------------------------------------------------

# --------------------
# set the files to export pdfs 

outFile_report = paste0(dir, 'outputs/inc_mort_for_synthesis_unaids_who.pdf')
# ---------------------------------------
# load/prep GBD mortality estimates data

# load data
data = copy(dt)

# subset to metrics we want to map
data = data[(metric=='Rate' & measure%in%c('Incidence', 'Mortality')) | (measure=="Treatment coverage" & metric=="Percentage")]

# subset to 2000+
data = data[year>=2010]

# Make year a number 
data[, year:=as.integer(year)]

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

# Set different legend labels for treatment coverage. 
data[cause=="HIV/AIDS" & measure=="Treatment coverage", cause:="ART Coverage"]
data[cause=="Malaria" & measure=="Treatment coverage", cause:="Effective antimalarial\ncoverage"]
data[cause=="Tuberculosis" & measure=="Treatment coverage", cause:="TB treatment coverage"]

# Format tx coverage label for graph 
data[measure=='Treatment coverage', measure:="treatment coverage"]

# Drop out Guatemala treatment coverage 
data = data[!(measure=="treatment coverage" & location=="Guatemala")]
# ----------------------------------------------------
# graphs for report 

pdf(outFile_report, height=5.5, width=9)

#-----------------------
# graphs with all countries for report

for (m in unique(data$measure)) {
  
  if (m=='Incidence'){
    ytitle='Rate per 100,000 population*'
    colors = c('HIV/AIDS'=colormap()[1], 'Malaria'=colormap()[30], 'Tuberculosis'=colormap()[61])
    cap='Sources: WHO all forms TB, UNAIDS HIV, WHO malaria\n*Malaria incidence rate displayed per 1,000 population'
  } else if (m=="Mortality") { 
    ytitle='Rate per 100,000 population*'
    colors = c('HIV/AIDS'=colormap()[1], 'Malaria'=colormap()[30], 'Tuberculosis'=colormap()[61])
    cap='Sources: WHO all forms TB, UNAIDS HIV, WHO malaria'
  } else {
    ytitle="Percentage (%)*"
    colors = c('ART Coverage'=colormap()[1], 'Effective antimalarial\ncoverage'=colormap()[30], 
               'TB treatment coverage'=colormap()[61])
    cap='Sources: WHO all forms TB, UNAIDS HIV, Effective anti-malarial coverage estimated by GBD'
  }

  p1 = ggplot(data[measure==m], aes(y=val, x=year, ymin=lower, ymax=upper, color=cause, fill=cause)) + 
    geom_ribbon(alpha=.2, colour=NA) + 
    geom_line(size=0.8) + 
    facet_wrap(~location, scales='free_y') +
    scale_y_continuous(limits=c(0,NA)) + 
    scale_color_manual('', values=colors) + 
    scale_fill_manual('', values=colors) + 
    labs(title=paste0('National Trends in ', m , ', 2010 - 2018'),
         y=ytitle, x='', caption=cap) + 
    theme_minimal(base_size=14) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
          plot.title=element_text(size=14), plot.subtitle=element_text(size=11), 
          axis.title.y=element_text(size=11), plot.caption=element_text(size=8)) 
  
  # print the graph
  print(p1) }

dev.off()

