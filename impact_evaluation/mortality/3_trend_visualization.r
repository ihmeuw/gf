# ----------------------------------------------------
# David Phillips
# 
# 11/9/2019
# Make a beautiful GBD graphic for reports
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
dir = 'J:/Project/Evaluation/GF/impact_evaluation/mortality/'

# input file downlaoded from GBD results tool on 11/9/2019
inFile = paste0(dir, 'prepped_data/IHME-GBD_2017_DATA-1cc0261b-1.csv')

# output file
outFile = paste0(dir, '/visualizations/mortality_incidence_trends.pdf')
# ------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# load/prep GBD mortality estimates data

# load data
data = fread(inFile)

# setset to IHME/PATH countries
countries = c('Uganda', 'Guatemala', 'Democratic Republic of the Congo', 'Senegal')
# data = data[location %in% countries]

# subset to age-standardized rates
a = 'Age-standardized'
a = 'All Ages'
data = data[age==a & metric=='Rate']

# subset to both sexes combined
data = data[sex=='Both']

# subset to incidence and mortality
data = data[measure %in% c('Incidence','Deaths')]

# subset to 2000+
data = data[year>=2000]

# display incidence rates per 1000 for malaria
data[cause=='Malaria' & measure=='Incidence', val:=val/100]
data[cause=='Malaria' & measure=='Incidence', lower:=lower/100]
data[cause=='Malaria' & measure=='Incidence', upper:=upper/100]
# ---------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Compute rates of change

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
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------
# Aesthetic formatting for graphs

# relabel "deaths" to "mortality"
data[measure=='Deaths', measure:='Mortality']
rocs[measure=='Deaths', measure:='Mortality']

# relabel "Democratic Republic of the Congo" to "DRC"
data[location=='Democratic Republic of the Congo', location:='DRC']
rocs[location=='Democratic Republic of the Congo', location:='DRC']

# add nice ROC labels
rocs[roc<0, roc_lab:=paste0(round(-roc*100, 1), '% decrease\nper year')]
rocs[roc>=0, roc_lab:=paste0(round(roc*100, 1), '% increase\nper year')]
rocs[, roc_lab_short:=paste0(round(roc*100, 1), '%')]

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
c1 = 'Source: GBD 2017\n*Malaria incidence rate displayed per 1,000 population'
c2 = 'Source: GBD 2017\nRates are age-standardized'
if (a!='Age-standardized') c1 = gsub('\nRates are age-standardized', '', c1) 
if (a!='Age-standardized') c2 = gsub('\nRates are age-standardized', '', c2) 
# ----------------------------------------------------------------------


# ----------------------------------------------------
# Graph Gbd estimates

# open pdf
pdf(outFile, height=5.5, width=9)

# graph national trends
for(c in countries) { 
	if (c=='Democratic Republic of the Congo') c='DRC' 
	p = ggplot(data[location==c], aes(y=val, x=year, ymin=lower, ymax=upper, color=cause, fill=cause)) + 
		geom_ribbon(alpha=.2, , colour=NA) + 
		geom_line(size=1.5) + 
		geom_text(data=rocs[location==c], aes(y=label_y, x=label_x, label=roc_lab, color=cause), 
			inherit.aes=FALSE, show.legend=FALSE, vjust=0, hjust=0) + 
		facet_wrap(~measure, scales='free_y') + 
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
	print(p)
}

# set malaria back to per 100,000 for final graph
data[cause=='Malaria' & measure=='Incidence', val:=val*100]
rocs[cause=='Malaria' & measure=='Incidence', val2017:=val2017*100]

# graph combined trends by disease
for(d in unique(data$cause)) { 
	p = ggplot(data[cause==d], aes(y=val, x=year, color=cause, group=interaction(location, cause))) + 
			geom_line(size=1, show.legend=FALSE) + 
			geom_text_repel(data=data[year==min & cause==d], aes(x=year-1, label=iso3), 
				color='black', size=2, show.legend=FALSE, box.padding=0, direction='y', segment.color='transparent') + 
			geom_text_repel(data=rocs[cause==d], aes(y=val2017, x=max+1, label=roc_lab_short), 
				color='black', size=2, show.legend=FALSE, box.padding=0, direction='y', segment.color='transparent') + 
			facet_wrap(~measure, scales='free_y') + 
			scale_y_continuous(limits=c(0,NA)) + 
			scale_color_manual('', values=colors) + 
			labs(title='National Trends in Mortality and Incidence', 
				subtitle=d, y='Rate per 100,000 Population*',
				x='', caption=c2) + 
			theme_minimal(base_size=14) + 
			theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
				plot.title=element_text(size=14), plot.subtitle=element_text(size=11), 
				axis.title.y=element_text(size=11), plot.caption=element_text(size=8))
	print(p)
}

# one last graph with everything
plots = list()
i=1
for(m in c('Incidence','Mortality')) { 
	for(d in c('HIV/AIDS', 'Tuberculosis', 'Malaria')) {
	
		# dynamic titles
		if (d=='HIV/AIDS' & m=='Incidence') ytitle='Incidence Rate\nper 100,000 Population'
		if (d=='HIV/AIDS' & m=='Mortality') ytitle='Mortality Rate\nper 100,000 Population'
		if (d!='HIV/AIDS') ytitle = ''
		if (m=='Incidence') subtitle=d
		if (m!='Incidence') subtitle=''
		
		plots[[i]] = ggplot(data[cause==d & measure==m], aes(y=val, x=year, color=cause, group=interaction(location, cause))) + 
				geom_line(size=1, show.legend=FALSE) + 
				geom_text_repel(data=data[year==min & cause==d & measure==m], aes(x=year-1, label=iso3), 
					color='black', size=2, show.legend=FALSE, box.padding=0, direction='y', segment.color='transparent') + 
				geom_text_repel(data=rocs[cause==d & measure==m], aes(y=val2017, x=max+1, label=roc_lab_short), 
					color='black', size=2, show.legend=FALSE, box.padding=0, direction='y', segment.color='transparent') + 
				scale_y_continuous(limits=c(0,NA)) + 
				scale_color_manual('', values=colors) + 
				labs(title=subtitle, y=ytitle, x='') + 
				theme_minimal(base_size=14) + 
				theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
					plot.title=element_text(size=11), axis.title.y=element_text(size=11), plot.caption=element_text(size=8))
		i=i+1
	}
}
grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=3)

# close pdf
dev.off()
# ----------------------------------------------------
	