# ----------------------------------------------------
# David Phillips, Audrey Batzel and Francisco Rios Casas
# 
# 8/23/2019
# Measure drivers of mortality using using GBD estimates for TB and malaria
# Code adapted from ./gf/impact_evaluation/gtm/mortality_drivers/mortality_drivers.r
# ----------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(GGally)
library(gridExtra)
library(boot)
library(RColorBrewer)
# --------------------


# ------------------------------------------------------------------------
# Files and directories

# root directory
dir = 'J:/Project/Evaluation/GF/impact_evaluation/mortality/'

# input file
inFile = paste0(dir, 'prepped_data/tb_malaria_pce_countries_data.rds')

# output file
outFile1 = paste0(dir, '/visualizations/mortality_explained_variance.pdf')
outFile2 = paste0(dir, '/visualizations/mortality_data.pdf')
outFile3 = paste0(dir, '/visualizations/mortality_incidence_trends.pdf')
# ------------------------------------------------------------------------


# --------------------------------------------
# load/prep GBD mortality estimates data

# load file prepped by 1_prep_data.R
data = readRDS(inFile)

# rename
setnames(data, 'Deaths', 'mortality_rate')
setnames(data, 'Incidence', 'incidence_rate')

# drop malaria in guatemala because there's nearly zero mortality
data = data[!which(country=='gtm' & disease=='malaria')]
# --------------------------------------------


# -----------------------------------------------------------------------
# Set up for analysis

# define smithsonTransform function
smithsonTransform = function(x) { 
  N=length( x[!is.na(x)] )
  prop_lsqueeze = logit(((x*(N-1))+0.5)/N)
}

# transform using offset log (offset computed by country-disease) or smithson
byVars = c('country','disease')
data[mortality_rate>0, offset1:=quantile(mortality_rate,.01), by=byVars]
data[incidence_rate>0, offset2:=quantile(incidence_rate,.01), by=byVars]
data[mi_ratio>0, offset3:=quantile(mi_ratio,.01), by=byVars]
data[, log_mortality_rate:=log(mortality_rate+offset1)]
data[, log_incidence_rate:=log(incidence_rate+offset2)]
data[, logit_mi_ratio:=smithsonTransform(mi_ratio)]

# z-standardize by country-disease
zstd = function(x) { (x-mean(x))/sd(x) }
data[, mortality_rate_std:=zstd(mortality_rate), by=byVars]
data[, log_incidence_rate_std:=zstd(log_incidence_rate), by=byVars]
data[, logit_mi_ratio_std:=zstd(logit_mi_ratio), by=byVars]
# -----------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# Estimate explained variance

# assign regression formula
form = as.formula('mortality_rate_std ~ log_incidence_rate_std + logit_mi_ratio_std')

# initialize explained variances
evs = NULL

# loop over country-diseases
i=1
for(c in unique(data$country)) { 
	for(d in unique(data$disease)) { 

		# subset to current country-disease
		tmpData = data[country==c & disease==d]

		# Get glm estimate
		lmFit = lm(form, tmpData)

		# set up frame to store explained variance
		ev = data.table(variable=names(coef(lmFit))[-1])

		# loop over variables and compute explained variance
		for(v in ev$variable) {
			# test for standardization
			if (round(mean(tmpData[[v]]),5)!=0 | round(sd(tmpData[[v]]),5)!=1) { 
				stop(paste('Variable', v, 'is not z-standardized'))
			}
			
			# compute explained variance using pseudo decomposition of r squared
			# (see Anusar Farooqui 2016. A Natural Decomposition of R2 in Multiple Linear Regression)
			est = coef(lmFit)[[v]] * cov(tmpData[[v]], lmFit$fitted.values)
			ev[variable==v, explained_variance:=est]
		}
		
		# store results
		ev[, country:=c]
		ev[, disease:=d]
		if (i==1) evs = copy(ev)
		if (i>1) evs = rbind(evs, ev)
		i=i+1
	}
}
# ---------------------------------------------------------------------------------


# ----------------------------------------------------
# Set up to graph

# set up graph data
graphData = copy(evs)
graphData[variable=='log_incidence_rate_std', 
          label:=paste('Incidence -', 
			round(explained_variance*100, 1),'%')]
graphData[variable=='logit_mi_ratio_std', 
          label:=paste('Case Fatality -', 
			round(explained_variance*100, 1),'%')]
graphData[variable=='Residuals', 
          label:=paste('Unexplained by Model -', 
			round(explained_variance*100, 1),'%')]


# colors
cols = brewer.pal(3, 'Paired')
cols = c(cols[c(3,2)], '#969696')

# caption
cap ='Case fatality approximated by mortality:incidence ratio\nMortality and incidence rates come from GBD 2017 national estimates'

# melt long for trend graph
long = melt(data, id.vars=c('country','disease','year'))
long[variable=='incidence_rate', label:='Incidence Rate']
long[variable=='mortality_rate', label:='Mortality Rate']
long[variable %in% c('incidence_rate', 'mortality_rate'), value:=value*100000]
# ----------------------------------------------------


# ----------------------------------------------------
# Graph explained variance

# open pdf
pdf(outFile1, height=5.5, width=8)

# graph EV by country/disease
for(c in unique(data$country)) { 
	for(d in unique(data$disease)) { 
		if (nrow(data[country==c & disease==d])==0) next
		p = ggplot(graphData[country==c & disease==d], 
			aes(y=explained_variance, x=1, fill=label)) + 
		  geom_bar(width=1, color='gray90', stat='identity', position='stack') + 
		  geom_text(aes(label=label), size=3, position=position_stack(vjust=.5)) +
		  annotate('text', label='Declining\nMortality\nRates', y=0, x=-0.5, size=5) +
		  coord_polar(theta='y') + 
		  scale_fill_manual('', values=cols) +
		  labs(title=paste(toupper(c), toupper(d)), caption=cap) + 
		  theme_void() + 
		  theme(legend.position='none')
		print(p)
	}
}

# close pdf
dev.off()
# ----------------------------------------------------


# ----------------------------------------------------
# Graph data behind each EV

# open pdf
pdf(outFile2, height=5.5, width=8)

# make graphs
vars = c('mortality_rate_std','log_incidence_rate_std','logit_mi_ratio_std')
for(c in unique(data$country)) { 
	for(d in unique(data$disease)) { 
		if (nrow(data[country==c & disease==d])==0) next
		p = ggpairs(data[country==c & disease==d, vars, with=F], 
			title=paste(toupper(c), toupper(d)))
		print(p)
	}
}

# close pdf
dev.off()
# ----------------------------------------------------


# ----------------------------------------------------
# Graph Gbd estimates

# open pdf
pdf(outFile3, height=5.5, width=8)

# graph national trends
vars = c('mortality_rate','incidence_rate')
for(c in unique(data$country)) { 
	for(d in unique(data$disease)) { 
		if (nrow(data[country==c & disease==d])==0) next
		# subset data for convenience
		tmpData = data[country==c & disease==d]
	
		# compute % change for each variable
		startY = min(tmpData$year)
		endY = max(tmpData$year)
		startI = tmpData[year==startY]$incidence_rate
		endI = tmpData[year==endY]$incidence_rate
		startM = tmpData[year==startY]$mortality_rate
		endM = tmpData[year==endY]$mortality_rate
		dI = log(endI/startI)/(endY-startY)
		dM = log(endM/startM)/(endY-startY)
		
		# add % change to data
		long[country==c & disease==d & variable=='incidence_rate', change:=dI]
		long[country==c & disease==d & variable=='mortality_rate', change:=dM]
		long[change<0, change_lab:=paste0(round(-change*100, 1), '% decrease\nper year')]
		long[change>=0, change_lab:=paste0(round(change*100, 1), '% increase\nper year')]
		
		p = ggplot(long[country==c & disease==d & variable %in% vars], 
			aes(y=value, x=year)) + 
		  geom_line(size=1.5, color='#33A02C') + 
		  geom_text(y=Inf, x=Inf, aes(label=change_lab), vjust=1.5, hjust=1.1) + 
		  facet_wrap(~label, scales='free_y') + 
		  scale_x_continuous(breaks=seq(startY, endY, by=2)) + 
		  labs(title='National Trends in Mortality and Incidence', 
			subtitle=paste(toupper(c), toupper(d)), y='Rate per 100,000 Population',
			x='', caption='Source: GBD 2018\nRates are Age-Standardized') + 
		  theme_bw(base_size=16)
		print(p)
	}
}

# close pdf
dev.off()
# ----------------------------------------------------
