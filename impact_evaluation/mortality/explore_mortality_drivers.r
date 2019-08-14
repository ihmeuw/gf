# ----------------------------------------------------
# David Phillips
# 
# 7/25/2019
# Test alternate way of assessing drivers of mortality
# ----------------------------------------------------


# -------------------------------------------
rm(list=ls())
source('C:/Users/frc2/Documents/gf/impact_evaluation/drc/set_up_r.r')
library(GGally)
library(gridExtra)
outFile = 'J:/Project/Evaluation/GF/impact_evaluation/cod/visualizations/miscellaneous/mortality_explained_variance.pdf'
# -------------------------------------------


# ----------------------------------------------------
# load impact file
outputFile2c = 'J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/outcomes_impact.RDS'
data = readRDS(outputFile2c)
data[mortality_rate>250, mortality_rate:=NA]
data = data[date%%1==0 & date<=2015]
data[, mi_ratio:=mortality_rate/incidence_rate]

data = data[, c('health_zone','date','mortality_rate','incidence_rate','mi_ratio'), with=F]

# drop health zones that are completely missing or zero
data[, mortality_rate_sum:=sum(mortality_rate, na.rm=T), by='health_zone']
data[, incidence_rate_sum:=sum(incidence_rate, na.rm=T), by='health_zone']
data = data[mortality_rate_sum!=0 & incidence_rate_sum!=0]
data$mortality_rate_sum = NULL
data$incidence_rate_sum = NULL

# graph data
ggpairs(data[, c('mortality_rate','incidence_rate','mi_ratio'), with=F])

# transform
data[, log_mortality_rate:=log(mortality_rate)]
data[, log_incidence_rate:=log(incidence_rate)]
data[, logit_mi_ratio:=logit(mi_ratio)]

# graph transformed data
ggpairs(data[, c('log_mortality_rate','log_incidence_rate','logit_mi_ratio'), with=F])
# ----------------------------------------------------


# ----------------------------------------------------
# Get glm estimate

lmFits = lapply(unique(data$health_zone), function(h) { 
	lm(mortality_rate ~ log_incidence_rate + logit_mi_ratio, data[health_zone==h])
})
afs = lapply(lmFits, anova)

evs = lapply(seq(length(afs)), function(x) { 
	data.table(variable=rownames(afs[[x]]), explained_variance=afs[[x]][['Sum Sq']]/sum(afs[[x]][['Sum Sq']]), health_zone=unique(data$health_zone)[x])
})

evs = rbindlist(evs)
evs_mean = evs[, .(explained_variance=mean(explained_variance)), by='variable']
options(scipen=999)
evs_mean
# ----------------------------------------------------


# ----------------------------------------------------
# Graph

# graph example health zones
pdf(outFile, height=5.5, width=8)
for(h in unique(evs[variable=='logit_mi_ratio'][order(explained_variance)]$health_zone)) { 		
	evmi = round(evs[health_zone==h & variable=='logit_mi_ratio']$explained_variance,3)
	evinc = round(evs[health_zone==h & variable=='log_incidence_rate']$explained_variance,3)
	tmp = melt(data[health_zone==h], id.vars=c('health_zone','date'))
	tmp = tmp[!grepl('log',variable)]
	p=ggplot(tmp, aes(y=value, x=date)) + 
		geom_point() + 
		geom_line() + 
		facet_wrap(~variable, scales='free_y') + 
		labs(title=paste('Health Zone:', h), 
			subtitle=paste('Explained Variance by MI Ratio:',evmi,'\nExplained Variance by Incidence Rate:',evinc),
			x='') + 
		theme_bw()
	print(p)
}
dev.off()
# ----------------------------------------------------

