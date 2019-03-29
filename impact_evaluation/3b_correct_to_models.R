# ----------------------------------------------
# David Phillips
# 
# 3/10/2019
# Use LBD/MAP model estimates to correct observed program data 
# for outcomes and impact
# ----------------------------------------------


source('./impact_evaluation/_common/set_up_r.r')


# ------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(outputFile2c)

# apply limits
data[ITN>1000, ITN:=NA]
data[SSCACT>50000, SSCACT:=NA]
data[!is.finite(SP_rate), SP_rate:=NA]
data[!is.finite(RDT_rate), RDT_rate:=NA]
data[ACTs_CHWs_rate>1000, ACTs_CHWs_rate:=NA]
data[mildMalariaTreated_rate>2, mildMalariaTreated_rate:=NA]
data[severeMalariaTreated_rate>2.5, severeMalariaTreated_rate:=NA]
data[newCasesMalariaMild_rate>100000, newCasesMalariaMild_rate:=NA]
data[newCasesMalariaSevere_rate>100000, newCasesMalariaSevere_rate:=NA]
data[malariaDeaths_rate>500, malariaDeaths_rate:=NA]
# ------------------------------------------------------------------


# ------------------------------------------------------------------------
# Correct directly-modeled indicators
modInds = c('mildMalariaTreated_rate', 'ITN_rate', 
	'newCasesMalariaMild_rate', 'malariaDeaths_rate')
compInds = c('act_coverage_rate','itn_coverage_rate', 
	'incidence_rate', 'mortality_rate')

for(i in seq(length(modInds))) {  
	for(h in unique(data$health_zone)) { 
		# store variables
		print(paste(i, h))
		m = modInds[i]
		c = compInds[i]
		
		# test
		if (sum(!is.na(data[health_zone==h][[m]]))==0) next

		# fit regressions
		form1 = as.formula(paste0(m, '~date'))
		form2 = as.formula(paste0(c, '~date'))
		fit1 = glm(form1, data[health_zone==h], family='gaussian', na.action=na.exclude)
		fit2 = glm(form2, data[health_zone==h], family='gaussian', na.action=na.exclude)

		# correct values
		preds = predict(fit2, newdata=data[health_zone==h])
		resids = residuals(fit1, newdata=data[health_zone==h], na.action=na.pass)
		
		# add uncertainty based on variance of the model's residuals
		v = var(data[health_zone==h][[c]] - preds, na.rm=TRUE)
		v = v + var(resids,na.rm=T)
		r = optim(1, fn=function(r) { abs(var(resids/r,na.rm=T)-v) })$par
		resids = resids/r
		
		# store adjusted values
		data[health_zone==h, (paste0(m, '_adj')):=preds + resids]
		data[health_zone==h & get(paste0(m, '_adj'))<0, (paste0(m, '_adj')):=0]
	}
}
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Correct other indicators
adjInds = c('ACTs_CHWs_rate', 'severeMalariaTreated_rate', 
		'SP_rate', 'newCasesMalariaSevere_rate')
compInds = c('mildMalariaTreated_rate', 'mildMalariaTreated_rate', 
	'mildMalariaTreated_rate', 'newCasesMalariaMild_rate')

for(i in seq(length(modInds))) {  
	a=adjInds[i]
	c=compInds[i]

	# adjust to maintain the same ratio with the corrected indicators
	data[, ratio:=get(a)/get(c)]
	data[, (paste0(a, '_adj')):=get(paste0(c,'_adj'))*ratio]
	data$ratio = NULL
}
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Graph
colors = c('Original Data'='black', 
	'LBD/MAP Estimate'='blue', 'Corrected Data'='red')
plots = list()
set.seed(1)
for(h in unique(data$health_zone)) {  
		
	plots[[h]] = lapply(1:4, function(i) { 
		m = modInds[i]
		c = compInds[i]
		tmp = data[health_zone==h]
		tmp[, y1:=get(m)]
		tmp[, y2:=get(c)]
		tmp[, y3:=get(paste0(m,'_adj'))]
		ggplot(tmp, aes(y=y1,x=date)) + 
			geom_point(aes(color='Original Data')) + 
			geom_line(aes(y=y2, color='LBD/MAP Estimate')) + 
			geom_point(aes(y=y3, color='Corrected Data')) +
			scale_color_manual('', values=colors) + 
			labs(title=m, y='', x='') + 
			theme_bw()
	})
}

# make one nice example for slides
h = 'libenge'
p1 = ggplot(data[health_zone=='libenge'], aes(y=RDT_rate, x=date)) + 
	geom_point() + 
	labs(title='RDTs Conducted per Suspected Case',y='', x='') + 
	theme_bw()
p2 = ggplot(data[health_zone=='libenge'], aes(y=newCasesMalariaMild_rate, x=date)) + 
	geom_point(aes(color='Original Data')) + 
	geom_line(aes(y=incidence_rate, color='LBD/MAP Estimate')) + 
	geom_point(aes(y=newCasesMalariaMild_rate_adj, color='Corrected Data')) +
	scale_color_manual('', values=colors) + 
	labs(title='Incidence Rate per 100,000 Population', y='', x='') + 
	theme_bw() + 
	theme(legend.position=c(0.225, 0.5), 
		legend.box.background=element_rect(colour='black'), 
		legend.background=element_blank(), 
		legend.title=element_blank())
# ------------------------------------------------------------------------


# --------------------------------
# Save intermediate file
saveRDS(data, outputFile3b)

# save graphs
pdf(outputFile3bGraphs, height=6, width=9)
grid.arrange(p1, p2, ncol=2, 
	top=textGrob('Example: Libenge Health Zone', gp=gpar(fontsize=18)))
for(h in names(plots)) { 
	grid.arrange(plots[[h]][[1]], plots[[h]][[2]], 
		plots[[h]][[3]], plots[[h]][[4]], 
		top=textGrob(h, gp=gpar(fontsize=18)))
}
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile3b)
# --------------------------------
