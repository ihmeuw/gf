# ----------------------------------------------
# David Phillips
# 
# 12/18/2018
# Generic template for a self-contained script
# ----------------------------------------------


# ----------------------------------------
# Set up R
source('./impact_evaluation/drc/set_up_r.r')
library(RColorBrewer)
# ----------------------------------------


# --------------------------------------------------------------------------
# Files and directories

# output files
graphFile = paste0(ieDir, '../visualizations/miscellaneous/sem_vs_glm_pc.pdf')
# --------------------------------------------------------------------------


# ----------------------------------------------
# Load/prep data

# load
load(outputFile5a)
data1=copy(data)
means1 = copy(means)
urFits1 = copy(urFits)
summaries1 = copy(summaries)
load(outputFile5b)
data2=copy(data)
means2 = copy(means)
summaries2 = copy(summaries)
urFits2 = copy(urFits)

# drop the single mistaken health zone that didn't run as per-capita
means1 = means1[grepl('_pc', lhs)]
urFits1 = urFits1[grepl('_pc', lhs)]

# collapse unrelated fits to national level
# (approximation of standard error, would be better as Monte Carlo simulation)
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
urFits1[, se_ratio.std:=se.std/est.std]
urFits1[, se_ratio:=se/est]
urMeans1 = urFits1[, lapply(.SD, mean), .SDcols=paramVars, by=c('lhs','op','rhs')]
urMeans1[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urMeans1[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
urFits2[, se_ratio.std:=se.std/est.std]
urFits2[, se_ratio:=se/est]
urMeans2 = urFits2[, lapply(.SD, mean), .SDcols=paramVars, by=c('lhs','op','rhs')]
urMeans2[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urMeans2[se>abs(se_ratio*est), se:=abs(se_ratio*est)]

# merge
model1 = merge(means1, urMeans1, by=c('lhs','op','rhs'), suffixes=c('.sem','.glm'))
model2 = merge(means2, urMeans2, by=c('lhs','op','rhs'), suffixes=c('.sem','.glm'))

# label variables
nodeTable1 = fread(nodeTableFile1)
nodeTable2 = fread(nodeTableFile2)
nodeTable1 = nodeTable1[, c('variable','label')]
nodeTable2 = nodeTable2[, c('variable','label')]
model1 = merge(model1, nodeTable1, by.x='rhs', by.y='variable', all.x=TRUE)
model1 = merge(model1, nodeTable1, by.x='lhs', by.y='variable', suffixes=c('.rhs','.lhs'), all.x=TRUE)
model2 = merge(model2, nodeTable2, by.x='rhs', by.y='variable', all.x=TRUE)
model2 = merge(model2, nodeTable2, by.x='lhs', by.y='variable', suffixes=c('.rhs','.lhs'), all.x=TRUE)
model1[is.na(label.rhs), label.rhs:=rhs]
model1[is.na(label.lhs), label.rhs:=lhs]
model2[is.na(label.rhs), label.rhs:=rhs]
model2[is.na(label.lhs), label.rhs:=lhs]

# drop intercept terms and append models together
model1[, model:=1]
model2[, model:=2]
models = rbind(model1, model2, fill=T)
models = models[op=='~']

# label sections
models[grepl('received',lhs), link:=1]
models[!grepl('received',lhs) & model==1, link:=2]
models[!grepl('case|death',lhs, ignore.case=TRUE) & model==2, link:=3]
models[grepl('case|death',lhs, ignore.case=TRUE) & model==2, link:=4]
models[grepl('received',lhs), section:='Inputs -> Activities']
models[!grepl('received',lhs) & model==1, section:='Activities -> Outputs']
models[!grepl('case|death',lhs, ignore.case=TRUE) & model==2, section:='Outputs -> Outcomes']
models[grepl('case|death',lhs, ignore.case=TRUE) & model==2, section:='Outcomes -> Impact']
# ----------------------------------------------


# ----------------------------------------------
# Graph

# colors
cols = brewer.pal(10, 'Paired')
cols = c(cols, rev(brewer.pal(12, 'Set3')))

# store graphs
plots = lapply(seq(4), function(l) { 
	section = unique(models[link==l]$section)
	ggplot(models[link==l], 
			aes(y=est.glm, x=est.sem, color=label.rhs, 
				ymax=est.glm+se.glm, ymin=est.glm-se.glm, 
				xmax=est.sem+se.sem, xmin=est.sem-se.sem)) + 
		geom_pointrange() + 
		geom_errorbarh(aes(height = 0)) + 
		geom_abline(slope=1, intercept=0) + 
		scale_color_manual('RHS Variable', values=cols) + 
		labs(title='Comparison of Estimates from GLM vs SEM', subtitle=section, 
			y='GLM', x='SEM', caption='Error bars show +/- 1 Standard Error') + 
		theme_bw()
})

# store graphs
controls = unique(models$rhs)
controls = controls[grepl('completeness|date|population', controls)]
plots_custom = lapply(seq(4), function(l) { 
	section = unique(models[link==l]$section)
	ggplot(models[link==l & !rhs %in% controls], 
			aes(y=est.glm, x=est.sem, color=label.rhs, 
				ymax=est.glm+se.glm, ymin=est.glm-se.glm, 
				xmax=est.sem+se.sem, xmin=est.sem-se.sem)) + 
		geom_pointrange() + 
		geom_errorbarh(aes(height = 0)) + 
		geom_abline(slope=1, intercept=0) + 
		scale_color_manual('RHS Variable', values=cols) + 
		labs(title='Comparison of Estimates from GLM vs SEM', subtitle=paste(section, '(Excluding Control Variables)'), 
			y='GLM', x='SEM', caption='Error bars show +/- 1 Standard Error') + 
		theme_bw()
})
# ----------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
for(p in seq(length(plots))) { 
	print(plots[[p]])
	print(plots_custom[[p]])
}
dev.off()
# --------------------------------
