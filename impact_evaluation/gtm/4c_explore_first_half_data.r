# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# Exploratory visualizations to get a good sense of the impact evaluation data
# The current working directory should be the root of this repo (set manually by user)
# ------------------------------------------------

library(GGally)

# --------------------------------------------------------------------
# Load/prep data

# load data from step 4a
load(outputFile4a)

# load model object
source(paste0('./impact_evaluation/gtm/models/', modelVersion1, '.R'))

# load "node table" for convenient labels
nodeTable = fread(nodeTableFile1)

# sample n random health zones to graph
n = 6
dpts = sample(data$department, n)
sample = data[department %in% dpts]
sample_untr = untransformed[department %in% dpts]

#Grab financial variables only, and activity/output variables only to compare separately
fin_vars = names(data)[grep("dah_|gf_|ghe_", names(data))]
act_vars = names(data)[grep("_act|_out", names(data))]

fin_vars = c('department', 'date', fin_vars)
act_vars = c('department', 'date', act_vars)

fin_data = data[, fin_vars, with=FALSE]
act_data = data[, act_vars, with=FALSE]

fin_data = melt(fin_data, id.vars=c('department', 'date'))
act_data = melt(act_data, id.vars = c('department', 'date'))
# --------------------------------------------------------------------


# ----------------------------------------------
# Set up to graph

# parse model object
parsedModel = lavParseModelString(model) 
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))

# organize completeness variables last, remove date
complVars = modelVars[grepl('completeness',modelVars)]
modelVars = c(modelVars[!modelVars %in% complVars], complVars)
modelVars = modelVars[modelVars!='date']

# organize groups of variables
lhsVars = unique(parsedModel$lhs[parsedModel$op=='~'])
varGroups = lapply(lhsVars, function(v) { 
	c(v, parsedModel$rhs[parsedModel$lhs==v & parsedModel$rhs!='date'])
})

# melt long
long = melt(sample, id.vars=c('department','date'))
long = merge(long, nodeTable, by='variable', all.x=TRUE)
long[is.na(label), label:=variable]
# ----------------------------------------------

#----------------------------------------------------
# Limit graph data to 2012, the year the activities/outputs data starts 
# and limit variables to only ones currently used by model 
sample = sample[date>=2009, c(modelVars, 'department', 'date'), with=F]
sample_untr = sample_untr[date>=2009, c(modelVars, 'department', 'date'), with=F]
long = long[date>=2009 & variable%in%modelVars]
fin_data = fin_data[variable%in%modelVars]
act_data = act_data[variable%in%modelVars]
data = data[, c(modelVars, 'department', 'date'), with=F]

# -------------------------------------------------------------------
# Make histograms

#Limit datasets to only variables that are currently in the model
sample = sample[, c(modelVars, 'date', 'department'), with=F]
sample_untr = sample_untr[,  c(modelVars, 'date', 'department'), with=F]
long = long[variable%in%modelVars]

# transformed data as seen by the model
histograms = lapply(modelVars, function(v) {
	l = nodeTable[variable==v]$label
	ggplot(sample, aes_string(x=v)) + 
		geom_histogram() + 
		facet_wrap(~department, scales='free') + 
		labs(title=paste('Histograms of', l), y='Frequency', x=l, 
			subtitle=paste('Random Sample of', n, 'Departments'),
			caption='Variables are post-transformation. Transformations may include: 
			cumulative, log, logit and lag.') + 
		theme_bw()
})

# untransformed data
histograms_untr = lapply(modelVars, function(v) {
	l = nodeTable[variable==v]$label
	for(ext in c('_cumulative','lag_','lead_')) {
		if (grepl(ext, v)) v = gsub(ext,'',v) 
	}
	if (!v %in% names(sample_untr)) v = paste0('value_',v) 
	ggplot(sample_untr, aes_string(x=v)) + 
		geom_histogram() + 
		facet_wrap(~department, scales='free') + 
		labs(title=paste('Histograms of', l, '(Without Transformation)'), 
			y='Frequency', x=l, 
			subtitle=paste('Random Sample of', n, 'Departments')) + 
		theme_bw()
})
# -------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Make time series graphs
tsPlots = lapply(seq(length(varGroups)), function(g) {
	l = nodeTable[variable==lhsVars[g]]$label
	ggplot(long[variable%in%varGroups[[g]]], aes(y=value, x=date, color=label)) + 
		geom_line() + 
		facet_wrap(~department) + 
		labs(title=paste('Time series of variables related to', l), y='Value', x='Date', 
			subtitle=paste('Random Sample of', n, 'Departments'),
			caption='Variables are post-transformation. Transformations may include: 
			cumulative, log, logit and lag.') + 
		theme_bw()
}) 
# ---------------------------------------------------------------------------------------


# ----------------------------------------------
# Make correlation graphs
corPlots = lapply(seq(length(varGroups)), function(g) {
	l = nodeTable[variable==lhsVars[g]]$label
	vars = nodeTable[variable %in% varGroups[[g]]]
	leftout = varGroups[[g]][!varGroups[[g]] %in% vars$variable]
	vars = rbind(vars, data.table(variable=leftout, label=leftout), fill=TRUE)
	ggpairs(sample[, vars$variable, with=FALSE], 
		title=paste('Correlations between variables related to', l),
		columnLabels=vars$label, lower=list(continuous='smooth'))
})
# ----------------------------------------------

#----------------------------------------------
# make department time-series plots 
fin_plot = function(d){
  ggplot(fin_data[date>=2010 & department==d], aes(y=value, x=date)) + 
    geom_line() + 
    facet_wrap(~variable, scales='free') + 
    labs(title=paste('Time series of all financial vars for department ', d), y='Value', x='Date') + 
    theme_bw()
}

act_plot = function(d){
  ggplot(act_data[date>=2010 & department==d], aes(y=value, x=date)) + 
    geom_line() + 
    facet_wrap(~variable, scales='free') + 
    labs(title=paste('Time series of all activity/output vars for department ', d), y='Value', x='Date') + 
    theme_bw()
}
#----------------------------------------------

#Full time series of all municipalities for each column. 
# Once normal, and once log-transformed. 
cols = names(data)[!names(data)%in%c('department', 'date', 'year', 'min')]
cols = cols[!grepl('total', cols)]
pdf(paste0(visIeDir, 'log_transform_check_all.pdf'), height=5.5, width=9)
for (c in cols){
  print(c)
  #Normal graph 
  hist1 = hist(data[, get(c)], main=paste0(c, " Untransformed"))

  #Log transformed
  p = quantile(data[get(c)!=0], .01, na.rm=T)
  if (is.na(p) | is.infinite(p)){ p=0}
  data[, paste0(c, "_log"):=log(get(c)+p)]
  data[is.na(get(paste0(c, "_log"))) | is.infinite(get(paste0(c, "_log"))), (paste0(c, "_log")):=0] #Adding a step to turn NAs into 0. 
  hist2 = hist(data[, get(paste0(c, "_log"))], main=paste0(c, " Log-transformed"))
  
  #Graph 
  par(mfrow=c(1,2)) 
  print(hist1)
  print(hist2)
}
dev.off()

# --------------------------------
# Save file
print(paste('Saving:', outputFile4c)) 
pdf(outputFile4c, height=5.5, width=9)
for(d in unique(fin_data$department)){
  print(fin_plot(d))
}
for(d in unique(act_data$department)){
  print(act_plot(d))
}
for(i in seq(length(tsPlots))) { 
	print(tsPlots[[i]])
}
#Print one ggpairs plot of all variables in the model. 
print(ggpairs(data[, -c('department', 'date')], title="Correlations between all variables in model for all departments"))
for(i in seq(length(corPlots))) { 
	print(corPlots[[i]])
}
# for(i in seq(length(histograms))) { 
# 	print(histograms[[i]])
# 	print(histograms_untr[[i]])
# }
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile4c)
# --------------------------------


#Adding a correlation plot to check Isoniazid Distributed to Total Drugs Distributed, and Cases notified to cases started on treatment and MDR cases started on treatment. 
pdf("J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/check_linear_dependencies.pdf", height=5.5, width=9) 
for (d in unique(data$department)){
  subData = data[department==d]
  print(ggpairs(subData[, .(Isoniazid_Distributed_act_cumulative, Total_Drugs_Distributed_act_cumulative)], title=paste0("Checking correlation coefficients for dept. ", d)))
  print(ggpairs(subData[, .(Cases_Notified_out_cumulative, Cases_Started_on_Treatment_out_cumulative, MDR_Cases_Started_Treatment_out_cumulative)], title=paste0("Checking correlation coefficients for dept. ", d)))
}
dev.off() 