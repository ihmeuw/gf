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
load(outputFile4b)

# load model object
source(paste0('./impact_evaluation/gtm/models/', modelVersion2, '.r'))

# load "node table" for convenient labels
nodeTable = fread(nodeTableFile2)

# sample n random health zones to graph
n = 6
dpts = sample(data$department, n)
sample = data[department %in% dpts]
sample_untr = untransformed[department %in% dpts]

#Grab financial variables only, and activity/output variables only to compare separately
outp_vars = names(data)[grep("_outp", names(data))]
outc_vars = names(data)[grep("_outc", names(data))]

outp_vars = c('department', 'date', outp_vars)
outc_vars = c('department', 'date', outc_vars)

outp_data = data[, outp_vars, with=FALSE]
outc_data = data[, outc_vars, with=FALSE]

outp_data = melt(outp_data, id.vars=c('department', 'date'))
outc_data = melt(outc_data, id.vars = c('department', 'date'))
# --------------------------------------------------------------------

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
# Limit graph data to START_YEAR, the year the activities/outputs data starts 
# and limit variables to only ones currently used by model 
sample = sample[date>=START_YEAR, c(modelVars, 'department', 'date'), with=F]
sample_untr = sample_untr[date>=START_YEAR, c(modelVars, 'department', 'date'), with=F]
long = long[date>=START_YEAR & variable%in%modelVars]
outc_data = outc_data[variable%in%modelVars]
outp_data = outp_data[variable%in%modelVars]
data = data[, c(modelVars, 'department', 'date'), with=F]

# -------------------------------------------------------------------
# Make histograms

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
outp_plot = function(d){
  ggplot(outp_data[date>=2010 & department==d], aes(y=value, x=date)) + 
    geom_line() + 
    facet_wrap(~variable, scales='free') + 
    labs(title=paste('Time series of all output vars for department ', d), y='Value', x='Date') + 
    theme_bw()
}

outc_plot = function(d){
  ggplot(outc_data[date>=2010 & department==d], aes(y=value, x=date)) + 
    geom_line() + 
    facet_wrap(~variable, scales='free') + 
    labs(title=paste('Time series of all outcome vars for department ', d), y='Value', x='Date') + 
    theme_bw()
}

# --------------------------------
# Save file
print(paste('Saving:', outputFile4d)) 
pdf(outputFile4d, height=5.5, width=9)
for(d in unique(outp_data$department)){
  print(outp_plot(d))
}
for(d in unique(outc_data$department)){
  print(outc_plot(d))
}
for(i in seq(length(tsPlots))) { 
  print(tsPlots[[i]])
}
for(i in seq(length(corPlots))) {
  print(corPlots[[i]])
}
# for(i in seq(length(histograms))) { 
#   print(histograms[[i]])
#   print(histograms_untr[[i]])
# }
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile4d)
# --------------------------------


#Full time series of all municipalities for each column. 
# Once normal, and once log-transformed. 
cols = names(data)[!names(data)%in%c('department', 'date', 'year', 'min')]
cols = cols[!grepl('total', cols)]
pdf(paste0(visIeDir, 'log_transform_second_half.pdf'), height=5.5, width=9)
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

