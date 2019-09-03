# Drivers of morality analysis for Senegal
# ----------------------
library(data.table)
library(ggplot2)
library(GGally)
library(boot)
library(stats)
library(lavaan)
# ----------------------

# ----------------------------------------------------
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, "/Project/Evaluation/GF/impact_evaluation/mortality/")

inFile = paste0(dir, "prepped_data/tb_pce_data.RDS")
outFile_pairwise_figures = paste0(dir, "visualizations/pairwise_correlation_plots.pdf")
outFile_transformed_figures = paste0(dir, "visualizations/transformed_pairwise_correlation_plots.pdf")
# ----------------------------------------------------

# ----------------------------------------------------
# load 
data = readRDS(inFile)
# ----------------------------------------------------

# ----------------------------------------------------
# plot correlations
list_of_plots = list()
i = 1

for ( x in unique(data$country) ){
  list_of_plots[[i]]  = ggpairs(data[country==x, c('Deaths', 'Incidence', 'mi_ratio'), with=F]) + ggtitle(x)
  i = i+1
}

# save figures:
pdf(outFile_pairwise_figures, height = 10, width = 9)
for (plot in 1:length(list_of_plots)){
  print(list_of_plots[[plot]])
}
dev.off()
# ----------------------------------------------------

# ----------------------------------------------------
# transform variables
data[, log_incidence_rate:=log(Incidence)]
data[, logit_mi_ratio:=logit(mi_ratio)]

# plot transformed data correlations
list_of_plots2 = list()
i = 1

for ( x in unique(data$country) ){
  list_of_plots2[[i]]  = ggpairs(data[country==x, c('Deaths', 'log_incidence_rate', 'logit_mi_ratio'), with=F]) + ggtitle(x)
  i = i+1
}

# save figures:
pdf(outFile_transformed_figures, height = 10, width = 9)
for (plot in 1:length(list_of_plots2)){
  print(list_of_plots2[[plot]])
}
dev.off()
# ----------------------------------------------------

# ----------------------------------------------------
# Get glm estimate
lmFits = lm(Deaths ~ log_incidence_rate + logit_mi_ratio, data = data)
afs = anova(lmFits)
data.table(variable=rownames(afs), explained_variance=afs[['Sum Sq']]/sum(afs[['Sum Sq']]))
# ----------------------------------------------------


